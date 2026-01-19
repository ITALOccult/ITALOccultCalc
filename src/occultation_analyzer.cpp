#include "ioccultcalc/occultation_analyzer.h"
#include "astdyn_wrapper.h"
#include "topocentric.h"
#include <cmath>
#include <chrono>
#include <iostream>

namespace ioccultcalc {

// ========== Costruttore ==========

OccultationAnalyzer::OccultationAnalyzer(const Config& config)
    : config_(config) {}

// ========== Metodi Statici di Utilità ==========

double OccultationAnalyzer::calculateApparentDiameter(double diameter_km, double distance_au) {
    const double AU_KM = 149597870.7;
    double distance_km = distance_au * AU_KM;
    double theta_rad = diameter_km / distance_km;
    return theta_rad * RAD_TO_DEG * 3600.0;
}

double OccultationAnalyzer::estimateDiameter(double H, double albedo) {
    return (1329.0 / std::sqrt(albedo)) * std::pow(10.0, -0.2 * H);
}

double OccultationAnalyzer::angularDistance(double ra1_deg, double dec1_deg,
                                            double ra2_deg, double dec2_deg) {
    double ra1 = ra1_deg * DEG_TO_RAD;
    double dec1 = dec1_deg * DEG_TO_RAD;
    double ra2 = ra2_deg * DEG_TO_RAD;
    double dec2 = dec2_deg * DEG_TO_RAD;
    
    double dra = ra2 - ra1;
    double ddec = dec2 - dec1;
    
    double a = std::sin(ddec/2) * std::sin(ddec/2) +
               std::cos(dec1) * std::cos(dec2) *
               std::sin(dra/2) * std::sin(dra/2);
    
    double c = 2.0 * std::atan2(std::sqrt(a), std::sqrt(1-a));
    return c * RAD_TO_DEG * 3600.0;
}

double OccultationAnalyzer::calculateShadowVelocity(const BesselianElements& bessel) {
    double v_er_h = std::sqrt(bessel.dx * bessel.dx + bessel.dy * bessel.dy);
    return v_er_h * 6378.137 / 3600.0;  // km/s
}

// ========== Metodi Privati Comuni ==========

double OccultationAnalyzer::findClosestApproachTime(
    std::function<double(double)> distance_func,
    double start_mjd,
    double end_mjd)
{
    const double invphi = (std::sqrt(5.0) - 1.0) / 2.0;
    const double invphi2 = (3.0 - std::sqrt(5.0)) / 2.0;
    
    double a = start_mjd;
    double b = end_mjd;
    double h = b - a;
    
    if (h <= config_.search_tolerance) return (a + b) / 2.0;
    
    int n = std::ceil(std::log(config_.search_tolerance / h) / std::log(invphi));
    
    double x1 = a + invphi2 * h;
    double x2 = a + invphi * h;
    double f1 = distance_func(x1);
    double f2 = distance_func(x2);
    
    for (int i = 0; i < n; i++) {
        if (f1 < f2) {
            b = x2;
            x2 = x1;
            f2 = f1;
            h = invphi * h;
            x1 = a + invphi2 * h;
            f1 = distance_func(x1);
        } else {
            a = x1;
            x1 = x2;
            f1 = f2;
            h = invphi * h;
            x2 = a + invphi * h;
            f2 = distance_func(x2);
        }
    }
    
    return (a + b) / 2.0;
}

double OccultationAnalyzer::estimateApparentVelocity(
    std::function<std::pair<double, double>(double)> ra_dec_getter,
    double mjd,
    double dt)
{
    auto [ra1, dec1] = ra_dec_getter(mjd);
    auto [ra2, dec2] = ra_dec_getter(mjd + dt);
    
    double dist_arcsec = angularDistance(ra1, dec1, ra2, dec2);
    double dt_sec = dt * 86400.0;
    return dist_arcsec / dt_sec;
}

// ========== LIVELLO 1: Check Veloce ==========

bool OccultationAnalyzer::isOccultation(
    double star_ra_deg,
    double star_dec_deg,
    std::shared_ptr<AstDynWrapper> astdyn,
    double search_start_mjd,
    double search_end_mjd,
    double asteroid_H,
    double asteroid_diameter_km)
{
    auto distance_func = [&](double mjd) {
        auto state = astdyn->getApparentStateGeocentric(mjd);
        return angularDistance(star_ra_deg, star_dec_deg, state.ra_deg, state.dec_deg);
    };
    
    double ca_mjd = findClosestApproachTime(distance_func, search_start_mjd, search_end_mjd);
    double min_dist_arcsec = distance_func(ca_mjd);
    
    auto state_ca = astdyn->getApparentStateGeocentric(ca_mjd);
    double diameter_km = (asteroid_diameter_km > 0) ? 
        asteroid_diameter_km : estimateDiameter(asteroid_H);
    
    double apparent_diam = calculateApparentDiameter(diameter_km, state_ca.distance_au);
    
    return min_dist_arcsec <= apparent_diam;
}

// ========== LIVELLO 2: Parametri Completi ==========

OccultationParameters OccultationAnalyzer::analyzeOccultation(
    double star_ra_deg,
    double star_dec_deg,
    double star_mag,
    uint64_t star_id,
    std::shared_ptr<AstDynWrapper> astdyn,
    double search_start_mjd,
    double search_end_mjd,
    double asteroid_H,
    double asteroid_diameter_km)
{
    OccultationParameters params;
    params.star_id = star_id;
    params.star_ra_deg = star_ra_deg;
    params.star_dec_deg = star_dec_deg;
    params.star_magnitude = star_mag;
    
    // Funzione distanza
    auto distance_func = [&](double mjd) {
        auto state = astdyn->getApparentStateGeocentric(mjd);
        return angularDistance(star_ra_deg, star_dec_deg, state.ra_deg, state.dec_deg);
    };
    
    // Trova CA
    double ca_mjd = findClosestApproachTime(distance_func, search_start_mjd, search_end_mjd);
    params.closest_approach_mjd = ca_mjd;
    params.closest_approach_arcsec = distance_func(ca_mjd);
    
    // Stato asteroide al CA
    auto state_ca = astdyn->getApparentStateGeocentric(ca_mjd);
    params.asteroid_ra_deg = state_ca.ra_deg;
    params.asteroid_dec_deg = state_ca.dec_deg;
    params.asteroid_distance_au = state_ca.distance_au;
    
    // Diametro
    params.asteroid_diameter_km = (asteroid_diameter_km > 0) ?
        asteroid_diameter_km : estimateDiameter(asteroid_H);
    
    params.apparent_diameter_arcsec = calculateApparentDiameter(
        params.asteroid_diameter_km, params.asteroid_distance_au);
    
    // DEBUG: Print values
    if (config_.verbose) {
        std::cout << "[OccAnalyzer] Star " << star_id << ": CA=" << params.closest_approach_arcsec 
                  << " arcsec, Diam=" << params.apparent_diameter_arcsec 
                  << " arcsec, Ratio=" << (params.closest_approach_arcsec / params.apparent_diameter_arcsec)
                  << std::endl;
    }
    
    // Occultazione?
    // Per calcolare shadow path anche per passaggi vicini (near-miss),
    // consideriamo "occultazione" qualsiasi cosa entro 2 arcsec o 10x diametro
    double diameter_threshold = params.apparent_diameter_arcsec;
    double near_miss_threshold = std::max(2.0, 10.0 * diameter_threshold);
    
    params.is_occultation = (params.closest_approach_arcsec <= near_miss_threshold);
    params.impact_parameter = params.closest_approach_arcsec / params.apparent_diameter_arcsec;
    
    if (config_.verbose) {
        std::cout << "[OccAnalyzer] Star " << star_id << " Result: " 
                  << (params.closest_approach_arcsec <= diameter_threshold ? "✅ HIT" : "⚠️ NEAR-MISS")
                  << " (CA=" << params.closest_approach_arcsec << "\", Diam=" << diameter_threshold << "\")" 
                  << std::endl;
    }
    
    // Velocità apparente
    auto ra_dec_getter = [&](double mjd) -> std::pair<double, double> {
        auto s = astdyn->getApparentStateGeocentric(mjd);
        return {s.ra_deg, s.dec_deg};
    };
    params.asteroid_velocity_arcsec_per_sec = estimateApparentVelocity(ra_dec_getter, ca_mjd);
    
    // Durata stimata
    if (params.is_occultation && params.asteroid_velocity_arcsec_per_sec > 0) {
        double r = params.apparent_diameter_arcsec / 2.0;
        double d = params.closest_approach_arcsec;
        
        if (d < r) {
            double chord = 2.0 * std::sqrt(r*r - d*d);
            params.estimated_duration_sec = chord / params.asteroid_velocity_arcsec_per_sec;
        } else {
            params.estimated_duration_sec = 0.0;
        }
    } else {
        params.estimated_duration_sec = 0.0;
    }
    
    return params;
}

// ========== LIVELLO 3: Shadow Path ==========

ShadowPathResult OccultationAnalyzer::calculateShadowPath(
    const OccultationParameters& params,
    std::shared_ptr<AstDynWrapper> astdyn)
{
    auto start_time = std::chrono::high_resolution_clock::now();
    
    ShadowPathResult result;
    result.parameters = params;
    result.shadow_computed = false;
    result.num_propagations = 0;
    
    // Calcoliamo la shadow path sempre per ogni candidato analizzato,
    // anche se è un near-miss, per mostrare la traiettoria sulla mappa.
    
    try {
        if (config_.verbose) std::cout << "[OccAnalyzer] Step 1: Besselian elements..." << std::endl;
        // Calculate Besselian elements at CA
        result.bessel = calculateBesselianElements(params, astdyn);
        result.num_propagations++;
        
        if (config_.verbose) std::cout << "[OccAnalyzer] Step 2: Velocity calculation..." << std::endl;
        // Calculate velocity (dx, dy) using numerical differentiation
        double dt = 1.0 / 1440.0;  // 1 minute
        if (config_.verbose) std::cout << "[OccAnalyzer]   Fetching state at dt..." << std::endl;
        auto state_dt = astdyn->getApparentStateGeocentric(params.closest_approach_mjd + dt);
        result.num_propagations++;
        
        if (config_.verbose) std::cout << "[OccAnalyzer]   Calculating Besselian elements at dt..." << std::endl;
        // Create temporary params for dt calculation
        OccultationParameters params_dt = params;
        params_dt.closest_approach_mjd += dt;
        params_dt.asteroid_ra_deg = state_dt.ra_deg;
        params_dt.asteroid_dec_deg = state_dt.dec_deg;
        
        auto bessel_dt = calculateBesselianElements(params_dt, astdyn);
        if (config_.verbose) std::cout << "[OccAnalyzer]   Velocity: x_dt=" << bessel_dt.x << ", y_dt=" << bessel_dt.y << std::endl;
        
        // Calculate derivatives (Earth radii per hour)
        double dt_hours = dt * 24.0;
        result.bessel.dx = (bessel_dt.x - result.bessel.x) / dt_hours;
        result.bessel.dy = (bessel_dt.y - result.bessel.y) / dt_hours;
        
        if (config_.verbose) std::cout << "[OccAnalyzer]   Derivatives: dx=" << result.bessel.dx << ", dy=" << result.bessel.dy << std::endl;

        // Calculate shadow velocity
        result.shadow_velocity_km_s = calculateShadowVelocity(result.bessel);
        result.max_duration_sec = params.asteroid_diameter_km / result.shadow_velocity_km_s;
        
        if (config_.verbose) std::cout << "[OccAnalyzer] Step 3: Generating points..." << std::endl;
        // Generate shadow path points
        double time_span_min = config_.shadow_time_span_min;
        double time_step = (2.0 * time_span_min) / (config_.shadow_points - 1) / 1440.0;
        double start_mjd = params.closest_approach_mjd - (time_span_min / 1440.0);
        
        for (int i = 0; i < config_.shadow_points; ++i) {
            double mjd = start_mjd + i * time_step;
            
            auto point = projectShadowPoint(result.bessel, mjd);
            if (point.has_value()) {
                result.shadow_path.push_back(*point);
            }
        }
        if (config_.verbose) std::cout << "[OccAnalyzer]   Done. Points: " << result.shadow_path.size() << std::endl;
        
        result.shadow_computed = true;
        
    } catch (const std::exception& e) {
        std::string err = std::string("Shadow path calculation failed: ") + e.what();
        result.error_message = err;
        result.shadow_computed = false;
        if (config_.verbose) {
            std::cout << "[OccAnalyzer] ❌ " << err << std::endl;
        }
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    result.calculation_time_sec = std::chrono::duration<double>(end_time - start_time).count();
    
    return result;
}

// ========== Metodi Privati per Shadow Path ==========

BesselianElements OccultationAnalyzer::calculateBesselianElements(
    const OccultationParameters& params,
    std::shared_ptr<AstDynWrapper> astdyn)
{
    BesselianElements bessel;
    bessel.epoch_mjd_tdb = params.closest_approach_mjd;
    
    // Get asteroid state at CA
    auto state = astdyn->getApparentStateGeocentric(params.closest_approach_mjd);
    
    // Convert RA/Dec/Distance to Cartesian (ICRF, geocentric, AU)
    double ra_rad = params.asteroid_ra_deg * DEG_TO_RAD;
    double dec_rad = params.asteroid_dec_deg * DEG_TO_RAD;
    double dist_au = state.distance_au;
    
    Eigen::Vector3d asteroid_pos(
        dist_au * std::cos(dec_rad) * std::cos(ra_rad),
        dist_au * std::cos(dec_rad) * std::sin(ra_rad),
        dist_au * std::sin(dec_rad)
    );
    
    // Star direction (unit vector)
    double star_ra_rad = params.star_ra_deg * DEG_TO_RAD;
    double star_dec_rad = params.star_dec_deg * DEG_TO_RAD;
    
    Eigen::Vector3d star_dir(
        std::cos(star_dec_rad) * std::cos(star_ra_rad),
        std::cos(star_dec_rad) * std::sin(star_ra_rad),
        std::sin(star_dec_rad)
    );
    
    // Shadow axis points from Earth toward star
    Eigen::Vector3d shadow_axis = star_dir.normalized();
    
    // Define fundamental plane coordinate system
    // Z-axis (k): along shadow axis (star_dir)
    // X-axis (i): perpendicular to star_dir and celestial north pole (direction of increasing RA)
    // Y-axis (j): perpendicular to star_dir and X-axis (direction of increasing Dec)
    Eigen::Vector3d north_pole(0, 0, 1);
    Eigen::Vector3d x_axis = north_pole.cross(shadow_axis).normalized();
    Eigen::Vector3d y_axis = shadow_axis.cross(x_axis);
    
    // Project asteroid position onto fundamental plane
    // The fundamental plane passes through the geocenter.
    // The shadow axis (k) points toward the star.
    // Asteroid position from geocenter: asteroid_pos (AU)
    
    // In our ICRF basis (x_axis, y_axis, shadow_axis):
    // Asteroid = x_au*x_axis + y_au*y_axis + z_au*shadow_axis
    double x_au = asteroid_pos.dot(x_axis);
    double y_au = asteroid_pos.dot(y_axis);
    double z_au = asteroid_pos.dot(shadow_axis);
    
    // 1 AU in km = 149597870.7
    // Earth equatorial radius in km = 6378.137
    const double AU_TO_RADIANS = 1.0; 
    const double AU_TO_EARTH_RADII = 149597870.7 / 6378.137; // ~23454.78
    
    bessel.x = x_au * AU_TO_EARTH_RADII;
    bessel.y = y_au * AU_TO_EARTH_RADII;
    
    // Calculate shadow radius
    double distance_km = dist_au * 149597870.7;  // AU to km
    double angular_radius_rad = (params.asteroid_diameter_km / 2.0) / distance_km;
    
    double z_km = z_au * 149597870.7;
    double shadow_radius_km = z_km * angular_radius_rad;
    double shadow_radius_earth_radii = shadow_radius_km / 6378.137;
    
    bessel.L1 = shadow_radius_earth_radii;  // Penumbra
    bessel.L2 = shadow_radius_earth_radii;  // Umbra (same for asteroids)
    
    // Declination and hour angle of shadow axis
    bessel.d = std::asin(shadow_axis.z());
    bessel.mu = std::atan2(shadow_axis.y(), shadow_axis.x());
    
    // Cone angles (small for asteroids)
    bessel.f1 = angular_radius_rad;
    bessel.f2 = angular_radius_rad;
    
    // Velocity will be calculated separately
    bessel.dx = 0.0;
    bessel.dy = 0.0;
    
    return bessel;
}

std::optional<AnalyzerShadowPoint> OccultationAnalyzer::calculateShadowPointAtTime(
    double mjd,
    const OccultationParameters& params,
    std::shared_ptr<AstDynWrapper> astdyn)
{
    // This will be called with Bessel elements from calculateShadowPath
    // For now, return empty - full implementation in next step
    return std::nullopt;
}

std::optional<AnalyzerShadowPoint> OccultationAnalyzer::projectShadowPoint(
    const BesselianElements& bessel,
    double mjd_tdb)
{
    AnalyzerShadowPoint point;
    point.mjd_tdb = mjd_tdb;
    
    // Calculate time offset from Bessel epoch
    double dt_hours = (mjd_tdb - bessel.epoch_mjd_tdb) * 24.0;
    
    // Update shadow position using velocity
    double x_t = bessel.x + bessel.dx * dt_hours;
    double y_t = bessel.y + bessel.dy * dt_hours;
    
    // Proper rotation from Fundamental Plane to ECEF
    // At mjd_tdb, we calculate GMST to go from ICRF to ECEF
    double mjd_ut1 = mjd_tdb - (69.0 / 86400.0);
    double T = (mjd_ut1 - 51544.5) / 36525.0;
    double gmst_deg = 280.46061837 + 360.98564736629 * (mjd_ut1 - 51544.5)
                      + 0.000387933 * T * T - T * T * T / 38710000.0;
    double gmst_rad = std::fmod(gmst_deg, 360.0) * DEG_TO_RAD;

    // Rotation Matrix from ICRF to ECEF (simplest form: just rotation around Z)
    Eigen::Matrix3d R_icrf_to_ecef;
    double cG = std::cos(gmst_rad);
    double sG = std::sin(gmst_rad);
    R_icrf_to_ecef <<  cG,  sG, 0.0,
                      -sG,  cG, 0.0,
                       0.0, 0.0, 1.0;

    // 4. Fundamental axis in ECEF
    // Bessel elements mu/d are RA/Dec of shadow axis in ICRF
    double d_rad = bessel.d;
    double mu_rad = bessel.mu;
    
    Eigen::Vector3d k_icrf(std::cos(d_rad)*std::cos(mu_rad), 
                           std::cos(d_rad)*std::sin(mu_rad), 
                           std::sin(d_rad));
    
    // Basis vectors of fundamental plane in ICRF
    Eigen::Vector3d north(0,0,1);
    Eigen::Vector3d j_icrf = (north - north.dot(k_icrf)*k_icrf).normalized();
    Eigen::Vector3d i_icrf = j_icrf.cross(k_icrf);

    // Rotate basis to ECEF
    Eigen::Vector3d i_ecef = R_icrf_to_ecef * i_icrf;
    Eigen::Vector3d j_ecef = R_icrf_to_ecef * j_icrf;
    Eigen::Vector3d k_ecef = R_icrf_to_ecef * k_icrf;

    // 5. Intersection of line P0 + zeta*k with ellipsoid: (X^2+Y^2)/a2 + Z^2/b2 = 1
    // Let P0 = x_t*i_ecef + y_t*j_ecef
    Eigen::Vector3d P0 = x_t * i_ecef + y_t * j_ecef;
    
    const double r_b_ratio = 6356.7523142 / 6378.137;
    const double r_b2 = r_b_ratio * r_b_ratio;

    double Ak = (k_ecef.x()*k_ecef.x() + k_ecef.y()*k_ecef.y()) + (k_ecef.z()*k_ecef.z()) / r_b2;
    double Bk = 2.0 * (P0.x()*k_ecef.x() + P0.y()*k_ecef.y() + (P0.z()*k_ecef.z()) / r_b2);
    double Ck = (P0.x()*P0.x() + P0.y()*P0.y()) + (P0.z()*P0.z()) / r_b2 - 1.0;

    double delta = Bk * Bk - 4.0 * Ak * Ck;
    double zeta;
    
    if (delta >= 0) {
        // Shadow axis hits the Earth. Use the surface point facing the star.
        zeta = (-Bk + std::sqrt(delta)) / (2.0 * Ak);
    } else {
        // Near-miss: shadow axis doesn't hit Earth.
        // Fallback: use the "substellar point" projection. 
        // This is the point on the ellipsoid closest to the shadow axis.
        // For a sphere, it would be the projection of k_ecef. 
        // We'll use the projection of k_ecef onto the ellipsoid as a "center line" fallback.
        zeta = -Bk / (2.0 * Ak); // Point of closest approach to ellipsoid surface
    }
    
    Eigen::Vector3d P = P0 + zeta * k_ecef;
    
    // Safety check: ensure P is not at the geocenter
    if (P.norm() < 1e-4) {
        P = k_ecef; // Just use the direction if something goes wrong
    }
    double x_ecef = P.x() * 6378.137;
    double y_ecef = P.y() * 6378.137;
    double z_ecef = P.z() * 6378.137;

    double dist_p = std::sqrt(x_ecef * x_ecef + y_ecef * y_ecef);
    if (dist_p < 1e-6) {
        point.lat_deg = (z_ecef > 0) ? 90.0 : -90.0;
        point.lon_deg = 0.0;
        return point;
    }
    
    // 6. Correct Iterative Geodetic Conversion
    const double wgs84_a = 6378.137;
    const double wgs84_b = 6356.7523142;
    const double wgs84_e2 = (wgs84_a*wgs84_a - wgs84_b*wgs84_b) / (wgs84_a*wgs84_a);
    
    auto to_geodetic = [&](const Eigen::Vector3d& p_ecef, double& lat, double& lon) {
        double d_p = std::sqrt(p_ecef.x()*p_ecef.x() + p_ecef.y()*p_ecef.y());
        if (d_p < 1e-6) {
            lat = (p_ecef.z() > 0) ? 90.0 : -90.0;
            lon = 0.0;
            return;
        }
        double l_rad = std::atan2(p_ecef.z(), d_p);
        for (int i = 0; i < 5; i++) {
            double s_phi = std::sin(l_rad);
            double n_val = wgs84_a / std::sqrt(1.0 - wgs84_e2 * s_phi * s_phi);
            l_rad = std::atan2(p_ecef.z() * 6378.137 + wgs84_e2 * n_val * s_phi, d_p * 6378.137);
        }
        lat = l_rad * RAD_TO_DEG;
        lon = std::atan2(p_ecef.y(), p_ecef.x()) * RAD_TO_DEG;
        if (lon > 180.0) lon -= 360.0;
        if (lon < -180.0) lon += 360.0;
    };

    to_geodetic(P, point.lat_deg, point.lon_deg);
    
    // 7. Calculate Limits by offset on fundamental plane
    // L1 is the radius in Earth radii.
    // The y-axis (j_ecef) is the north direction on the fundamental plane.
    double radius = bessel.L1;
    
    Eigen::Vector3d P_north = P0 + radius * j_ecef + zeta * k_ecef;
    Eigen::Vector3d P_south = P0 - radius * j_ecef + zeta * k_ecef;
    
    to_geodetic(P_north, point.geonorth_lat_deg, point.geonorth_lon_deg);
    to_geodetic(P_south, point.geosouth_lat_deg, point.geosouth_lon_deg);
    
    // For now, set sigma limits equal to geometric if no covariance is provided
    point.north_lat_deg = point.geonorth_lat_deg;
    point.north_lon_deg = point.geonorth_lon_deg;
    point.south_lat_deg = point.geosouth_lat_deg;
    point.south_lon_deg = point.geosouth_lon_deg;
    
    return point;
}

void OccultationAnalyzer::calculateGeometricLimits(
    AnalyzerShadowPoint& point,
    const Eigen::Vector3d& asteroid_pos_itrf,
    const Eigen::Vector3d& star_dir_itrf,
    double asteroid_radius_m)
{
    // TODO: Implementare calcolo limiti geometrici
}

void OccultationAnalyzer::calculateSigmaLimits(
    AnalyzerShadowPoint& point,
    const Eigen::Vector3d& asteroid_pos_itrf,
    const Eigen::Vector3d& star_dir_itrf,
    const Eigen::Vector3d& velocity_itrf,
    const Eigen::Matrix<double, 6, 6>& covariance)
{
    // TODO: Implementare calcolo limiti sigma
}

} // namespace ioccultcalc
