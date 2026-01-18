/**
 * @file phase2_occultation_geometry.cpp
 * @brief Implementazione FASE 2 - AstDyn Native & GSS Optimization
 */

#include "phase2_occultation_geometry.h"
#include <iostream>
#include <iomanip>
#include <cmath>
#include <algorithm>
#include <functional>
#include <nlohmann/json.hpp>
#include <fstream>
#include "topocentric.h"
#include "ioccultcalc/types.h"
#include "ioccultcalc/mpc_client.h"
#include "ioccultcalc/astdyn_interface.h"
#include "ioccultcalc/orbital_elements.h"
#include "ioccultcalc/asteroid_sqlite_db.h"

namespace ioccultcalc {

// Funzione helper per distanza angolare tra due punti (RA/Dec in gradi)
double angularDistanceDeg(double ra1, double dec1, double ra2, double dec2) {
    double r1 = ra1 * DEG_TO_RAD;
    double d1 = dec1 * DEG_TO_RAD;
    double r2 = ra2 * DEG_TO_RAD;
    double d2 = dec2 * DEG_TO_RAD;
    
    double cos_dist = std::sin(d1) * std::sin(d2) + 
                      std::cos(d1) * std::cos(d2) * std::cos(r1 - r2);
    if (cos_dist > 1.0) cos_dist = 1.0;
    if (cos_dist < -1.0) cos_dist = -1.0;
    return std::acos(cos_dist) * RAD_TO_DEG;
}

class Phase2OccultationGeometry::Impl {
public:
    std::shared_ptr<AstDynWrapper> astdyn;

    Impl() {
        std::cout << "[Phase2OccultationGeometry] Constructor started." << std::endl;
        astdyn = std::make_shared<AstDynWrapper>(PropagationSettings::highAccuracy());
        std::cout << "[Phase2OccultationGeometry] Constructor finished." << std::endl;
    }

    void refineOrbit(const Phase2Config& config) {
        if (!config.refine_orbit) return;

        std::string designation = astdyn->getObjectName();
        if (designation.empty()) return;

        std::cout << "[PHASE2] Refining orbit for asteroid " << designation 
                  << " using last " << config.last_n_obs << " observations..." << std::endl;

        try {
            MPCClient mpc;
            ObservationSet obsSet = mpc.getRecentObservations(designation, config.last_n_obs);
            
            if (obsSet.observations.empty()) {
                std::cerr << "[PHASE2] No observations found for refinement." << std::endl;
                return;
            }

            std::cout << "[PHASE2] Retrieved " << obsSet.observations.size() << " observations. "
                      << "Time range: MJD " << (obsSet.observations.front().epoch.jd - 2400000.5)
                      << " to " << (obsSet.observations.back().epoch.jd - 2400000.5) << std::endl;

            // Convert ObservationSet to RWOObservation for AstDynOrbitFitter
            std::vector<RWOObservation> rwoList;
            for (const auto& obs : obsSet.observations) {
                RWOObservation rwo;
                rwo.designation = designation;
                rwo.mjd_utc = obs.epoch.jd - 2400000.5;
                rwo.ra_deg = obs.obs.ra * RAD_TO_DEG;
                rwo.dec_deg = obs.obs.dec * RAD_TO_DEG;
                rwo.ra_sigma_arcsec = 0.5; // Default if not available
                rwo.dec_sigma_arcsec = 0.5;
                rwo.obs_code = obs.observatoryCode;
                rwoList.push_back(rwo);
            }

            // Get current elements from AstDynWrapper
            // We need to bridge AstDynWrapper elements to AstDySElements
            // This is a bit tricky as AstDynWrapper doesn't expose raw elements easily
            // For now, assume loadAsteroidFromEQ1 was called and we have them.
            // Actually, we can use astdyn_utils::toAstDySElements if we had KeplerianElements
            
            // For now, let's assume we use the initial elements loaded
            // In a real implementation we'd need a way to get 'em back.
            // Let's use a simplified approach: we trust the EQ1 file was loaded.
            
            // Perform bridge
            auto aelem = astdyn->getKeplerianElements();
            AstDySElements initial;
            initial.name = aelem.object_name;
            initial.a = aelem.semi_major_axis;
            initial.e = aelem.eccentricity;
            initial.i = aelem.inclination * RAD_TO_DEG;
            initial.Omega = aelem.longitude_asc_node * RAD_TO_DEG;
            initial.omega = aelem.argument_perihelion * RAD_TO_DEG;
            initial.M = aelem.mean_anomaly * RAD_TO_DEG;
            initial.epoch_mjd = aelem.epoch_mjd_tdb;
            initial.H = aelem.magnitude;
            initial.G = aelem.mag_slope;

            AstDynOrbitFitter fitter;
            fitter.setConvergenceTolerance(1e-10); // Better balance for 50 observations
            fitter.setVerbose(true); // Enable detailed convergence logging
            std::cout << "[PHASE2] Starting refined orbital fit (verbose mode)..." << std::endl;
            
            auto fitRes = fitter.fit(initial, rwoList);
            
            std::cout << "[PHASE2] Orbit fit completed. RMS=" << fitRes.rms_total_arcsec 
                      << " arcsec (" << fitRes.n_used << " obs used)" << std::endl;
            
            if (fitRes.n_used > 0) {
                // Estrare covarianza se presente
                std::optional<Eigen::Matrix<double, 6, 6>> cov_opt;
                if (fitRes.fitted_elements.has_covariance && fitRes.fitted_elements.covariance.size() == 21) {
                    Eigen::Matrix<double, 6, 6> cov = Eigen::Matrix<double, 6, 6>::Zero();
                    int idx = 0;
                    for (int i = 0; i < 6; ++i) {
                        for (int j = i; j < 6; ++j) {
                            cov(i, j) = fitRes.fitted_elements.covariance[idx++];
                            cov(j, i) = cov(i, j);
                        }
                    }
                    cov_opt = cov;
                }

                // Update wrapper
                astdyn->setKeplerianElements(
                    fitRes.fitted_elements.a, fitRes.fitted_elements.e, 
                    fitRes.fitted_elements.i * DEG_TO_RAD,
                    fitRes.fitted_elements.Omega * DEG_TO_RAD, 
                    fitRes.fitted_elements.omega * DEG_TO_RAD,
                    fitRes.fitted_elements.M * DEG_TO_RAD,
                    fitRes.fitted_elements.epoch_mjd,
                    designation,
                    astdyn::propagation::HighPrecisionPropagator::InputFrame::ECLIPTIC,
                    fitRes.fitted_elements.H, fitRes.fitted_elements.G, 0.0,
                    cov_opt
                );
            } else {
                std::cerr << "[PHASE2] WARNING: Orbit fit did not use any observations. Keeping initial orbit." << std::endl;
            }

        } catch (const std::exception& e) {
            std::cerr << "[PHASE2] Error during orbit refinement: " << e.what() << std::endl;
        }
    }

    void fetchHorizons(const std::vector<CandidateStar>& candidates, const Phase2Config& config) {
        if (!config.use_horizons || candidates.empty()) return;

        std::string designation = astdyn->getObjectName();
        if (designation.empty()) return;

        // Usa l'epoca del primo candidato come riferimento per gli elementi osculanti
        double target_mjd = candidates[0].closest_approach_mjd;
        std::cout << "[PHASE2] Fetching osculating elements from JPL Horizons for " << designation 
                  << " at MJD " << std::fixed << std::setprecision(5) << target_mjd << "..." << std::endl;

        if (astdyn->loadFromHorizons(designation, target_mjd)) {
            std::cout << "[PHASE2] Successfully initialized orbit from JPL Horizons." << std::endl;
        } else {
            std::cerr << "[PHASE2] WARNING: Failed to fetch JPL Horizons elements. Using existing orbit." << std::endl;
        }
    }

    Phase2OccultationEvent processSingleCandidate(const CandidateStar& star, const Phase2Config& config) {
        Phase2OccultationEvent event;
        event.star_id = star.source_id;

        // 0. REFINEMENT
        // Note: Refinement is now called once per session in calculatePreciseGeometry

        // 1. OTTIMIZZAZIONE GSS (Golden Section Search)
        // Cerchiamo il minimo della distanza angolare ICRF
        // Se star.closest_approach_mjd non è impostato da Phase 1, cerchiamo in tutto l'intervallo?
        // Assumiamo che star.closest_approach_mjd sia una stima valida (es. dal checkInitialCA)
        
        double t_ref = star.closest_approach_mjd;
        if (t_ref == 0) {
           // Fallback if MJD is not set (should not happen with new Phase 1)
           // But just in case, use a default date or error
        }

        double t_start = t_ref - (config.search_window_sec / 86400.0);
        double t_end = t_ref + (config.search_window_sec / 86400.0);
        
        auto computeDist = [&](double mjd) {
            // AstDynWrapper fornisce RA/Dec ICRF (Topocentrico/Geocentrico)
            auto state = astdyn->getApparentStateGeocentric(mjd);
            return angularDistanceDeg(state.ra_deg, state.dec_deg, star.ra_deg, star.dec_deg);
        };

        // Algoritmo GSS per trovare il tempo esatto del CA
        double t_ca = findMinimumGSS(computeDist, t_start, t_end, 1e-9); // Precisione 
        double min_dist_deg = computeDist(t_ca);
        double min_dist_mas = min_dist_deg * 3600000.0;

        if (min_dist_mas < 10000.0) { // Log any CA within 10 arcsec
            std::cout << "[PHASE2] Candidate Star: " << star.source_id 
                      << " CA MJD: " << std::fixed << std::setprecision(5) << t_ca
                      << " Min Dist: " << std::fixed << std::setprecision(2) << min_dist_mas << " mas" << std::endl;
        }

        // Fill results
        event.t_ca_mjd = t_ca;
        event.min_dist_mas = min_dist_deg * 3600000.0;
        event.star_id = star.source_id;
        event.mjd_tdb = t_ca;
        
        // Final state for report
        auto final_state = astdyn->getApparentStateGeocentric(t_ca);
        event.star_ra_deg = star.ra_deg;
        event.star_dec_deg = star.dec_deg;
        event.asteroid_ra_deg = final_state.ra_deg;
        event.asteroid_dec_deg = final_state.dec_deg;
        event.closest_approach_arcsec = min_dist_deg * 3600.0;

        // UTC conversion (simple format for now)
        event.utc_string = "MJD " + std::to_string(t_ca);

        // 2. VERIFICA E CALCOLO OMBRA
        if (event.min_dist_mas < 300000.0) { // Allarga la soglia per calcolare l'ombra per TUTTI gli eventi cercati (300")
            event.is_valid = (event.min_dist_mas < 1000.0);
            
            // Calcolo parametri ombra
            calculateShadowDetails(star, event, config);

            if (event.is_valid) {
                std::cout << "[PHASE2] Event Found! Star: " << star.source_id 
                          << " Min Dist: " << std::fixed << std::setprecision(2) 
                          << event.min_dist_mas << " mas" << std::endl;
            }
        }

        return event;
    }

    void calculateShadowDetails(const CandidateStar& star, Phase2OccultationEvent& event, const Phase2Config& config) {
        std::cout << "[PHASE2] Entering calculateShadowDetails for star " << star.source_id << std::endl;
        if (!config.compute_shadow) {
            std::cout << "[PHASE2] WARNING: compute_shadow is false!" << std::endl;
            return;
        }

        // 1. Calcolo Diametro Fisico
        auto kep = astdyn->getKeplerianElements();
        double H = kep.magnitude;
        double G = kep.mag_slope;
        double diameter_km = kep.diameter;
        
        if (diameter_km <= 0) {
            // Albedo estimate: 0.15 if G is standard, otherwise empirical formula
            double albedo = (std::abs(G - 0.15) < 0.001) ? 0.15 : std::pow(10.0, -(1.139 * G + 0.52));
            diameter_km = (1329.0 / std::sqrt(albedo)) * std::pow(10.0, -0.2 * H);
        }
        
        auto state_ca = astdyn->getApparentStateGeocentric(event.t_ca_mjd);
        double distance_au = state_ca.distance_au;
        event.asteroid_distance_au = distance_au;
        
        // Calcolo Elementi di Bessel (x, y, dx, dy)
        double ra_s = star.ra_deg * DEG_TO_RAD;
        double dec_s = star.dec_deg * DEG_TO_RAD;
        
        auto computeBessel = [&](double mjd) {
            auto s = astdyn->getApparentStateGeocentric(mjd);
            Vector3D ast_pos_icrf(s.position.x(), s.position.y(), s.position.z());
            Vector3D xi(-std::sin(ra_s), std::cos(ra_s), 0.0);
            Vector3D eta(-std::sin(dec_s)*std::cos(ra_s), -std::sin(dec_s)*std::sin(ra_s), std::cos(dec_s));
            double r_earth = 6378.137;
            return std::make_pair(ast_pos_icrf.dot(xi) * AU / r_earth, 
                                  ast_pos_icrf.dot(eta) * AU / r_earth);
        };

        auto bessel_ca = computeBessel(event.t_ca_mjd);
        event.besselian_x = bessel_ca.first;
        event.besselian_y = bessel_ca.second;
        
        double dt_b_day = (1.0 / 86400.0); // 1 sec
        auto b1 = computeBessel(event.t_ca_mjd - dt_b_day);
        auto b2 = computeBessel(event.t_ca_mjd + dt_b_day);
        event.besselian_dx = (b2.first - b1.first) / (2.0 * dt_b_day * 24.0); // rays / hour
        event.besselian_dy = (b2.second - b1.second) / (2.0 * dt_b_day * 24.0);

        // 2. Substellar/Subsolar points
        double jd_ca = event.t_ca_mjd + 2400000.5;
        double jd_ut1 = jd_ca - (69.184 / 86400.0); // Delta T correction
        double gmst_deg = TopocentricConverter::greenwichMeanSiderealTime(jd_ut1) * RAD_TO_DEG;
        
        event.substellar_lat_deg = star.dec_deg;
        event.substellar_lon_deg = star.ra_deg - gmst_deg;
        while (event.substellar_lon_deg < -180.0) event.substellar_lon_deg += 360.0;
        while (event.substellar_lon_deg > 180.0) event.substellar_lon_deg -= 360.0;

        // Subsolar point (requires Sun position)
        auto sun_state = astdyn::ephemeris::PlanetaryEphemeris::getState(astdyn::ephemeris::CelestialBody::SUN, jd_ca);
        double sun_ra = std::atan2(sun_state.position().y(), sun_state.position().x()) * RAD_TO_DEG;
        double sun_dec = std::asin(sun_state.position().z() / sun_state.position().norm()) * RAD_TO_DEG;
        
        event.subsolar_lat_deg = sun_dec;
        event.subsolar_lon_deg = sun_ra - gmst_deg;
        while (event.subsolar_lon_deg < -180.0) event.subsolar_lon_deg += 360.0;
        while (event.subsolar_lon_deg > 180.0) event.subsolar_lon_deg -= 360.0;

        // Apparent Star Position (simplified precession/nutation for header chart)
        // In a real implementation we would apply aberration/precession from J2000 to Date
        event.star_app_ra_deg = star.ra_deg;
        event.star_app_dec_deg = star.dec_deg;

        // 3. Velocità dell'ombra (km/s) sul piano di Bessel
        // Calcolata tramite i derivati besseliani (unità: raggi terrestri / ora)
        double v_er_h = std::sqrt(event.besselian_dx * event.besselian_dx + event.besselian_dy * event.besselian_dy);
        
        // Conversione in km/s (R_earth = 6378.137 km)
        double v_km_s = v_er_h * 6378.137 / 3600.0;
        
        event.duration_sec = (v_km_s > 0) ? (diameter_km / v_km_s) : 0.0;

        // 2. Percorso ombra (Campionamento +/- 5 min)
        TopocentricConverter converter;
        double star_ra_rad = star.ra_deg * DEG_TO_RAD;
        double star_dec_rad = star.dec_deg * DEG_TO_RAD;
        Vector3D star_dir_icrf(
            std::cos(star_dec_rad) * std::cos(star_ra_rad),
            std::cos(star_dec_rad) * std::sin(star_ra_rad),
            std::sin(star_dec_rad)
        );

        for (int i = -10; i <= 10; ++i) {
            double mjd = event.t_ca_mjd + (i * 30.0 / 86400.0); // Ogni 30 secondi
            // Asteroid in ITRF [meters]
            auto ast_state = astdyn->propagateToEpoch(mjd);
            
            // Trasformazione in ECEF corporea
            double rotation_matrix[3][3];
            double jd_tdb = mjd + 2400000.5;
            
            // CORREZIONE CRITICA: La rotazione terrestre vuole UT1, non TDB.
            // Approssimazione: UT1 = TDB - (ΔAT + 32.184) s. 
            // Nel 2026, ΔAT = 37s (TAI-UTC) + 32.184s (TT-TAI) = 69.184s totali.
            double jd_ut1 = jd_tdb - (69.184 / 86400.0);
            
            converter.rotationMatrixITRFtoCelestial(jd_ut1, "J2000", rotation_matrix);
            
            // Inverti matrice per Celestial -> ITRF
            double inv_matrix[3][3];
            for (int r=0; r<3; ++r) for (int c=0; c<3; ++c) inv_matrix[r][c] = rotation_matrix[c][r];

            Vector3D ast_pos_icrf(ast_state.position.x() * AU_M, 
                                  ast_state.position.y() * AU_M, 
                                  ast_state.position.z() * AU_M);
            Vector3D vel_icrf(ast_state.velocity.x() * (AU_M / 86400.0),
                              ast_state.velocity.y() * (AU_M / 86400.0),
                              ast_state.velocity.z() * (AU_M / 86400.0));
            
            Vector3D ast_pos_itrf = converter.applyRotation(inv_matrix, ast_pos_icrf);
            Vector3D vel_itrf = converter.applyRotation(inv_matrix, vel_icrf);
            Vector3D star_dir_itrf = converter.applyRotation(inv_matrix, star_dir_icrf);
            
            if (i == 0) {
                std::cout << "[DEBUG] CA Asteroid ICRF: " << ast_pos_icrf.x << ", " << ast_pos_icrf.y << ", " << ast_pos_icrf.z << " [m]\n";
                std::cout << "[DEBUG] CA Asteroid ITRF: " << ast_pos_itrf.x << ", " << ast_pos_itrf.y << ", " << ast_pos_itrf.z << " [m]\n";
                std::cout << "[DEBUG] Star Dir ITRF:    " << star_dir_itrf.x << ", " << star_dir_itrf.y << ", " << star_dir_itrf.z << "\n";
            }
            
            // Intersezione
            Vector3D shadow_ray = star_dir_itrf * -1.0; // Dal asteroide verso terra
            Vector3D inter = converter.intersectWithWGS84(ast_pos_itrf, shadow_ray);
            
            if (inter.magnitude() > 0) {
                auto geo = converter.getGeodeticPosition(inter);
                ShadowPoint pt;
                pt.mjd_tdb = mjd;
                pt.lat_deg = geo.latitude_deg;
                pt.lon_deg = geo.longitude_deg;
                pt.star_alt_deg = 30.0; // Placeholder
                
                // --- CALCOLO LIMITI ---
                // Vettore perpendicolare alla traccia (Cross-track)
                // Usiamo il prodotto croce tra Direzione Stella e Vettore Posizione (approssimato)
                Vector3D cross_track = star_dir_itrf.cross(ast_pos_itrf).normalize();
                
                // 1. Margine Geometrico (raggio asteroide)
                double r_ast = diameter_km * 500.0; // raggio in metri
                Vector3D pos_gn = ast_pos_itrf + cross_track * r_ast;
                Vector3D pos_gs = ast_pos_itrf - cross_track * r_ast;
                
                Vector3D inter_gn = converter.intersectWithWGS84(pos_gn, shadow_ray);
                Vector3D inter_gs = converter.intersectWithWGS84(pos_gs, shadow_ray);
                
                if (inter_gn.magnitude() > 0) {
                    auto g = converter.getGeodeticPosition(inter_gn);
                    pt.geonorth_lat_deg = g.latitude_deg;
                    pt.geonorth_lon_deg = g.longitude_deg;
                } else {
                    pt.geonorth_lat_deg = pt.lat_deg; pt.geonorth_lon_deg = pt.lon_deg;
                }
                
                if (inter_gs.magnitude() > 0) {
                    auto g = converter.getGeodeticPosition(inter_gs);
                    pt.geosouth_lat_deg = g.latitude_deg;
                    pt.geosouth_lon_deg = g.longitude_deg;
                } else {
                    pt.geosouth_lat_deg = pt.lat_deg; pt.geosouth_lon_deg = pt.lon_deg;
                }
                               // 2. Limite 1-sigma (incertezza orbitale)
                // Se abbiamo covarianza propagata, usiamo quella per calcolare il cross-track error
                double sigma_km = 50.0; // Default fallback
                
                if (ast_state.covariance.has_value()) {
                    // Proietta covarianza (posizione 3x3 superiore sx) sulla direzione perpendicolare
                    // al moto apparente proiettato sul piano dell'ombra.
                    // Semplificazione: usiamo la deviazione standard della posizione in metri
                    // trasformata in km. 
                    // TODO: Proiezione rigorosa nel piano dell'ombra (piano di Bessel)
                    const auto& cov = *ast_state.covariance;
                    
                    // Direzione "cross-track" approssimativa: 
                    // vettore ortogonale a star_dir e alla velocità dell'asteroide
                    // v_unit and cross_dir are Eigen::Vector3d
                    Eigen::Vector3d v_unit_eig = {vel_itrf.x, vel_itrf.y, vel_itrf.z};
                    v_unit_eig.normalize();
                    
                    Eigen::Vector3d star_dir_eig = {star_dir_itrf.x, star_dir_itrf.y, star_dir_itrf.z};
                    Eigen::Vector3d cross_dir = star_dir_eig.cross(v_unit_eig).normalized();
                    
                    // Varianza nella direzione cross-track: sigma^2 = n^T * P_pos * n
                    // cov è in AU^2, convertiamo in metri^2
                    Eigen::Matrix3d P_pos = cov.template block<3, 3>(0, 0) * (AU_M * AU_M);
                    double var_cross = cross_dir.transpose() * P_pos * cross_dir;
                    
                    if (var_cross > 0) {
                        sigma_km = std::sqrt(var_cross) / 1000.0;
                    }
                }
                
                double r_sigma = sigma_km * 1000.0;
                
                Vector3D pos_sn = ast_pos_itrf + cross_track * r_sigma;
                Vector3D pos_ss = ast_pos_itrf - cross_track * r_sigma;

                Vector3D inter_sn = converter.intersectWithWGS84(pos_sn, shadow_ray);
                Vector3D inter_ss = converter.intersectWithWGS84(pos_ss, shadow_ray);
                
                if (inter_sn.magnitude() > 0) {
                    auto g = converter.getGeodeticPosition(inter_sn);
                    pt.north_lat_deg = g.latitude_deg;
                    pt.north_lon_deg = g.longitude_deg;
                } else {
                    pt.north_lat_deg = pt.lat_deg; pt.north_lon_deg = pt.lon_deg;
                }
                
                if (inter_ss.magnitude() > 0) {
                    auto g = converter.getGeodeticPosition(inter_ss);
                    pt.south_lat_deg = g.latitude_deg;
                    pt.south_lon_deg = g.longitude_deg;
                } else {
                    pt.south_lat_deg = pt.lat_deg; pt.south_lon_deg = pt.lon_deg;
                }
                
                event.shadow_path.push_back(pt);
            }
        }
    }

    double findMinimumGSS(std::function<double(double)> f, double a, double b, double tol) {
        const double invphi = (std::sqrt(5.0) - 1.0) / 2.0;
        const double invphi2 = (3.0 - std::sqrt(5.0)) / 2.0;
        
        double h = b - a;
        if (h <= tol) return (a + b) / 2.0;

        int n = std::ceil(std::log(tol / h) / std::log(invphi));
        
        double x1 = a + invphi2 * h;
        double x2 = a + invphi * h;
        double f1 = f(x1);
        double f2 = f(x2);
        
        for (int i = 0; i < n; i++) {
            if (f1 < f2) {
                b = x2;
                x2 = x1;
                f2 = f1;
                h = invphi * h;
                x1 = a + invphi2 * h;
                f1 = f(x1);
            } else {
                a = x1;
                x1 = x2;
                f1 = f2;
                h = invphi * h;
                x2 = a + invphi * h;
                f2 = f(x2);
            }
        }
        return (f1 < f2) ? x1 : x2;
    }
};

Phase2OccultationGeometry::Phase2OccultationGeometry() : pimpl_(std::make_unique<Impl>()) {}
Phase2OccultationGeometry::~Phase2OccultationGeometry() = default;

void Phase2OccultationGeometry::setAstDynWrapper(std::shared_ptr<AstDynWrapper> wrapper) {
    std::cout << "[Phase2OccultationGeometry] setAstDynWrapper called with wrapper=" << wrapper.get() << std::endl;
    pimpl_->astdyn = wrapper;
}

void Phase2OccultationGeometry::setSPKReader(std::shared_ptr<ISPReader> reader) {
    std::cout << "[Phase2OccultationGeometry] setSPKReader called. Forwarding to wrapper=" << pimpl_->astdyn.get() << std::endl;
    if (pimpl_->astdyn) {
        pimpl_->astdyn->setSPKReader(reader);
    }
}

bool Phase2OccultationGeometry::loadAsteroidFromJSON(int number, const std::string& path) {
    std::string searchPath = path;
    if (searchPath.empty()) {
        const char* home = getenv("HOME");
        if (home) {
            searchPath = std::string(home) + "/.ioccultcalc/data/all_numbered_asteroids.json";
        }
    }

    std::ifstream f(searchPath);
    if (!f.is_open()) return false;

    try {
        nlohmann::json j;
        f >> j;

        std::string s_num = std::to_string(number);
        if (!j.contains(s_num)) return false;

        auto data = j[s_num];
        
        double epoch = data["epoch"];
        if (epoch > 2400000.5) epoch -= 2400000.5;

        double deg2rad = DEG_TO_RAD;
        pimpl_->astdyn->setKeplerianElements(
            data["a"], data["e"], (double)data["i"] * deg2rad, 
            (double)data["om"] * deg2rad, (double)data["w"] * deg2rad, (double)data["ma"] * deg2rad, 
            epoch, s_num, astdyn::propagation::HighPrecisionPropagator::InputFrame::ECLIPTIC,
            data.value("H", 0.0), data.value("G", 0.15), data.value("diameter", 0.0)
        );
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Error loading asteroid from JSON: " << e.what() << "\n";
        return false;
    }
}

bool Phase2OccultationGeometry::loadAsteroidFromDB(int number) {
    try {
        AsteroidSqliteDatabase db;
        auto orbital = db.getOrbitalElements(number);
        if (orbital) {
            pimpl_->astdyn->setKeplerianElements(
                orbital->a, orbital->e, orbital->i,
                orbital->Omega, orbital->omega, orbital->M,
                orbital->epoch.jd - 2400000.5,
                std::to_string(number),
                astdyn::propagation::HighPrecisionPropagator::InputFrame::ECLIPTIC,
                orbital->H, orbital->G, orbital->diameter
            );
            return true;
        }
    } catch (const std::exception& e) {
        std::cerr << "Error loading asteroid from DB: " << e.what() << "\n";
        return false;
    }
    return false;
}

bool Phase2OccultationGeometry::loadAsteroidFromEQ1(int asteroid_number, const std::string& eq1_path) {
    if (!pimpl_->astdyn->loadFromEQ1File(eq1_path)) {
        std::cerr << "Phase2: Failed to load asteroid from EQ1: " << eq1_path << "\n";
        return false;
    }
    return true;
}

bool Phase2OccultationGeometry::setAsteroidElements(const AstDynEquinoctialElements& elements) {
    pimpl_->astdyn->setAsteroidElements(elements);
    return true;
}

std::vector<Phase2OccultationEvent> Phase2OccultationGeometry::calculatePreciseGeometry(
    const std::vector<CandidateStar>& candidates,
    const Phase2Config& config) {
    
    std::vector<Phase2OccultationEvent> events;
    std::cout << "[PHASE2] calculatePreciseGeometry using AstDynWrapper:" << pimpl_->astdyn.get() << std::endl;
    if (config.refine_orbit) {
        pimpl_->refineOrbit(config);
    }
    if (config.use_horizons) {
       pimpl_->fetchHorizons(candidates, config);
    }
    for (const auto& star : candidates) {
        events.push_back(pimpl_->processSingleCandidate(star, config));
    }
    return events;
}

} // namespace ioccultcalc
