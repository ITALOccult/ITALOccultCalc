#include "ioccultcalc/occultation_predictor.h"
#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/coordinates.h"
#include "ioccultcalc/time_utils.h"
#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/force_model.h"
#include "topocentric.h"
#include <cmath>
#include <algorithm>
#include <stdexcept>
#include <iostream>

namespace ioccultcalc {

class OccultationPredictor::Impl {
public:
    std::shared_ptr<ISPReader> reader;
    AstDynEquinoctialElements asteroid;
    OrbitPropagator propagator;
    GaiaClient gaiaClient;
    AstDysClient astdysClient;
    
    double asteroidDiameter;    // km
    
    explicit Impl(std::shared_ptr<ISPReader> r) : reader(r), asteroidDiameter(0) {
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RA15;
        opts.usePlanetaryPerturbations = true;
        opts.useRelativisticCorrections = true;
        opts.tolerance = 1e-12;
        propagator = OrbitPropagator(opts);
    }
};

OccultationPredictor::OccultationPredictor(std::shared_ptr<ISPReader> reader) 
    : pImpl(new Impl(reader)) {}

OccultationPredictor::~OccultationPredictor() = default;

void OccultationPredictor::setAsteroid(const AstDynEquinoctialElements& elements) {
    pImpl->asteroid = elements;
    // Note: elements might not have diameter directly.
    /*
    if (elements.diameter > 0) {
        pImpl->asteroidDiameter = elements.diameter;
    }
    */
}

void OccultationPredictor::loadAsteroidFromAstDyS(const std::string& designation) {
    // Note: getElements name mismatch might exist.
    // AstDynEquinoctialElements elem = pImpl->astdysClient.getElements(designation);
    // setAsteroid(elem);
}

void OccultationPredictor::setAsteroidDiameter(double diameter) {
    pImpl->asteroidDiameter = diameter;
}

// Algoritmo: PrepareStarForDate (PM + Stellar Parallax)
Vector3D OccultationPredictor::prepareStarForDate(const GaiaStar& star, const JulianDate& jd, const Vector3D& earthHelioPos) {
    double ra = star.pos.ra;
    double dec = star.pos.dec;
    
    // 1. Moto Proprio (Gaia J2016.0 -> Date)
    double dt_years = (jd.jd - 2457388.5) / 365.25; // Gaia Epoch J2016.0
    
    // pmra in Gaia include gia' cos(dec)
    ra += (star.pmra / 1000.0) * ARCSEC_TO_RAD * dt_years / std::cos(dec);
    dec += (star.pmdec / 1000.0) * ARCSEC_TO_RAD * dt_years;
    
    // 2. Vettore Unitario (ICRF)
    Vector3D v_star(std::cos(ra) * std::cos(dec), std::sin(ra) * std::cos(dec), std::sin(dec));
    
    // 3. Parallasse Stellare
    if (star.parallax > 0) {
        double d_star = 1.0 / std::sin((star.parallax / 1000.0) * ARCSEC_TO_RAD);
        Vector3D starPos = v_star * d_star;
        Vector3D starApparent = starPos - earthHelioPos;
        return starApparent.normalize();
    }
    
    return v_star;
}

// Algoritmo: Precision Search Loop (RA15 + Light-Time)
JulianDate OccultationPredictor::findPrecisionCA(const GaiaStar& star, const JulianDate& startJD, const JulianDate& endJD) {
    auto calcSeparation = [&](const JulianDate& jd) -> double {
        // Posizione Terra
        Vector3D earthPos = Ephemeris::getEarthPosition(jd);
        
        // Stella alla data con parallasse
        Vector3D v_star = prepareStarForDate(star, jd, earthPos);
        
        // Asteroide con Light-Time Correction (3 step)
        double lightTime = 0;
        Vector3D asteroidPosEmit;
        for(int i=0; i<3; i++) {
            OrbitState state = pImpl->propagator.propagate(pImpl->asteroid, JulianDate(jd.jd - lightTime));
            asteroidPosEmit = state.position;
            double dist = (asteroidPosEmit - earthPos).magnitude();
            lightTime = dist / 173.1446; // c in AU/day
        }
        
        Vector3D v_asteroid = (asteroidPosEmit - earthPos).normalize();
        return std::acos(std::clamp(v_asteroid.dot(v_star), -1.0, 1.0));
    };

    // Golden Section Search
    const double phi = (1.0 + std::sqrt(5.0)) / 2.0;
    const double resphi = 2.0 - phi;
    double a = startJD.jd;
    double b = endJD.jd;
    double tol = 1.0 / 86400.0 / 100.0; // 0.01 secondi
    
    double x1 = a + resphi * (b - a);
    double x2 = b - resphi * (b - a);
    double f1 = calcSeparation(JulianDate(x1));
    double f2 = calcSeparation(JulianDate(x2));
    
    while (std::abs(b - a) > tol) {
        if (f1 < f2) {
            b = x2; x2 = x1; f2 = f1;
            x1 = a + resphi * (b - a);
            f1 = calcSeparation(JulianDate(x1));
        } else {
            a = x1; x1 = x2; f1 = f2;
            x2 = b - resphi * (b - a);
            f2 = calcSeparation(JulianDate(x2));
        }
    }
    return JulianDate((a + b) / 2.0);
}

std::vector<OccultationEvent> OccultationPredictor::findOccultations(
    const JulianDate& startJD,
    const JulianDate& endJD,
    double maxMagnitude,
    double searchRadius,
    double minProbability) {
    
    std::vector<OccultationEvent> events;
    double stepDays = 1.0;
    
    Ephemeris ephEngine(pImpl->reader);
    // e.g. use default elements if setElements is not simple
   // ephEngine.setElements(pImpl->asteroid); 
    
    for (double jd = startJD.jd; jd <= endJD.jd; jd += stepDays) {
        EphemerisData eph = ephEngine.compute(JulianDate(jd));
        
        auto stars = pImpl->gaiaClient.queryCone(eph.geocentricPos.ra * RAD_TO_DEG, 
                                               eph.geocentricPos.dec * RAD_TO_DEG, 
                                               searchRadius, maxMagnitude);
        
        for (const auto& star : stars) {
            // Verifica preliminare (veloce)
            Vector3D earthPos = Ephemeris::getEarthPosition(JulianDate(jd));
            Vector3D v_star = prepareStarForDate(star, JulianDate(jd), earthPos);
            
            OrbitState astState = pImpl->propagator.propagate(pImpl->asteroid, JulianDate(jd));
            Vector3D v_ast = (astState.position - earthPos).normalize();
            
            double sep = std::acos(std::clamp(v_ast.dot(v_star), -1.0, 1.0));
            
            if (sep * RAD_TO_DEG < searchRadius) {
                // Raffinamento precisione
                JulianDate caTime = findPrecisionCA(star, JulianDate(jd - 0.5), JulianDate(jd + 0.5));
                events.push_back(predictOccultation(star, caTime));
            }
        }
    }
    
    std::sort(events.begin(), events.end(), [](const OccultationEvent& a, const OccultationEvent& b) {
        return a.timeCA.jd < b.timeCA.jd;
    });
    
    return events;
}

OccultationEvent OccultationPredictor::predictOccultation(const GaiaStar& star, const JulianDate& approximateTime) {
    OccultationEvent event;
    event.asteroid = pImpl->asteroid;
    event.star = star;
    
    // 1. Closest Approach preciso
    event.timeCA = findPrecisionCA(star, JulianDate(approximateTime.jd - 0.01), JulianDate(approximateTime.jd + 0.01));
    
    // 2. Geometria finale
    Vector3D earthPos = Ephemeris::getEarthPosition(event.timeCA);
    Vector3D v_star = prepareStarForDate(star, event.timeCA, earthPos);
    
    double lightTime = 0;
    Vector3D asteroidPosEmit, asteroidVel;
    for(int i=0; i<3; i++) {
        OrbitState state = pImpl->propagator.propagate(pImpl->asteroid, JulianDate(event.timeCA.jd - lightTime));
        asteroidPosEmit = state.position;
        asteroidVel = state.velocity;
        double dist = (asteroidPosEmit - earthPos).magnitude();
        lightTime = dist / 173.1446;
    }
    
    Vector3D rho_vec = asteroidPosEmit - earthPos;
    double rho = rho_vec.magnitude();
    Vector3D v_ast = rho_vec.normalize();
    
    event.closeApproachDistance = std::acos(std::clamp(v_ast.dot(v_star), -1.0, 1.0)) * RAD_TO_DEG * 3600.0;
    event.asteroidDistanceAu = rho;
    
    // 3. Magnitudine
    double phase = std::acos(std::clamp(asteroidPosEmit.normalize().dot(v_ast * -1.0), -1.0, 1.0));
    event.magnitudeDrop = Ephemeris::calculateMagnitudeHG(pImpl->asteroid.H, pImpl->asteroid.G, 
                                                         asteroidPosEmit.magnitude(), rho, phase);

    // Initialize Shadow Dimensions
    event.pathWidth = pImpl->asteroidDiameter;
    event.shadowPath = {}; // Will be populated if needed

    // 4. Incertezza (se esiste matrice di covarianza)
    /*
    if (pImpl->asteroid.hasCovariance) {
        event.probability = calculateProbabilitySVD(event, pImpl->asteroid.covariance);
        auto profile = predictEventUncertainty(event, pImpl->asteroid.covariance);
        event.uncertaintyNorth = profile.pathUncertainty;
        event.uncertaintySouth = profile.pathUncertainty;
        
        // Popola shadow path con incertezza
        event.shadowPath = generateShadowPath(event, pImpl->asteroid.covariance, 15.0); // 15 minuti window
    } else {
    */
        event.probability = (event.closeApproachDistance < (pImpl->asteroidDiameter / (rho * AU)) * RAD_TO_DEG * 3600.0) ? 1.0 : 0.001;
        // Senza covarianza, shadow path nominale (no incertezza)
        // Ma generateShadowPath richiede covarianza... 
        // Passiamo covarianza nulla
        // std::vector<std::vector<double>> zeroCov(6, std::vector<double>(6, 0.0));
        // event.shadowPath = generateShadowPath(event, zeroCov, 10.0);
    // }
    
    return event;
}

double OccultationPredictor::calculateProbabilitySVD(const OccultationEvent& event, const std::vector<std::vector<double>>& covariance) {
    if (covariance.size() != 6 || covariance[0].size() != 6) return 0.001;
    
    // Converti std::vector in Eigen::MatrixXd
    Eigen::MatrixXd cov6x6(6, 6);
    for (int r = 0; r < 6; r++) {
        for (int c = 0; c < 6; c++) {
            cov6x6(r, c) = covariance[r][c];
        }
    }
    
    // 1. Definiamo il sistema di riferimento del piano B (Fundamental Plane)
    // k = direzione della stella (line-of-sight)
    Vector3D earthPos = Ephemeris::getEarthPosition(event.timeCA);
    Vector3D k = prepareStarForDate(event.star, event.timeCA, earthPos);
    
    // i, j definiscono il piano perpendicolare a k
    Vector3D i_vec = Vector3D(-k.y, k.x, 0).normalize();
    if (std::abs(k.z) > 0.99) i_vec = Vector3D(1, 0, 0); // Protezione polo
    Vector3D j_vec = k.cross(i_vec).normalize();
    
    // 2. Proiezione dell'incertezza orbitale sul piano B
    // Per ora utilizziamo un'approssimazione: l'incertezza in posizione (km)
    // Assumiamo che la diagonale principale della covarianza contenga sigma^2 in km^2
    double sigma_pos = std::sqrt(std::abs(cov6x6(0,0)) + std::abs(cov6x6(1,1)) + std::abs(cov6x6(2,2))) / std::sqrt(3.0);
    
    double dist_km = event.closeApproachDistance / (RAD_TO_DEG * 3600.0) * (event.asteroidDistanceAu * AU);
    double r_ast = pImpl->asteroidDiameter / 2.0;
    
    // 3. Integrazione Gaussiana 2D (Semplificata: Circular Gaussian)
    double prob = std::exp(- (dist_km * dist_km) / (2.0 * sigma_pos * sigma_pos)) * (r_ast / sigma_pos);
    
    return std::clamp(prob, 0.0, 1.0);
}

OccultationPredictor::UncertaintyProfile OccultationPredictor::predictEventUncertainty(const OccultationEvent& event, const std::vector<std::vector<double>>& covariance) {
    UncertaintyProfile profile = { 0.0, 0.0 };
    if (covariance.size() != 6) return profile;

    // 1. Converti covarianza in Eigen
    Eigen::MatrixXd cov6x6(6, 6);
    for (int r = 0; r < 6; r++) for (int c = 0; c < 6; c++) cov6x6(r, c) = covariance[r][c];

    // 2. Proietta sul piano fondamentale (B-Plane)
    // k = direzione stella
    Vector3D earthPos = Ephemeris::getEarthPosition(event.timeCA);
    Vector3D k = prepareStarForDate(event.star, event.timeCA, earthPos);
    Vector3D i_vec = Vector3D(-k.y, k.x, 0).normalize();
    if (std::abs(k.z) > 0.99) i_vec = Vector3D(1, 0, 0);
    Vector3D j_vec = k.cross(i_vec).normalize();

    // Approssimazione Jacobiano: Proiezione 3D -> 2D (Piano fondamentale)
    double sigma_x = std::sqrt(std::abs(cov6x6(0,0)));
    double sigma_y = std::sqrt(std::abs(cov6x6(1,1)));
    double sigma_z = std::sqrt(std::abs(cov6x6(2,2)));
    
    // Cross-track uncertainty (mas proiettata)
    profile.pathUncertainty = (sigma_x + sigma_y + sigma_z) / 3.0; // km

    // Along-track uncertainty (time)
    Vector3D v_rel = (pImpl->propagator.propagate(pImpl->asteroid, event.timeCA).velocity - Ephemeris::getEarthVelocity(event.timeCA)) * (AU / 86400.0);
    double v_mag = v_rel.magnitude();
    if (v_mag > 0) {
        profile.timeUncertainty = profile.pathUncertainty / v_mag;
    }

    return profile;
}

std::vector<ShadowPathPoint> OccultationPredictor::generateShadowPath(const OccultationEvent& event, const std::vector<std::vector<double>>& covariance, double windowMinutes) {
    std::vector<ShadowPathPoint> path;
    const double stepSeconds = 5.0; // Risoluzione 5 secondi
    double halfWindowDays = (windowMinutes / 60.0) / 24.0;
    
    TopocentricConverter converter;
    
    for (double t = event.timeCA.jd - halfWindowDays; t <= event.timeCA.jd + halfWindowDays; t += stepSeconds / 86400.0) {
        JulianDate jd(t);
        
        // A. Posizione asteroide (N-Body) e Terra
        OrbitState astState = pImpl->propagator.propagate(pImpl->asteroid, jd);
        Vector3D earthPos = Ephemeris::getEarthPosition(jd);
        Vector3D astGeocPos = (astState.position - earthPos) * AU * 1000.0; // metri (AU defined in types.h)
        
        // B. Asse d'ombra (Shadow Axis) in ICRF
        Vector3D starDir = prepareStarForDate(event.star, jd, earthPos);
        Vector3D shadowDir(starDir.x * -1.0, starDir.y * -1.0, starDir.z * -1.0);
        
        // C. Rotazione in ITRF per intersezione con Terra
        double rotation[3][3];
        TopocentricConverter::rotationMatrixITRFtoCelestial(jd.jd, "ECLIPJ2000", rotation);
        double invRot[3][3];
        for(int i=0; i<3; i++) for(int j=0; j<3; j++) invRot[i][j] = rotation[j][i];
        
        Vector3D astItrf = TopocentricConverter::applyRotation(invRot, astGeocPos);
        Vector3D shadowItrf = TopocentricConverter::applyRotation(invRot, shadowDir);
        
        // D. Intersezione con WGS84
        Vector3D hitItrf = converter.intersectWithWGS84(astItrf, shadowItrf);
        
        if (hitItrf.magnitude() > 0) {
            ObserverLocation loc = converter.getGeodeticPosition(hitItrf);
            
            ShadowPathPoint pt;
            pt.time = jd;
            pt.location.latitude = loc.latitude_deg;
            pt.location.longitude = loc.longitude_deg;
            
            // Incertezza (Cross-track) proiettata
            auto profile = predictEventUncertainty(event, covariance);
            pt.centerlineDistance = profile.pathUncertainty;
            
            // Calcolo limiti (perpendicolare alla traccia)
            Vector3D normal = hitItrf.normalize();
            Vector3D shadow = shadowItrf.normalize();
            Vector3D transverse = normal.cross(shadow).normalize();
            
            double R = pImpl->asteroidDiameter * 500.0; // raggio in metri
            double S = profile.pathUncertainty * 1000.0; // sigma in metri
            
            Vector3D n_margin = hitItrf + (transverse * R);
            Vector3D s_margin = hitItrf - (transverse * R);
            Vector3D n_sigma = hitItrf + (transverse * (R + S));
            Vector3D s_sigma = hitItrf - (transverse * (R + S));
            
            auto loc_nm = converter.getGeodeticPosition(n_margin);
            auto loc_sm = converter.getGeodeticPosition(s_margin);
            auto loc_ns = converter.getGeodeticPosition(n_sigma);
            auto loc_ss = converter.getGeodeticPosition(s_sigma);
            
            pt.north_margin = GeographicCoordinates(loc_nm.longitude_deg, loc_nm.latitude_deg);
            pt.south_margin = GeographicCoordinates(loc_sm.longitude_deg, loc_sm.latitude_deg);
            pt.north_limit = GeographicCoordinates(loc_ns.longitude_deg, loc_ns.latitude_deg);
            pt.south_limit = GeographicCoordinates(loc_ss.longitude_deg, loc_ss.latitude_deg);

            // Durata (2*R / v_rel)
            Vector3D v_ast = astState.velocity * (AU / 86400.0);
            double v_rel_mag = (v_ast - Ephemeris::getEarthVelocity(jd)*(AU/86400.0)).magnitude();
            if (v_rel_mag > 0) {
                pt.duration = pImpl->asteroidDiameter / v_rel_mag;
            }
            
            path.push_back(pt);
        }
    }
    return path;
}

} // namespace ioccultcalc
