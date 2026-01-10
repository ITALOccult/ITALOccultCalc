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

namespace ioccultcalc {

// Funzione helper per distanza angolare tra due punti (RA/Dec in gradi)
double angularDistanceDeg(double ra1, double dec1, double ra2, double dec2) {
    double r1 = ra1 * M_PI / 180.0;
    double d1 = dec1 * M_PI / 180.0;
    double r2 = ra2 * M_PI / 180.0;
    double d2 = dec2 * M_PI / 180.0;
    
    double cos_dist = std::sin(d1) * std::sin(d2) + 
                      std::cos(d1) * std::cos(d2) * std::cos(r1 - r2);
    if (cos_dist > 1.0) cos_dist = 1.0;
    if (cos_dist < -1.0) cos_dist = -1.0;
    return std::acos(cos_dist) * 180.0 / M_PI;
}

class Phase2OccultationGeometry::Impl {
public:
    std::shared_ptr<AstDynWrapper> astdyn;

    Impl() : astdyn(std::make_shared<AstDynWrapper>(PropagationSettings::highAccuracy())) {}

    Phase2OccultationEvent processSingleCandidate(const CandidateStar& star, const Phase2Config& config) {
        Phase2OccultationEvent event;
        event.star_id = star.source_id;

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
        if (event.min_dist_mas < 5000.0) { // Allarga la soglia per calcolare l'ombra anche se CA > 1"
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
        if (!config.compute_shadow) return;

        // 1. Durata stimata (Diametro from Occult4 reference)
        double diameter_km = 18.02; 
        double distance_au = astdyn->getApparentStateGeocentric(event.t_ca_mjd).distance_au;
        double parallax_arcsec = 8.794 / distance_au; // Semplificata
        
        // Velocità relativa apparente (arcsec/sec)
        double dt = 1.0 / 86400.0;
        auto s1 = astdyn->getApparentStateGeocentric(event.t_ca_mjd - dt);
        auto s2 = astdyn->getApparentStateGeocentric(event.t_ca_mjd + dt);
        double dra_deg = (s2.ra_deg - s1.ra_deg);
        double ddec_deg = (s2.dec_deg - s1.dec_deg);
        double dist_deg = std::sqrt(dra_deg * dra_deg * std::cos(s1.dec_deg*M_PI/180.0) * std::cos(s1.dec_deg*M_PI/180.0) + ddec_deg * ddec_deg);
        double v_arcsec_sec = (dist_deg * 3600.0) / 2.0;
        
        // Diametro angolare asteroide (mas)
        double diameter_mas = (diameter_km / (distance_au * 149597870.7)) * 180.0/M_PI * 3600.0 * 1000.0;
        event.duration_sec = diameter_mas / (v_arcsec_sec * 1000.0);

        // 2. Percorso ombra (Campionamento +/- 5 min)
        TopocentricConverter converter;
        double star_ra_rad = star.ra_deg * M_PI / 180.0;
        double star_dec_rad = star.dec_deg * M_PI / 180.0;
        Vector3D star_dir_icrf(
            std::cos(star_dec_rad) * std::cos(star_ra_rad),
            std::cos(star_dec_rad) * std::sin(star_ra_rad),
            std::sin(star_dec_rad)
        );

        for (int i = -10; i <= 10; ++i) {
            double mjd = event.t_ca_mjd + (i * 30.0 / 86400.0); // Ogni 30 secondi
            auto state = astdyn->getApparentStateGeocentric(mjd);
            
            // Trasformazione in ECEF
            double rotation_matrix[3][3];
            double jd_tdb = mjd + 2400000.5;
            converter.rotationMatrixITRFtoCelestial(jd_tdb, "J2000", rotation_matrix);
            
            // Inverti matrice per Celestial -> ITRF
            double inv_matrix[3][3];
            for (int r=0; r<3; ++r) for (int c=0; c<3; ++c) inv_matrix[r][c] = rotation_matrix[c][r];
            
            // Asteroide in ITRF [m]
            Vector3D ast_pos_icrf(state.position.x() * 149597870700.0, 
                                  state.position.y() * 149597870700.0, 
                                  state.position.z() * 149597870700.0);
            Vector3D ast_pos_itrf = converter.applyRotation(inv_matrix, ast_pos_icrf);
            Vector3D star_dir_itrf = converter.applyRotation(inv_matrix, star_dir_icrf);
            
            // Intersezione
            Vector3D shadow_ray = star_dir_itrf * -1.0; // Dal asteroide verso terra
            Vector3D inter = converter.intersectWithWGS84(ast_pos_itrf, shadow_ray);
            
            if (inter.magnitude() > 0) {
                auto geo = converter.getGeodeticPosition(inter);
                ShadowPoint pt;
                pt.mjd_tdb = mjd;
                pt.lat_deg = geo.latitude_deg;
                pt.lon_deg = geo.longitude_deg;
                // Calcolo altezza stella locale? (opzionale per ora)
                pt.star_alt_deg = 30.0; // Placeholder
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
    pimpl_->astdyn = wrapper;
}

void Phase2OccultationGeometry::setSPKReader(std::shared_ptr<ISPReader> reader) {
    pimpl_->astdyn->setSPKReader(reader);
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

        double deg2rad = M_PI / 180.0;
        pimpl_->astdyn->setKeplerianElements(
            data["a"], data["e"], (double)data["i"] * deg2rad, 
            (double)data["om"] * deg2rad, (double)data["w"] * deg2rad, (double)data["ma"] * deg2rad, 
            epoch, s_num
        );
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Error loading asteroid from JSON: " << e.what() << "\n";
        return false;
    }
}

bool Phase2OccultationGeometry::loadAsteroidFromEQ1(int asteroid_number, const std::string& eq1_path) {
    if (!pimpl_->astdyn->loadFromEQ1File(eq1_path)) {
        std::cerr << "Phase2: Failed to load asteroid from EQ1: " << eq1_path << "\n";
        return false;
    }
    return true;
}

std::vector<Phase2OccultationEvent> Phase2OccultationGeometry::calculatePreciseGeometry(
    const std::vector<CandidateStar>& candidates,
    const Phase2Config& config) {
    
    std::vector<Phase2OccultationEvent> events;
    for (const auto& star : candidates) {
        events.push_back(pimpl_->processSingleCandidate(star, config));
    }
    return events;
}

} // namespace ioccultcalc
