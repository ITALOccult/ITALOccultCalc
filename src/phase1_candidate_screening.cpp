/**
 * @file phase1_candidate_screening.cpp
 * @brief Riscrittura integrale Fase 1 - AstDyn & GaiaLib Native API
 * * LOGICA:
 * 1. Utilizzo di AstDyn::Observation per gestire la parallasse geocentrica.
 * 2. Query nativa su GaiaLib::UnifiedGaiaCatalog usando coordinate ICRF.
 * 3. Eliminazione di matrici di rotazione manuali per evitare mismatch di frame.
 */

#include "phase1_candidate_screening.h"
#include <astdyn/AstDyn.hpp>
#include <astdyn/api/OrbitFitAPI.hpp>
#include <astdyn/propagation/HighPrecisionPropagator.hpp>
#include "ioc_gaialib/unified_gaia_catalog.h"
#include "ioc_gaialib/types.h"
#include <iostream>
#include <vector>
#include <nlohmann/json.hpp>
#include <fstream>
#include <iomanip>
#include "ioccultcalc/orbital_elements.h"
#include "ioccultcalc/asteroid_sqlite_db.h"

namespace ioccultcalc {

class Phase1CandidateScreening::Impl {
public:
    std::unique_ptr<astdyn::propagation::HighPrecisionPropagator> propagator;
    astdyn::propagation::KeplerianElements initial_kep_ecl;
    ioc::gaia::UnifiedGaiaCatalog* catalog;
    int verbose_level = 0;
    
    Impl() : catalog(nullptr) {
        std::cout << "[Phase1CandidateScreening] Constructor started." << std::endl;
        // Initialize High Precision Propagator with DE441
        astdyn::propagation::HighPrecisionPropagator::Config config;
        config.de441_path = std::string(getenv("HOME")) + "/.ioccultcalc/ephemerides/de441_part-2.bsp";
        config.perturbations_planets = true;
        config.perturbations_asteroids = false; // Not needed for coarse corridor
        config.relativity = true;
        config.tolerance = 1e-9; // Sufficient for screening
        propagator = std::make_unique<astdyn::propagation::HighPrecisionPropagator>(config);
        std::cout << "[Phase1CandidateScreening] Constructor finished." << std::endl;
    }

    /**
     * @brief Calcola il punto apparente (RA/Dec ICRF) dell'asteroide visto dalla Terra.
     * Sfrutta le routine di alta precisione di AstDyn.
     */
    ioc::gaia::CelestialPoint getApparentPoint(double mjd) {
        // Usa l'API ad alta precisione con frame Eclittico
        double target_jd_tdb = mjd + 2400000.5;
        auto obs = propagator->calculateGeocentricObservation(
            initial_kep_ecl, 
            target_jd_tdb, 
            astdyn::propagation::HighPrecisionPropagator::InputFrame::ECLIPTIC
        );
        return ioc::gaia::CelestialPoint(obs.ra_deg, obs.dec_deg);
    }
};

Phase1CandidateScreening::Phase1CandidateScreening() : pimpl_(std::make_unique<Impl>()) {}
Phase1CandidateScreening::~Phase1CandidateScreening() = default;

void Phase1CandidateScreening::setCatalog(ioc::gaia::UnifiedGaiaCatalog* catalog) {
    pimpl_->catalog = catalog;
}

void Phase1CandidateScreening::setSPKReader(std::shared_ptr<ioccultcalc::ISPReader> reader) {
    // Note: HighPrecisionPropagator now handles DE441 internally via path.
    // We could bridge setSPKReader but the user specifically asked for HighPrecisionPropagator config.
}

void Phase1CandidateScreening::setVerbose(int level) {
    pimpl_->verbose_level = level;
}

bool Phase1CandidateScreening::loadAsteroidFromJSON(int number, const std::string& path) {
    // Logic from previous implementation to handle JD/MJD and load from JSON
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

        nlohmann::json data;
        bool found = false;
        nlohmann::json asteroidList;

        if (j.is_array()) {
            asteroidList = j;
        } else if (j.is_object() && j.contains("asteroids")) {
            asteroidList = j["asteroids"];
        }

        if (asteroidList.is_array()) {
            for (const auto& item : asteroidList) {
                if (item.value("number", 0) == number) {
                    data = item;
                    found = true;
                    break;
                }
            }
        } else if (j.is_object()) {
            std::string s_num = std::to_string(number);
            if (j.contains(s_num)) {
                data = j[s_num];
                found = true;
            }
        }

        if (found) {
            // Epoch guard clause
            double epoch = data["epoch"];
            if (epoch > 2400000.5) {
                epoch -= 2400000.5; // JD to MJD
            }

            double deg2rad = M_PI / 180.0;
            
            // Populate initial elements for the propagator
            pimpl_->initial_kep_ecl.semi_major_axis = data["a"];
            pimpl_->initial_kep_ecl.eccentricity = data["e"];
            pimpl_->initial_kep_ecl.inclination = (double)data["i"] * deg2rad;
            pimpl_->initial_kep_ecl.longitude_ascending_node = (double)data["om"] * deg2rad;
            pimpl_->initial_kep_ecl.argument_perihelion = (double)data["w"] * deg2rad;
            pimpl_->initial_kep_ecl.mean_anomaly = (double)data["ma"] * deg2rad;
            pimpl_->initial_kep_ecl.epoch_mjd_tdb = epoch;
            pimpl_->initial_kep_ecl.gravitational_parameter = 2.959122082855911e-04; // GMS in AU^3/day^2
            
            return true;
        }
    } catch (...) {
        return false;
    }
}

bool Phase1CandidateScreening::loadAsteroidFromDB(int number) {
    try {
        AsteroidSqliteDatabase db;
        auto orbital = db.getOrbitalElements(number);
        if (orbital) {
            // Convert OrbitalElements to Keplerian for the propagator
            pimpl_->initial_kep_ecl.semi_major_axis = orbital->a;
            pimpl_->initial_kep_ecl.eccentricity = orbital->e;
            pimpl_->initial_kep_ecl.inclination = orbital->i;
            pimpl_->initial_kep_ecl.longitude_ascending_node = orbital->Omega;
            pimpl_->initial_kep_ecl.argument_perihelion = orbital->omega;
            pimpl_->initial_kep_ecl.mean_anomaly = orbital->M;
            pimpl_->initial_kep_ecl.epoch_mjd_tdb = orbital->epoch.jd - 2400000.5;
            pimpl_->initial_kep_ecl.gravitational_parameter = 2.959122082855911e-04;
            return true;
        }
    } catch (...) {
        return false;
    }
    return false;
}

bool Phase1CandidateScreening::loadAsteroidFromEQ1(int number, const std::string& eq1_path) {
    try {
        // Usa OrbitFitAPI per il parsing
        auto equ = astdyn::api::OrbitFitAPI::parse_eq1(eq1_path);
        pimpl_->initial_kep_ecl = astdyn::propagation::equinoctial_to_keplerian(equ);
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Phase1: Failed to load asteroid from EQ1: " << eq1_path << " - " << e.what() << "\n";
        return false;
    }
}

bool Phase1CandidateScreening::setAsteroidElements(const AstDynEquinoctialElements& elements) {
    // Ottieni elementi osculanti (applica correzione se type == MEAN_ASTDYS)
    auto kep = elements.toOsculatingKeplerian();
    
    // Popola elementi per HighPrecisionPropagator
    pimpl_->initial_kep_ecl.semi_major_axis = kep.a;
    pimpl_->initial_kep_ecl.eccentricity = kep.e;
    pimpl_->initial_kep_ecl.inclination = kep.i;
    pimpl_->initial_kep_ecl.longitude_ascending_node = kep.Omega;
    pimpl_->initial_kep_ecl.argument_perihelion = kep.omega;
    pimpl_->initial_kep_ecl.mean_anomaly = kep.M;
    pimpl_->initial_kep_ecl.epoch_mjd_tdb = kep.epoch.jd - 2400000.5;
    pimpl_->initial_kep_ecl.gravitational_parameter = 2.959122082855911e-04; // GMS in AU^3/day^2
    
    return true;
}

Phase1Results Phase1CandidateScreening::screenCandidates(const Phase1Config& config) {
    Phase1Results results;
    
    // 1. Validazione Catalogo
    if (!pimpl_->catalog) {
        pimpl_->catalog = &ioc::gaia::UnifiedGaiaCatalog::getInstance();
    }

    std::cout << "[PHASE1] Starting native screening for " << config.asteroid_name << " (MJD " 
              << config.start_mjd_tdb << " to " << config.end_mjd_tdb << ")..." << std::endl;

    auto start_time = std::chrono::high_resolution_clock::now();

    // 2. Generazione Path Topocentrico (ICRF)
    std::vector<ioc::gaia::CelestialPoint> corridor_points;
    double duration = config.end_mjd_tdb - config.start_mjd_tdb;
    int num_steps = std::max(20, static_cast<int>(duration * 24)); 

    for (int i = 0; i <= num_steps; ++i) {
        double mjd = config.start_mjd_tdb + (duration * i / num_steps);
        corridor_points.push_back(pimpl_->getApparentPoint(mjd));
    }

    std::cout << "[PHASE1] Path start: RA=" << corridor_points.front().ra << " Dec=" << corridor_points.front().dec << std::endl;
    std::cout << "[PHASE1] Path end:   RA=" << corridor_points.back().ra << " Dec=" << corridor_points.back().dec << std::endl;

    // 3. Query Nativa a GaiaLib
    ioc::gaia::CorridorQueryParams query_params;
    query_params.path = corridor_points;
    query_params.width = config.corridor_width_deg;
    query_params.max_magnitude = config.max_magnitude;

    std::cout << "[PHASE1] Querying Gaia catalog with " << corridor_points.size() << " ICRF points..." << std::endl;
    auto gaia_stars = pimpl_->catalog->queryCorridor(query_params);
    results.num_stars_in_corridor = static_cast<int>(gaia_stars.size());

    // 4. Trasformazione e Filtro Closest Approach
    double mid_mjd = (config.start_mjd_tdb + config.end_mjd_tdb) / 2.0;

    for (const auto& star : gaia_stars) {
        // Applichiamo il moto proprio (PM) all'epoca dell'evento
        auto star_at_epoch = star.propagateToEpoch(mid_mjd); 

        CandidateStar candidate;
        candidate.source_id = star.source_id;
        candidate.ra_deg = star_at_epoch.ra;
        candidate.dec_deg = star_at_epoch.dec;
        candidate.phot_g_mean_mag = star.phot_g_mean_mag;
        candidate.pmra = star.pmra;
        candidate.pmdec = star.pmdec;
        candidate.parallax = star.parallax;

        // Calcolo rapido del Closest Approach per lo screening iniziale
        double step_days = duration / num_steps;
        if (checkInitialCA(candidate, corridor_points, config.threshold_arcsec, config.start_mjd_tdb, step_days)) {
            results.candidates.push_back(candidate);
        }
    }

    auto end_time = std::chrono::high_resolution_clock::now();
    results.propagation_time_ms = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time).count();

    std::cout << "[PHASE1] Screening complete. Found " << results.candidates.size() << " candidates (" 
              << results.num_stars_in_corridor << " stars in corridor)." << std::endl;
    return results;
}

bool Phase1CandidateScreening::checkInitialCA(CandidateStar& star, 
                                              const std::vector<ioc::gaia::CelestialPoint>& path, 
                                              double threshold,
                                              double start_mjd,
                                              double step_days) {
    ioc::gaia::EquatorialCoordinates star_coord(star.ra_deg, star.dec_deg);
    double min_dist_deg = 1e9;
    double best_mjd = 0;
    bool found = false;

    for (size_t i = 0; i < path.size(); ++i) {
        ioc::gaia::EquatorialCoordinates path_coord(path[i].ra, path[i].dec);
        double dist = ioc::gaia::angularDistance(star_coord, path_coord);
        
        if (dist < min_dist_deg) {
            min_dist_deg = dist;
            best_mjd = start_mjd + (i * step_days);
        }

        if (dist * 3600.0 <= threshold) {
            found = true;
        }
    }

    if (found) {
        star.closest_approach_mjd = best_mjd;
        star.closest_approach_arcsec = min_dist_deg * 3600.0;
        return true;
    }
    return false;
}

} // namespace ioccultcalc
