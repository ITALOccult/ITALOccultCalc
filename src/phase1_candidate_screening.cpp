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
#include <astdyn/core/Constants.hpp>
#include <astdyn/time/TimeScale.hpp>
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

namespace ac = astdyn::constants;
namespace at = astdyn::time;

class Phase1CandidateScreening::Impl {
public:
    std::unique_ptr<astdyn::propagation::HighPrecisionPropagator> propagator;
    astdyn::propagation::KeplerianElements initial_kep_ecl;
    ioc::gaia::UnifiedGaiaCatalog* catalog;
    int verbose_level = 0;
    
    Impl() : catalog(nullptr) {
        std::cout << "[Phase1CandidateScreening] Constructor started." << std::endl;
        astdyn::propagation::HighPrecisionPropagator::Config config;
        const char* home = getenv("HOME");
        std::string cacheDir = home ? (std::string(home) + "/.ioccultcalc/ephemerides/") : "";
        // Prefer de441_part-2.bsp (user choice), then de440, then full de441 to avoid OOM
        std::vector<std::string> tryPaths = {
            cacheDir + "de441_part-2.bsp",    // preferred
            cacheDir + "de440.bsp",
            cacheDir + "de441.bsp"            // full ~3.1 GB – last resort
        };
        config.de441_path = "";
        for (const auto& p : tryPaths) {
            if (p.empty()) continue;
            std::ifstream f(p);
            if (f.good()) {
                config.de441_path = p;
                break;
            }
        }
        if (config.de441_path.empty() && !cacheDir.empty()) {
            std::cerr << "[Phase1CandidateScreening] WARNING: No DE441/DE440 SPK found in " << cacheDir
                      << ". Using analytical ephemeris (lower precision)." << std::endl;
        }
        config.perturbations_planets = true;
        config.perturbations_asteroids = false;
        config.relativity = true;
        config.tolerance = 1e-9;
        if (!config.de441_path.empty()) {
            if (config.de441_path.find("de441.bsp") != std::string::npos && config.de441_path.find("part-2") == std::string::npos) {
                std::cout << "[Phase1CandidateScreening] Loading full de441.bsp (~3 GB) – needs ~4+ GB RAM; consider using de440.bsp to avoid OOM." << std::endl;
            }
            std::cout << "[Phase1CandidateScreening] Loading DE ephemeris (may take 1–2 min for large .bsp)... " << std::flush;
        }
        propagator = std::make_unique<astdyn::propagation::HighPrecisionPropagator>(config);
        if (!config.de441_path.empty()) {
            std::cout << "done." << std::endl;
        }
        std::cout << "[Phase1CandidateScreening] Constructor finished." << std::endl;
    }

    /**
     * @brief Calcola il punto apparente (RA/Dec ICRF) dell'asteroide visto dalla Terra.
     * Sfrutta le routine di alta precisione di AstDyn.
     */
    ioc::gaia::CelestialPoint getApparentPoint(astdyn::MJD mjd) {
        double target_jd_tdb = at::mjd_to_jd(mjd);
        auto obs = propagator->calculateGeocentricObservation(
            initial_kep_ecl, 
            target_jd_tdb, 
            astdyn::propagation::HighPrecisionPropagator::InputFrame::ECLIPTIC
        );

 // DEBUG: Stampiamo cosa restituisce
    if (verbose_level >= 2) {
        std::cout << "[ASTDYN DEBUG] JD=" << target_jd_tdb 
         << " Raw output: ra=" << obs.ra_deg << " dec=" << obs.dec_deg
        << " (frame: ECLIPTIC input -> ??? output)" << std::endl;
    }
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
            double epoch = data["epoch"];
            if (epoch > at::mjd_to_jd(0))
                epoch = at::jd_to_mjd(epoch);

            pimpl_->initial_kep_ecl.semi_major_axis = data["a"];
            pimpl_->initial_kep_ecl.eccentricity = data["e"];
            pimpl_->initial_kep_ecl.inclination = (double)data["i"] * ac::DEG_TO_RAD;
            pimpl_->initial_kep_ecl.longitude_ascending_node = (double)data["om"] * ac::DEG_TO_RAD;
            pimpl_->initial_kep_ecl.argument_perihelion = (double)data["w"] * ac::DEG_TO_RAD;
            pimpl_->initial_kep_ecl.mean_anomaly = (double)data["ma"] * ac::DEG_TO_RAD;
            pimpl_->initial_kep_ecl.epoch_mjd_tdb = epoch;
            pimpl_->initial_kep_ecl.gravitational_parameter = ac::GMS;

            return true;
        }
    } catch (...) {
        return false;
    }
    return false;
}

bool Phase1CandidateScreening::loadAsteroidFromDB(int number) {
    try {
        AsteroidSqliteDatabase db;
        auto orbital = db.getOrbitalElements(number);
        if (orbital) {
            astdyn::MJD mjd_epoch = at::jd_to_mjd(orbital->epoch.jd);
            std::cout << "[Phase1] Elementi orbitali utilizzati (asteroid " << number << ", da asteroids.db):\n"
                      << "  a=" << std::fixed << std::setprecision(8) << orbital->a << " AU  e=" << orbital->e
                      << "  i=" << std::setprecision(5) << (orbital->i * ac::RAD_TO_DEG) << " deg\n"
                      << "  Omega=" << (orbital->Omega * ac::RAD_TO_DEG) << "  omega=" << (orbital->omega * ac::RAD_TO_DEG)
                      << "  M=" << (orbital->M * ac::RAD_TO_DEG) << " deg  epoch MJD=" << std::setprecision(2) << mjd_epoch
                      << "  H=" << orbital->H << "  diam=" << orbital->diameter << " km\n";

            pimpl_->initial_kep_ecl.semi_major_axis = orbital->a;
            pimpl_->initial_kep_ecl.eccentricity = orbital->e;
            pimpl_->initial_kep_ecl.inclination = orbital->i;
            pimpl_->initial_kep_ecl.longitude_ascending_node = orbital->Omega;
            pimpl_->initial_kep_ecl.argument_perihelion = orbital->omega;
            pimpl_->initial_kep_ecl.mean_anomaly = orbital->M;
            pimpl_->initial_kep_ecl.epoch_mjd_tdb = at::jd_to_mjd(orbital->epoch.jd);
            pimpl_->initial_kep_ecl.gravitational_parameter = ac::GMS;
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
    pimpl_->initial_kep_ecl.epoch_mjd_tdb = at::jd_to_mjd(kep.epoch.jd);
    pimpl_->initial_kep_ecl.gravitational_parameter = ac::GMS;
    
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
