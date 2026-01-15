#include "ioccultcalc/config_manager.h"
#include "ioc_gaialib/unified_gaia_catalog.h"
#include "starmap/StarMap.h"
#include "phase1_candidate_screening.h"
#include "phase2_occultation_geometry.h"
#include "ioccultcalc/spice_spk_reader.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main(int argc, char* argv[]) {
    // 1. Initialize StarMap (Catalog System)
    starmap::config::LibraryConfig::CatalogPaths paths;
    paths.gaiaSaoDatabase = std::string(getenv("HOME")) + "/.catalog/crossreference/gaia_sao_xmatch.db";
    paths.iauCatalog = "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/external/IOC_GaiaLib/data/IAU-CSN.json";
    paths.starNamesDatabase = "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/external/IOC_GaiaLib/data/common_star_names.csv";
    starmap::initialize(paths);

    // 2. Initialize UnifiedGaiaCatalog
    std::string homeDir = getenv("HOME");
    nlohmann::json gaiaConfJson;
    gaiaConfJson["catalog_type"] = "sqlite_dr3";
    gaiaConfJson["sqlite_file_path"] = "/Users/michelebigi/.catalog/crossreference/gaia_dr3_occult_pro.db";
    gaiaConfJson["log_level"] = "warning";
    
    ioc::gaia::UnifiedGaiaCatalog::initialize(gaiaConfJson.dump());
    
    std::cout << "ITALOccultCalc v1.0 [Native API Workflow]" << std::endl;

    try {
    // 3. Setup Configuration
        Phase1Config p1Config;
        int asteroidNumber = 0;
        bool useHorizons = false;
        
        // Default values
        p1Config.max_magnitude = 14.0;
        p1Config.corridor_width_deg = 5.0;
        p1Config.threshold_arcsec = 300.0;
        
        // Parse arguments for config
        std::string configFile;
        if (argc > 1) {
            configFile = argv[1];
        }
        
        if (!configFile.empty()) {
            std::cout << "📄 Loading configuration from: " << configFile << std::endl;
            ConfigManager config;
            config.loadFromJson(configFile);
            
            // OBJECT Section
            auto objParams = config.getSection(ConfigSection::OBJECT);
            if (objParams) {
                auto idParam = objParams->getParameter("id");
                if (idParam) {
                    p1Config.asteroid_name = idParam->asString();
                    try {
                        asteroidNumber = std::stoi(p1Config.asteroid_name);
                    } catch (...) {
                         std::cerr << "Warning: Asteroid ID is not a number: " << p1Config.asteroid_name << std::endl;
                    }
                }
            }
            
            // SEARCH Section
            auto searchParams = config.getSection(ConfigSection::SEARCH);
            if (searchParams) {
                auto startParam = searchParams->getParameter("start_mjd");
                if (startParam) p1Config.start_mjd_tdb = startParam->asDouble();
                
                auto endParam = searchParams->getParameter("end_mjd");
                if (endParam) p1Config.end_mjd_tdb = endParam->asDouble();
                
                auto magParam = searchParams->getParameter("mag_limit");
                if (magParam) p1Config.max_magnitude = magParam->asDouble();
                
                auto widthParam = searchParams->getParameter("corridor_width");
                if (widthParam) p1Config.corridor_width_deg = widthParam->asDouble();
            }
            
            // PROPAGATION Section (Optional flags)
            auto propParams = config.getSection(ConfigSection::PROPAGATION);
            if (propParams) {
                auto horizonsParam = propParams->getParameter("use_horizons");
                if (horizonsParam) useHorizons = horizonsParam->asBool();
            }

        } else {
            // Fallback Legacy Default
            std::cout << "⚠️ No config file provided. Using hardcoded defaults (1272 Gefion)." << std::endl;
            p1Config.asteroid_name = "1272";
            asteroidNumber = 1272;
            p1Config.start_mjd_tdb = 61050.0;
            p1Config.end_mjd_tdb = 61055.0;
        }

        std::cout << "Target: " << p1Config.asteroid_name << " (" << asteroidNumber << ")" << std::endl;
        std::cout << "Window: " << p1Config.start_mjd_tdb << " -> " << p1Config.end_mjd_tdb << std::endl;
        
        // 4. Initialize SPK Reader for Earth positions
        auto spk = std::make_shared<SPICESPKReader>();
        std::string kernelsDir = homeDir + "/.ioccultcalc/ephemerides/";
        
        if (!spk->loadFile(kernelsDir + "de440.bsp")) {
             // Fallback
             if (!spk->loadFile(kernelsDir + "de441_part-2.bsp")) {
                 std::cerr << "Critical: Failed to load DE ephemeris." << std::endl;
                 return 1;
             }
        }
        std::cout << "Ephemeris loaded." << std::endl;
        
        // 5. Initialize Components
        auto shared_astdyn = std::make_shared<AstDynWrapper>(PropagationSettings::highAccuracy());
        
        Phase1CandidateScreening p1;
        p1.setSPKReader(spk);
        
        Phase2OccultationGeometry p2; 
        std::cout << "Setting AstDynWrapper to Phase 2..." << std::endl;
        p2.setAstDynWrapper(shared_astdyn);
        p2.setSPKReader(spk);
        std::cout << "Phase 2 setup complete." << std::endl;

        std::cout << "Loading asteroid " << asteroidNumber << " from DB..." << std::endl;
        // Use loadAsteroidFromDB for reliability with SQLite
        bool p1_loaded = p1.loadAsteroidFromDB(asteroidNumber);
        if (!p1_loaded) {
             // Try JSON fallback if DB fails (legacy behavior compatibility)
             std::cout << "DB load failed, trying JSON/Allnum fallback..." << std::endl;
             p1_loaded = p1.loadAsteroidFromJSON(asteroidNumber);
        }
        
        bool p2_loaded = p2.loadAsteroidFromDB(asteroidNumber);
        if (!p2_loaded) {
             p2_loaded = p2.loadAsteroidFromJSON(asteroidNumber);
        }
        
        if (!p1_loaded || !p2_loaded) {
            std::cerr << "Failed to load asteroid elements." << std::endl;
            // Manual injection fallback for testing if 13477
            if (asteroidNumber == 13477) {
                 std::cerr << "Attempting manual injection for 13477 (MJD 61000)..." << std::endl;
                 shared_astdyn->setKeplerianElements(
                    2.4485080003194, 0.143329820474419, 0.147662766626375, // a, e, i
                    1.18281763160276, 4.73269210633698, 1.03166444813114, // Omega, omega, M
                    61000.0, "13477", 
                    astdyn::propagation::HighPrecisionPropagator::InputFrame::ECLIPTIC
                );
                // Proceed as if loaded (Phase 2 uses shared wrapper)
            } else {
                return 1;
            }
        }

        // 6. Run Phase 1
        std::cout << "Phase 1: Screening stars in " << p1Config.corridor_width_deg << " deg corridor...\n";
        Phase1Results p1Results = p1.screenCandidates(p1Config);
        std::cout << "Found " << p1Results.candidates.size() << " candidates (" 
                  << p1Results.num_stars_in_corridor << " stars initially in corridor).\n";

        // 7. Run Phase 2 for candidates
        if (!p1Results.candidates.empty()) {
            std::cout << "\nPhase 2: Calculating precise geometry for " << p1Results.candidates.size() << " candidates...\n";
            Phase2Config p2Config;
            p2Config.refine_orbit = true;  // ENABLE REFINEMENT (Correction enabled)
            p2Config.last_n_obs = 100;     // Use last 100 observations (Verified High Precision)
            p2Config.use_horizons = useHorizons; // Configurable
            
            std::vector<Phase2OccultationEvent> events = p2.calculatePreciseGeometry(p1Results.candidates, p2Config);
            std::cout << "Processed Phase 2 for all candidates. Found " << events.size() << " confirmed events.\n";
            
            for (const auto& evt : events) {
                std::cout << "--------------------------------------------------\n";
                // ... (Output formatting same as before)
                std::cout << "CANDIDATE RESULT (Phase 2):\n";
                std::cout << "  Star ID:  " << evt.star_id << "\n";
                std::cout << "  Star Pos: RA=" << std::fixed << std::setprecision(6) << evt.star_ra_deg 
                          << " Dec=" << evt.star_dec_deg << "\n";
                std::cout << "  Date:     " << std::fixed << std::setprecision(5) << evt.mjd_tdb << "\n";
                std::cout << "  CA Dist:  " << std::fixed << std::setprecision(3) << (evt.min_dist_mas / 1000.0) << "\"\n";
                std::cout << "  Duration: " << std::fixed << std::setprecision(2) << evt.duration_sec << " s\n";
                
                if (!evt.shadow_path.empty()) {
                    std::cout << "  Shadow Path (" << evt.shadow_path.size() << " pts):\n";
                    // Print center
                    size_t center_idx = evt.shadow_path.size() / 2;
                    const auto& pt = evt.shadow_path[center_idx];
                    std::cout << "    [CENTER] Lat: " << pt.lat_deg << " Lon: " << pt.lon_deg << " MJD: " << pt.mjd_tdb << "\n";
                    
                    for (size_t i = 0; i < evt.shadow_path.size(); i += std::max(1ul, evt.shadow_path.size()/5)) { 
                        const auto& p = evt.shadow_path[i];
                        std::cout << "    - Lat: " << std::setw(8) << std::fixed << std::setprecision(4) << p.lat_deg 
                                  << " Lon: " << std::setw(8) << p.lon_deg << " MJD: " << p.mjd_tdb << "\n";
                    }
                }

                if (evt.is_valid) {
                    std::cout << "  *** VALID OCCULTATION (<1\") ***\n";
                }
                std::cout << "--------------------------------------------------\n";
            }
        } else {
             std::cout << "No candidates found in Phase 1.\n";
        }

        std::cout << "\nNative API refactor workflow completed.\n";
        return 0;

    } catch (const std::exception& e) {
        std::cerr << "\n✗ CRITICAL ERROR: " << e.what() << "\n\n";
        return 1;
    }
}
