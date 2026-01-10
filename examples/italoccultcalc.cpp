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
    std::string catalogPath = homeDir + "/.catalog/gaia_mag18_v2_multifile";
    
    nlohmann::json gaiaConfJson;
    gaiaConfJson["catalog_type"] = "multifile_v2";
    gaiaConfJson["multifile_directory"] = catalogPath;
    gaiaConfJson["log_level"] = "warning";
    
    ioc::gaia::UnifiedGaiaCatalog::initialize(gaiaConfJson.dump());
    
    std::cout << "ITALOccultCalc v1.0 [Native API Workflow]\n";

    try {
        // 3. Setup Configuration
        Phase1Config p1Config;
        p1Config.asteroid_name = "34713";
        p1Config.max_magnitude = 18.0;
        p1Config.corridor_width_deg = 1.0;
        p1Config.start_mjd_tdb = 61049.0; // 2026-01-09
        p1Config.end_mjd_tdb = 61051.0;   // 2026-01-11
        p1Config.threshold_arcsec = 60.0; // Relaxed threshold
        
        // 4. Initialize SPK Reader for Earth positions
        auto spk = std::make_shared<SPICESPKReader>();
        std::string kernelsDir = homeDir + "/.ioccultcalc/ephemerides/";
        
        // Forzatura DE440.bsp
        if (!spk->loadFile(kernelsDir + "de440.bsp")) {
             std::cerr << "Critical: Failed to load de440.bsp.\n";
             return 1;
        } else {
            std::cout << "Successfully loaded de440.bsp\n";
        }
        
        // 5. Initialize Components
        Phase1CandidateScreening p1;
        p1.setSPKReader(spk);
        
        Phase2OccultationGeometry p2; 
        p2.setSPKReader(spk);

        std::cout << "Loading asteroid 34713 elements from EQ1...\n";
        bool p1_loaded = p1.loadAsteroidFromEQ1(34713, "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/34713.eq1");
        bool p2_loaded = p2.loadAsteroidFromEQ1(34713, "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/34713.eq1");
        
        if (!p1_loaded || !p2_loaded) {
            std::cerr << "Failed to load asteroid elements.\n";
            return 1;
        }

        // 6. Run Phase 1
        std::cout << "Phase 1: Screening stars in " << p1Config.corridor_width_deg << " deg corridor...\n";
        Phase1Results p1Results = p1.screenCandidates(p1Config);
        std::cout << "Found " << p1Results.candidates.size() << " candidates (" 
                  << p1Results.num_stars_in_corridor << " stars initially in corridor).\n";

        // 7. Run Phase 2 for candidates
        if (!p1Results.candidates.empty()) {
            // VERIFICA SPECIFICA STELLA RICHIESTA DALL'UTENTE
            long long target_star_id = 3441366322859801216LL;
            bool found_target = false;
            for (const auto& c : p1Results.candidates) {
                if (c.source_id == target_star_id) {
                    std::cout << "[VERIFY] Star " << target_star_id << " FOUND in Phase 1 candidates!\n";
                    found_target = true;
                    break;
                }
            }
            if (!found_target) {
                std::cout << "[VERIFY] Star " << target_star_id << " NOT found in Phase 1 candidates.\n";
            }

            std::cout << "\nPhase 2: Calculating precise geometry for " << p1Results.candidates.size() << " candidates...\n";
            Phase2Config p2Config;
            
            std::vector<Phase2OccultationEvent> events = p2.calculatePreciseGeometry(p1Results.candidates, p2Config);
            std::cout << "Processed Phase 2 for all candidates.\n";
            
            for (const auto& evt : events) {
                std::cout << "--------------------------------------------------\n";
                std::cout << "CANDIDATE RESULT (Phase 2):\n";
                std::cout << "  Star ID:  " << evt.star_id << (evt.star_id == target_star_id ? " (TARGET)" : "") << "\n";
                std::cout << "  Star Pos: RA=" << std::fixed << std::setprecision(6) << evt.star_ra_deg 
                          << " Dec=" << evt.star_dec_deg << "\n";
                std::cout << "  Date:     " << std::fixed << std::setprecision(5) << evt.mjd_tdb << "\n";
                std::cout << "  CA Dist:  " << std::fixed << std::setprecision(3) << (evt.min_dist_mas / 1000.0) << "\"\n";
                std::cout << "  Duration: " << std::fixed << std::setprecision(2) << evt.duration_sec << " s\n";
                
                if (!evt.shadow_path.empty()) {
                    std::cout << "  Shadow Path (" << evt.shadow_path.size() << " pts):\n";
                    for (size_t i = 0; i < evt.shadow_path.size(); i += 5) { // Print every 5th point
                        const auto& pt = evt.shadow_path[i];
                        std::cout << "    - Lat: " << std::setw(8) << std::fixed << std::setprecision(4) << pt.lat_deg 
                                  << " Lon: " << std::setw(8) << pt.lon_deg << " MJD: " << pt.mjd_tdb << "\n";
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
