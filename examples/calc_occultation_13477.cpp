#include "ioccultcalc/config_manager.h"
#include "ioc_gaialib/unified_gaia_catalog.h"
#include "starmap/StarMap.h"
#include "phase1_candidate_screening.h"
#include "phase2_occultation_geometry.h"
#include "ioccultcalc/spice_spk_reader.h"
#include "astdyn_wrapper.h" // For direct verification if needed
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main(int argc, char* argv[]) {
    // 1. Initialize StarMap (Catalog System) - standard paths
    starmap::config::LibraryConfig::CatalogPaths paths;
    paths.gaiaSaoDatabase = std::string(getenv("HOME")) + "/.catalog/crossreference/gaia_sao_xmatch.db";
    // Dummy paths for map features not strictly needed for calculation but initialization requires them
    paths.iauCatalog = "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/external/IOC_GaiaLib/data/IAU-CSN.json";
    paths.starNamesDatabase = "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/external/IOC_GaiaLib/data/common_star_names.csv";
    starmap::initialize(paths);

    // 2. Initialize UnifiedGaiaCatalog (SQLite DR3)
    std::string homeDir = getenv("HOME");
    nlohmann::json gaiaConfJson;
    gaiaConfJson["catalog_type"] = "sqlite_dr3";
    gaiaConfJson["sqlite_file_path"] = homeDir + "/.catalog/crossreference/gaia_dr3_occult_pro.db";
    gaiaConfJson["log_level"] = "warning";
    
    ioc::gaia::UnifiedGaiaCatalog::initialize(gaiaConfJson.dump());
    
    std::cout << "=== Calculating Occultation for 13477 Utkin ===" << std::endl;

    try {
        // 3. Setup Configuration for Event: Jan 20, 2026
        // Search window: Jan 19 - Jan 21 to cover the event
        Phase1Config p1Config;
        p1Config.asteroid_name = "13477"; // Will trigger DB load
        // p1Config.asteroid_number = 13477; // Removed as it's not a member
        p1Config.max_magnitude = 14.0;    // HIP 13752 is bright (~8.7)
        p1Config.corridor_width_deg = 5.0; // Wide corridor to be safe
        p1Config.start_mjd_tdb = 61059.0; // 2026-01-19
        p1Config.end_mjd_tdb = 61061.0;   // 2026-01-21
        p1Config.threshold_arcsec = 600.0; // Very generous threshold
        
        // 4. Initialize SPK Reader
        auto spk = std::make_shared<SPICESPKReader>();
        std::string kernelsDir = homeDir + "/.ioccultcalc/ephemerides/";
        
        // Try to load default DE440
        if (!spk->loadFile(kernelsDir + "de440.bsp")) {
             // Fallback to what we have
             if (!spk->loadFile(kernelsDir + "de441_part-2.bsp")) {
                 std::cerr << "Critical: Failed to load DE ephemeris." << std::endl;
                 return 1;
             }
        }
        std::cout << "Ephemeris loaded." << std::endl;
        
        // 5. Initialize Pipeline
        // Use High Accuracy settings (with the fixes we just applied)
        auto shared_astdyn = std::make_shared<AstDynWrapper>(PropagationSettings::highAccuracy());
        
        Phase1CandidateScreening p1;
        p1.setSPKReader(spk);
        
        Phase2OccultationGeometry p2; 
        p2.setAstDynWrapper(shared_astdyn);
        p2.setSPKReader(spk);

        // Load 13477 from DB
        std::cout << "Loading 13477 from SQLite DB..." << std::endl;
        bool p1_loaded = p1.loadAsteroidFromDB(13477);
        bool p2_loaded = p2.loadAsteroidFromDB(13477);
        
        if (!p1_loaded || !p2_loaded) {
            std::cerr << "Failed to load asteroid 13477 from database!" << std::endl;
            // Fallback: Set manually if DB fails (Redundancy for the demo)
            std::cerr << "Attempting manual injection of 13477 elements (MJD 61000)..." << std::endl;
            shared_astdyn->setKeplerianElements(
                2.4485080003194, 0.143329820474419, 0.147662766626375, // a, e, i
                1.18281763160276, 4.73269210633698, 1.03166444813114, // Omega, omega, M
                61000.0, "13477", 
                astdyn::propagation::HighPrecisionPropagator::InputFrame::ECLIPTIC
            );
        } else {
            std::cout << "Asteroid loaded successfully." << std::endl;
        }

        // 6. Run Phase 1
        std::cout << "Running Phase 1 Screening..." << std::endl;
        Phase1Results p1Results = p1.screenCandidates(p1Config);
        std::cout << "Phase 1 Candidates: " << p1Results.candidates.size() << std::endl;

        // 7. Run Phase 2
        if (!p1Results.candidates.empty()) {
            std::cout << "Running Phase 2 Precise Geometry..." << std::endl;
            Phase2Config p2Config;
            p2Config.refine_orbit = true;
            p2Config.use_horizons = false; // DISABLED as requested
            
            std::vector<Phase2OccultationEvent> events = p2.calculatePreciseGeometry(p1Results.candidates, p2Config);
            
            std::cout << "\n=== RESULTS ===" << std::endl;
            bool found_target = false;
            for (const auto& evt : events) {
                // Check if this is HIP 13752 (Gaia ID 35169528681869696)
                // The HIP ID might be stored or we check coordinates (RA ~ 44.256, Dec ~ 17.245)
                bool is_target = (std::abs(evt.star_ra_deg - 44.256) < 0.1 && std::abs(evt.star_dec_deg - 17.245) < 0.1);
                
                if (is_target) found_target = true;

                std::cout << "Star RA: " << evt.star_ra_deg << " Dec: " << evt.star_dec_deg << "\n";
                // std::cout << "Gaia ID: " << evt.star_id << "\n"; // If available
                std::cout << "Closest Approach: " << (evt.min_dist_mas / 1000.0) << " arcsec\n";
                std::cout << "Time (MJD): " << std::fixed << std::setprecision(5) << evt.mjd_tdb << "\n";
                
                if (evt.shadow_path.empty()) {
                    std::cout << "Shadow Path: NO SHADOW (Miss?)\n";
                } else {
                    std::cout << "Shadow Path (" << evt.shadow_path.size() << " points):\n";
                    // Print center point (closest approach)
                    size_t center_idx = evt.shadow_path.size() / 2;
                    const auto& pt = evt.shadow_path[center_idx];
                    std::cout << "  CENTER -> Lat: " << pt.lat_deg << " Lon: " << pt.lon_deg 
                              << " Time: " << pt.mjd_tdb << "\n";
                    
                    // Comparison
                    std::cout << "  [COMPARE] Benchmark Center: Lat 17.34, Lon -76.86\n";
                }
                std::cout << "-----------------------------------\n";
            }
            
            if (found_target) {
                std::cout << "SUCCESS: Target star HIP 13752 was processed." << std::endl;
            } else {
                std::cout << "WARNING: Target star HIP 13752 NOT found in candidates." << std::endl;
                // Double check P1 config corridor
            }
        } else {
            std::cout << "No candidates found in Phase 1." << std::endl;
        }

        return 0;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
}
