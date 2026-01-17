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
        std::cout << "Skipping Phase 1 Screening..." << std::endl;
        Phase1Results p1Results; //Empty
        //Phase1Results p1Results = p1.screenCandidates(p1Config);
        //std::cout << "Phase 1 Candidates: " << p1Results.candidates.size() << std::endl;

        // 7. Run Phase 2
        std::cout << "Running Phase 2 Precise Geometry..." << std::endl;
        Phase2Config p2Config;
        p2Config.refine_orbit = false;
        p2Config.use_horizons = false; // Using manual injection below
        
        // Load from database (.oel file)
        std::string oel_path = "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/build/13477.oel";
        std::cout << "Loading elements from database: " << oel_path << std::endl;
        
        if (!shared_astdyn->loadFromEQ1File(oel_path)) {
            std::cerr << "CRITICAL: Failed to load .oel file!" << std::endl;
            return 1;
        }     
        
        // Manual verification of HIP 13752
        CandidateStar target;
        target.ra_deg = 44.2564778388109; // Star position at 2016.0
        target.dec_deg = 17.2394073877972;
        target.pmra = 18.93398835008;
        target.pmdec = -19.3307773902512;
        target.pmdec = -19.3307773902512;
        target.source_id = 35169528681869696;
        target.closest_approach_mjd = 61060.0058; // Estimated time
        
        std::vector<CandidateStar> targets = {target};
        std::vector<Phase2OccultationEvent> events = p2.calculatePreciseGeometry(targets, p2Config);
        
        for (const auto& evt : events) {
            std::cout << "\n=== UTKIN VERIFICATION ===\n";
            std::cout << "Event Time: " << std::fixed << std::setprecision(6) << evt.mjd_tdb << " (TDB)\n";
            std::cout << "Star App RA:  " << evt.star_ra_deg << " deg\n";
            std::cout << "Star App Dec: " << evt.star_dec_deg << " deg\n";
            std::cout << "CA Dist:      " << evt.min_dist_mas << " mas\n";
            if (!evt.shadow_path.empty()) {
                size_t mid_idx = evt.shadow_path.size() / 2;
                std::cout << "Path Center:  Lat " << evt.shadow_path[mid_idx].lat_deg 
                          << " Lon " << evt.shadow_path[mid_idx].lon_deg << "\n";
            }
            std::cout << "==========================\n";
        }

        return 0;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
}
