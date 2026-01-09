/**
 * @file italoccultcalc.cpp
 * @brief ITALOccultCalc - Refactored Main Entry Point
 * 
 * Thin wrapper that delegates all simulation logic to SimulationEngine.
 */

#include "ioccultcalc/simulation_engine.h"
#include "ioccultcalc/config_manager.h"
#include "ioc_gaialib/unified_gaia_catalog.h"
#include "starmap/StarMap.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main(int argc, char* argv[]) {
    // 1. Initialize StarMap (Catalog System)
    starmap::config::LibraryConfig::CatalogPaths paths;
    paths.gaiaSaoDatabase = "/Users/michelebigi/.catalog/crossreference/gaia_sao_xmatch.db";
    paths.iauCatalog = "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/external/IOC_GaiaLib/data/IAU-CSN.json";
    paths.starNamesDatabase = "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/external/IOC_GaiaLib/data/common_star_names.csv";
    // 2. Initialize UnifiedGaiaCatalog
    std::string homeDir = getenv("HOME");
    std::string catalogPath = homeDir + "/.catalog/gaia_mag18_v2_multifile";
    
    nlohmann::json gaiaConfJson;
    gaiaConfJson["catalog_type"] = "multifile_v2";
    gaiaConfJson["multifile_directory"] = catalogPath;
    gaiaConfJson["log_level"] = "warning";
    
    ioc::gaia::UnifiedGaiaCatalog::initialize(gaiaConfJson.dump());
    
    std::cout << "ITALOccultCalc v1.0 [Refactored]\n";
    std::cout << "Composition Root initialized.\n\n";

    try {
        // 2. Resolve Configuration File
        std::string configFile = "preset_default.json";
        if (argc > 1) configFile = argv[1];

        // 3. Load Configuration
        ConfigManager config;
        if (configFile.find(".json") != std::string::npos) {
            config.loadFromJson(configFile);
        } else {
            config.loadFromOop(configFile);
        }

        // 4. Instantiate and Run Simulation Engine
        SimulationEngine engine(config);
        SimulationSummary summary = engine.run();

        // 5. Final Summary
        std::cout << "\n================================================================\n";
        std::cout << "SIMULATION SUMMARY\n";
        std::cout << "================================================================\n";
        std::cout << "Total Asteroids:     " << summary.total_asteroids << "\n";
        std::cout << "Precise Events:      " << summary.total_precise_events << "\n";
        std::cout << "Execution Time:      " << std::fixed << std::setprecision(2) << summary.execution_time_sec << " s\n";
        std::cout << "================================================================\n\n";

        return 0;

    } catch (const std::exception& e) {
        std::cerr << "\n✗ CRITICAL ERROR: " << e.what() << "\n\n";
        return 1;
    }
}
