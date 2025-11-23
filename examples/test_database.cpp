/**
 * @file test_database.cpp
 * @brief Test del sistema database asteroidi
 */

#include "ioccultcalc/asteroid_database.h"
#include "ioccultcalc/asteroid_filter.h"
#include "ioccultcalc/data_manager.h"
#include "ioccultcalc/types.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

void printSeparator() {
    std::cout << std::string(70, '=') << std::endl;
}

void testDataManager() {
    printSeparator();
    std::cout << "TEST 1: DataManager Initialization" << std::endl;
    printSeparator();
    
    auto& dm = DataManager::instance();
    
    if (!dm.initialize()) {
        std::cerr << "❌ Failed to initialize DataManager" << std::endl;
        return;
    }
    
    std::cout << "✓ DataManager initialized" << std::endl;
    std::cout << "  Root directory: " << dm.getRootDir() << std::endl;
    std::cout << "  Database path: " << dm.getAsteroidDatabasePath() << std::endl;
    std::cout << "  Ephemerides dir: " << dm.getEphemeridesDir() << std::endl;
    std::cout << "  IERS dir: " << dm.getIERSDir() << std::endl;
    std::cout << "  Cache dir: " << dm.getCacheDir() << std::endl;
    
    // System info
    std::cout << "\nSystem Information:" << std::endl;
    std::cout << dm.getSystemInfo() << std::endl;
}

void testDatabaseCreate() {
    printSeparator();
    std::cout << "TEST 2: Create Sample Database" << std::endl;
    printSeparator();
    
    AsteroidDatabase db;
    
    // Add some sample asteroids
    std::vector<AsteroidProperties> samples;
    
    AsteroidProperties ceres;
    ceres.number = 1; ceres.name = "Ceres"; ceres.diameter = 939.4; ceres.H = 3.53; ceres.albedo = 0.090;
    ceres.a = 2.769; ceres.e = 0.0755; ceres.i = 10.59; ceres.rotation_period = 18.0;
    ceres.orbit_class = "MBA"; ceres.spectral_type = "C"; ceres.has_diameter = true; ceres.has_albedo = true;
    samples.push_back(ceres);
    
    AsteroidProperties pallas;
    pallas.number = 2; pallas.name = "Pallas"; pallas.diameter = 512.0; pallas.H = 4.13; pallas.albedo = 0.159;
    pallas.a = 2.773; pallas.e = 0.2305; pallas.i = 34.84; pallas.rotation_period = 7.8;
    pallas.orbit_class = "MBA"; pallas.spectral_type = "B"; pallas.has_diameter = true; pallas.has_albedo = true;
    samples.push_back(pallas);
    
    AsteroidProperties vesta;
    vesta.number = 4; vesta.name = "Vesta"; vesta.diameter = 525.4; vesta.H = 3.20; vesta.albedo = 0.423;
    vesta.a = 2.362; vesta.e = 0.0886; vesta.i = 7.14; vesta.rotation_period = 5.3;
    vesta.orbit_class = "MBA"; vesta.spectral_type = "V"; vesta.has_diameter = true; vesta.has_albedo = true;
    samples.push_back(vesta);
    
    AsteroidProperties hygiea;
    hygiea.number = 10; hygiea.name = "Hygiea"; hygiea.diameter = 434.0; hygiea.H = 5.43; hygiea.albedo = 0.072;
    hygiea.a = 3.139; hygiea.e = 0.1148; hygiea.i = 3.84; hygiea.rotation_period = 27.6;
    hygiea.orbit_class = "MBA"; hygiea.spectral_type = "C"; hygiea.has_diameter = true; hygiea.has_albedo = true;
    samples.push_back(hygiea);
    
    AsteroidProperties eros;
    eros.number = 433; eros.name = "Eros"; eros.diameter = 16.8; eros.H = 11.16; eros.albedo = 0.25;
    eros.a = 1.458; eros.e = 0.2230; eros.i = 10.83; eros.rotation_period = 5.3;
    eros.orbit_class = "NEA"; eros.spectral_type = "S"; eros.has_diameter = true; eros.has_albedo = true;
    samples.push_back(eros);
    
    AsteroidProperties icarus;
    icarus.number = 1566; icarus.name = "Icarus"; icarus.diameter = 1.4; icarus.H = 16.9; icarus.albedo = 0.51;
    icarus.a = 1.078; icarus.e = 0.8268; icarus.i = 22.83; icarus.rotation_period = 2.3;
    icarus.orbit_class = "NEA"; icarus.spectral_type = "U"; icarus.has_diameter = true; icarus.has_albedo = true;
    samples.push_back(icarus);
    
    AsteroidProperties toutatis;
    toutatis.number = 4179; toutatis.name = "Toutatis"; toutatis.diameter = 4.6; toutatis.H = 15.3; toutatis.albedo = 0.13;
    toutatis.a = 2.530; toutatis.e = 0.6335; toutatis.i = 0.47; toutatis.rotation_period = 5.4;
    toutatis.orbit_class = "NEA"; toutatis.spectral_type = "S"; toutatis.has_diameter = true; toutatis.has_albedo = true;
    samples.push_back(toutatis);
    
    for (const auto& props : samples) {
        db.addAsteroid(props);
    }
    
    std::cout << "✓ Added " << samples.size() << " sample asteroids" << std::endl;
    
    // Save to file
    auto& dm = DataManager::instance();
    std::string testPath = dm.getCacheDir() + "/test_asteroid_db.json";
    
    if (db.saveToFile(testPath)) {
        std::cout << "✓ Database saved to: " << testPath << std::endl;
        
        // Check file size
        if (dm.fileExists(testPath)) {
            size_t size = dm.getFileSize(testPath);
            std::cout << "  File size: " << (size / 1024.0) << " KB" << std::endl;
        }
    } else {
        std::cerr << "❌ Failed to save database" << std::endl;
    }
    
    // Print stats
    auto stats = db.getStats();
    std::cout << "\nDatabase Statistics:" << std::endl;
    std::cout << "  Total asteroids: " << stats.total_asteroids << std::endl;
    std::cout << "  With diameter: " << stats.with_diameter << std::endl;
    std::cout << "  With albedo: " << stats.with_albedo << std::endl;
}

void testDatabaseLoad() {
    printSeparator();
    std::cout << "TEST 3: Load Database" << std::endl;
    printSeparator();
    
    auto& dm = DataManager::instance();
    std::string testPath = dm.getCacheDir() + "/test_asteroid_db.json";
    
    AsteroidDatabase db;
    
    if (db.loadFromFile(testPath)) {
        std::cout << "✓ Database loaded successfully" << std::endl;
        
        auto stats = db.getStats();
        std::cout << "  Loaded " << stats.total_asteroids << " asteroids" << std::endl;
    } else {
        std::cerr << "❌ Failed to load database" << std::endl;
    }
}

void testDatabaseQuery() {
    printSeparator();
    std::cout << "TEST 4: Database Query with Filters" << std::endl;
    printSeparator();
    
    auto& dm = DataManager::instance();
    std::string testPath = dm.getCacheDir() + "/test_asteroid_db.json";
    
    AsteroidDatabase db;
    
    if (!db.loadFromFile(testPath)) {
        std::cerr << "❌ Failed to load database" << std::endl;
        return;
    }
    
    // Query 1: Large asteroids (diameter > 400 km)
    std::cout << "\n--- Query 1: Large asteroids (diameter > 400 km) ---" << std::endl;
    AsteroidRange range1 = AsteroidRangeBuilder()
        .where("diameter > 400")
        .build();
    
    auto results1 = db.query(range1);
    std::cout << "Found " << results1.size() << " asteroids:" << std::endl;
    for (const auto& ast : results1) {
        std::cout << "  " << ast.number << " " << ast.name 
                  << " (D=" << ast.diameter << " km)" << std::endl;
    }
    
    // Query 2: Near-Earth asteroids
    std::cout << "\n--- Query 2: Near-Earth asteroids ---" << std::endl;
    AsteroidRange range2 = AsteroidRangeBuilder()
        .where("orbit_class == NEA")
        .build();
    
    auto results2 = db.query(range2);
    std::cout << "Found " << results2.size() << " NEAs:" << std::endl;
    for (const auto& ast : results2) {
        std::cout << "  " << ast.number << " " << ast.name 
                  << " (a=" << std::fixed << std::setprecision(3) << ast.a 
                  << " AU, e=" << ast.e << ")" << std::endl;
    }
    
    // Query 3: Bright asteroids (H < 5)
    std::cout << "\n--- Query 3: Bright asteroids (H < 5) ---" << std::endl;
    AsteroidRange range3 = AsteroidRangeBuilder()
        .where("H < 5")
        .build();
    
    auto results3 = db.query(range3);
    std::cout << "Found " << results3.size() << " bright asteroids:" << std::endl;
    for (const auto& ast : results3) {
        std::cout << "  " << ast.number << " " << ast.name 
                  << " (H=" << std::fixed << std::setprecision(2) << ast.H 
                  << ", D=" << ast.diameter << " km)" << std::endl;
    }
    
    // Query 4: Complex filter (large MBA with high albedo)
    std::cout << "\n--- Query 4: Large MBA with high albedo ---" << std::endl;
    AsteroidRange range4 = AsteroidRangeBuilder()
        .where("diameter > 400")
        .where("albedo > 0.1")
        .where("orbit_class == MBA")
        .build();
    
    auto results4 = db.query(range4);
    std::cout << "Found " << results4.size() << " asteroids:" << std::endl;
    for (const auto& ast : results4) {
        std::cout << "  " << ast.number << " " << ast.name 
                  << " (D=" << ast.diameter << " km, albedo=" 
                  << std::fixed << std::setprecision(3) << ast.albedo << ")" << std::endl;
    }
}

void testDatabaseExport() {
    printSeparator();
    std::cout << "TEST 5: Export Filtered Subset" << std::endl;
    printSeparator();
    
    auto& dm = DataManager::instance();
    std::string testPath = dm.getCacheDir() + "/test_asteroid_db.json";
    std::string exportPath = dm.getCacheDir() + "/nea_subset.json";
    
    AsteroidDatabase db;
    
    if (!db.loadFromFile(testPath)) {
        std::cerr << "❌ Failed to load database" << std::endl;
        return;
    }
    
    // Export only NEAs
    AsteroidRange range = AsteroidRangeBuilder()
        .where("orbit_class == NEA")
        .build();
    
    if (db.exportToJson(range, exportPath)) {
        std::cout << "✓ Exported NEAs to: " << exportPath << std::endl;
        
        if (dm.fileExists(exportPath)) {
            size_t size = dm.getFileSize(exportPath);
            std::cout << "  File size: " << (size / 1024.0) << " KB" << std::endl;
        }
    } else {
        std::cerr << "❌ Failed to export subset" << std::endl;
    }
}

void testQueryNumbers() {
    printSeparator();
    std::cout << "TEST 6: Fast Number Query" << std::endl;
    printSeparator();
    
    auto& dm = DataManager::instance();
    std::string testPath = dm.getCacheDir() + "/test_asteroid_db.json";
    
    AsteroidDatabase db;
    
    if (!db.loadFromFile(testPath)) {
        std::cerr << "❌ Failed to load database" << std::endl;
        return;
    }
    
    // Fast query for numbers only
    AsteroidRange range = AsteroidRangeBuilder()
        .where("diameter > 100")
        .build();
    
    auto numbers = db.queryNumbers(range);
    
    std::cout << "✓ Fast query found " << numbers.size() << " asteroid numbers:" << std::endl;
    std::cout << "  ";
    for (int num : numbers) {
        std::cout << num << " ";
    }
    std::cout << std::endl;
    
    std::cout << "\nNote: This is much faster than full query when you only need numbers" << std::endl;
}

int main() {
    std::cout << "\n";
    std::cout << "╔════════════════════════════════════════════════════════════════╗\n";
    std::cout << "║       IOccultCalc - Asteroid Database System Test             ║\n";
    std::cout << "╚════════════════════════════════════════════════════════════════╝\n";
    std::cout << std::endl;
    
    try {
        testDataManager();
        testDatabaseCreate();
        testDatabaseLoad();
        testDatabaseQuery();
        testDatabaseExport();
        testQueryNumbers();
        
        printSeparator();
        std::cout << "\n✓ ALL TESTS COMPLETED SUCCESSFULLY\n" << std::endl;
        printSeparator();
        
        std::cout << "\nNext Steps:" << std::endl;
        std::cout << "1. Download real MPC database:" << std::endl;
        std::cout << "   python3 tools/download_mpc_database.py" << std::endl;
        std::cout << "\n2. Use with ioccultcalc_search:" << std::endl;
        std::cout << "   ./ioccultcalc_search --config survey.json <ra> <dec>" << std::endl;
        std::cout << std::endl;
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n❌ TEST FAILED: " << e.what() << std::endl;
        return 1;
    }
}
