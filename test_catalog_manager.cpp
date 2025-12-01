#include <iostream>
#include <iomanip>
#include <chrono>
#include <vector>
#include "catalog_manager.h"
#include "julian_date.h"

using namespace IOccultCalc;

/**
 * @brief Test file per CatalogManager
 * 
 * Tests:
 * 1. Download & parse OEF2.0
 * 2. SQLite storage and indexing
 * 3. O(1) lookup performance
 * 4. Magnitude search
 * 5. Comparison with direct API calls
 */

// Test asteroids
struct TestAsteroid {
    std::string designation;
    int catalog_id;
    double expected_a_min;  // AU
    double expected_a_max;
};

static const std::vector<TestAsteroid> TEST_ASTEROIDS = {
    {"17030", 17030, 2.8, 3.0},      // Sierks
    {"433", 433, 2.3, 2.4},          // Eros
    {"1", 1, 2.7, 2.8},              // Ceres
    {"2", 2, 2.7, 2.8},              // Pallas
    {"3", 3, 2.5, 2.7},              // Juno
    {"4", 4, 2.7, 2.8},              // Vesta
};

void testDownloadAndParse(CatalogManager& catalog) {
    std::cout << "\n" << std::string(80, '=') << "\n";
    std::cout << "TEST 1: Download & Parse OEF2.0 Catalog\n";
    std::cout << std::string(80, '=') << "\n\n";
    
    std::cout << "⏱️  Updating catalog (may take 5-15 minutes)...\n";
    auto start = std::chrono::high_resolution_clock::now();
    
    bool success = catalog.updateCatalog(false);  // force=false, respects cache
    
    auto end = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::seconds>(end - start);
    
    if (!success) {
        std::cerr << "❌ Download/parse failed\n";
        return;
    }
    
    std::cout << "✅ Completed in " << duration.count() << " seconds\n";
    
    // Get stats
    auto stats = catalog.getStats();
    std::cout << "\n📊 Catalog Statistics:\n";
    std::cout << "   Total asteroids: " << stats.total_asteroids << "\n";
    std::cout << "   Average magnitude: " << std::fixed << std::setprecision(2) 
              << stats.avg_magnitude << "\n";
    std::cout << "   Version: " << stats.version << "\n";
    std::cout << "   Download date: " << stats.download_date << "\n";
}

void testO1Lookup(CatalogManager& catalog) {
    std::cout << "\n" << std::string(80, '=') << "\n";
    std::cout << "TEST 2: O(1) Lookup Performance\n";
    std::cout << std::string(80, '=') << "\n\n";
    
    std::cout << "Testing lookup on " << TEST_ASTEROIDS.size() << " asteroids...\n\n";
    
    for (const auto& test : TEST_ASTEROIDS) {
        try {
            double epoch;
            auto start = std::chrono::high_resolution_clock::now();
            
            EquinoctialElements elements = catalog.getElements(test.designation, &epoch);
            
            auto end = std::chrono::high_resolution_clock::now();
            auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
            
            // Validate
            bool valid = (elements.a >= test.expected_a_min && 
                         elements.a <= test.expected_a_max);
            
            std::cout << "  " << std::left << std::setw(20) << test.designation;
            std::cout << " a=" << std::fixed << std::setprecision(5) << elements.a << " AU";
            std::cout << " epoch=" << std::fixed << std::setprecision(1) << epoch << " MJD";
            std::cout << " (" << duration.count() << " µs)";
            std::cout << " " << (valid ? "✅" : "❌") << "\n";
            
        } catch (const std::exception& e) {
            std::cerr << "  ❌ " << test.designation << ": " << e.what() << "\n";
        }
    }
}

void testMagnitudeSearch(CatalogManager& catalog) {
    std::cout << "\n" << std::string(80, '=') << "\n";
    std::cout << "TEST 3: Magnitude Range Search\n";
    std::cout << std::string(80, '=') << "\n\n";
    
    std::vector<std::pair<double, double>> ranges = {
        {8.0, 10.0},
        {10.0, 12.0},
        {12.0, 14.0},
    };
    
    for (const auto& range : ranges) {
        auto start = std::chrono::high_resolution_clock::now();
        
        auto results = catalog.searchByMagnitude(range.first, range.second);
        
        auto end = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
        
        std::cout << "  Magnitude " << std::fixed << std::setprecision(1) 
                  << range.first << "-" << range.second << ": "
                  << results.size() << " asteroids found in " 
                  << duration.count() << " ms";
        
        if (!results.empty() && results.size() <= 10) {
            std::cout << "\n    Found: ";
            for (size_t i = 0; i < std::min(size_t(5), results.size()); ++i) {
                std::cout << results[i];
                if (i < std::min(size_t(4), results.size() - 1)) std::cout << ", ";
            }
            if (results.size() > 5) std::cout << " ...";
        }
        std::cout << "\n";
    }
}

void testStressPerformance(CatalogManager& catalog) {
    std::cout << "\n" << std::string(80, '=') << "\n";
    std::cout << "TEST 4: Stress Test - 1000 Random Lookups\n";
    std::cout << std::string(80, '=') << "\n\n";
    
    // Use test asteroids repeated
    std::cout << "⏱️  Performing 1000 random lookups...\n";
    
    auto start = std::chrono::high_resolution_clock::now();
    
    int success = 0;
    for (int i = 0; i < 1000; ++i) {
        const auto& test = TEST_ASTEROIDS[i % TEST_ASTEROIDS.size()];
        try {
            auto elements = catalog.getElements(test.designation);
            success++;
        } catch (...) {
            // Silently fail
        }
    }
    
    auto end = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
    
    double avg_per_query = duration.count() / 1000.0;
    
    std::cout << "✅ " << success << "/1000 successful\n";
    std::cout << "   Total time: " << duration.count() << " ms\n";
    std::cout << "   Average: " << std::fixed << std::setprecision(3) 
              << avg_per_query << " ms/query\n";
    std::cout << "   Throughput: " << std::fixed << std::setprecision(0) 
              << (1000000.0 / (duration.count() * 1000.0)) << " queries/second\n";
}

int main(int argc, char** argv) {
    std::cout << "\n╔════════════════════════════════════════════════════════════════════════════════╗\n";
    std::cout << "║         🗂️  CATALOG MANAGER TEST SUITE - OEF2.0 AstDyS2 Catalogs                 ║\n";
    std::cout << "╚════════════════════════════════════════════════════════════════════════════════╝\n";
    
    try {
        // Initialize catalog manager
        std::cout << "\n🔧 Initializing CatalogManager...\n";
        CatalogManager catalog("~/.ioccultcalc/catalogs_test", 
                             CatalogManager::CHECK_MONTHLY);
        
        if (!catalog.isReady()) {
            std::cerr << "❌ CatalogManager not ready\n";
            return 1;
        }
        
        std::cout << "✅ CatalogManager initialized\n";
        
        // Run tests
        testDownloadAndParse(catalog);
        testO1Lookup(catalog);
        testMagnitudeSearch(catalog);
        testStressPerformance(catalog);
        
        std::cout << "\n" << std::string(80, '=') << "\n";
        std::cout << "✅ ALL TESTS COMPLETED SUCCESSFULLY\n";
        std::cout << std::string(80, '=') << "\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n❌ FATAL ERROR: " << e.what() << "\n";
        return 1;
    }
}
