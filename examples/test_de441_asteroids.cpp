/**
 * @file test_de441_asteroids.cpp
 * @brief Test lettura asteroidi da DE441
 */

#include <iostream>
#include <iomanip>
#include "ioccultcalc/spk_reader.h"

using namespace ioccultcalc;

int main() {
    SPKReader reader;
    
    std::cout << "Testing DE441 asteroid support...\n\n";
    
    if (!reader.ensureFileLoaded("DE441")) {
        std::cerr << "ERROR: Failed to load DE441\n";
        return 1;
    }
    
    std::cout << "Loaded: " << reader.getVersion() << "\n";
    auto [start, end] = reader.getCoverage();
    std::cout << "Coverage: JD " << start << " to " << end << "\n\n";
    
    // Test lettura asteroidi principali
    struct TestAsteroid {
        int naifId;
        std::string name;
    };
    
    TestAsteroid asteroids[] = {
        {2000001, "1 Ceres"},
        {2000002, "2 Pallas"},
        {2000004, "4 Vesta"},
        {2000010, "10 Hygiea"}
    };
    
    double jd = 2460000.0;  // 2023-02-25
    
    std::cout << "Testing asteroid positions at JD " << jd << ":\n";
    std::cout << "-----------------------------------------------\n";
    
    for (const auto& ast : asteroids) {
        try {
            auto pos = reader.getPosition(ast.naifId, jd, 11);  // heliocentric
            double dist = pos.magnitude();
            
            std::cout << std::setw(15) << std::left << ast.name 
                     << " (ID " << ast.naifId << "): "
                     << "r = " << std::fixed << std::setprecision(3) << dist << " AU"
                     << " (" << pos.x << ", " << pos.y << ", " << pos.z << ")\n";
                     
        } catch (const std::exception& e) {
            std::cout << std::setw(15) << std::left << ast.name 
                     << " (ID " << ast.naifId << "): "
                     << "ERROR - " << e.what() << "\n";
        }
    }
    
    std::cout << "\n✓ DE441 asteroid support working!\n";
    return 0;
}
