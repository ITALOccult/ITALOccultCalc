/**
 * @file test_localasteroid_real.cpp
 * @brief Test con file localasteroid reale
 * @author IOccultCalc Team
 */

#include "ioccultcalc/asteroid_filter.h"
#include <iostream>
#include <fstream>
#include <set>
#include <cstdio>

using namespace ioccultcalc;

int main() {
    std::cout << "=== Test con file localasteroid reale ===\n\n";
    
    try {
        // TEST 1: Solo localasteroid
        std::cout << "TEST 1: Carica solo localasteroid\n";
        {
            AsteroidRangeBuilder builder;
            builder.addListFromFile("../etc/localasteroid");
            
            AsteroidRange range = builder.build();
            auto list = range.getAsteroidList();
            
            std::set<int> unique(list.begin(), list.end());
            std::cout << "Total unique asteroids: " << unique.size() << "\n";
            std::cout << "First 10: ";
            int count = 0;
            for (int num : unique) {
                std::cout << num << " ";
                if (++count >= 10) break;
            }
            std::cout << "\n";
        }
        
        // TEST 2: Range 1-1000 + localasteroid
        std::cout << "\nTEST 2: Range [1-1000] + localasteroid\n";
        {
            AsteroidRangeBuilder builder;
            builder.from(1).to(1000)
                   .addListFromFile("../etc/localasteroid");
            
            AsteroidRange range = builder.build();
            auto list = range.getAsteroidList();
            
            std::set<int> unique(list.begin(), list.end());
            std::cout << "Total unique asteroids: " << unique.size() << "\n";
            
            // Verifica che ci siano alcuni asteroidi oltre 1000
            int countAbove1000 = 0;
            for (int num : unique) {
                if (num > 1000) countAbove1000++;
            }
            std::cout << "Asteroids beyond 1000: " << countAbove1000 << "\n";
            
            if (unique.size() > 1000 && countAbove1000 > 0) {
                std::cout << "✓ PASS: Combined list created successfully\n";
            } else {
                std::cout << "✗ FAIL: Expected more than 1000 asteroids\n";
                return 1;
            }
        }
        
        // TEST 3: File multipli - localasteroid + file custom
        std::cout << "\nTEST 3: localasteroid + file custom\n";
        {
            // Crea file temporaneo
            std::ofstream out("test_custom.txt");
            out << "99999\n88888\n77777\n";
            out.close();
            
            AsteroidRangeBuilder builder;
            builder.addListFromFile("../etc/localasteroid")
                   .addListFromFile("test_custom.txt");
            
            AsteroidRange range = builder.build();
            auto list = range.getAsteroidList();
            
            std::set<int> unique(list.begin(), list.end());
            
            bool has99999 = unique.find(99999) != unique.end();
            bool has88888 = unique.find(88888) != unique.end();
            
            std::cout << "Total unique asteroids: " << unique.size() << "\n";
            
            if (has99999 && has88888) {
                std::cout << "✓ PASS: Multiple files merged correctly\n";
            } else {
                std::cout << "✗ FAIL: Custom numbers not found\n";
                return 1;
            }
            
            std::remove("test_custom.txt");
        }
        
    } catch (const std::exception& e) {
        std::cerr << "ERROR: " << e.what() << "\n";
        return 1;
    }
    
    std::cout << "\n=== ALL TESTS PASSED ===\n";
    return 0;
}
