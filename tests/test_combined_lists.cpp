/**
 * @file test_combined_lists.cpp
 * @brief Test per liste combinate (range + filtri + file multipli)
 * @author IOccultCalc Team
 */

#include "ioccultcalc/asteroid_filter.h"
#include <iostream>
#include <fstream>
#include <set>
#include <algorithm>

using namespace ioccultcalc;

// Helper per creare file di test
void createTestFile(const std::string& filename, const std::vector<int>& numbers) {
    std::ofstream out(filename);
    for (int num : numbers) {
        out << num << "\n";
    }
    out.close();
}

int main() {
    std::cout << "=== Test Combined Lists ===\n\n";
    
    // Crea file di test
    std::cout << "Creating test files...\n";
    createTestFile("test_local1.txt", {1, 5, 10, 15, 20});
    createTestFile("test_local2.txt", {100, 200, 300, 400, 500});
    
    // TEST 1: Range semplice + file singolo
    std::cout << "\nTEST 1: Range [1-10] + file (test_local1.txt)\n";
    {
        AsteroidRangeBuilder builder;
        builder.from(1).to(10)
               .addListFromFile("test_local1.txt");
        
        AsteroidRange range = builder.build();
        auto list = range.getAsteroidList();
        
        std::cout << "Expected: 1-10 + {1,5,10,15,20} = {1-10, 15, 20}\n";
        std::cout << "Result (" << list.size() << " items): ";
        
        // Dedup e sort
        std::set<int> unique(list.begin(), list.end());
        for (int num : unique) {
            std::cout << num << " ";
        }
        std::cout << "\n";
        
        // Verifica che contenga almeno 15 e 20 (oltre range)
        bool has15 = std::find(list.begin(), list.end(), 15) != list.end();
        bool has20 = std::find(list.begin(), list.end(), 20) != list.end();
        
        if (has15 && has20) {
            std::cout << "✓ PASS: File merged correctly\n";
        } else {
            std::cout << "✗ FAIL: Missing numbers from file\n";
            return 1;
        }
    }
    
    // TEST 2: Range vuoto + file multipli
    std::cout << "\nTEST 2: File multipli (test_local1 + test_local2)\n";
    {
        AsteroidRangeBuilder builder;
        builder.addListFromFile("test_local1.txt")
               .addListFromFile("test_local2.txt");
        
        AsteroidRange range = builder.build();
        auto list = range.getAsteroidList();
        
        std::cout << "Expected: {1,5,10,15,20,100,200,300,400,500}\n";
        std::cout << "Result (" << list.size() << " items): ";
        
        // Dedup e sort
        std::set<int> unique(list.begin(), list.end());
        for (int num : unique) {
            std::cout << num << " ";
        }
        std::cout << "\n";
        
        // Verifica numeri da entrambi i file
        bool hasFromFile1 = std::find(list.begin(), list.end(), 15) != list.end();
        bool hasFromFile2 = std::find(list.begin(), list.end(), 300) != list.end();
        
        if (hasFromFile1 && hasFromFile2 && unique.size() == 10) {
            std::cout << "✓ PASS: Multiple files merged correctly\n";
        } else {
            std::cout << "✗ FAIL: Expected 10 unique numbers\n";
            return 1;
        }
    }
    
    // TEST 3: addToList() method
    std::cout << "\nTEST 3: addToList() con vettori custom\n";
    {
        AsteroidRangeBuilder builder;
        builder.from(1).to(5);
        
        // Aggiungi liste custom
        std::vector<int> custom1 = {100, 200};
        std::vector<int> custom2 = {300, 400, 500};
        
        builder.addToList(custom1);
        builder.addToList(custom2);
        
        AsteroidRange range = builder.build();
        auto list = range.getAsteroidList();
        
        std::cout << "Expected: {1-5, 100, 200, 300, 400, 500}\n";
        std::cout << "Result (" << list.size() << " items): ";
        
        // Dedup e sort
        std::set<int> unique(list.begin(), list.end());
        for (int num : unique) {
            std::cout << num << " ";
        }
        std::cout << "\n";
        
        bool hasRange = std::find(list.begin(), list.end(), 3) != list.end();
        bool hasCustom = std::find(list.begin(), list.end(), 400) != list.end();
        
        if (hasRange && hasCustom && unique.size() == 10) {
            std::cout << "✓ PASS: addToList() works correctly\n";
        } else {
            std::cout << "✗ FAIL: Expected 10 unique numbers\n";
            return 1;
        }
    }
    
    // TEST 4: Range + explicit list + file (caso reale utente)
    std::cout << "\nTEST 4: Range [1-1000] + file localasteroid (simulated)\n";
    {
        // Simula localasteroid con numeri sparsi
        createTestFile("test_localasteroid.txt", {2999, 4579, 7481, 8558, 12345});
        
        AsteroidRangeBuilder builder;
        builder.from(1).to(1000)
               .addListFromFile("test_localasteroid.txt");
        
        AsteroidRange range = builder.build();
        auto list = range.getAsteroidList();
        
        std::cout << "Expected: 1-1000 + {2999, 4579, 7481, 8558, 12345}\n";
        std::cout << "Total items: " << list.size() << "\n";
        
        // Verifica che contenga numeri da localasteroid
        bool has2999 = std::find(list.begin(), list.end(), 2999) != list.end();
        bool has12345 = std::find(list.begin(), list.end(), 12345) != list.end();
        
        // Dedup per verificare dimensione
        std::set<int> unique(list.begin(), list.end());
        
        if (has2999 && has12345 && unique.size() == 1005) {
            std::cout << "✓ PASS: 1000 from range + 5 from file = 1005 unique\n";
        } else {
            std::cout << "✗ FAIL: Expected 1005 unique numbers, got " << unique.size() << "\n";
            return 1;
        }
    }
    
    // Cleanup
    std::remove("test_local1.txt");
    std::remove("test_local2.txt");
    std::remove("test_localasteroid.txt");
    
    std::cout << "\n=== ALL TESTS PASSED ===\n";
    return 0;
}
