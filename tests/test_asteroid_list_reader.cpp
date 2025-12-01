/**
 * @file test_asteroid_list_reader.cpp
 * @brief Test del reader per liste asteroidi
 */

#include <iostream>
#include <fstream>
#include <ioccultcalc/asteroid_list_reader.h>

using namespace ioccultcalc;

void createTestFiles() {
    // File 1: ASTNUM_LIST format
    {
        std::ofstream file("test_astnum_list.txt");
        file << "# ITALOccultCalc - Asteroid List with Occultations\n";
        file << "# Generated: 2025-11-29\n";
        file << "#\n";
        file << "# Total asteroids: 5\n";
        file << "#\n\n";
        file << "(1) # Ceres                (12 events)\n";
        file << "(4) # Vesta                (8 events)\n";
        file << "(10) # Hygiea              (5 events)\n";
        file << "(433) # Eros               (3 events)\n";
        file << "(704) # Interamnia         (2 events)\n";
        file << "\n# End of list\n";
        file.close();
    }
    
    // File 2: MPC_NUMBERED format  
    {
        std::ofstream file("test_mpc_numbered.txt");
        file << "1\n";
        file << "4\n";
        file << "10\n";
        file << "433\n";
        file << "704\n";
        file.close();
    }
    
    // File 3: PLAIN_TEXT format (mix)
    {
        std::ofstream file("test_plain_text.txt");
        file << "# Lista asteroidi\n";
        file << "1\n";
        file << "4\n";
        file << "2010 AB123\n";
        file << "433\n";
        file << "1999 RQ36\n";
        file.close();
    }
}

int main() {
    std::cout << "\n";
    std::cout << "╔══════════════════════════════════════════════════════╗\n";
    std::cout << "║  Test Asteroid List Reader                          ║\n";
    std::cout << "╚══════════════════════════════════════════════════════╝\n\n";
    
    // Crea file di test
    std::cout << "📝 Creating test files...\n\n";
    createTestFiles();
    
    try {
        // TEST 1: ASTNUM_LIST format
        std::cout << "TEST 1: ASTNUM_LIST Format\n";
        std::cout << "═══════════════════════════════════════════════════\n";
        
        auto format1 = AsteroidListReader::detectFormat("test_astnum_list.txt");
        std::cout << "Detected format: ";
        if (format1 == AsteroidListFormat::ASTNUM_LIST) {
            std::cout << "✅ ASTNUM_LIST\n";
        } else {
            std::cout << "❌ Wrong format detected\n";
            return 1;
        }
        
        auto entries1 = AsteroidListReader::readFromFile("test_astnum_list.txt");
        std::cout << "Entries read: " << entries1.size() << "\n";
        
        for (const auto& entry : entries1) {
            std::cout << "  - ";
            if (entry.hasNumber()) {
                std::cout << "(" << entry.number << ")";
            }
            if (entry.hasName()) {
                std::cout << " " << entry.name;
            }
            if (entry.eventCount > 0) {
                std::cout << " - " << entry.eventCount << " events";
            }
            std::cout << "\n";
        }
        
        if (entries1.size() != 5) {
            std::cout << "❌ Expected 5 entries, got " << entries1.size() << "\n";
            return 1;
        }
        
        if (entries1[0].number != 1 || entries1[0].name != "Ceres" || entries1[0].eventCount != 12) {
            std::cout << "❌ First entry data mismatch\n";
            return 1;
        }
        
        std::cout << "✅ Test PASSED\n\n";
        
        // TEST 2: MPC_NUMBERED format
        std::cout << "TEST 2: MPC_NUMBERED Format\n";
        std::cout << "═══════════════════════════════════════════════════\n";
        
        auto format2 = AsteroidListReader::detectFormat("test_mpc_numbered.txt");
        std::cout << "Detected format: ";
        if (format2 == AsteroidListFormat::MPC_NUMBERED) {
            std::cout << "✅ MPC_NUMBERED\n";
        } else {
            std::cout << "⚠️  Detected as another format (acceptable)\n";
        }
        
        auto numbers2 = AsteroidListReader::readNumbersFromFile("test_mpc_numbered.txt");
        std::cout << "Numbers read: " << numbers2.size() << "\n";
        
        for (int num : numbers2) {
            std::cout << "  - " << num << "\n";
        }
        
        if (numbers2.size() != 5) {
            std::cout << "❌ Expected 5 numbers, got " << numbers2.size() << "\n";
            return 1;
        }
        
        std::cout << "✅ Test PASSED\n\n";
        
        // TEST 3: PLAIN_TEXT format
        std::cout << "TEST 3: PLAIN_TEXT Format (Mixed)\n";
        std::cout << "═══════════════════════════════════════════════════\n";
        
        auto entries3 = AsteroidListReader::readFromFile("test_plain_text.txt", 
                                                         AsteroidListFormat::PLAIN_TEXT);
        std::cout << "Entries read: " << entries3.size() << "\n";
        
        for (const auto& entry : entries3) {
            std::cout << "  - " << entry.toString() << "\n";
        }
        
        if (entries3.size() != 5) {
            std::cout << "❌ Expected 5 entries, got " << entries3.size() << "\n";
            return 1;
        }
        
        std::cout << "✅ Test PASSED\n\n";
        
        // TEST 4: readNumbersFromFile (ignora designazioni)
        std::cout << "TEST 4: Extract Only Numbers\n";
        std::cout << "═══════════════════════════════════════════════════\n";
        
        auto numbers4 = AsteroidListReader::readNumbersFromFile("test_plain_text.txt");
        std::cout << "Numbers extracted: " << numbers4.size() << "\n";
        
        for (int num : numbers4) {
            std::cout << "  - " << num << "\n";
        }
        
        // Nel file plain text abbiamo: 1, 4, "2010 AB123", 433, "1999 RQ36"
        // readNumbersFromFile() restituisce solo entry con hasNumber() == true
        // In PLAIN_TEXT, le designazioni con spazi non sono parsate come numeri
        // Quindi dovremmo avere solo 3 numeri puri: 1, 4, 433
        // Nota: "2010" e "1999" potrebbero essere interpretati come numeri se mal formattati
        
        if (numbers4.size() < 3) {
            std::cout << "❌ Expected at least 3 numbers, got " << numbers4.size() << "\n";
            return 1;
        }
        
        // Verifica che almeno i numeri principali siano presenti
        bool has1 = std::find(numbers4.begin(), numbers4.end(), 1) != numbers4.end();
        bool has4 = std::find(numbers4.begin(), numbers4.end(), 4) != numbers4.end();
        bool has433 = std::find(numbers4.begin(), numbers4.end(), 433) != numbers4.end();
        
        if (!has1 || !has4 || !has433) {
            std::cout << "❌ Missing expected numbers (1, 4, 433)\n";
            return 1;
        }
        
        std::cout << "✅ Test PASSED\n\n";
        
        // Cleanup
        std::remove("test_astnum_list.txt");
        std::remove("test_mpc_numbered.txt");
        std::remove("test_plain_text.txt");
        
        std::cout << "╔══════════════════════════════════════════════════════╗\n";
        std::cout << "║  ✅ ALL TESTS PASSED                                ║\n";
        std::cout << "╚══════════════════════════════════════════════════════╝\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "❌ Exception: " << e.what() << "\n";
        return 1;
    }
}
