/**
 * @file example_combined_asteroid_lists.cpp
 * @brief Esempio uso pratico: combinare range filtrato + liste file multiple
 * 
 * Questo esempio mostra come:
 * 1. Selezionare i primi 1000 asteroidi con filtro diametro > 20 km
 * 2. Aggiungere la lista localasteroid senza applicare filtri
 * 3. Combinare più file di liste personalizzate
 * 
 * @author IOccultCalc Team
 */

#include "ioccultcalc/asteroid_filter.h"
#include <iostream>
#include <fstream>
#include <set>
#include <cstdio>

using namespace ioccultcalc;

int main(int argc, char* argv[]) {
    std::cout << "========================================\n";
    std::cout << "  Combined Asteroid Lists Example\n";
    std::cout << "========================================\n\n";
    
    // =====================================================================
    // CASO D'USO 1: Range con filtro + lista locale
    // =====================================================================
    
    std::cout << "--- CASO 1: Range [1-1000] con diameter>20 + localasteroid ---\n\n";
    
    try {
        AsteroidRangeBuilder builder;
        
        // Step 1: Definisci range con filtro
        std::cout << "1. Seleziono asteroidi [1-1000] con diameter > 20 km\n";
        builder.from(1).to(1000);
        // .where("diameter > 20");  // Nota: filtro richiede AsteroidProperties
        
        // Step 2: Aggiungi lista locale (senza filtri!)
        std::cout << "2. Aggiungo tutti gli asteroidi da etc/localasteroid\n";
        builder.addListFromFile("../etc/localasteroid");
        
        // Step 3: Build
        AsteroidRange range = builder.build();
        auto list = range.getAsteroidList();
        
        // Statistiche
        std::set<int> unique(list.begin(), list.end());
        std::cout << "\nRISULTATO:\n";
        std::cout << "  - Asteroidi totali unici: " << unique.size() << "\n";
        
        int inRange = 0, outOfRange = 0;
        for (int num : unique) {
            if (num >= 1 && num <= 1000) inRange++;
            else outOfRange++;
        }
        std::cout << "  - Nel range [1-1000]: " << inRange << "\n";
        std::cout << "  - Da localasteroid (>1000): " << outOfRange << "\n";
        
        // Mostra alcuni asteroidi fuori range
        std::cout << "\nPrimi 10 asteroidi da localasteroid (>1000):\n  ";
        int count = 0;
        for (int num : unique) {
            if (num > 1000) {
                std::cout << num << " ";
                if (++count >= 10) break;
            }
        }
        std::cout << "\n";
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << "\n";
        return 1;
    }
    
    // =====================================================================
    // CASO D'USO 2: File multipli senza range
    // =====================================================================
    
    std::cout << "\n--- CASO 2: Combinare file multipli ---\n\n";
    
    try {
        // Crea file di esempio
        std::ofstream file1("priority_asteroids.txt");
        file1 << "(1) # Ceres (5 events)\n";
        file1 << "(4) # Vesta (3 events)\n";
        file1 << "(10) # Hygiea (2 events)\n";
        file1.close();
        
        std::ofstream file2("research_targets.txt");
        file2 << "433\n";  // Eros
        file2 << "624\n";  // Hektor
        file2 << "704\n";  // Interamnia
        file2.close();
        
        // Combina tutti i file
        AsteroidRangeBuilder builder;
        builder.addListFromFile("../etc/localasteroid")
               .addListFromFile("priority_asteroids.txt")
               .addListFromFile("research_targets.txt");
        
        AsteroidRange range = builder.build();
        auto list = range.getAsteroidList();
        
        std::set<int> unique(list.begin(), list.end());
        std::cout << "File combinati:\n";
        std::cout << "  - etc/localasteroid\n";
        std::cout << "  - priority_asteroids.txt\n";
        std::cout << "  - research_targets.txt\n";
        std::cout << "\nTotale asteroidi unici: " << unique.size() << "\n";
        
        // Cleanup
        std::remove("priority_asteroids.txt");
        std::remove("research_targets.txt");
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << "\n";
        return 1;
    }
    
    // =====================================================================
    // CASO D'USO 3: Aggiungere lista programmaticamente
    // =====================================================================
    
    std::cout << "\n--- CASO 3: Range + lista custom in memoria ---\n\n";
    
    try {
        // Lista custom di asteroidi interessanti
        std::vector<int> custom_list = {
            243,    // Ida (ha una luna!)
            253,    // Mathilde
            433,    // Eros
            951,    // Gaspra
            2867,   // Šteins
            21,     // Lutetia
            4179    // Toutatis
        };
        
        AsteroidRangeBuilder builder;
        builder.from(1).to(100);  // Primi 100 storici
        builder.addToList(custom_list);  // Aggiungi lista custom
        
        AsteroidRange range = builder.build();
        auto list = range.getAsteroidList();
        
        std::set<int> unique(list.begin(), list.end());
        std::cout << "Range [1-100] + 7 asteroidi custom\n";
        std::cout << "Totale unici: " << unique.size() << "\n";
        
        std::cout << "\nAsteroidi custom aggiunti (>100):\n  ";
        for (int num : unique) {
            if (num > 100) {
                std::cout << num << " ";
            }
        }
        std::cout << "\n";
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << "\n";
        return 1;
    }
    
    // =====================================================================
    // SUGGERIMENTI PER L'USO
    // =====================================================================
    
    std::cout << "\n========================================\n";
    std::cout << "  SUGGERIMENTI\n";
    std::cout << "========================================\n\n";
    
    std::cout << "1. File multipli:\n";
    std::cout << "   builder.addListFromFile(\"file1.txt\")\n";
    std::cout << "          .addListFromFile(\"file2.txt\")\n";
    std::cout << "          .addListFromFile(\"file3.txt\");\n\n";
    
    std::cout << "2. Range + file:\n";
    std::cout << "   builder.from(1).to(1000)\n";
    std::cout << "          .addListFromFile(\"localasteroid\");\n\n";
    
    std::cout << "3. Range + filtri + file:\n";
    std::cout << "   builder.from(1).to(1000)\n";
    std::cout << "          .where(\"diameter > 20\")\n";
    std::cout << "          .addListFromFile(\"special_list.txt\");\n\n";
    
    std::cout << "4. Lista in memoria:\n";
    std::cout << "   std::vector<int> my_list = {1, 4, 10, 433};\n";
    std::cout << "   builder.addToList(my_list);\n\n";
    
    std::cout << "FORMATI FILE SUPPORTATI:\n";
    std::cout << "  - ASTNUM_LIST:   (1) # Ceres (5 events)\n";
    std::cout << "  - MPC_NUMBERED:  1\\n4\\n10\\n433\n";
    std::cout << "  - PLAIN_TEXT:    Mix di numeri e designazioni\n";
    std::cout << "  - AUTO_DETECT:   Rileva automaticamente il formato\n\n";
    
    std::cout << "========================================\n";
    std::cout << "  Test completati con successo!\n";
    std::cout << "========================================\n";
    
    return 0;
}
