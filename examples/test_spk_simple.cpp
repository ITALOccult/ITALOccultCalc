/**
 * Test minimale per SPKReader
 */

#include "ioccultcalc/spk_reader.h"
#include <iostream>

using namespace ioccultcalc;

int main() {
    std::cout << "Test SPKReader minimale" << std::endl;
    std::cout << "========================" << std::endl;
    
    SPKReader reader;
    
    std::string path = getenv("HOME") + std::string("/.ioccultcalc/ephemerides/linux_p1550p2650.430");
    std::cout << "Tentativo caricamento: " << path << std::endl;
    
    bool success = reader.loadFile(path);
    
    if (success) {
        std::cout << "✓ File caricato con successo!" << std::endl;
        std::cout << "  Versione: " << reader.getVersion() << std::endl;
        
        auto [start, end] = reader.getCoverage();
        std::cout << "  Coverage: JD " << start << " - " << end << std::endl;
        
        // Test lettura posizione Giove
        std::cout << "\nTest lettura Giove a JD 2460000:" << std::endl;
        try {
            Vector3D pos = reader.getPosition(5, 2460000.0, 11);  // Jupiter heliocentric
            std::cout << "  Posizione: (" << pos.x << ", " << pos.y << ", " << pos.z << ") AU" << std::endl;
        } catch (const std::exception& e) {
            std::cout << "  ✗ Errore: " << e.what() << std::endl;
        }
        
    } else {
        std::cout << "✗ Caricamento fallito" << std::endl;
        return 1;
    }
    
    return 0;
}
