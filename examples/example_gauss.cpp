#include "ioccultcalc/orbit_determination_gauss.h"
#include "ioccultcalc/mpc_client.h"
#include "ioccultcalc/astdys_client.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main(int argc, char** argv) {
    std::string asteroidName = "433";  // Eros
    if (argc > 1) {
        asteroidName = argv[1];
    }
    
    std::cout << "=== GAUSS ORBIT DETERMINATION ===\n";
    std::cout << "Asteroide: " << asteroidName << "\n\n";
    
    // 1. Scarica osservazioni
    std::cout << "1. Download osservazioni da AstDyS...\n";
    MPCClient mpc;
    auto observations = mpc.getObservations(asteroidName);
    std::cout << "   Osservazioni: " << observations.observations.size() << "\n";
    std::cout << "   Arc: " << observations.arcLength << " giorni\n\n";
    
    if (observations.observations.size() < 3) {
        std::cerr << "Errore: servono almeno 3 osservazioni\n";
        return 1;
    }
    
    // 2. Determina orbita con metodo Gauss
    std::cout << "2. Determinazione orbitale...\n";
    GaussOrbitDeterminer gauss;
    auto result = gauss.determineOrbit(observations);
    
    if (!result.success) {
        std::cerr << "Determinazione fallita: " << result.message << "\n";
        return 1;
    }
    
    // 3. Stampa risultati
    std::cout << "\n=== RISULTATI ===\n";
    std::cout << "Status: " << result.message << "\n";
    std::cout << "Orbita affidabile: " << (result.reliableOrbit ? "SI" : "NO") << "\n\n";
    
    auto kep = result.elements.toKeplerian();
    std::cout << "Elementi orbitali determinati:\n";
    std::cout << std::fixed << std::setprecision(8);
    std::cout << "  a     = " << kep.a << " AU\n";
    std::cout << "  e     = " << kep.e << "\n";
    std::cout << "  i     = " << kep.i * 180/M_PI << " deg\n";
    std::cout << "  Omega = " << kep.Omega * 180/M_PI << " deg\n";
    std::cout << "  omega = " << kep.omega * 180/M_PI << " deg\n";
    std::cout << "  M     = " << kep.M * 180/M_PI << " deg\n";
    std::cout << "  epoch = " << kep.epoch.jd << "\n\n";
    
    // 4. Confronto con elementi AstDyS (se disponibili)
    try {
        AstDysClient astdys;
        auto astdysElements = astdys.getElements(asteroidName);
        auto astdysKep = astdysElements.toKeplerian();
        
        std::cout << "=== CONFRONTO CON ASTDYS ===\n";
        std::cout << "                  Gauss             AstDyS            Differenza\n";
        std::cout << "  a (AU):         " << std::setw(15) << kep.a 
                  << "  " << std::setw(15) << astdysKep.a
                  << "  " << std::setw(15) << (kep.a - astdysKep.a) << "\n";
        std::cout << "  e:              " << std::setw(15) << kep.e 
                  << "  " << std::setw(15) << astdysKep.e
                  << "  " << std::setw(15) << (kep.e - astdysKep.e) << "\n";
        std::cout << "  i (deg):        " << std::setw(15) << kep.i * 180/M_PI
                  << "  " << std::setw(15) << astdysKep.i * 180/M_PI
                  << "  " << std::setw(15) << (kep.i - astdysKep.i) * 180/M_PI << "\n";
    } catch (...) {
        std::cout << "(Elementi AstDyS non disponibili per confronto)\n";
    }
    
    return 0;
}
