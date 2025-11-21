/**
 * @file test_rwo.cpp
 * @brief Test program per verificare lo scaricamento osservazioni .rwo da AstDyS
 */

#include "ioccultcalc/mpc_client.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Uso: " << argv[0] << " <asteroid_number>\n";
        std::cerr << "Esempio: " << argv[0] << " 433\n";
        return 1;
    }
    
    std::string asteroidNumber = argv[1];
    
    std::cout << "========================================\n";
    std::cout << "Test scaricamento osservazioni .rwo\n";
    std::cout << "========================================\n\n";
    std::cout << "Asteroide: " << asteroidNumber << "\n\n";
    
    try {
        MPCClient client;
        client.setTimeout(30);
        
        std::cout << "Scaricamento osservazioni...\n";
        std::cout << "URL: https://newton.spacedys.com/~astdys2/mpcobs/numbered/" 
                 << (std::stoi(asteroidNumber) / 1000) << "/" << asteroidNumber << ".rwo\n\n";
        
        ObservationSet obsSet = client.getObservations(asteroidNumber);
        
        std::cout << "\n========================================\n";
        std::cout << "RISULTATO\n";
        std::cout << "========================================\n\n";
        
        std::cout << "Osservazioni scaricate: " << obsSet.observations.size() << "\n";
        
        if (obsSet.observations.empty()) {
            std::cout << "\n⚠️  Nessuna osservazione trovata!\n";
            std::cout << "Possibili cause:\n";
            std::cout << "  - File .rwo non disponibile su AstDyS\n";
            std::cout << "  - Formato file diverso da quello atteso\n";
            std::cout << "  - Parsing delle linee MPC fallito\n";
            return 1;
        }
        
        std::cout << "\nStatistiche:\n";
        std::cout << "  Object: " << obsSet.objectDesignation << "\n";
        std::cout << "  Prima osservazione: JD " << std::fixed << std::setprecision(2) 
                 << obsSet.firstObservation.jd << "\n";
        std::cout << "  Ultima osservazione: JD " << obsSet.lastObservation.jd << "\n";
        std::cout << "  Arc: " << std::setprecision(1) << obsSet.arcLength << " giorni\n";
        std::cout << "  Numero osservazioni: " << obsSet.numberOfObservations << "\n";
        std::cout << "  Osservatori: " << obsSet.numberOfObservatories << "\n";
        
        // Mostra prime 10 osservazioni
        std::cout << "\nPrime " << std::min(10, (int)obsSet.observations.size()) << " osservazioni:\n";
        std::cout << "  Epoca (JD)        RA (deg)      Dec (deg)     Obs\n";
        std::cout << "  -------------------------------------------------------\n";
        
        for (size_t i = 0; i < std::min((size_t)10, obsSet.observations.size()); ++i) {
            const auto& obs = obsSet.observations[i];
            std::cout << "  " << std::fixed << std::setprecision(5) << obs.epoch.jd
                     << "  " << std::setw(12) << std::setprecision(6) << obs.obs.ra
                     << "  " << std::setw(12) << obs.obs.dec
                     << "  " << obs.observatoryCode << "\n";
        }
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n========================================\n";
        std::cerr << "ERRORE!\n";
        std::cerr << "========================================\n\n";
        std::cerr << "Errore: " << e.what() << "\n";
        return 1;
    }
}
