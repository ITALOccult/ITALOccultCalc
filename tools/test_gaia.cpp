/**
 * @file test_gaia.cpp
 * @brief Test program per verificare query Gaia DR3
 */

#include "ioccultcalc/gaia_client.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "Uso: " << argv[0] << " <ra_deg> <dec_deg> [radius_deg] [max_mag]\n";
        std::cerr << "Esempio: " << argv[0] << " 187.5 15.3 0.1 12.0\n";
        std::cerr << "\nParametri:\n";
        std::cerr << "  ra_deg      Right Ascension in gradi (0-360)\n";
        std::cerr << "  dec_deg     Declination in gradi (-90 to +90)\n";
        std::cerr << "  radius_deg  Raggio di ricerca in gradi (default: 0.1)\n";
        std::cerr << "  max_mag     Magnitudine massima G (default: 15.0)\n";
        return 1;
    }
    
    double ra = std::stod(argv[1]);
    double dec = std::stod(argv[2]);
    double radius = (argc > 3) ? std::stod(argv[3]) : 0.1;
    double maxMag = (argc > 4) ? std::stod(argv[4]) : 15.0;
    
    std::cout << "========================================\n";
    std::cout << "Test query Gaia DR3\n";
    std::cout << "========================================\n\n";
    std::cout << "Centro ricerca:\n";
    std::cout << "  RA:     " << std::fixed << std::setprecision(4) << ra << "°\n";
    std::cout << "  Dec:    " << dec << "°\n";
    std::cout << "  Raggio: " << radius << "° (" << (radius * 60.0) << " arcmin)\n";
    std::cout << "  Mag G:  < " << maxMag << "\n\n";
    
    try {
        GaiaClient client;
        client.setTimeout(30);
        
        std::cout << "Esecuzione query Gaia DR3...\n\n";
        
        std::vector<GaiaStar> stars = client.queryCone(ra, dec, radius, maxMag);
        
        std::cout << "\n========================================\n";
        std::cout << "RISULTATO\n";
        std::cout << "========================================\n\n";
        
        std::cout << "Stelle trovate: " << stars.size() << "\n";
        
        if (stars.empty()) {
            std::cout << "\n⚠️  Nessuna stella trovata!\n";
            std::cout << "Possibili cause:\n";
            std::cout << "  - Nessuna stella nell'area specificata\n";
            std::cout << "  - Limite di magnitudine troppo basso\n";
            std::cout << "  - GaiaClient non ancora implementato\n";
            std::cout << "  - Servizio TAP Gaia non disponibile\n";
            return 1;
        }
        
        // Mostra prime 20 stelle
        std::cout << "\nPrime " << std::min(20, (int)stars.size()) << " stelle:\n";
        std::cout << "  Source ID           RA (deg)      Dec (deg)     G mag  BP mag  RP mag\n";
        std::cout << "  ------------------------------------------------------------------------\n";
        
        for (size_t i = 0; i < std::min((size_t)20, stars.size()); ++i) {
            const auto& star = stars[i];
            std::cout << "  " << std::setw(18) << star.sourceId
                     << "  " << std::fixed << std::setw(12) << std::setprecision(6) << star.pos.ra
                     << "  " << std::setw(12) << star.pos.dec
                     << "  " << std::setw(6) << std::setprecision(2) << star.phot_g_mean_mag
                     << "  " << std::setw(6) << star.phot_bp_mean_mag
                     << "  " << std::setw(6) << star.phot_rp_mean_mag
                     << "\n";
        }
        
        // Statistiche
        double minMag = 99.0, maxMag_found = 0.0;
        for (const auto& star : stars) {
            if (star.phot_g_mean_mag < minMag) minMag = star.phot_g_mean_mag;
            if (star.phot_g_mean_mag > maxMag_found) maxMag_found = star.phot_g_mean_mag;
        }
        
        std::cout << "\nStatistiche:\n";
        std::cout << "  Magnitudine più brillante: G = " << std::fixed << std::setprecision(2) << minMag << "\n";
        std::cout << "  Magnitudine più debole:    G = " << maxMag_found << "\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n========================================\n";
        std::cerr << "ERRORE!\n";
        std::cerr << "========================================\n\n";
        std::cerr << "Errore: " << e.what() << "\n";
        return 1;
    }
}
