/**
 * @file test_horizons_vectors.cpp
 * @brief Test rapido: usa vettori di stato da Horizons invece di elementi
 * 
 * Approccio più semplice:
 * 1. Scarica vettori posizione/velocità da Horizons (già funzionante)
 * 2. Usa direttamente quelli invece di convertire da elementi
 * 3. Valida che dia la posizione corretta
 */

#include <iostream>
#include <iomanip>
#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/time_utils.h"

using namespace ioccultcalc;

int main() {
    try {
        std::cout << "=== TEST VETTORI DA HORIZONS (metodo diretto) ===\n\n";
        
        // Epoca: 2025-11-21 (MJD 61000)
        JulianDate epoch;
        epoch.jd = 2461000.5;
        
        std::cout << "Epoca: JD " << epoch.jd << " (MJD " << epoch.toMJD() << ")\n";
        std::cout << "Asteroide: (704) Interamnia\n\n";
        
        // Usa direttamente getStateVectors (già testato e funzionante)
        std::cout << "Scaricamento vettori di stato da JPL Horizons...\n";
        JPLHorizonsClient horizons;
        
        auto [position, velocity] = horizons.getStateVectors("704", epoch, "@sun");
        
        std::cout << "\nVettori ricevuti:\n";
        std::cout << "  r = (" << std::fixed << std::setprecision(6)
                  << position.x << ", " 
                  << position.y << ", " 
                  << position.z << ") AU\n";
        
        double r = position.magnitude();
        std::cout << "  |r| = " << r << " AU\n";
        
        // Coordinate equatoriali
        double ra = atan2(position.y, position.x) * 180.0 / M_PI;
        if (ra < 0) ra += 360.0;
        double dec = asin(position.z / r) * 180.0 / M_PI;
        
        std::cout << "  RA  = " << ra << "°\n";
        std::cout << "  Dec = " << dec << "°\n\n";
        
        std::cout << "Velocità:\n";
        std::cout << "  v = (" << velocity.x << ", " 
                  << velocity.y << ", " 
                  << velocity.z << ") AU/day\n\n";
        
        std::cout << "Valore atteso:\n";
        std::cout << "  |r| ~ 19.50 AU\n";
        std::cout << "  RA ~ 56.90°\n";
        std::cout << "  Dec ~ 19.77°\n\n";
        
        // Verifica
        double ra_error = fabs(ra - 56.90);
        double dec_error = fabs(dec - 19.77);
        double r_error = fabs(r - 19.50);
        
        if (ra_error < 0.1 && dec_error < 0.1 && r_error < 0.1) {
            std::cout << "✓ TEST PASSATO: Vettori Horizons corretti!\n";
            std::cout << "\nCONCLUSIONE: Usa getStateVectors() direttamente per popolare il database\n";
            std::cout << "invece di convertire da elementi orbitali.\n";
        } else {
            std::cout << "✗ TEST FALLITO: Errori troppo grandi\n";
        }
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
