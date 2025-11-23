/**
 * @file test_osculating_elements.cpp
 * @brief Test per ottenere elementi osculanti da Horizons e validare posizione
 * 
 * Test che:
 * 1. Ottiene elementi osculanti da JPL Horizons per (704) Interamnia
 * 2. Converte a stato cartesiano
 * 3. Confronta con posizione nota da Horizons
 * 
 * Obiettivo: Verificare che elementi osculanti diano posizione corretta
 */

#include <iostream>
#include <iomanip>
#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/time_utils.h"

using namespace ioccultcalc;

int main() {
    try {
        std::cout << "=== TEST ELEMENTI OSCULANTI DA HORIZONS ===\n\n";
        
        // Epoca: 2025-11-21 (MJD 61000)
        JulianDate epoch;
        epoch.jd = 2461000.5;
        
        std::cout << "Epoca: JD " << epoch.jd << " (MJD " << epoch.toMJD() << ")\n\n";
        
        // Ottieni elementi osculanti da Horizons tramite AstDysClient
        std::cout << "Scaricamento elementi OSCULANTI da JPL Horizons...\n";
        AstDysClient client;
        OrbitalElements osculating = client.getOsculatingElements("704", epoch);
        
        std::cout << "\nElementi Osculanti ricevuti:\n";
        std::cout << "  a     = " << std::fixed << std::setprecision(6) << osculating.a << " AU\n";
        std::cout << "  e     = " << osculating.e << "\n";
        std::cout << "  i     = " << osculating.i * 180.0 / M_PI << "°\n";
        std::cout << "  Omega = " << osculating.Omega * 180.0 / M_PI << "°\n";
        std::cout << "  omega = " << osculating.omega * 180.0 / M_PI << "°\n";
        std::cout << "  M     = " << osculating.M * 180.0 / M_PI << "°\n\n";
        
        // Converti a stato cartesiano
        std::cout << "Conversione elementi → stato cartesiano...\n";
        PropagatorOptions opts;
        OrbitPropagator prop(opts);
        
        // Converti Keplerian → Equinoctial per usare elementsToState
        EquinoctialElements eq;
        eq.a = osculating.a;
        eq.h = osculating.e * sin(osculating.omega + osculating.Omega);
        eq.k = osculating.e * cos(osculating.omega + osculating.Omega);
        eq.p = tan(osculating.i / 2.0) * sin(osculating.Omega);
        eq.q = tan(osculating.i / 2.0) * cos(osculating.Omega);
        eq.lambda = osculating.M + osculating.omega + osculating.Omega;
        eq.epoch = epoch;
        
        auto state = prop.elementsToState(eq);
        
        std::cout << "\nPosizione calcolata:\n";
        std::cout << "  r = (" << state.position.x << ", " 
                  << state.position.y << ", " 
                  << state.position.z << ") AU\n";
        
        double r = state.position.magnitude();
        std::cout << "  |r| = " << r << " AU\n";
        
        // Coordinate equatoriali
        double ra = atan2(state.position.y, state.position.x) * 180.0 / M_PI;
        if (ra < 0) ra += 360.0;
        double dec = asin(state.position.z / r) * 180.0 / M_PI;
        
        std::cout << "  RA  = " << ra << "°\n";
        std::cout << "  Dec = " << dec << "°\n\n";
        
        std::cout << "Risultato atteso da JPL Horizons (vettori 2025-11-21):\n";
        std::cout << "  r ~ (10.02, 16.72, -0.06) AU\n";
        std::cout << "  |r| ~ 19.50 AU\n";
        std::cout << "  RA ~ 56.90°\n";
        std::cout << "  Dec ~ 19.77°\n\n";
        
        std::cout << "Se i valori coincidono, elementi osculanti funzionano correttamente!\n";
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
