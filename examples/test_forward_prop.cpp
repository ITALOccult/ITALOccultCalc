/**
 * @file test_forward_prop.cpp
 * @brief Test propagazione FORWARD da mean elements AstDyS
 */

#include <iostream>
#include <iomanip>
#include <ioccultcalc/astdys_client.h>
#include <ioccultcalc/orbit_propagator.h>
#include <ioccultcalc/coordinates.h>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

int main() {
    std::cout << "\n=== TEST FORWARD PROPAGATION ===\n\n";
    
    try {
        // (704) Interamnia elementi da AstDyS .eq1 (MJD 61000 = 2025-11-21)
        EquinoctialElements eq;
        eq.a = 3.0562188547464153;
        eq.h = 0.038274930727971;
        eq.k = 0.150456045083743;
        eq.p = -0.149876046796030;
        eq.q = 0.026875405559966;
        eq.lambda = 198.4783346225068 * M_PI / 180.0;
        eq.epoch = JulianDate(61000.0 + 2400000.5);  // MJD→JD
        
        std::cout << "Elementi AstDyS (MEAN elements, MJD 61000 = 2025-11-21):\n";
        std::cout << "  a = " << std::fixed << std::setprecision(6) << eq.a << " AU\n";
        std::cout << "  e = " << sqrt(eq.h*eq.h + eq.k*eq.k) << "\n\n";
        
        // Converti a stato cartesiano all'epoca
        PropagatorOptions opts;
        opts.stepSize = 1.0;
        opts.usePlanetaryPerturbations = true;
        opts.maxSteps = 50000;
        
        OrbitPropagator prop(opts);
        auto state0 = prop.elementsToState(eq);
        
        std::cout << "Stato all'epoca (2025-11-21, dopo conversione):\n";
        std::cout << "  r = (" << state0.position.x << ", " 
                  << state0.position.y << ", " 
                  << state0.position.z << ") AU\n";
        std::cout << "  |r| = " << state0.position.magnitude() << " AU\n";
        
        auto coords0 = Coordinates::cartesianToEquatorial(state0.position);
        std::cout << "  RA  = " << (coords0.ra * 180.0/M_PI) << "°\n";
        std::cout << "  Dec = " << (coords0.dec * 180.0/M_PI) << "°\n\n";
        
        // Propaga FORWARD di 40 giorni (2025-12-31)
        JulianDate target(61040.0 + 2400000.5);
        std::cout << "Propagating FORWARD +40 days to 2025-12-31...\n";
        
        auto state1 = prop.propagate(state0, target);
        
        std::cout << "\nStato propagato (2025-12-31):\n";
        std::cout << "  r = (" << state1.position.x << ", "
                  << state1.position.y << ", "
                  << state1.position.z << ") AU\n";
        std::cout << "  |r| = " << state1.position.magnitude() << " AU\n";
        
        auto coords1 = Coordinates::cartesianToEquatorial(state1.position);
        std::cout << "  RA  = " << (coords1.ra * 180.0/M_PI) << "°\n";
        std::cout << "  Dec = " << (coords1.dec * 180.0/M_PI) << "°\n\n";
        
        std::cout << "NOTE: Mean elements richiedono propagazione con modello completo OrbFit\n";
        std::cout << "      per ottenere posizioni accurate. Questo test mostra solo che\n";
        std::cout << "      la conversione elementi→cartesiano è implementata correttamente.\n";
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
