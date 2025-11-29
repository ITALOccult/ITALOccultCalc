/**
 * @file test_eq0_direct.cpp  
 * @brief Test usando elementi equinoziali direttamente da .eq0
 */

#include <iostream>
#include <iomanip>
#include <cmath>
#include <ioccultcalc/astdys_client.h>
#include <ioccultcalc/orbit_propagator.h>
#include <ioccultcalc/coordinates.h>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

int main() {
    std::cout << "\n=== TEST with .eq0 elements ===\n\n";
    
    try {
        // Uso gli elementi VECCHI .eq0 (epoca 2009)
        AstDysClient client;
        auto eq = client.getElements("704");
        
        std::cout << "Epoch: " << TimeUtils::jdToISO(eq.epoch) << "\n";
        std::cout << "Equinoctial (from .eq0):\n";
        std::cout << "  a = " << std::fixed << std::setprecision(6) << eq.a << " AU\n";
        std::cout << "  h = " << eq.h << "\n";
        std::cout << "  k = " << eq.k << "\n";
        std::cout << "  p = " << eq.p << "\n";
        std::cout << "  q = " << eq.q << "\n";
        std::cout << "  λ = " << (eq.lambda * 180.0/M_PI) << "°\n\n";
        
        PropagatorOptions opts;
        opts.stepSize = 1.0;
        opts.usePlanetaryPerturbations = false;  // NO pert per test veloce
        opts.maxSteps = 10;
        OrbitPropagator prop(opts);
        
        // Posizione all'epoca (no propagazione)
        OrbitState state0 = prop.elementsToState(eq);
        
        auto coords = Coordinates::cartesianToEquatorial(state0.position);
        double ra = coords.ra * 180.0 / M_PI;
        double dec = coords.dec * 180.0 / M_PI;
        
        std::cout << "Position at epoch:\n";
        std::cout << "  RA  = " << std::setprecision(2) << ra << "°\n";
        std::cout << "  Dec = " << dec << "°\n\n";
        
        // Query Horizons per 2009-12-25
        std::cout << "Expected from Horizons @ 2009-12-25:\n";
        std::cout << "  (query manually to verify)\n\n";
        
        std::cout << "==============================\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
}
