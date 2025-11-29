/**
 * @file debug_conversion.cpp
 * @brief Debug della conversione Keplerian→Equinoctial
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
    std::cout << "\n=== DEBUG CONVERSION ===\n\n";
    
    try {
        // Hardcoded (704) Interamnia from AstDyS .eq1 file
        EquinoctialElements eq;
        eq.a = 3.0562188547464153;
        eq.h = 0.038274930727971;
        eq.k = 0.150456045083743;
        eq.p = -0.149876046796030;
        eq.q = 0.026875405559966;
        eq.lambda = 198.4783346225068 * M_PI / 180.0;  // Convert to radians
        eq.epoch = JulianDate(61000.0 + 2400000.5);  // Convert MJD to JD
        
        std::cout << "EQUINOCTIAL (from AstDyS ECLM J2000):\n";
        std::cout << "  a      = " << std::fixed << std::setprecision(9) << eq.a << " AU\n";
        std::cout << "  h      = " << eq.h << "\n";
        std::cout << "  k      = " << eq.k << "\n";
        std::cout << "  p      = " << eq.p << "\n";
        std::cout << "  q      = " << eq.q << "\n";
        std::cout << "  lambda = " << (eq.lambda * 180.0/M_PI) << "°\n";
        std::cout << "  epoch  = " << TimeUtils::jdToISO(eq.epoch) << " (MJD 61000)\n\n";
        
        // Skip Keplerian conversion steps - go directly to state
        // Compute Keplerian from equinoctial for display
        double e = sqrt(eq.h*eq.h + eq.k*eq.k);
        double omega_plus_Omega = atan2(eq.h, eq.k);
        double tan_half_i = sqrt(eq.p*eq.p + eq.q*eq.q);
        double i = 2.0 * atan(tan_half_i);
        double Omega = atan2(eq.p, eq.q);
        double omega = omega_plus_Omega - Omega;
        double M = eq.lambda - omega_plus_Omega;
        
        std::cout << "  e     = " << std::setprecision(6) << e << "\n";
        std::cout << "  i     = " << (i * 180.0/M_PI) << "°\n";
        std::cout << "  Omega = " << (Omega * 180.0/M_PI) << "°\n";
        std::cout << "  omega = " << (omega * 180.0/M_PI) << "°\n";
        std::cout << "  M     = " << (M * 180.0/M_PI) << "°\n\n";
        
        // Ora propaga senza perturbazioni per 0 giorni (solo conversione)
        PropagatorOptions opts;
        opts.stepSize = 1.0;
        opts.usePlanetaryPerturbations = false; // NO perturbazioni
        opts.maxSteps = 1;
        OrbitPropagator prop(opts);
        
        OrbitState state0 = prop.elementsToState(eq);
        
        std::cout << "CARTESIAN (after elementsToState, in EQUATORIAL):\n";
        std::cout << "  r = (" << state0.position.x << ", " 
                 << state0.position.y << ", " 
                 << state0.position.z << ") AU\n";
        std::cout << "  |r| = " << state0.position.magnitude() << " AU\n";
        std::cout << "  v = (" << state0.velocity.x << ", " 
                 << state0.velocity.y << ", " 
                 << state0.velocity.z << ") AU/day\n\n";
        
        auto coords = Coordinates::cartesianToEquatorial(state0.position);
        double ra0 = coords.ra * 180.0 / M_PI;
        double dec0 = coords.dec * 180.0 / M_PI;
        
        std::cout << "EQUATORIAL (at epoch):\n";
        std::cout << "  RA  = " << std::fixed << std::setprecision(2) << ra0 << "°\n";
        std::cout << "  Dec = " << dec0 << "°\n\n";
        
        std::cout << "=======================\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
}
