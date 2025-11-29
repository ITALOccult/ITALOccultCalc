/**
 * @file validate_horizons.cpp
 * @brief Valida propagatore vs JPL Horizons
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
    std::cout << "\n=== VALIDATION vs JPL HORIZONS ===\n\n";
    
    try {
        // (704) Interamnia - JPL Horizons 2024-12-10: RA=52.86° Dec=+18.80°
        std::cout << "(704) Interamnia @ 2024-12-10\n";
        std::cout << "Horizons: RA=52.86° Dec=+18.80°\n\n";
        
        AstDysClient client;
        auto elem = client.getRecentElements("704");
        std::cout << "Epoch: " << TimeUtils::jdToISO(elem.epoch) << "\n\n";
        
        PropagatorOptions opts;
        opts.stepSize = 0.1;
        opts.usePlanetaryPerturbations = true;
        opts.maxSteps = 500000;
        OrbitPropagator prop(opts);
        
        JulianDate target = TimeUtils::isoToJD("2024-12-10T00:00:00");
        double days = target.jd - elem.epoch.jd;
        std::cout << "Propagating " << days << " days...\n\n";
        
        auto equElem = elem.toEquinoctial();
        OrbitState state0 = prop.elementsToState(equElem);
        OrbitState stateF = prop.propagate(state0, target);
        
        auto coords = Coordinates::cartesianToEquatorial(stateF.position);
        double ra = coords.ra * 180.0 / M_PI;
        double dec = coords.dec * 180.0 / M_PI;
        
        std::cout << "Our result:\n";
        std::cout << "  RA  = " << std::fixed << std::setprecision(2) << ra << "°\n";
        std::cout << "  Dec = " << dec << "°\n\n";
        
        double dra = ra - 52.86;
        double ddec = dec - 18.80;
        double err = sqrt(dra*dra + ddec*ddec);
        
        std::cout << "Error: " << std::setprecision(1) << err << "° = " 
                 << (err*3600.0) << " arcsec\n\n";
        
        if (err < 1.0) {
            std::cout << "✓ ACCEPTABLE (< 1°)\n";
        } else {
            std::cout << "✗ POOR (> 1°)\n";
        }
        
        std::cout << "\n==================================\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
}
