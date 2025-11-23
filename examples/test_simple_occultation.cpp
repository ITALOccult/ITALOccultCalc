/**
 * @file test_simple_occultation.cpp
 * @brief Test semplificato per verificare il workflow base
 */

#include <iostream>
#include <iomanip>
#include <ioccultcalc/astdys_client.h>
#include <ioccultcalc/orbit_propagator.h>
#include <ioccultcalc/coordinates.h>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

int main() {
    std::cout << "\n=== Test Semplificato Occultazione ===\n\n";
    
    try {
        // Step 1: Download elementi
        std::cout << "1. Downloading elements for Ceres...\n";
        AstDysClient client;
        auto elements = client.getElements("1");
        std::cout << "   ✓ Name: " << elements.name << "\n";
        std::cout << "   ✓ Epoch: " << TimeUtils::jdToISO(elements.epoch) << "\n\n";
        
        // Step 2: Setup propagator
        std::cout << "2. Setting up propagator...\n";
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RK4;
        opts.stepSize = 0.1;
        opts.usePlanetaryPerturbations = true;
        OrbitPropagator propagator(opts);
        std::cout << "   ✓ Propagator ready\n\n";
        
        // Step 3: Propagate to a single date
        std::cout << "3. Propagating to 2026-03-15...\n";
        JulianDate targetDate = TimeUtils::isoToJD("2026-03-15T00:00:00");
        
        OrbitState initialState = propagator.elementsToState(elements);
        std::cout << "   Initial epoch: " << TimeUtils::jdToISO(elements.epoch) << "\n";
        std::cout << "   Target epoch:  " << TimeUtils::jdToISO(targetDate) << "\n";
        std::cout << "   Propagating...\n";
        
        OrbitState finalState = propagator.propagate(initialState, targetDate);
        std::cout << "   ✓ Propagation complete\n\n";
        
        // Step 4: Convert to RA/Dec
        std::cout << "4. Converting to equatorial coordinates...\n";
        auto coords = Coordinates::cartesianToEquatorial(finalState.position);
        
        double ra_deg = coords.ra * 180.0 / M_PI;
        double dec_deg = coords.dec * 180.0 / M_PI;
        double dist_au = finalState.position.magnitude();
        
        std::cout << "   Position at " << TimeUtils::jdToISO(targetDate) << ":\n";
        std::cout << "   RA:  " << std::fixed << std::setprecision(6) << ra_deg << "°\n";
        std::cout << "   Dec: " << dec_deg << "°\n";
        std::cout << "   Distance: " << std::setprecision(3) << dist_au << " AU\n\n";
        
        // Step 5: Compare with test star
        std::cout << "5. Comparing with test star position...\n";
        double star_ra = 180.0;   // 12h 00m
        double star_dec = 10.0;   // +10°
        
        std::cout << "   Star RA:  " << star_ra << "°\n";
        std::cout << "   Star Dec: " << star_dec << "°\n";
        
        double delta_ra = (ra_deg - star_ra) * 3600.0 * cos(dec_deg * M_PI / 180.0);
        double delta_dec = (dec_deg - star_dec) * 3600.0;
        double separation = sqrt(delta_ra * delta_ra + delta_dec * delta_dec);
        
        std::cout << "   Separation: " << std::setprecision(1) << separation << " arcsec\n\n";
        
        if (separation < 3600.0) {  // < 1 degree
            std::cout << "✓ Within search range - potential occultation geometry\n";
        } else {
            std::cout << "✗ Too far - no occultation expected\n";
        }
        
        std::cout << "\n=== Test Complete ===\n\n";
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n❌ Error: " << e.what() << "\n\n";
        return 1;
    }
}
