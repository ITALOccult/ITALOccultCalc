/**
 * @file test_preston_quick.cpp
 * @brief Test rapido con occultazione Preston - single point check
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
    std::cout << "\n╔═══════════════════════════════════════════╗\n";
    std::cout << "║  Preston Occultation Quick Test          ║\n";
    std::cout << "╚═══════════════════════════════════════════╝\n\n";
    
    try {
        // (704) Interamnia occulting TYC 5857-01303-1
        // Date: 2024-12-10 02:30 UT
        
        std::cout << "Event: (704) Interamnia × TYC 5857-01303-1\n";
        std::cout << "Date: 2024-12-10 02:30:00 UTC\n";
        std::cout << "Star: RA=51.077° Dec=+23.278°\n\n";
        
        // Download elements
        std::cout << "1. Downloading elements...\n";
        AstDysClient client;
        auto elements = client.getElements("704");
        std::cout << "   ✓ Epoch: " << TimeUtils::jdToISO(elements.epoch) << "\n\n";
        
        // Setup propagator
        std::cout << "2. Setup propagator...\n";
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RK4;
        opts.stepSize = 0.1;
        opts.usePlanetaryPerturbations = true;
        opts.maxSteps = 5000000;
        OrbitPropagator propagator(opts);
        std::cout << "   ✓ Ready\n\n";
        
        // Propagate to event date
        std::cout << "3. Propagating to 2024-12-10 02:30...\n";
        JulianDate targetDate = TimeUtils::isoToJD("2024-12-10T02:30:00");
        
        OrbitState initialState = propagator.elementsToState(elements);
        OrbitState finalState = propagator.propagate(initialState, targetDate);
        std::cout << "   ✓ Done\n\n";
        
        // Get position
        auto coords = Coordinates::cartesianToEquatorial(finalState.position);
        double ast_ra = coords.ra * 180.0 / M_PI;
        double ast_dec = coords.dec * 180.0 / M_PI;
        double dist = finalState.position.magnitude();
        
        // Calculate separation
        double star_ra = 51.077;
        double star_dec = 23.278;
        
        double dra = (ast_ra - star_ra) * cos(coords.dec);
        double ddec = ast_dec - star_dec;
        double separation = sqrt(dra*dra + ddec*ddec) * 3600.0;  // arcsec
        
        // Results
        std::cout << "═══════════ RESULTS ═══════════\n\n";
        std::cout << "Asteroid position:\n";
        std::cout << "  RA:  " << std::fixed << std::setprecision(6) << ast_ra << "°\n";
        std::cout << "  Dec: " << ast_dec << "°\n";
        std::cout << "  Dist: " << std::setprecision(3) << dist << " AU\n\n";
        
        std::cout << "Star position:\n";
        std::cout << "  RA:  " << star_ra << "°\n";
        std::cout << "  Dec: " << star_dec << "°\n\n";
        
        std::cout << "Separation: " << std::setprecision(2) << separation << " arcsec\n\n";
        
        // Angular size
        double diameter_km = 306.0;
        double dist_km = dist * 149597870.7;
        double angular_size = (diameter_km / dist_km) * 206264.806;  // arcsec
        
        std::cout << "Asteroid angular diameter: " << std::setprecision(2) 
                 << angular_size << " arcsec\n\n";
        
        // Assessment
        if (separation < angular_size) {
            std::cout << "✓✓ OCCULTATION!\n";
            std::cout << "The asteroid will occult the star.\n";
        } else if (separation < 2.0 * angular_size) {
            std::cout << "✓ Very close approach!\n";
            std::cout << "Possible occultation or near miss.\n";
        } else if (separation < 3600.0) {
            std::cout << "~ Close approach (< 1°)\n";
        } else {
            std::cout << "✗ Too far for occultation\n";
        }
        
        std::cout << "\n═══════════════════════════════\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "❌ Error: " << e.what() << "\n\n";
        return 1;
    }
}
