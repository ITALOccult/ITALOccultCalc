/**
 * @file test_preston_recent.cpp
 * @brief Test con elementi AstDyS RECENTI (epoca 2025)
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
    std::cout << "\n╔════════════════════════════════════════════╗\n";
    std::cout << "║  Preston Test - ELEMENTI RECENTI (2025)   ║\n";
    std::cout << "╚════════════════════════════════════════════╝\n\n";
    
    try {
        // (704) Interamnia occulting TYC 5857-01303-1
        // Date: 2024-12-10 02:30 UT
        
        std::cout << "Event: (704) Interamnia × TYC 5857-01303-1\n";
        std::cout << "Date: 2024-12-10 02:30:00 UTC\n";
        std::cout << "Star: RA=51.077° Dec=+23.278°\n\n";
        
        // Download RECENT elements
        std::cout << "1. Downloading RECENT elements from allnum.cat...\n";
        AstDysClient client;
        auto kepElements = client.getRecentElements("704");
        std::cout << "   ✓ Epoch: " << TimeUtils::jdToISO(kepElements.epoch) << "\n";
        std::cout << "   ✓ a=" << std::fixed << std::setprecision(3) << kepElements.a << " AU\n";
        std::cout << "   ✓ e=" << std::setprecision(4) << kepElements.e << "\n";
        std::cout << "   ✓ i=" << std::setprecision(2) << (kepElements.i * 180.0 / M_PI) << "°\n\n";
        
        // Converti in elementi equinoziali per il propagatore
        std::cout << "2. Converting to equinoctial for propagation...\n";
        EquinoctialElements equElements = kepElements.toEquinoctial();
        std::cout << "   ✓ Converted\n\n";
        
        // Setup propagator
        std::cout << "3. Setup propagator...\n";
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RK4;
        opts.stepSize = 0.1;  // Step più piccolo per propagazione indietro
        opts.usePlanetaryPerturbations = true;
        opts.maxSteps = 5000000;
        OrbitPropagator propagator(opts);
        std::cout << "   ✓ Ready (step=0.1 day)\n\n";
        
        // Propagate to event date
        std::cout << "4. Propagating to 2024-12-10 02:30...\n";
        JulianDate targetDate = TimeUtils::isoToJD("2024-12-10T02:30:00");
        double daysToPropagate = targetDate.jd - kepElements.epoch.jd;
        std::cout << "   Days: " << std::fixed << std::setprecision(1) << daysToPropagate << "\n";
        
        OrbitState initialState = propagator.elementsToState(equElements);
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
            double miss_diameters = separation / angular_size;
            std::cout << "Miss distance: " << std::setprecision(1) << miss_diameters << " asteroid diameters\n";
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
