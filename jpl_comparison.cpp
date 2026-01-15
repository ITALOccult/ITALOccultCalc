#include <iostream>
#include <vector>
#include <iomanip>
#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/time_utils.h"
#include <astdyn/AstDyn.hpp>
#include <astdyn/propagation/HighPrecisionPropagator.hpp>

using namespace ioccultcalc;

int main() {
    try {
        std::cout << "=== COMPREHENSIVE JPL COMPARISON ===\n\n";
        
        JPLHorizonsClient horizons;
        JulianDate epoch;
        epoch.jd = 2461053.229; // Occult4 Event Time (approx)
        
        std::cout << "Epoch: JD " << epoch.jd << " (MJD " << epoch.toMJD() << ")\n";
        
        // 1. Fetch Apparent RA/Dec from JPL Horizons (THE TRUTH)
        std::cout << "\n[SOURCE 1] JPL Horizons Geocentric Vectors (Rotated to Equatorial):\n";
        auto [pos_ecl, vel_ecl] = horizons.getStateVectors("1272", epoch, "500");
        
        auto rotateToEquatorial = [](const Vector3D& p) {
            double eps = 23.43929 * M_PI / 180.0;
            Vector3D q;
            q.x = p.x;
            q.y = p.y * cos(eps) - p.z * sin(eps);
            q.z = p.y * sin(eps) + p.z * cos(eps);
            return q;
        };

        Vector3D pos_equ = rotateToEquatorial(pos_ecl);
        double r_equ = pos_equ.magnitude();
        double ra_equ = atan2(pos_equ.y, pos_equ.x) * 180.0 / M_PI;
        if (ra_equ < 0) ra_equ += 360.0;
        double dec_equ = asin(pos_equ.z / r_equ) * 180.0 / M_PI;
        
        std::cout << "  JPL RA (Equatorial):  " << ra_equ << " deg\n";
        std::cout << "  JPL Dec (Equatorial): " << dec_equ << " deg\n";

        // 3. Occult4 Star Position (RA 123.2355, Dec 25.0585)
        double star_ra = 123.2355;
        double star_dec = 25.0585;
        std::cout << "\n[SOURCE 2] Occult4 Star Position:\n";
        std::cout << "  Star RA:  " << star_ra << " deg\n";
        std::cout << "  Star Dec: " << star_dec << " deg\n";
        
        // 4. Comparison
        double d_ra = (ra_equ - star_ra) * std::cos(star_dec * M_PI / 180.0);
        double d_dec = (dec_equ - star_dec);
        double dist = std::sqrt(d_ra*d_ra + d_dec*d_dec);
        std::cout << "\nDistance (JPL Equatorial Vector vs Occult4 Star): " << dist << " deg (" << dist * 3600.0 << " arcsec)\n";

        // 5. Check if RA 123.23 matches ANY date near Jan 13
        std::cout << "\nSearching for the date when JPL RA = 123.2355...\n";
        for (double d = -10.0; d <= 10.0; d += 0.2) {
            JulianDate t(epoch.jd + d);
            auto [p, v] = horizons.getStateVectors("1272", t, "500");
            Vector3D pe = rotateToEquatorial(p);
            double ra = atan2(pe.y, pe.x) * 180.0 / M_PI;
            if (ra < 0) ra += 360.0;
            if (std::abs(ra - star_ra) < 0.05) {
                std::cout << "  At MJD " << t.toMJD() << " (Delta " << d << " days), RA = " << ra << "\n";
            }
        }

    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << std::endl;
        return 1;
    }
    return 0;
}
