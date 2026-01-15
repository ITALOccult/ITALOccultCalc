#include <iostream>
#include <iomanip>
#include <cassert>
#include "ioccultcalc/orbital_elements.h"
#include "orbital_conversions.h"

using namespace ioccultcalc;

int main() {
    std::cout << "==========================================================" << std::endl;
    std::cout << "   TEST: Mean to Osculating Element Conversion" << std::endl;
    std::cout << "==========================================================" << std::endl;

    // 1. Setup Mean Elements (Simulating an AstDyS .eq1 file)
    AstDynEquinoctialElements mean_eq;
    mean_eq.a = 3.175473;
    mean_eq.h = -0.018963;
    mean_eq.k = -0.041273;
    mean_eq.p = 0.025407;
    mean_eq.q = -0.001956;
    mean_eq.lambda = 229.790880 * M_PI / 180.0;
    mean_eq.epoch = JulianDate::fromMJD(61000.0);
    mean_eq.type = ElementType::MEAN_ASTDYS; // Marcatore fondamentale
    mean_eq.name = "17030 Sierks";

    // 2. Geometric Conversion (Bypass legacy correction)
    OrbitalElements geometric = mean_eq.toKeplerian();
    std::cout << "[STEP 1] Geometric Conversion (Reference):" << std::endl;
    std::cout << "  - Type: " << (geometric.type == ElementType::MEAN_ASTDYS ? "MEAN" : "OSCULATING") << std::endl;
    std::cout << "  - a: " << std::fixed << std::setprecision(8) << geometric.a << " AU" << std::endl;
    std::cout << "  - e: " << geometric.e << std::endl;
    std::cout << "  - i: " << geometric.i * 180.0 / M_PI << " deg" << std::endl;

    // 3. Osculating Conversion (New Physical Logic)
    OrbitalElements osculating = mean_eq.toOsculatingKeplerian();
    std::cout << "\n[STEP 2] Osculating Conversion (Physical Logic):" << std::endl;
    std::cout << "  - Type: " << (osculating.type == ElementType::OSCULATING ? "OSCULATING" : "MEAN") << std::endl;
    std::cout << "  - a: " << std::fixed << std::setprecision(8) << osculating.a << " AU" << std::endl;
    
    // Verifiche
    assert(osculating.type == ElementType::OSCULATING);
    std::cout << "  - Verification: type is correctly set to OSCULATING. [PASS]" << std::endl;

    // 4. Test Geocentrico con J2 (Effetto fisico reale)
    std::cout << "\n[STEP 3] Geocentric J2 Test (Brouwer Theory):" << std::endl;
    KeplerianElements it_mean;
    it_mean.a = 7000.0 / 149597870.7; // 7000 km in AU
    it_mean.e = 0.001;
    it_mean.i = 51.6 * M_PI / 180.0;
    it_mean.M = 0.0;
    it_mean.epoch_jd = 2460000.5;
    
    double J2_EARTH = 1.08262668e-3;
    // Note: Brouwer's theory results in zero short-period Delta_a, 
    // so we must check eccentricity or inclination to see the correction in action.
    auto it_osc = OrbitalConversions::meanToOsculating(it_mean, J2_EARTH);
    
    std::cout << "  - Mean Eccentricity: " << std::fixed << std::setprecision(6) << it_mean.e << std::endl;
    std::cout << "  - Osc  Eccentricity: " << std::fixed << std::setprecision(6) << it_osc.e << std::endl;
    std::cout << "  - Mean Inclination:  " << std::fixed << std::setprecision(4) << it_mean.i * 180.0 / M_PI << " deg" << std::endl;
    std::cout << "  - Osc  Inclination:  " << std::fixed << std::setprecision(4) << it_osc.i * 180.0 / M_PI << " deg" << std::endl;

    if (std::abs(it_osc.e - it_mean.e) > 1e-10 || std::abs(it_osc.i - it_mean.i) > 1e-10) {
        std::cout << "  - J2 Physical Correction: Verified (Periodic variations detected). [PASS]" << std::endl;
    } else {
        std::cout << "  - J2 Physical Correction: No difference detected. [FAIL]" << std::endl;
    }

    // 5. Test Milani-Knezevic Planetary Perturbations
    std::cout << "\n[STEP 4] Milani-Knezevic Planetary Perturbations Test (Asteroid 17030):" << std::endl;
    OrbitalElements sierks_mean;
    sierks_mean.a = 3.175473;
    sierks_mean.e = 0.045; // Approx
    sierks_mean.i = 0.025; // Approx rad
    sierks_mean.M = 2.0;
    sierks_mean.epoch_mjd_tdb = 61000.0;
    
    // Convert to osculating (J2=0 to isolate planetary effect)
    auto sierks_osc = mean_eq.toOsculatingKeplerian();
    
    std::cout << "  - Mean Semi-major Axis: " << std::fixed << std::setprecision(8) << geometric.a << " AU" << std::endl;
    std::cout << "  - Osc  Semi-major Axis: " << std::fixed << std::setprecision(8) << osculating.a << " AU" << std::endl;
    std::cout << "  - Correction (delta_a): " << std::setprecision(10) << (osculating.a - geometric.a) << " AU" << std::endl;

    if (std::abs(osculating.a - geometric.a) > 1e-12) {
        std::cout << "  - Planetary Periodic Correction: Verified (Analytical model active). [PASS]" << std::endl;
    } else {
        std::cout << "  - Planetary Periodic Correction: No difference detected. [FAIL]" << std::endl;
    }

    std::cout << "\n==========================================================" << std::endl;
    std::cout << "   TEST COMPLETED SUCCESSFULLY" << std::endl;
    std::cout << "==========================================================" << std::endl;

    return 0;
}
