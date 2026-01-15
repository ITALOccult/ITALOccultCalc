#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>
#include "astdyn/ephemeris/DE441Provider.hpp"
#include "astdyn/core/Constants.hpp"

extern "C" {
#include "SpiceUsr.h"
}

int main(int argc, char** argv) {
    std::cout << "Starting Earth Position Verification (DE441 vs CSPICE)...\n";

    // 1. Initialize AstDyn DE441Provider
    std::string ephemeris_file = std::string(getenv("HOME")) + "/.ioccultcalc/ephemerides/de440.bsp";
    std::cout << "Loading ephemeris: " << ephemeris_file << "\n";

    astdyn::ephemeris::DE441Provider provider(ephemeris_file);
    
    // 2. Initialize CSPICE
    furnsh_c(ephemeris_file.c_str());

    // 3. Define test epoch: MJD 60611.014536 (from trace)
    double mjd_test = 60611.014536;
    double jd_test = mjd_test + 2400000.5;
    
    // Convert to Et (Ephemeris Time) for CSPICE
    // For this test, we assume TDB/ET are effectively the same as input scale for provider
    // In CSPICE, ET is seconds past J2000 TDB.
    // J2000.0 TDB = JD 2451545.0
    double et = (jd_test - 2451545.0) * 86400.0;
    
    std::cout << "Test Date: MJD " << std::fixed << std::setprecision(6) << mjd_test << "\n";
    std::cout << "           JD  " << jd_test << "\n";
    std::cout << "           ET  " << et << " (seconds past J2000)\n\n";

    // 4. Get AstDyn Position (Earth relative to ???)
    // DE441Provider::getPosition returns vector directly
    Eigen::Vector3d pos_astdyn_earth = provider.getPosition(
        astdyn::ephemeris::CelestialBody::EARTH, jd_test);

    // Get Sun Position from AstDyn (to check if provider is Barycentric)
    Eigen::Vector3d pos_astdyn_sun = provider.getPosition(
        astdyn::ephemeris::CelestialBody::SUN, jd_test);
    
    std::cout << "AstDyn Raw Earth: [" << pos_astdyn_earth.transpose() << "] (Norm: " << pos_astdyn_earth.norm() << ")\n";
    std::cout << "AstDyn Raw Sun:   [" << pos_astdyn_sun.transpose() << "] (Norm: " << pos_astdyn_sun.norm() << ")\n";
    
    // Compute Heliocentric Earth from AstDyn (assuming raw is Barycentric)
    Eigen::Vector3d pos_astdyn_helio = pos_astdyn_earth - pos_astdyn_sun;
    
    // Compare Heliocentric vectors
    Eigen::Vector3d pos_astdyn = pos_astdyn_helio; // Use this for comparison
    
    // Convert AstDyn to AU if it is in km (DE441 is usually km)
    // A quick check: Earth is ~1e8 km or ~1 AU.
    if (pos_astdyn.norm() > 1000.0) {
        std::cout << "AstDyn output appears to be in KM (Norm: " << pos_astdyn.norm() << "). Converting to AU.\n";
        pos_astdyn /= astdyn::constants::AU;
    } else {
        std::cout << "AstDyn output appears to be in AU (Norm: " << pos_astdyn.norm() << ").\n";
    }

    // 5. Get CSPICE Position
    SpiceDouble state_spice[3];
    SpiceDouble lt;
    // target="EARTH", obs="SUN", ref="J2000", abcorr="NONE"
    spkpos_c("EARTH", et, "J2000", "NONE", "SUN", state_spice, &lt);

    Eigen::Vector3d pos_spice(state_spice[0], state_spice[1], state_spice[2]);
    
    // CSPICE always returns KM
    pos_spice /= astdyn::constants::AU;

    // 6. Compare
    Eigen::Vector3d diff = pos_astdyn - pos_spice;
    double diff_norm_au = diff.norm();
    double diff_norm_km = diff_norm_au * astdyn::constants::AU;
    double diff_norm_m = diff_norm_km * 1000.0;

    std::cout << "\n=== RESULTS ===\n";
    std::cout << "AstDyn (AU): [" << pos_astdyn.transpose() << "]\n";
    std::cout << "CSPICE (AU): [" << pos_spice.transpose() << "]\n";
    std::cout << "Difference (AU): [" << diff.transpose() << "]\n";
    std::cout << "Magnitude Error:\n";
    std::cout << "  " << std::scientific << diff_norm_au << " AU\n";
    std::cout << "  " << std::fixed << std::setprecision(3) << diff_norm_km << " km\n";
    std::cout << "  " << std::fixed << std::setprecision(3) << diff_norm_m << " meters\n";

    if (diff_norm_m < 1.0) {
        std::cout << "\nSUCCESS: AstDyn and CSPICE match to within 1 meter!\n";
    } else {
        std::cout << "\nFAILURE: Significant discrepancy found (> 1 meter).\n";
        std::cout << "Possible causes: coordinate frame mismatch, time scale offset, or interpolation error.\n";
    }

    return 0;
}
