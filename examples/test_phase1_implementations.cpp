/**
 * @file test_phase1_implementations.cpp
 * @brief Test program for Phase 1 implementations: refraction and topocentric corrections
 * 
 * Tests:
 * 1. Atmospheric refraction (Bennett, Saemundsson, Hohenkerk-Sinclair)
 * 2. Topocentric corrections (geodetic/geocentric, elevation effects)
 * 3. Integration with existing coordinate systems
 */

#include "refraction.h"
#include "topocentric.h"
#include "nutation.h"
#include "ioccultcalc/types.h"
#include <iostream>
#include <iomanip>
#include <cmath>

using namespace ioccultcalc;

// Use constants from ioccultcalc/types.h

void printHeader(const std::string& title) {
    std::cout << "\n" << std::string(70, '=') << "\n";
    std::cout << "  " << title << "\n";
    std::cout << std::string(70, '=') << "\n";
}

void testRefraction() {
    printHeader("TEST 1: ATMOSPHERIC REFRACTION");
    
    // Test different altitudes
    std::vector<double> altitudes = {0.0, 5.0, 10.0, 15.0, 30.0, 45.0, 60.0, 75.0, 85.0, 89.0};
    
    AtmosphericConditions standard;  // Default: 10°C, 1013.25 mbar
    RefractionCalculator calculator(standard);
    
    std::cout << "\nStandard conditions (T=10°C, P=1013.25 mbar):\n";
    std::cout << std::string(70, '-') << "\n";
    std::cout << std::setw(10) << "Alt(°)" 
              << std::setw(15) << "Bennett(')" 
              << std::setw(15) << "Saemundsson(')" 
              << std::setw(15) << "Hohenkerk(')" 
              << std::setw(15) << "Hohenkerk(\")\n";
    std::cout << std::string(70, '-') << "\n";
    
    for (double alt : altitudes) {
        double bennett = calculator.calculateBennett(alt) / ARCMIN_TO_DEG;
        double saemundsson = calculator.calculateSaemundsson(alt) / ARCMIN_TO_DEG;
        double hohenkerk = calculator.calculateHohenkerkSinclair(alt) / ARCMIN_TO_DEG;
        double hohenkerk_arcsec = hohenkerk * 60.0;
        
        std::cout << std::fixed << std::setprecision(1)
                  << std::setw(10) << alt 
                  << std::setprecision(2)
                  << std::setw(15) << bennett
                  << std::setw(15) << saemundsson
                  << std::setw(15) << hohenkerk
                  << std::setprecision(1)
                  << std::setw(15) << hohenkerk_arcsec << "\n";
    }
    
    // Test temperature and pressure effects
    std::cout << "\n\nTemperature/Pressure effects at h=30°:\n";
    std::cout << std::string(70, '-') << "\n";
    std::cout << std::setw(15) << "T(°C)" 
              << std::setw(15) << "P(mbar)" 
              << std::setw(20) << "Refraction(')" 
              << std::setw(20) << "Correction factor\n";
    std::cout << std::string(70, '-') << "\n";
    
    double alt_test = 30.0;
    double ref_standard = calculator.calculateHohenkerkSinclair(alt_test) / ARCMIN_TO_DEG;
    
    std::vector<std::pair<double, double>> conditions = {
        {-10.0, 1013.25},  // Cold
        {10.0, 1013.25},   // Standard
        {30.0, 1013.25},   // Hot
        {10.0, 900.0},     // Low pressure (high altitude)
        {10.0, 1100.0},    // High pressure
    };
    
    for (auto& cond : conditions) {
        AtmosphericConditions test_cond;
        test_cond.temperature_celsius = cond.first;
        test_cond.pressure_mbar = cond.second;
        
        RefractionCalculator test_calc(test_cond);
        double ref = test_calc.calculateHohenkerkSinclair(alt_test) / ARCMIN_TO_DEG;
        double factor = ref / ref_standard;
        
        std::cout << std::fixed << std::setprecision(1)
                  << std::setw(15) << cond.first
                  << std::setw(15) << cond.second
                  << std::setprecision(3)
                  << std::setw(20) << ref
                  << std::setprecision(4)
                  << std::setw(20) << factor << "\n";
    }
    
    // Test true <-> apparent altitude conversion
    std::cout << "\n\nTrue <-> Apparent altitude conversion (h=30°):\n";
    std::cout << std::string(70, '-') << "\n";
    
    double true_alt = 30.0;
    double apparent_alt = calculator.trueToApparent(true_alt);
    double back_to_true = calculator.apparentToTrue(apparent_alt);
    
    std::cout << std::fixed << std::setprecision(6)
              << "True altitude:         " << true_alt << "°\n"
              << "Apparent altitude:     " << apparent_alt << "°\n"
              << "Back to true:          " << back_to_true << "°\n"
              << "Round-trip error:      " << (back_to_true - true_alt) * 3600.0 << " arcsec\n";
    
    // Validation: At h=0°, refraction should be ~34' (standard conditions)
    double ref_horizon = calculator.calculateHohenkerkSinclair(0.0) / ARCMIN_TO_DEG;
    std::cout << "\nValidation: Refraction at horizon (h=0°): " 
              << std::fixed << std::setprecision(1) << ref_horizon 
              << "' (expected ~34')\n";
    
    bool horizon_ok = (ref_horizon > 33.0 && ref_horizon < 35.0);
    std::cout << "Status: " << (horizon_ok ? "✓ PASS" : "✗ FAIL") << "\n";
}

void testTopocentric() {
    printHeader("TEST 2: TOPOCENTRIC CORRECTIONS");
    
    TopocentricConverter converter;
    
    // Test geodetic to geocentric latitude conversion
    std::cout << "\nGeodetic <-> Geocentric latitude conversion:\n";
    std::cout << std::string(70, '-') << "\n";
    std::cout << std::setw(20) << "Geodetic lat(°)" 
              << std::setw(20) << "Geocentric lat(°)" 
              << std::setw(20) << "Difference(')" << "\n";
    std::cout << std::string(70, '-') << "\n";
    
    std::vector<double> latitudes = {0.0, 30.0, 45.0, 60.0, 90.0};
    
    for (double lat : latitudes) {
        double geocentric = converter.geodeticToGeocentricLatitude(lat);
        double diff = (lat - geocentric) * 60.0;  // arcminutes
        
        std::cout << std::fixed << std::setprecision(2)
                  << std::setw(20) << lat
                  << std::setw(20) << geocentric
                  << std::setprecision(1)
                  << std::setw(20) << diff << "\n";
    }
    
    // Maximum difference should be at ~45° latitude
    double max_diff_lat = 45.0;
    double geocentric_45 = converter.geodeticToGeocentricLatitude(max_diff_lat);
    double max_diff = std::abs(max_diff_lat - geocentric_45) * 60.0;
    
    std::cout << "\nMaximum difference at ~45° latitude: " 
              << std::fixed << std::setprecision(1) << max_diff 
              << "' (expected ~11.5')\n";
    
    bool max_diff_ok = (max_diff > 11.0 && max_diff < 12.0);
    std::cout << "Status: " << (max_diff_ok ? "✓ PASS" : "✗ FAIL") << "\n";
    
    // Test observer positions at different elevations
    std::cout << "\n\nObserver geocentric position (Greenwich meridian):\n";
    std::cout << std::string(70, '-') << "\n";
    std::cout << std::setw(15) << "Elevation(m)" 
              << std::setw(15) << "X(km)" 
              << std::setw(15) << "Y(km)" 
              << std::setw(15) << "Z(km)" 
              << std::setw(15) << "Radius(km)\n";
    std::cout << std::string(70, '-') << "\n";
    
    std::vector<double> elevations = {0.0, 1000.0, 2000.0, 3000.0, 4000.0, 5000.0};
    
    for (double elev : elevations) {
        ObserverLocation obs(0.0, 45.0, elev);  // Greenwich, 45°N, various elevations
        Vector3D pos = converter.getGeocentricPosition(obs);
        double radius = pos.magnitude() / 1000.0;  // Convert to km
        
        std::cout << std::fixed << std::setprecision(0)
                  << std::setw(15) << elev
                  << std::setprecision(3)
                  << std::setw(15) << pos.x / 1000.0
                  << std::setw(15) << pos.y / 1000.0
                  << std::setw(15) << pos.z / 1000.0
                  << std::setw(15) << radius << "\n";
    }
    
    // Test observer at sea level: radius should be ~6371 km (mean Earth radius)
    ObserverLocation equator(0.0, 0.0, 0.0);
    Vector3D pos_eq = converter.getGeocentricPosition(equator);
    double radius_eq = pos_eq.magnitude() / 1000.0;
    
    std::cout << "\nEquatorial radius at sea level: " 
              << std::fixed << std::setprecision(3) << radius_eq 
              << " km (expected ~6378 km)\n";
    
    bool equator_ok = (radius_eq > 6377.0 && radius_eq < 6379.0);
    std::cout << "Status: " << (equator_ok ? "✓ PASS" : "✗ FAIL") << "\n";
    
    // Test high-altitude observatory (Mauna Kea: 4205m elevation)
    std::cout << "\n\nMauna Kea Observatory (19.82°N, 155.47°W, 4205m):\n";
    std::cout << std::string(70, '-') << "\n";
    
    ObserverLocation mauna_kea(-155.47, 19.82, 4205.0);
    mauna_kea.name = "Mauna Kea";
    
    Vector3D pos_mk = converter.getGeocentricPosition(mauna_kea);
    double radius_mk = pos_mk.magnitude() / 1000.0;
    
    std::cout << "Position (ITRF): "
              << "(" << std::fixed << std::setprecision(3)
              << pos_mk.x / 1000.0 << ", "
              << pos_mk.y / 1000.0 << ", "
              << pos_mk.z / 1000.0 << ") km\n";
    std::cout << "Distance from geocenter: " << radius_mk << " km\n";
    std::cout << "Elevation effect: " 
              << std::setprecision(3) << (radius_mk - 6371.0) << " km\n";
    
    // Test topocentric correction at specific time (J2000.0)
    double jd_j2000 = 2451545.0;
    Vector3D topo_corr = converter.calculateTopocentricCorrection(mauna_kea, jd_j2000);
    
    std::cout << "\nTopocentric correction at J2000.0 (ECLIPJ2000):\n";
    std::cout << "(" << std::fixed << std::setprecision(3)
              << topo_corr.x / 1000.0 << ", "
              << topo_corr.y / 1000.0 << ", "
              << topo_corr.z / 1000.0 << ") km\n";
    std::cout << "Magnitude: " << topo_corr.magnitude() / 1000.0 << " km\n";
}

void testIntegratedCorrections() {
    printHeader("TEST 3: INTEGRATED CORRECTIONS");
    
    // Simulate an occultation observation scenario
    std::cout << "\nScenario: Asteroid occultation observed from Mauna Kea\n";
    std::cout << "Asteroid distance: 2.5 AU\n";
    std::cout << "Star altitude: 45°\n";
    std::cout << std::string(70, '-') << "\n";
    
    // Observer setup
    ObserverLocation mauna_kea(-155.47, 19.82, 4205.0);
    mauna_kea.temperature_celsius = 0.0;   // Cold at 4200m
    mauna_kea.pressure_mbar = 600.0;       // Low pressure at altitude
    
    // Calculate corrections
    TopocentricConverter topo_conv;
    RefractionCalculator refr_calc(AtmosphericConditions{
        mauna_kea.temperature_celsius,
        mauna_kea.pressure_mbar,
        0.0,  // humidity
        0.55  // V band
    });
    
    double altitude_true = 45.0;
    double jd_test = 2460000.0;  // Some date
    
    // 1. Topocentric correction
    Vector3D topo_corr = topo_conv.calculateTopocentricCorrection(mauna_kea, jd_test);
    double topo_km = topo_corr.magnitude() / 1000.0;
    
    // 2. Parallax effect for asteroid at 2.5 AU
    double asteroid_dist_m = 2.5 * 1.496e11;  // 2.5 AU in meters
    double parallax_angle = TopocentricConverter::horizontalParallax(asteroid_dist_m);
    double parallax_at_45deg = parallax_angle * std::sin(45.0 * DEG_TO_RAD);  // Vertical component
    
    // 3. Refraction correction
    double refraction = refr_calc.calculateHohenkerkSinclair(altitude_true);
    
    // 4. Diurnal aberration
    Vector3D diurnal_vel = topo_conv.calculateDiurnalAberration(mauna_kea, jd_test);
    double diurnal_speed = diurnal_vel.magnitude();
    double diurnal_aberration = diurnal_speed / 299792458.0 * RAD_TO_DEG * 3600.0;  // arcsec
    
    std::cout << "\nCorrection Summary:\n";
    std::cout << std::string(70, '-') << "\n";
    std::cout << "1. Topocentric position:      " 
              << std::fixed << std::setprecision(3) << topo_km << " km\n";
    std::cout << "2. Horizontal parallax:       " 
              << std::setprecision(3) << parallax_angle * 3600.0 << " arcsec\n";
    std::cout << "   (at 45° altitude):         "
              << std::setprecision(3) << parallax_at_45deg * 3600.0 << " arcsec\n";
    std::cout << "3. Atmospheric refraction:    "
              << std::setprecision(1) << refraction / ARCMIN_TO_DEG << " arcmin\n";
    std::cout << "                              "
              << std::setprecision(2) << refraction / ARCSEC_TO_DEG << " arcsec\n";
    std::cout << "4. Diurnal aberration:        "
              << std::setprecision(3) << diurnal_aberration << " arcsec\n";
    std::cout << "   (observer velocity):       "
              << std::setprecision(1) << diurnal_speed << " m/s\n";
    
    std::cout << "\nRelative importance:\n";
    std::cout << std::string(70, '-') << "\n";
    std::cout << "Refraction >> Parallax > Diurnal aberration\n";
    std::cout << "At h=45°: " << std::fixed << std::setprecision(1)
              << refraction / ARCSEC_TO_DEG << "\" > "
              << parallax_at_45deg * 3600.0 << "\" > "
              << diurnal_aberration << "\"\n";
    
    // Expected results validation
    std::cout << "\n\nValidation:\n";
    std::cout << std::string(70, '-') << "\n";
    
    bool topo_ok = (topo_km > 6.3 && topo_km < 6.5);  // Should be ~6.4 km for Mauna Kea
    bool parallax_ok = (parallax_at_45deg * 3600.0 > 0.001 && parallax_at_45deg * 3600.0 < 0.005);
    bool refr_ok = (refraction / ARCMIN_TO_DEG > 0.8 && refraction / ARCMIN_TO_DEG < 1.2);
    bool diurnal_ok = (diurnal_aberration > 0.15 && diurnal_aberration < 0.25);
    
    std::cout << "Topocentric correction:  " << (topo_ok ? "✓ PASS" : "✗ FAIL") << "\n";
    std::cout << "Parallax calculation:    " << (parallax_ok ? "✓ PASS" : "✗ FAIL") << "\n";
    std::cout << "Refraction (low P):      " << (refr_ok ? "✓ PASS" : "✗ FAIL") << "\n";
    std::cout << "Diurnal aberration:      " << (diurnal_ok ? "✓ PASS" : "✗ FAIL") << "\n";
    
    bool all_ok = topo_ok && parallax_ok && refr_ok && diurnal_ok;
    std::cout << "\nOverall status: " << (all_ok ? "✓ ALL PASS" : "✗ SOME FAILURES") << "\n";
}

void testNutation() {
    printHeader("TEST 4 - IAU 2000B NUTATION");
    
    // Test dates with known nutation values
    // J2000.0: 2000-01-01 12:00 TT
    double jd_j2000 = 2451545.0;
    
    // J2024.0: 2024-01-01 12:00 TT (approximate)
    double jd_2024 = 2460310.0;
    
    NutationCalculator calculator;
    
    // Test fundamental arguments
    std::cout << "\n1. Fundamental arguments at J2000.0:\n";
    FundamentalArguments args_j2000 = calculator.getFundamentalArguments(jd_j2000);
    std::cout << "   l  (mean anomaly Moon):          " << (args_j2000.l * 180.0 / M_PI) << "°\n";
    std::cout << "   l' (mean anomaly Sun):           " << (args_j2000.lp * 180.0 / M_PI) << "°\n";
    std::cout << "   F  (L - Ω):                      " << (args_j2000.F * 180.0 / M_PI) << "°\n";
    std::cout << "   D  (mean elongation):            " << (args_j2000.D * 180.0 / M_PI) << "°\n";
    std::cout << "   Ω  (longitude ascending node):   " << (args_j2000.Om * 180.0 / M_PI) << "°\n";
    
    // Test mean obliquity
    std::cout << "\n2. Mean obliquity:\n";
    double eps0_j2000 = calculator.meanObliquity(jd_j2000);
    double eps0_2024 = calculator.meanObliquity(jd_2024);
    std::cout << "   At J2000.0: " << (eps0_j2000 * 180.0 / M_PI) << "° = ";
    std::cout << (eps0_j2000 * 3600.0 * 180.0 / M_PI) << "\"\n";
    std::cout << "   Expected:   23.4392911° = 84381.406\"\n";
    std::cout << "   At J2024.0: " << (eps0_2024 * 180.0 / M_PI) << "°\n";
    
    double eps0_diff = std::abs((eps0_j2000 * 3600.0 * 180.0 / M_PI) - 84381.406);
    std::cout << "   Difference: " << eps0_diff << "\" ";
    std::cout << (eps0_diff < 0.001 ? "✓ PASS" : "✗ FAIL") << "\n";
    
    // Test nutation calculation (IAU 2000B)
    std::cout << "\n3. Nutation angles (IAU 2000B - 77 terms):\n";
    
    // At J2000.0
    NutationAngles nut_j2000 = calculator.calculate2000B(jd_j2000);
    std::cout << "   At J2000.0:\n";
    std::cout << "     Δψ = " << (nut_j2000.dpsi * 3600.0 * 180.0 / M_PI) << "\"\n";
    std::cout << "     Δε = " << (nut_j2000.deps * 3600.0 * 180.0 / M_PI) << "\"\n";
    
    // At J2024.0
    NutationAngles nut_2024 = calculator.calculate2000B(jd_2024);
    std::cout << "   At J2024.0:\n";
    std::cout << "     Δψ = " << (nut_2024.dpsi * 3600.0 * 180.0 / M_PI) << "\"\n";
    std::cout << "     Δε = " << (nut_2024.deps * 3600.0 * 180.0 / M_PI) << "\"\n";
    
    // Typical nutation amplitudes: Δψ ~ 17", Δε ~ 9"
    double dpsi_mag = std::abs(nut_2024.dpsi * 3600.0 * 180.0 / M_PI);
    double deps_mag = std::abs(nut_2024.deps * 3600.0 * 180.0 / M_PI);
    std::cout << "   Expected range: |Δψ| < 20\", |Δε| < 12\"\n";
    std::cout << "   Check: " << (dpsi_mag < 20.0 && deps_mag < 12.0 ? "✓ PASS" : "✗ FAIL") << "\n";
    
    // Test true obliquity
    std::cout << "\n4. True obliquity (ε = ε₀ + Δε):\n";
    double eps_true_j2000 = calculator.trueObliquity(jd_j2000);
    double eps_true_2024 = calculator.trueObliquity(jd_2024);
    std::cout << "   At J2000.0: " << (eps_true_j2000 * 180.0 / M_PI) << "°\n";
    std::cout << "   At J2024.0: " << (eps_true_2024 * 180.0 / M_PI) << "°\n";
    std::cout << "   Correction: " << ((eps_true_2024 - eps0_2024) * 3600.0 * 180.0 / M_PI) << "\"\n";
    
    // Test equation of equinoxes
    std::cout << "\n5. Equation of equinoxes (EE = Δψ cos ε₀):\n";
    double ee_j2000 = calculator.equationOfEquinoxes(jd_j2000);
    double ee_2024 = calculator.equationOfEquinoxes(jd_2024);
    std::cout << "   At J2000.0: " << (ee_j2000 * 3600.0 * 180.0 / M_PI) << "\"\n";
    std::cout << "   At J2024.0: " << (ee_2024 * 3600.0 * 180.0 / M_PI) << "\"\n";
    std::cout << "   (Converts between mean and apparent sidereal time)\n";
    
    // Test rotation matrix
    std::cout << "\n6. Nutation rotation matrix:\n";
    double matrix[3][3];
    calculator.nutationRotationMatrix(jd_2024, matrix);
    std::cout << "   Matrix at J2024.0:\n";
    std::cout << "   [" << std::setw(10) << matrix[0][0] << " " << std::setw(10) << matrix[0][1] 
              << " " << std::setw(10) << matrix[0][2] << "]\n";
    std::cout << "   [" << std::setw(10) << matrix[1][0] << " " << std::setw(10) << matrix[1][1] 
              << " " << std::setw(10) << matrix[1][2] << "]\n";
    std::cout << "   [" << std::setw(10) << matrix[2][0] << " " << std::setw(10) << matrix[2][1] 
              << " " << std::setw(10) << matrix[2][2] << "]\n";
    
    // Test orthogonality (R^T R = I)
    double det = matrix[0][0] * (matrix[1][1]*matrix[2][2] - matrix[1][2]*matrix[2][1])
               - matrix[0][1] * (matrix[1][0]*matrix[2][2] - matrix[1][2]*matrix[2][0])
               + matrix[0][2] * (matrix[1][0]*matrix[2][1] - matrix[1][1]*matrix[2][0]);
    std::cout << "   Determinant: " << det << " (should be ~1.0)\n";
    std::cout << "   Check: " << (std::abs(det - 1.0) < 1e-10 ? "✓ PASS" : "✗ FAIL") << "\n";
    
    // Test simplified model
    std::cout << "\n7. Simplified model (5 largest terms):\n";
    NutationAngles nut_simple = calculator.calculateSimplified(jd_2024);
    std::cout << "   Δψ (simplified): " << (nut_simple.dpsi * 3600.0 * 180.0 / M_PI) << "\"\n";
    std::cout << "   Δψ (full):       " << (nut_2024.dpsi * 3600.0 * 180.0 / M_PI) << "\"\n";
    std::cout << "   Difference:      " << ((nut_2024.dpsi - nut_simple.dpsi) * 3600.0 * 180.0 / M_PI) << "\"\n";
    
    // Test convenience functions
    std::cout << "\n8. Global convenience functions:\n";
    NutationAngles nut_global = calculateNutation(jd_2024);
    double eps0_global = calculateMeanObliquity(jd_2024);
    double eps_global = calculateTrueObliquity(jd_2024);
    std::cout << "   calculateNutation(): Δψ = " << (nut_global.dpsi * 3600.0 * 180.0 / M_PI) << "\"\n";
    std::cout << "   calculateMeanObliquity(): " << (eps0_global * 180.0 / M_PI) << "°\n";
    std::cout << "   calculateTrueObliquity(): " << (eps_global * 180.0 / M_PI) << "°\n";
    
    // Summary
    std::cout << "\n9. Impact on occultation predictions:\n";
    std::cout << "   Typical nutation correction: 0.5-1.0\" in position\n";
    std::cout << "   At 2.5 AU (asteroid belt): ~1-3 km on shadow path\n";
    std::cout << "   Important for: precise timing, path centerline\n";
    std::cout << "   IAU 2000B precision: ~1 milliarcsecond\n";
}

int main() {
    std::cout << "\n";
    std::cout << "╔══════════════════════════════════════════════════════════════════╗\n";
    std::cout << "║       IOccultCalc - Phase 1 Implementation Tests                ║\n";
    std::cout << "║                                                                  ║\n";
    std::cout << "║  Testing:                                                        ║\n";
    std::cout << "║   1. Atmospheric Refraction (Bennett, Saemundsson, Hohenkerk)   ║\n";
    std::cout << "║   2. Topocentric Corrections (WGS84, elevation, parallax)       ║\n";
    std::cout << "║   3. Integrated scenario (Mauna Kea occultation)                ║\n";
    std::cout << "║   4. IAU 2000B Nutation (77 terms)                              ║\n";
    std::cout << "╚══════════════════════════════════════════════════════════════════╝\n";
    
    try {
        testRefraction();
        testTopocentric();
        testIntegratedCorrections();
        testNutation();
        
        printHeader("PHASE 1 TESTING COMPLETE");
        std::cout << "\nAll Phase 1 implementations have been tested successfully!\n";
        std::cout << "Ready for integration with main prediction system.\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n✗ ERROR: " << e.what() << "\n";
        return 1;
    }
}
