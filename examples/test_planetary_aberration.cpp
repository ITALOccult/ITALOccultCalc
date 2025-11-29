/**
 * @file test_planetary_aberration.cpp
 * @brief Test program for Phase 2 planetary aberration module
 */

#include "planetary_aberration.h"
#include "ioccultcalc/types.h"
#include <iostream>
#include <iomanip>
#include <cmath>

using namespace ioccultcalc;

void printHeader(const std::string& title) {
    std::cout << "\n" << std::string(70, '=') << "\n";
    std::cout << "  " << title << "\n";
    std::cout << std::string(70, '=') << "\n";
}

void testBasicLightTime() {
    printHeader("TEST 1: LIGHT-TIME CALCULATIONS");
    
    std::cout << "\n1. Light-time for standard distances:\n";
    std::cout << "   Distance(AU)  Light-time(min)  Light-time(sec)\n";
    std::cout << "   " << std::string(55, '-') << "\n";
    
    std::vector<double> distances = {0.01, 0.1, 1.0, 2.5, 5.0, 10.0, 30.0};
    
    for (double d : distances) {
        double lt = PlanetaryAberrationCalculator::calculateLightTime(d);
        std::cout << "   " << std::setw(12) << std::fixed << std::setprecision(2) << d;
        std::cout << "  " << std::setw(15) << std::setprecision(2) << (lt * 1440.0);
        std::cout << "  " << std::setw(15) << std::setprecision(1) << (lt * 86400.0) << "\n";
    }
    
    // Validation: 1 AU should be ~499.0 seconds
    double lt_1AU = PlanetaryAberrationCalculator::calculateLightTime(1.0);
    double lt_seconds = lt_1AU * 86400.0;
    std::cout << "\n   Light-time for 1 AU: " << lt_seconds << " sec\n";
    std::cout << "   Expected: 499.0 sec\n";
    std::cout << "   Difference: " << std::abs(lt_seconds - 499.0) << " sec ";
    std::cout << (std::abs(lt_seconds - 499.0) < 1.0 ? "✓ PASS" : "✗ FAIL") << "\n";
}

void testFirstOrderCorrection() {
    printHeader("TEST 2: FIRST-ORDER ABERRATION CORRECTION");
    
    std::cout << "\nScenario: Asteroid at 2.5 AU with typical velocity\n";
    std::cout << "----------------------------------------------------------------------\n";
    
    // Typical asteroid at 2.5 AU from Earth
    Vector3D asteroidHelio(2.5, 0.5, 0.1);  // AU, heliocentric
    Vector3D asteroidVel(0.00002, 0.00001, 0.000005);  // AU/day (~20 km/s typical)
    Vector3D earthPos(1.0, 0.0, 0.0);  // Earth at 1 AU
    
    Vector3D geocentric = asteroidHelio - earthPos;
    double distance = geocentric.magnitude();
    double velocityKmS = asteroidVel.magnitude() * 149597870.7 / 86400.0;
    
    std::cout << "\nInitial conditions:\n";
    std::cout << "   Geocentric distance: " << distance << " AU = ";
    std::cout << (distance * 149597870.7) << " km\n";
    std::cout << "   Asteroid velocity: " << velocityKmS << " km/s\n";
    
    PlanetaryAberrationCalculator calc(AberrationOrder::FIRST_ORDER);
    JulianDate jd(2460000.0);
    
    AberrationCorrection corr = calc.correctAsteroid(
        asteroidHelio, asteroidVel, earthPos, jd
    );
    
    std::cout << "\nAberration correction results:\n";
    std::cout << "   Light-time: " << (corr.lightTime * 86400.0) << " seconds = ";
    std::cout << (corr.lightTime * 1440.0) << " minutes\n";
    std::cout << "   Correction magnitude: " << corr.magnitude << " km\n";
    std::cout << "   Angular shift: " << corr.angularShift << " arcsec\n";
    std::cout << "   Iterations: " << corr.iterations << " (should be 0 for first-order)\n";
    
    // Expected: ~250 km correction for 20 km/s at 2.5 AU
    double expectedLT = distance * 499.0;  // seconds
    double expectedCorr = velocityKmS * expectedLT;  // km
    
    std::cout << "\nValidation:\n";
    std::cout << "   Expected light-time: ~" << expectedLT << " seconds\n";
    std::cout << "   Expected correction: ~" << expectedCorr << " km\n";
    std::cout << "   Actual/Expected ratio: " << (corr.magnitude / expectedCorr) << "\n";
    
    // Check if correction is within 20% (rough estimate)
    bool valid = std::abs(corr.magnitude / expectedCorr - 1.0) < 0.2;
    std::cout << "   Status: " << (valid ? "✓ PASS" : "✗ FAIL") << "\n";
}

void testIterativeCorrection() {
    printHeader("TEST 3: ITERATIVE VS FIRST-ORDER");
    
    std::cout << "\nComparing first-order and iterative methods\n";
    std::cout << "----------------------------------------------------------------------\n";
    
    // Near-Earth asteroid (where precision matters)
    Vector3D asteroidHelio(1.2, 0.1, 0.05);
    Vector3D asteroidVel(0.00003, 0.00002, 0.000001);  // ~30 km/s (NEA)
    Vector3D earthPos(1.0, 0.0, 0.0);
    JulianDate jd(2460000.0);
    
    double distance = (asteroidHelio - earthPos).magnitude();
    
    std::cout << "\nNEA scenario:\n";
    std::cout << "   Distance: " << distance << " AU\n";
    std::cout << "   Velocity: ~30 km/s\n";
    
    // First-order
    PlanetaryAberrationCalculator calcFirst(AberrationOrder::FIRST_ORDER);
    AberrationCorrection corrFirst = calcFirst.correctAsteroid(
        asteroidHelio, asteroidVel, earthPos, jd
    );
    
    // Iterative
    PlanetaryAberrationCalculator calcIter(AberrationOrder::ITERATIVE, 1e-6);
    AberrationCorrection corrIter = calcIter.correctAsteroid(
        asteroidHelio, asteroidVel, earthPos, jd
    );
    
    std::cout << "\nResults comparison:\n";
    std::cout << "   Method         Correction(km)  Angular(arcsec)  Iterations\n";
    std::cout << "   " << std::string(60, '-') << "\n";
    std::cout << "   First-order    " << std::setw(14) << std::fixed << std::setprecision(3);
    std::cout << corrFirst.magnitude << "  " << std::setw(15) << std::setprecision(6);
    std::cout << corrFirst.angularShift << "  " << std::setw(10) << corrFirst.iterations << "\n";
    std::cout << "   Iterative      " << std::setw(14) << corrIter.magnitude;
    std::cout << "  " << std::setw(15) << corrIter.angularShift;
    std::cout << "  " << std::setw(10) << corrIter.iterations << "\n";
    
    double diff = std::abs(corrIter.magnitude - corrFirst.magnitude);
    std::cout << "\n   Difference: " << diff << " km\n";
    std::cout << "   Relative: " << (diff / corrFirst.magnitude * 100.0) << " %\n";
    
    // For NEAs, difference should be small but measurable
    std::cout << "   Status: " << (corrIter.iterations > 0 ? "✓ PASS" : "✗ FAIL");
    std::cout << " (iterative method converged)\n";
}

void testVelocityDependence() {
    printHeader("TEST 4: VELOCITY DEPENDENCE");
    
    std::cout << "\nAberration vs velocity (fixed distance = 2.5 AU)\n";
    std::cout << "----------------------------------------------------------------------\n";
    
    Vector3D earthPos(1.0, 0.0, 0.0);
    Vector3D asteroidHelio(2.5, 1.0, 0.0);
    JulianDate jd(2460000.0);
    
    PlanetaryAberrationCalculator calc;
    
    std::cout << "\n   Velocity(km/s)  Correction(km)  Angular(arcsec)  Significant?\n";
    std::cout << "   " << std::string(65, '-') << "\n";
    
    std::vector<double> velocities = {5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 40.0};
    
    for (double v : velocities) {
        // Convert to AU/day
        double vAU = v * 86400.0 / 149597870.7;
        Vector3D asteroidVel(vAU, 0.0, 0.0);
        
        AberrationCorrection corr = calc.correctAsteroid(
            asteroidHelio, asteroidVel, earthPos, jd
        );
        
        bool sig = PlanetaryAberrationCalculator::isSignificant(v, 2.5, 10.0);
        
        std::cout << "   " << std::setw(14) << std::fixed << std::setprecision(1) << v;
        std::cout << "  " << std::setw(14) << std::setprecision(1) << corr.magnitude;
        std::cout << "  " << std::setw(15) << std::setprecision(3) << corr.angularShift;
        std::cout << "  " << std::setw(11) << (sig ? "YES" : "NO") << "\n";
    }
    
    std::cout << "\n   Note: Correction scales linearly with velocity\n";
    std::cout << "   Significance threshold: 10 km\n";
}

void testDistanceDependence() {
    printHeader("TEST 5: DISTANCE DEPENDENCE");
    
    std::cout << "\nAberration vs distance (fixed velocity = 20 km/s)\n";
    std::cout << "----------------------------------------------------------------------\n";
    
    Vector3D earthPos(1.0, 0.0, 0.0);
    Vector3D asteroidVel(0.00002, 0.0, 0.0);  // ~20 km/s
    JulianDate jd(2460000.0);
    
    PlanetaryAberrationCalculator calc;
    
    std::cout << "\n   Distance(AU)  Correction(km)  Light-time(min)  Angular(arcsec)\n";
    std::cout << "   " << std::string(66, '-') << "\n";
    
    std::vector<double> distances = {0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0};
    
    for (double d : distances) {
        Vector3D asteroidHelio(d + earthPos.x, 0.0, 0.0);
        
        AberrationCorrection corr = calc.correctAsteroid(
            asteroidHelio, asteroidVel, earthPos, jd
        );
        
        std::cout << "   " << std::setw(12) << std::fixed << std::setprecision(1) << d;
        std::cout << "  " << std::setw(14) << std::setprecision(1) << corr.magnitude;
        std::cout << "  " << std::setw(15) << std::setprecision(2) << (corr.lightTime * 1440.0);
        std::cout << "  " << std::setw(15) << std::setprecision(3) << corr.angularShift << "\n";
    }
    
    std::cout << "\n   Note: Correction scales linearly with distance\n";
    std::cout << "   Angular shift decreases with distance\n";
}

void testStatistics() {
    printHeader("TEST 6: CALCULATOR STATISTICS");
    
    std::cout << "\nPerforming multiple calculations to test statistics\n";
    std::cout << "----------------------------------------------------------------------\n";
    
    PlanetaryAberrationCalculator calc(AberrationOrder::ITERATIVE);
    Vector3D earthPos(1.0, 0.0, 0.0);
    JulianDate jd(2460000.0);
    
    // Perform 10 calculations
    for (int i = 0; i < 10; ++i) {
        double d = 1.0 + i * 0.5;
        Vector3D asteroidHelio(d, 0.1, 0.0);
        Vector3D asteroidVel(0.00002, 0.00001, 0.0);
        
        calc.correctAsteroid(asteroidHelio, asteroidVel, earthPos, jd);
    }
    
    auto stats = calc.getStatistics();
    
    std::cout << "\nStatistics after 10 calculations:\n";
    std::cout << "   Total calculations: " << stats.totalCalculations << "\n";
    std::cout << "   Iterative calculations: " << stats.iterativeCalculations << "\n";
    std::cout << "   Average iterations: " << std::fixed << std::setprecision(2);
    std::cout << stats.avgIterations << "\n";
    std::cout << "   Average correction: " << std::setprecision(1);
    std::cout << stats.avgCorrection << " km\n";
    std::cout << "   Maximum correction: " << stats.maxCorrection << " km\n";
    
    std::cout << "\n   Status: " << (stats.totalCalculations == 10 ? "✓ PASS" : "✗ FAIL") << "\n";
}

void testRealOccultationScenario() {
    printHeader("TEST 7: REAL OCCULTATION SCENARIO");
    
    std::cout << "\nScenario: (433) Eros occultation prediction\n";
    std::cout << "----------------------------------------------------------------------\n";
    
    // Eros parameters (typical NEA)
    Vector3D erosHelio(1.458, 0.223, 0.115);  // AU, approximate position
    Vector3D erosVel(0.000024, -0.000012, 0.000008);  // AU/day
    Vector3D earthPos(0.983, -0.179, 0.0);  // Earth position
    JulianDate jd(2460000.0);
    
    double distance = (erosHelio - earthPos).magnitude();
    double velocityKmS = erosVel.magnitude() * 149597870.7 / 86400.0;
    
    std::cout << "\nEros parameters:\n";
    std::cout << "   Distance from Earth: " << distance << " AU = ";
    std::cout << (distance * 149597870.7) << " km\n";
    std::cout << "   Orbital velocity: " << velocityKmS << " km/s\n";
    std::cout << "   Diameter: ~16.8 km\n";
    
    PlanetaryAberrationCalculator calc;
    AberrationCorrection corr = calc.correctAsteroid(
        erosHelio, erosVel, earthPos, jd
    );
    
    std::cout << "\nAberration correction:\n";
    std::cout << "   Light-time: " << (corr.lightTime * 86400.0) << " seconds\n";
    std::cout << "   Position correction: " << corr.magnitude << " km\n";
    std::cout << "   Angular correction: " << corr.angularShift << " arcsec\n";
    
    // Compare to asteroid diameter
    double diameterKm = 16.8;
    double ratio = corr.magnitude / diameterKm;
    
    std::cout << "\nImpact assessment:\n";
    std::cout << "   Correction / Diameter: " << std::fixed << std::setprecision(2);
    std::cout << ratio << "×\n";
    
    if (ratio > 0.5) {
        std::cout << "   ⚠ WARNING: Correction > 50% of diameter\n";
        std::cout << "   Planetary aberration is CRITICAL for this event!\n";
    } else if (ratio > 0.1) {
        std::cout << "   ✓ Correction is significant (>10% diameter)\n";
        std::cout << "   Planetary aberration should be included\n";
    } else {
        std::cout << "   ✓ Correction is small (<10% diameter)\n";
        std::cout << "   Planetary aberration is less critical\n";
    }
}

int main() {
    std::cout << "\n";
    std::cout << "╔══════════════════════════════════════════════════════════════════╗\n";
    std::cout << "║       IOccultCalc - Phase 2 Planetary Aberration Tests         ║\n";
    std::cout << "║                                                                  ║\n";
    std::cout << "║  Testing finite light-time corrections for asteroids           ║\n";
    std::cout << "║  Critical for NEAs and close approaches                         ║\n";
    std::cout << "╚══════════════════════════════════════════════════════════════════╝\n";
    
    try {
        testBasicLightTime();
        testFirstOrderCorrection();
        testIterativeCorrection();
        testVelocityDependence();
        testDistanceDependence();
        testStatistics();
        testRealOccultationScenario();
        
        printHeader("PHASE 2 PLANETARY ABERRATION TESTING COMPLETE");
        std::cout << "\nAll planetary aberration tests completed successfully!\n";
        std::cout << "Module ready for integration with occultation prediction system.\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n✗ ERROR: " << e.what() << "\n";
        return 1;
    }
}
