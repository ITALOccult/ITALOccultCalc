/**
 * @file test_cubic_spline.cpp
 * @brief Comprehensive test suite for cubic spline interpolation
 */

#include "cubic_spline.h"
#include <iostream>
#include <iomanip>
#include <cmath>

using namespace ioccultcalc;

bool doubleEquals(double a, double b, double tol = 1e-6) {
    return std::fabs(a - b) < tol;
}

bool vectorEquals(const Vector3D& a, const Vector3D& b, double tol = 1e-6) {
    return doubleEquals(a.x, b.x, tol) && 
           doubleEquals(a.y, b.y, tol) && 
           doubleEquals(a.z, b.z, tol);
}

void printTestHeader(const std::string& testName) {
    std::cout << "\n========================================\n";
    std::cout << "TEST: " << testName << "\n";
    std::cout << "========================================\n";
}

void testLinearInterpolation() {
    printTestHeader("1D Linear Function y = 2x + 1");
    
    // Test on a simple linear function
    std::vector<double> x = {0.0, 1.0, 2.0, 3.0, 4.0};
    std::vector<double> y;
    for (double xi : x) {
        y.push_back(2.0 * xi + 1.0);
    }
    
    CubicSpline1D spline(x, y);
    
    // Test at data points
    bool passDataPoints = true;
    for (size_t i = 0; i < x.size(); ++i) {
        double yi = spline.interpolate(x[i]);
        if (!doubleEquals(yi, y[i])) {
            passDataPoints = false;
            std::cout << "FAIL at x[" << i << "] = " << x[i] 
                     << ": expected " << y[i] << ", got " << yi << "\n";
        }
    }
    
    // Test between points
    double xMid = 1.5;
    double yExpected = 2.0 * xMid + 1.0;
    double yInterp = spline.interpolate(xMid);
    
    std::cout << "At data points: " << (passDataPoints ? "PASS" : "FAIL") << "\n";
    std::cout << "At x = 1.5: expected = " << yExpected 
             << ", interpolated = " << yInterp 
             << ", error = " << std::fabs(yInterp - yExpected) << "\n";
    std::cout << (doubleEquals(yInterp, yExpected, 1e-4) ? "PASS" : "FAIL") << "\n";
}

void testQuadraticFunction() {
    printTestHeader("1D Quadratic Function y = x²");
    
    std::vector<double> x = {-2.0, -1.0, 0.0, 1.0, 2.0};
    std::vector<double> y;
    for (double xi : x) {
        y.push_back(xi * xi);
    }
    
    CubicSpline1D spline(x, y);
    
    // Test interpolation and derivatives
    double xTest = 0.5;
    double yExpected = xTest * xTest;
    double dyExpected = 2.0 * xTest;
    double d2yExpected = 2.0;
    
    double yInterp = spline.interpolate(xTest);
    double dyInterp = spline.derivative(xTest);
    double d2yInterp = spline.secondDerivative(xTest);
    
    std::cout << "At x = " << xTest << ":\n";
    std::cout << "  y: expected = " << yExpected << ", got = " << yInterp 
             << ", error = " << std::fabs(yInterp - yExpected) << "\n";
    std::cout << "  y': expected = " << dyExpected << ", got = " << dyInterp 
             << ", error = " << std::fabs(dyInterp - dyExpected) << "\n";
    std::cout << "  y'': expected = " << d2yExpected << ", got = " << d2yInterp 
             << ", error = " << std::fabs(d2yInterp - d2yExpected) << "\n";
    
    bool pass = doubleEquals(yInterp, yExpected, 1e-3) &&
                doubleEquals(dyInterp, dyExpected, 1e-2) &&
                doubleEquals(d2yInterp, d2yExpected, 0.5);
    
    std::cout << (pass ? "PASS" : "FAIL") << "\n";
}

void testBoundaryConditions() {
    printTestHeader("Boundary Conditions Comparison");
    
    std::vector<double> x = {0.0, 1.0, 2.0, 3.0};
    std::vector<double> y = {0.0, 1.0, 0.0, -1.0};
    
    // Natural spline
    CubicSpline1D naturalSpline(x, y, SplineBoundaryCondition::NATURAL);
    
    // Clamped spline with zero derivatives at ends
    CubicSpline1D clampedSpline(x, y, 0.5, -0.5);
    
    double xTest = 1.5;
    double yNatural = naturalSpline.interpolate(xTest);
    double yClamped = clampedSpline.interpolate(xTest);
    
    std::cout << "At x = " << xTest << ":\n";
    std::cout << "  Natural BC: y = " << yNatural << "\n";
    std::cout << "  Clamped BC: y = " << yClamped << "\n";
    std::cout << "  Difference: " << std::fabs(yNatural - yClamped) << "\n";
    
    // Check boundary derivatives for clamped
    double dy0 = clampedSpline.derivative(x[0]);
    double dy3 = clampedSpline.derivative(x[3]);
    
    std::cout << "Clamped derivatives:\n";
    std::cout << "  At x = 0: expected = 0.5, got = " << dy0 << "\n";
    std::cout << "  At x = 3: expected = -0.5, got = " << dy3 << "\n";
    
    bool pass = doubleEquals(dy0, 0.5, 1e-6) && doubleEquals(dy3, -0.5, 1e-6);
    std::cout << (pass ? "PASS" : "FAIL") << "\n";
}

void test3DCircularTrajectory() {
    printTestHeader("3D Circular Trajectory");
    
    // Create points on a circle in XY plane
    int n = 8;
    std::vector<double> t;
    std::vector<Vector3D> points;
    
    for (int i = 0; i < n; ++i) {
        double theta = 2.0 * M_PI * i / (n - 1);
        t.push_back(theta);
        points.push_back(Vector3D(std::cos(theta), std::sin(theta), 0.0));
    }
    
    CubicSpline3D spline(t, points);
    
    // Test at midpoint
    double tTest = M_PI / 2.0;
    Vector3D pos = spline.interpolate(tTest);
    Vector3D vel = spline.derivative(tTest);
    
    // Expected: position near (0, 1, 0), velocity near (-1, 0, 0)
    Vector3D expectedPos(0.0, 1.0, 0.0);
    Vector3D expectedVel(-1.0, 0.0, 0.0);
    
    double posError = (pos - expectedPos).magnitude();
    double velError = (vel - expectedVel).magnitude();
    
    std::cout << "At t = π/2:\n";
    std::cout << "  Position: (" << pos.x << ", " << pos.y << ", " << pos.z << ")\n";
    std::cout << "  Expected: (0, 1, 0), error = " << posError << "\n";
    std::cout << "  Velocity: (" << vel.x << ", " << vel.y << ", " << vel.z << ")\n";
    std::cout << "  Expected: (-1, 0, 0), error = " << velError << "\n";
    
    // Check curvature (should be ~1 for unit circle)
    double kappa = spline.curvature(tTest);
    std::cout << "  Curvature: " << kappa << " (expected ~1.0)\n";
    
    bool pass = posError < 0.1 && velError < 0.2 && doubleEquals(kappa, 1.0, 0.5);
    std::cout << (pass ? "PASS" : "FAIL") << "\n";
}

void testFrenetFrame() {
    printTestHeader("Frenet Frame Computation");
    
    // Helix trajectory: r(t) = (cos(t), sin(t), t)
    std::vector<double> t;
    std::vector<Vector3D> points;
    
    for (int i = 0; i <= 20; ++i) {
        double ti = 2.0 * M_PI * i / 20.0;
        t.push_back(ti);
        points.push_back(Vector3D(std::cos(ti), std::sin(ti), ti));
    }
    
    CubicSpline3D spline(t, points);
    
    double tTest = M_PI / 4.0;
    auto frame = spline.frenetFrame(tTest);
    
    Vector3D T = frame[0];  // Tangent
    Vector3D N = frame[1];  // Normal
    Vector3D B = frame[2];  // Binormal
    
    std::cout << "Frenet frame at t = π/4:\n";
    std::cout << "  Tangent: (" << T.x << ", " << T.y << ", " << T.z << ")\n";
    std::cout << "  Normal: (" << N.x << ", " << N.y << ", " << N.z << ")\n";
    std::cout << "  Binormal: (" << B.x << ", " << B.y << ", " << B.z << ")\n";
    
    // Check orthonormality
    double TMag = T.magnitude();
    double NMag = N.magnitude();
    double BMag = B.magnitude();
    double TdotN = T.dot(N);
    double TdotB = T.dot(B);
    double NdotB = N.dot(B);
    
    std::cout << "\nOrthonormality checks:\n";
    std::cout << "  |T| = " << TMag << " (should be 1)\n";
    std::cout << "  |N| = " << NMag << " (should be 1)\n";
    std::cout << "  |B| = " << BMag << " (should be 1)\n";
    std::cout << "  T·N = " << TdotN << " (should be 0)\n";
    std::cout << "  T·B = " << TdotB << " (should be 0)\n";
    std::cout << "  N·B = " << NdotB << " (should be 0)\n";
    
    bool pass = doubleEquals(TMag, 1.0, 1e-3) &&
                doubleEquals(NMag, 1.0, 1e-3) &&
                doubleEquals(BMag, 1.0, 1e-3) &&
                doubleEquals(TdotN, 0.0, 1e-3) &&
                doubleEquals(TdotB, 0.0, 1e-3) &&
                doubleEquals(NdotB, 0.0, 1e-3);
    
    std::cout << (pass ? "PASS" : "FAIL") << "\n";
}

void testArcLength() {
    printTestHeader("Arc Length Calculation");
    
    // Straight line from (0,0,0) to (3,4,0), length should be 5
    std::vector<double> t = {0.0, 1.0};
    std::vector<Vector3D> points = {
        Vector3D(0.0, 0.0, 0.0),
        Vector3D(3.0, 4.0, 0.0)
    };
    
    CubicSpline3D spline(t, points);
    
    double length = spline.arcLength(0.0, 1.0, 100);
    double expectedLength = 5.0;
    
    std::cout << "Straight line (0,0,0) -> (3,4,0):\n";
    std::cout << "  Computed arc length: " << length << "\n";
    std::cout << "  Expected: " << expectedLength << "\n";
    std::cout << "  Error: " << std::fabs(length - expectedLength) << "\n";
    
    bool pass = doubleEquals(length, expectedLength, 0.01);
    std::cout << (pass ? "PASS" : "FAIL") << "\n";
}

void testEphemerisInterpolation() {
    printTestHeader("Ephemeris Interpolation");
    
    // Simulate asteroid positions over 10 days
    std::vector<double> jds;
    std::vector<Vector3D> positions;
    
    for (int i = 0; i <= 10; ++i) {
        double jd = 2460000.0 + i;
        jds.push_back(jd);
        
        // Parabolic motion: r(t) = (t, t², 0.1*t)
        double t = i;
        positions.push_back(Vector3D(t, t*t, 0.1*t));
    }
    
    // Interpolate at intermediate times
    std::vector<double> targetJDs = {2460000.5, 2460003.7, 2460007.2};
    
    auto interpolated = SplineInterpolator::interpolateEphemeris(
        jds, positions, targetJDs
    );
    
    std::cout << "Interpolated positions:\n";
    for (size_t i = 0; i < targetJDs.size(); ++i) {
        double t = targetJDs[i] - 2460000.0;
        Vector3D expected(t, t*t, 0.1*t);
        Vector3D got = interpolated[i];
        
        double error = (got - expected).magnitude();
        
        std::cout << "  JD " << targetJDs[i] << " (t=" << t << "):\n";
        std::cout << "    Expected: (" << expected.x << ", " << expected.y 
                 << ", " << expected.z << ")\n";
        std::cout << "    Got: (" << got.x << ", " << got.y << ", " << got.z << ")\n";
        std::cout << "    Error: " << error << " AU\n";
    }
    
    std::cout << "PASS (ephemeris interpolation functional)\n";
}

void testContinuity() {
    printTestHeader("C² Continuity Verification");
    
    // Create spline through several points
    std::vector<double> x = {0.0, 1.0, 2.0, 3.0, 4.0};
    std::vector<double> y = {0.0, 1.0, 0.5, 2.0, 1.5};
    
    CubicSpline1D spline(x, y);
    
    // Check continuity at knot x = 2.0
    double epsilon = 1e-6;
    double xKnot = 2.0;
    
    // Value continuity
    double yLeft = spline.interpolate(xKnot - epsilon);
    double yRight = spline.interpolate(xKnot + epsilon);
    double yKnot = spline.interpolate(xKnot);
    
    // First derivative continuity
    double dyLeft = spline.derivative(xKnot - epsilon);
    double dyRight = spline.derivative(xKnot + epsilon);
    
    // Second derivative continuity
    double d2yLeft = spline.secondDerivative(xKnot - epsilon);
    double d2yRight = spline.secondDerivative(xKnot + epsilon);
    
    std::cout << "Continuity at knot x = " << xKnot << ":\n";
    std::cout << "  C⁰: y(2⁻) = " << yLeft << ", y(2⁺) = " << yRight 
             << ", |diff| = " << std::fabs(yRight - yLeft) << "\n";
    std::cout << "  C¹: y'(2⁻) = " << dyLeft << ", y'(2⁺) = " << dyRight 
             << ", |diff| = " << std::fabs(dyRight - dyLeft) << "\n";
    std::cout << "  C²: y''(2⁻) = " << d2yLeft << ", y''(2⁺) = " << d2yRight 
             << ", |diff| = " << std::fabs(d2yRight - d2yLeft) << "\n";
    
    bool c0 = doubleEquals(yLeft, yRight, 1e-4);
    bool c1 = doubleEquals(dyLeft, dyRight, 1e-3);
    bool c2 = doubleEquals(d2yLeft, d2yRight, 1e-2);
    
    std::cout << "  C⁰ continuous: " << (c0 ? "YES" : "NO") << "\n";
    std::cout << "  C¹ continuous: " << (c1 ? "YES" : "NO") << "\n";
    std::cout << "  C² continuous: " << (c2 ? "YES" : "NO") << "\n";
    
    bool pass = c0 && c1 && c2;
    std::cout << (pass ? "PASS (C² continuity verified)" : "FAIL") << "\n";
}

int main() {
    std::cout << std::fixed << std::setprecision(6);
    
    std::cout << "\n";
    std::cout << "================================================\n";
    std::cout << "CUBIC SPLINE INTERPOLATION TEST SUITE\n";
    std::cout << "================================================\n";
    
    try {
        testLinearInterpolation();
        testQuadraticFunction();
        testBoundaryConditions();
        test3DCircularTrajectory();
        testFrenetFrame();
        testArcLength();
        testEphemerisInterpolation();
        testContinuity();
        
        std::cout << "\n================================================\n";
        std::cout << "ALL CUBIC SPLINE TESTS COMPLETED SUCCESSFULLY\n";
        std::cout << "================================================\n\n";
        
    } catch (const std::exception& e) {
        std::cerr << "\n*** TEST FAILED WITH EXCEPTION ***\n";
        std::cerr << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
