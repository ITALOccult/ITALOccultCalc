/**
 * @file cubic_spline.h
 * @brief Cubic spline interpolation for smooth trajectory calculations
 * 
 * Provides C² continuous interpolation for ephemeris data, offering
 * smoother derivatives than Lagrange interpolation. Critical for
 * accurate velocity and acceleration calculations.
 * 
 * Theory:
 * - Cubic splines ensure C² continuity (continuous up to 2nd derivative)
 * - Natural boundary conditions: S''(x₀) = S''(xₙ) = 0
 * - Tridiagonal system solution: O(n) complexity
 * - Optimal for trajectory smoothing
 * 
 * Advantages over Lagrange:
 * - No Runge phenomenon (oscillations at edges)
 * - Smooth first and second derivatives
 * - Better for acceleration calculations
 * - More stable numerically
 * 
 * Applications in IOccultCalc:
 * - Ephemeris interpolation between computed points
 * - Velocity estimation from position data
 * - Acceleration for advanced force models
 * - Shadow path smoothing
 * 
 * References:
 * - de Boor (1978), "A Practical Guide to Splines"
 * - Press et al. (2007), Numerical Recipes, §3.3
 * - Burden & Faires (2010), Numerical Analysis, Ch. 3
 * 
 * @author IOccultCalc Phase 2 Implementation
 * @date 2025
 */

#ifndef IOCCULTCALC_CUBIC_SPLINE_H
#define IOCCULTCALC_CUBIC_SPLINE_H

#include "ioccultcalc/types.h"
#include <vector>
#include <string>
#include <stdexcept>

namespace ioccultcalc {

/**
 * @enum SplineBoundaryCondition
 * @brief Boundary conditions for spline interpolation
 */
enum class SplineBoundaryCondition {
    NATURAL,        ///< S''(x₀) = S''(xₙ) = 0 (most common)
    CLAMPED,        ///< Specify S'(x₀) and S'(xₙ) (if known)
    NOT_A_KNOT,     ///< S'''(x₁) = S'''(x₂), continuous 3rd derivative
    PERIODIC        ///< S(x₀) = S(xₙ), for periodic functions
};

/**
 * @class CubicSpline1D
 * @brief One-dimensional cubic spline interpolation
 * 
 * Interpolates scalar functions y = f(x) with C² continuity.
 * 
 * Usage:
 * ```cpp
 * std::vector<double> x = {0.0, 1.0, 2.0, 3.0, 4.0};
 * std::vector<double> y = {0.0, 1.0, 0.5, 2.0, 1.5};
 * 
 * CubicSpline1D spline(x, y);
 * 
 * double value = spline.interpolate(1.5);
 * double deriv = spline.derivative(1.5);
 * double accel = spline.secondDerivative(1.5);
 * ```
 */
class CubicSpline1D {
public:
    /**
     * @brief Construct spline from data points
     * @param x X-coordinates (must be sorted, unique)
     * @param y Y-coordinates (same size as x)
     * @param bc Boundary condition (default: natural)
     * @throws std::invalid_argument if data is invalid
     */
    CubicSpline1D(
        const std::vector<double>& x,
        const std::vector<double>& y,
        SplineBoundaryCondition bc = SplineBoundaryCondition::NATURAL
    );
    
    /**
     * @brief Construct clamped spline with specified derivatives
     * @param x X-coordinates
     * @param y Y-coordinates
     * @param dy0 First derivative at x[0]
     * @param dyn First derivative at x[n-1]
     */
    CubicSpline1D(
        const std::vector<double>& x,
        const std::vector<double>& y,
        double dy0,
        double dyn
    );
    
    ~CubicSpline1D();
    
    // ========================================================================
    // Interpolation methods
    // ========================================================================
    
    /**
     * @brief Interpolate at point x
     * @param x Evaluation point
     * @return Interpolated value
     * @throws std::out_of_range if x outside data range
     */
    double interpolate(double x) const;
    
    /**
     * @brief First derivative at point x
     * @param x Evaluation point
     * @return dy/dx
     */
    double derivative(double x) const;
    
    /**
     * @brief Second derivative at point x
     * @param x Evaluation point
     * @return d²y/dx²
     */
    double secondDerivative(double x) const;
    
    /**
     * @brief Evaluate at multiple points (more efficient)
     * @param xs Evaluation points
     * @return Interpolated values
     */
    std::vector<double> interpolate(const std::vector<double>& xs) const;
    
    /**
     * @brief Evaluate derivatives at multiple points
     */
    std::vector<double> derivative(const std::vector<double>& xs) const;
    
    // ========================================================================
    // Query methods
    // ========================================================================
    
    /**
     * @brief Get valid interpolation range
     */
    std::pair<double, double> getRange() const;
    
    /**
     * @brief Get number of data points
     */
    size_t size() const;
    
    /**
     * @brief Check if point is in valid range
     */
    bool inRange(double x) const;
    
    /**
     * @brief Get spline coefficients for interval i
     * 
     * Returns [a, b, c, d] such that:
     * S(x) = a + b(x-x[i]) + c(x-x[i])² + d(x-x[i])³
     */
    std::vector<double> getCoefficients(size_t interval) const;
    
private:
    class Impl;
    Impl* pImpl;
    
    void computeCoefficients();
    size_t findInterval(double x) const;
};

/**
 * @class CubicSpline3D
 * @brief Three-dimensional cubic spline interpolation
 * 
 * Interpolates vector functions r(t) = (x(t), y(t), z(t)) with C² continuity.
 * Each component is interpolated independently with its own spline.
 * 
 * Applications:
 * - Ephemeris position interpolation
 * - Shadow path smoothing
 * - Trajectory interpolation
 * 
 * Usage:
 * ```cpp
 * std::vector<double> t = {0.0, 1.0, 2.0, 3.0};
 * std::vector<Vector3D> positions = {
 *     Vector3D(0, 0, 0),
 *     Vector3D(1, 1, 0.5),
 *     Vector3D(2, 0.5, 1),
 *     Vector3D(3, 2, 1.5)
 * };
 * 
 * CubicSpline3D spline(t, positions);
 * 
 * Vector3D pos = spline.interpolate(1.5);
 * Vector3D vel = spline.derivative(1.5);  // Velocity
 * Vector3D acc = spline.secondDerivative(1.5);  // Acceleration
 * ```
 */
class CubicSpline3D {
public:
    /**
     * @brief Construct 3D spline from trajectory points
     * @param t Time or parameter values (sorted, unique)
     * @param points 3D positions at each t
     * @param bc Boundary condition
     */
    CubicSpline3D(
        const std::vector<double>& t,
        const std::vector<Vector3D>& points,
        SplineBoundaryCondition bc = SplineBoundaryCondition::NATURAL
    );
    
    /**
     * @brief Construct with velocity boundary conditions
     * @param t Time values
     * @param points Positions
     * @param v0 Velocity at t[0]
     * @param vn Velocity at t[n-1]
     */
    CubicSpline3D(
        const std::vector<double>& t,
        const std::vector<Vector3D>& points,
        const Vector3D& v0,
        const Vector3D& vn
    );
    
    ~CubicSpline3D();
    
    // ========================================================================
    // Interpolation methods
    // ========================================================================
    
    /**
     * @brief Interpolate position at time t
     */
    Vector3D interpolate(double t) const;
    
    /**
     * @brief First derivative (velocity) at time t
     */
    Vector3D derivative(double t) const;
    
    /**
     * @brief Second derivative (acceleration) at time t
     */
    Vector3D secondDerivative(double t) const;
    
    /**
     * @brief Interpolate at multiple times
     */
    std::vector<Vector3D> interpolate(const std::vector<double>& ts) const;
    
    /**
     * @brief Get velocities at multiple times
     */
    std::vector<Vector3D> derivative(const std::vector<double>& ts) const;
    
    // ========================================================================
    // Advanced methods
    // ========================================================================
    
    /**
     * @brief Arc length from t0 to t1
     * 
     * Computed by integrating |dr/dt| using Simpson's rule
     */
    double arcLength(double t0, double t1, int numSteps = 100) const;
    
    /**
     * @brief Curvature at time t
     * 
     * κ = |r' × r''| / |r'|³
     */
    double curvature(double t) const;
    
    /**
     * @brief Unit tangent vector at time t
     */
    Vector3D tangent(double t) const;
    
    /**
     * @brief Unit normal vector at time t
     */
    Vector3D normal(double t) const;
    
    /**
     * @brief Unit binormal vector at time t
     */
    Vector3D binormal(double t) const;
    
    /**
     * @brief Frenet-Serret frame at time t
     * @return {tangent, normal, binormal}
     */
    std::vector<Vector3D> frenetFrame(double t) const;
    
    // ========================================================================
    // Query methods
    // ========================================================================
    
    std::pair<double, double> getRange() const;
    size_t size() const;
    bool inRange(double t) const;
    
private:
    class Impl;
    Impl* pImpl;
};

/**
 * @class SplineInterpolator
 * @brief High-level interface for ephemeris interpolation
 * 
 * Convenience wrapper for common IOccultCalc use cases.
 */
class SplineInterpolator {
public:
    /**
     * @brief Interpolate ephemeris data
     * 
     * Given sparse ephemeris points, generate smooth trajectory
     * 
     * @param jds Julian dates of known points
     * @param positions Heliocentric positions (AU)
     * @param targetJDs Times for interpolation
     * @return Interpolated positions
     */
    static std::vector<Vector3D> interpolateEphemeris(
        const std::vector<double>& jds,
        const std::vector<Vector3D>& positions,
        const std::vector<double>& targetJDs
    );
    
    /**
     * @brief Interpolate with velocity estimation
     * 
     * Returns both positions and velocities
     * 
     * @param jds Julian dates
     * @param positions Known positions
     * @param targetJD Target time
     * @return {position, velocity}
     */
    static std::pair<Vector3D, Vector3D> interpolateWithVelocity(
        const std::vector<double>& jds,
        const std::vector<Vector3D>& positions,
        double targetJD
    );
    
    /**
     * @brief Smooth noisy trajectory data
     * 
     * Useful for fitting observations with uncertainties
     * 
     * @param times Time points
     * @param data Noisy position data
     * @param smoothingFactor 0=no smoothing, 1=maximum smoothing
     * @return Smoothed trajectory
     */
    static std::vector<Vector3D> smoothTrajectory(
        const std::vector<double>& times,
        const std::vector<Vector3D>& data,
        double smoothingFactor = 0.1
    );
    
    /**
     * @brief Compare spline vs Lagrange interpolation
     * 
     * Diagnostic tool to assess interpolation quality
     * 
     * @param jds Data times
     * @param positions Data points
     * @param testJD Evaluation time
     * @return {spline_result, lagrange_result, difference}
     */
    static std::vector<Vector3D> compareInterpolationMethods(
        const std::vector<double>& jds,
        const std::vector<Vector3D>& positions,
        double testJD
    );
};

// ============================================================================
// Utility functions
// ============================================================================

/**
 * @brief Quick 1D spline interpolation
 */
double cubicSplineInterpolate(
    const std::vector<double>& x,
    const std::vector<double>& y,
    double xTarget
);

/**
 * @brief Quick 3D spline interpolation
 */
Vector3D cubicSplineInterpolate(
    const std::vector<double>& t,
    const std::vector<Vector3D>& points,
    double tTarget
);

/**
 * @brief Estimate optimal number of spline points
 * 
 * Given desired accuracy and trajectory characteristics,
 * recommend spacing for ephemeris calculations
 * 
 * @param timeSpan Total time span (days)
 * @param maxVelocity Maximum velocity (AU/day)
 * @param accuracyKm Desired accuracy (km)
 * @return Recommended number of points
 */
int estimateOptimalSplinePoints(
    double timeSpan,
    double maxVelocity,
    double accuracyKm = 1.0
);

} // namespace ioccultcalc

#endif // IOCCULTCALC_CUBIC_SPLINE_H
