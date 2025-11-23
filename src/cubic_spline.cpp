/**
 * @file cubic_spline.cpp
 * @brief Implementation of cubic spline interpolation
 */

#include "cubic_spline.h"
#include <cmath>
#include <algorithm>
#include <stdexcept>
#include <sstream>

namespace ioccultcalc {

// ============================================================================
// CubicSpline1D Implementation
// ============================================================================

class CubicSpline1D::Impl {
public:
    std::vector<double> x;  // Data x-coordinates
    std::vector<double> y;  // Data y-coordinates
    std::vector<double> b;  // Linear coefficients
    std::vector<double> c;  // Quadratic coefficients
    std::vector<double> d;  // Cubic coefficients
    SplineBoundaryCondition bc;
    double dy0, dyn;  // Boundary derivatives for clamped case
    
    Impl() : bc(SplineBoundaryCondition::NATURAL), dy0(0.0), dyn(0.0) {}
};

CubicSpline1D::CubicSpline1D(
    const std::vector<double>& x,
    const std::vector<double>& y,
    SplineBoundaryCondition bc)
    : pImpl(new Impl())
{
    if (x.size() != y.size()) {
        throw std::invalid_argument("CubicSpline1D: x and y must have same size");
    }
    if (x.size() < 2) {
        throw std::invalid_argument("CubicSpline1D: need at least 2 points");
    }
    
    // Check if x is sorted
    for (size_t i = 1; i < x.size(); ++i) {
        if (x[i] <= x[i-1]) {
            throw std::invalid_argument("CubicSpline1D: x must be strictly increasing");
        }
    }
    
    pImpl->x = x;
    pImpl->y = y;
    pImpl->bc = bc;
    
    computeCoefficients();
}

CubicSpline1D::CubicSpline1D(
    const std::vector<double>& x,
    const std::vector<double>& y,
    double dy0,
    double dyn)
    : pImpl(new Impl())
{
    if (x.size() != y.size()) {
        throw std::invalid_argument("CubicSpline1D: x and y must have same size");
    }
    if (x.size() < 2) {
        throw std::invalid_argument("CubicSpline1D: need at least 2 points");
    }
    
    pImpl->x = x;
    pImpl->y = y;
    pImpl->bc = SplineBoundaryCondition::CLAMPED;
    pImpl->dy0 = dy0;
    pImpl->dyn = dyn;
    
    computeCoefficients();
}

CubicSpline1D::~CubicSpline1D() {
    delete pImpl;
}

void CubicSpline1D::computeCoefficients() {
    size_t n = pImpl->x.size();
    
    // Initialize coefficient vectors
    pImpl->b.resize(n);
    pImpl->c.resize(n);
    pImpl->d.resize(n);
    
    // Compute differences
    std::vector<double> h(n-1);  // h[i] = x[i+1] - x[i]
    std::vector<double> alpha(n-1);
    
    for (size_t i = 0; i < n-1; ++i) {
        h[i] = pImpl->x[i+1] - pImpl->x[i];
    }
    
    // Natural or clamped boundary conditions
    std::vector<double> l(n), mu(n), z(n);
    
    if (pImpl->bc == SplineBoundaryCondition::NATURAL) {
        // Natural spline: S''(x0) = S''(xn) = 0
        l[0] = 1.0;
        mu[0] = 0.0;
        z[0] = 0.0;
        
        for (size_t i = 1; i < n-1; ++i) {
            alpha[i] = (3.0/h[i]) * (pImpl->y[i+1] - pImpl->y[i]) 
                     - (3.0/h[i-1]) * (pImpl->y[i] - pImpl->y[i-1]);
            
            l[i] = 2.0 * (pImpl->x[i+1] - pImpl->x[i-1]) - h[i-1] * mu[i-1];
            mu[i] = h[i] / l[i];
            z[i] = (alpha[i] - h[i-1] * z[i-1]) / l[i];
        }
        
        l[n-1] = 1.0;
        z[n-1] = 0.0;
        pImpl->c[n-1] = 0.0;
        
    } else if (pImpl->bc == SplineBoundaryCondition::CLAMPED) {
        // Clamped spline: S'(x0) = dy0, S'(xn) = dyn
        l[0] = 2.0 * h[0];
        mu[0] = 0.5;
        alpha[0] = (3.0/h[0]) * (pImpl->y[1] - pImpl->y[0]) - 3.0 * pImpl->dy0;
        z[0] = alpha[0] / l[0];
        
        for (size_t i = 1; i < n-1; ++i) {
            alpha[i] = (3.0/h[i]) * (pImpl->y[i+1] - pImpl->y[i]) 
                     - (3.0/h[i-1]) * (pImpl->y[i] - pImpl->y[i-1]);
            
            l[i] = 2.0 * (pImpl->x[i+1] - pImpl->x[i-1]) - h[i-1] * mu[i-1];
            mu[i] = h[i] / l[i];
            z[i] = (alpha[i] - h[i-1] * z[i-1]) / l[i];
        }
        
        double alphan = 3.0 * pImpl->dyn - (3.0/h[n-2]) * (pImpl->y[n-1] - pImpl->y[n-2]);
        l[n-1] = h[n-2] * (2.0 - mu[n-2]);
        z[n-1] = (alphan - h[n-2] * z[n-2]) / l[n-1];
        pImpl->c[n-1] = z[n-1];
    }
    
    // Back substitution
    for (int i = n-2; i >= 0; --i) {
        pImpl->c[i] = z[i] - mu[i] * pImpl->c[i+1];
        pImpl->b[i] = (pImpl->y[i+1] - pImpl->y[i]) / h[i] 
                    - h[i] * (pImpl->c[i+1] + 2.0 * pImpl->c[i]) / 3.0;
        pImpl->d[i] = (pImpl->c[i+1] - pImpl->c[i]) / (3.0 * h[i]);
    }
}

size_t CubicSpline1D::findInterval(double x) const {
    // Binary search for interval
    auto it = std::upper_bound(pImpl->x.begin(), pImpl->x.end(), x);
    if (it == pImpl->x.begin()) return 0;
    if (it == pImpl->x.end()) return pImpl->x.size() - 2;
    return std::distance(pImpl->x.begin(), it) - 1;
}

double CubicSpline1D::interpolate(double x) const {
    if (!inRange(x)) {
        std::ostringstream oss;
        oss << "CubicSpline1D: x=" << x << " outside range ["
            << pImpl->x.front() << ", " << pImpl->x.back() << "]";
        throw std::out_of_range(oss.str());
    }
    
    size_t i = findInterval(x);
    double dx = x - pImpl->x[i];
    
    // S(x) = y[i] + b[i]*dx + c[i]*dx² + d[i]*dx³
    return pImpl->y[i] + dx * (pImpl->b[i] + dx * (pImpl->c[i] + dx * pImpl->d[i]));
}

double CubicSpline1D::derivative(double x) const {
    if (!inRange(x)) {
        throw std::out_of_range("CubicSpline1D: x outside valid range");
    }
    
    size_t i = findInterval(x);
    double dx = x - pImpl->x[i];
    
    // S'(x) = b[i] + 2*c[i]*dx + 3*d[i]*dx²
    return pImpl->b[i] + dx * (2.0 * pImpl->c[i] + 3.0 * dx * pImpl->d[i]);
}

double CubicSpline1D::secondDerivative(double x) const {
    if (!inRange(x)) {
        throw std::out_of_range("CubicSpline1D: x outside valid range");
    }
    
    size_t i = findInterval(x);
    double dx = x - pImpl->x[i];
    
    // S''(x) = 2*c[i] + 6*d[i]*dx
    return 2.0 * pImpl->c[i] + 6.0 * pImpl->d[i] * dx;
}

std::vector<double> CubicSpline1D::interpolate(const std::vector<double>& xs) const {
    std::vector<double> result;
    result.reserve(xs.size());
    for (double x : xs) {
        result.push_back(interpolate(x));
    }
    return result;
}

std::vector<double> CubicSpline1D::derivative(const std::vector<double>& xs) const {
    std::vector<double> result;
    result.reserve(xs.size());
    for (double x : xs) {
        result.push_back(derivative(x));
    }
    return result;
}

std::pair<double, double> CubicSpline1D::getRange() const {
    return {pImpl->x.front(), pImpl->x.back()};
}

size_t CubicSpline1D::size() const {
    return pImpl->x.size();
}

bool CubicSpline1D::inRange(double x) const {
    return x >= pImpl->x.front() && x <= pImpl->x.back();
}

std::vector<double> CubicSpline1D::getCoefficients(size_t i) const {
    if (i >= pImpl->x.size() - 1) {
        throw std::out_of_range("CubicSpline1D: interval index out of range");
    }
    return {pImpl->y[i], pImpl->b[i], pImpl->c[i], pImpl->d[i]};
}

// ============================================================================
// CubicSpline3D Implementation
// ============================================================================

class CubicSpline3D::Impl {
public:
    CubicSpline1D* splineX;
    CubicSpline1D* splineY;
    CubicSpline1D* splineZ;
    
    Impl() : splineX(nullptr), splineY(nullptr), splineZ(nullptr) {}
    
    ~Impl() {
        delete splineX;
        delete splineY;
        delete splineZ;
    }
};

CubicSpline3D::CubicSpline3D(
    const std::vector<double>& t,
    const std::vector<Vector3D>& points,
    SplineBoundaryCondition bc)
    : pImpl(new Impl())
{
    if (t.size() != points.size()) {
        throw std::invalid_argument("CubicSpline3D: t and points must have same size");
    }
    
    // Extract x, y, z components
    std::vector<double> x, y, z;
    x.reserve(points.size());
    y.reserve(points.size());
    z.reserve(points.size());
    
    for (const auto& p : points) {
        x.push_back(p.x);
        y.push_back(p.y);
        z.push_back(p.z);
    }
    
    // Create independent splines for each component
    pImpl->splineX = new CubicSpline1D(t, x, bc);
    pImpl->splineY = new CubicSpline1D(t, y, bc);
    pImpl->splineZ = new CubicSpline1D(t, z, bc);
}

CubicSpline3D::CubicSpline3D(
    const std::vector<double>& t,
    const std::vector<Vector3D>& points,
    const Vector3D& v0,
    const Vector3D& vn)
    : pImpl(new Impl())
{
    if (t.size() != points.size()) {
        throw std::invalid_argument("CubicSpline3D: t and points must have same size");
    }
    
    std::vector<double> x, y, z;
    for (const auto& p : points) {
        x.push_back(p.x);
        y.push_back(p.y);
        z.push_back(p.z);
    }
    
    pImpl->splineX = new CubicSpline1D(t, x, v0.x, vn.x);
    pImpl->splineY = new CubicSpline1D(t, y, v0.y, vn.y);
    pImpl->splineZ = new CubicSpline1D(t, z, v0.z, vn.z);
}

CubicSpline3D::~CubicSpline3D() {
    delete pImpl;
}

Vector3D CubicSpline3D::interpolate(double t) const {
    return Vector3D(
        pImpl->splineX->interpolate(t),
        pImpl->splineY->interpolate(t),
        pImpl->splineZ->interpolate(t)
    );
}

Vector3D CubicSpline3D::derivative(double t) const {
    return Vector3D(
        pImpl->splineX->derivative(t),
        pImpl->splineY->derivative(t),
        pImpl->splineZ->derivative(t)
    );
}

Vector3D CubicSpline3D::secondDerivative(double t) const {
    return Vector3D(
        pImpl->splineX->secondDerivative(t),
        pImpl->splineY->secondDerivative(t),
        pImpl->splineZ->secondDerivative(t)
    );
}

std::vector<Vector3D> CubicSpline3D::interpolate(const std::vector<double>& ts) const {
    std::vector<Vector3D> result;
    result.reserve(ts.size());
    for (double t : ts) {
        result.push_back(interpolate(t));
    }
    return result;
}

std::vector<Vector3D> CubicSpline3D::derivative(const std::vector<double>& ts) const {
    std::vector<Vector3D> result;
    result.reserve(ts.size());
    for (double t : ts) {
        result.push_back(derivative(t));
    }
    return result;
}

double CubicSpline3D::arcLength(double t0, double t1, int numSteps) const {
    // Simpson's rule integration
    double h = (t1 - t0) / numSteps;
    double length = 0.0;
    
    for (int i = 0; i <= numSteps; ++i) {
        double t = t0 + i * h;
        Vector3D v = derivative(t);
        double speed = v.magnitude();
        
        double weight = (i == 0 || i == numSteps) ? 1.0 : (i % 2 == 0 ? 2.0 : 4.0);
        length += weight * speed;
    }
    
    return length * h / 3.0;
}

double CubicSpline3D::curvature(double t) const {
    Vector3D r_prime = derivative(t);
    Vector3D r_double_prime = secondDerivative(t);
    
    Vector3D cross = r_prime.cross(r_double_prime);
    double numerator = cross.magnitude();
    double denominator = std::pow(r_prime.magnitude(), 3.0);
    
    return (denominator > 1e-10) ? (numerator / denominator) : 0.0;
}

Vector3D CubicSpline3D::tangent(double t) const {
    Vector3D v = derivative(t);
    double mag = v.magnitude();
    return (mag > 1e-10) ? v / mag : Vector3D(1, 0, 0);
}

Vector3D CubicSpline3D::normal(double t) const {
    Vector3D T = tangent(t);
    Vector3D dT = secondDerivative(t);
    Vector3D N = dT - T * T.dot(dT);
    double mag = N.magnitude();
    return (mag > 1e-10) ? N / mag : Vector3D(0, 1, 0);
}

Vector3D CubicSpline3D::binormal(double t) const {
    return tangent(t).cross(normal(t));
}

std::vector<Vector3D> CubicSpline3D::frenetFrame(double t) const {
    return {tangent(t), normal(t), binormal(t)};
}

std::pair<double, double> CubicSpline3D::getRange() const {
    return pImpl->splineX->getRange();
}

size_t CubicSpline3D::size() const {
    return pImpl->splineX->size();
}

bool CubicSpline3D::inRange(double t) const {
    return pImpl->splineX->inRange(t);
}

// ============================================================================
// SplineInterpolator
// ============================================================================

std::vector<Vector3D> SplineInterpolator::interpolateEphemeris(
    const std::vector<double>& jds,
    const std::vector<Vector3D>& positions,
    const std::vector<double>& targetJDs)
{
    CubicSpline3D spline(jds, positions);
    return spline.interpolate(targetJDs);
}

std::pair<Vector3D, Vector3D> SplineInterpolator::interpolateWithVelocity(
    const std::vector<double>& jds,
    const std::vector<Vector3D>& positions,
    double targetJD)
{
    CubicSpline3D spline(jds, positions);
    return {spline.interpolate(targetJD), spline.derivative(targetJD)};
}

std::vector<Vector3D> SplineInterpolator::smoothTrajectory(
    const std::vector<double>& times,
    const std::vector<Vector3D>& data,
    double smoothingFactor)
{
    // Simple smoothing using spline (more advanced would use smoothing splines)
    CubicSpline3D spline(times, data);
    return spline.interpolate(times);
}

std::vector<Vector3D> SplineInterpolator::compareInterpolationMethods(
    const std::vector<double>& jds,
    const std::vector<Vector3D>& positions,
    double testJD)
{
    // Spline result
    CubicSpline3D spline(jds, positions);
    Vector3D splineResult = spline.interpolate(testJD);
    
    // For comparison, would implement Lagrange here
    // For now, return spline result twice with zero difference
    return {splineResult, splineResult, Vector3D(0, 0, 0)};
}

// ============================================================================
// Utility functions
// ============================================================================

double cubicSplineInterpolate(
    const std::vector<double>& x,
    const std::vector<double>& y,
    double xTarget)
{
    CubicSpline1D spline(x, y);
    return spline.interpolate(xTarget);
}

Vector3D cubicSplineInterpolate(
    const std::vector<double>& t,
    const std::vector<Vector3D>& points,
    double tTarget)
{
    CubicSpline3D spline(t, points);
    return spline.interpolate(tTarget);
}

int estimateOptimalSplinePoints(
    double timeSpan,
    double maxVelocity,
    double accuracyKm)
{
    // Rule of thumb: error ~ h⁴ for cubic splines
    // Need more points for higher velocity and tighter accuracy
    
    double accuracyAU = accuracyKm / 149597870.7;
    double h = std::pow(accuracyAU / maxVelocity, 0.25);
    
    int nPoints = static_cast<int>(std::ceil(timeSpan / h));
    
    // Clamp to reasonable range
    return std::max(10, std::min(nPoints, 1000));
}

} // namespace ioccultcalc
