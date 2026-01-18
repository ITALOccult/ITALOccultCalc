/**
 * @file planetary_aberration.cpp
 * @brief Implementation of planetary aberration corrections
 */

#include "planetary_aberration.h"
#include <cmath>
#include <algorithm>
#include <numeric>

namespace ioccultcalc {

// Use constants from types.h
constexpr double ARCSEC_PER_RADIAN = 1.0 / ARCSEC_TO_RAD;

// ============================================================================
// Implementation class
// ============================================================================

class PlanetaryAberrationCalculator::Impl {
public:
    AberrationOrder order;
    double tolerance;
    int maxIterations;
    
    // Statistics
    int totalCalcs;
    int iterativeCalcs;
    std::vector<int> iterationCounts;
    std::vector<double> corrections;
    
    Impl() 
        : order(AberrationOrder::FIRST_ORDER)
        , tolerance(1e-6)
        , maxIterations(10)
        , totalCalcs(0)
        , iterativeCalcs(0)
    {}
    
    void recordCalculation(const AberrationCorrection& corr) {
        totalCalcs++;
        corrections.push_back(corr.magnitude);
        if (corr.iterations > 0) {
            iterativeCalcs++;
            iterationCounts.push_back(corr.iterations);
        }
    }
};

// ============================================================================
// Constructor / Destructor
// ============================================================================

PlanetaryAberrationCalculator::PlanetaryAberrationCalculator(
    AberrationOrder order,
    double tolerance)
    : pImpl(new Impl())
{
    pImpl->order = order;
    pImpl->tolerance = tolerance;
}

PlanetaryAberrationCalculator::~PlanetaryAberrationCalculator() {
    delete pImpl;
}

// ============================================================================
// Static utility methods
// ============================================================================

double PlanetaryAberrationCalculator::calculateLightTime(double distance) {
    // Light-time τ = d/c where d is in AU, c in AU/day
    return distance / C_LIGHT_AU_DAY;
}

double PlanetaryAberrationCalculator::calculateLightTime(const Vector3D& position) {
    return calculateLightTime(position.magnitude());
}

double PlanetaryAberrationCalculator::estimateCorrection(
    const Vector3D& velocity,
    double distance)
{
    // Δr ≈ v × τ = v × (d/c)
    double lightTime = calculateLightTime(distance);
    double correctionAU = velocity.magnitude() * lightTime;
    return correctionAU * AU_KM;
}

bool PlanetaryAberrationCalculator::isSignificant(
    double velocityKmS,
    double distance,
    double accuracyThreshold)
{
    // Convert velocity to AU/day
    double velocityAUPerDay = velocityKmS * DAY_SEC / AU_KM;
    
    // Estimate correction
    double estimatedCorrection = velocityAUPerDay * (distance / C_LIGHT_AU_DAY) * AU_KM;
    
    return estimatedCorrection > accuracyThreshold;
}

// ============================================================================
// Main correction methods
// ============================================================================

AberrationCorrection PlanetaryAberrationCalculator::correctAsteroid(
    const Vector3D& helioPos,
    const Vector3D& helioVel,
    const Vector3D& earthPos,
    const JulianDate& jd) const
{
    // Convert to geocentric
    Vector3D geoPos = helioPos - earthPos;
    Vector3D geoVel = helioVel;  // Earth velocity cancels in relative motion
    
    // Apply correction
    AberrationCorrection corr;
    if (pImpl->order == AberrationOrder::FIRST_ORDER) {
        corr = correctFirstOrder(geoPos, geoVel);
    } else {
        corr = correctIterative(geoPos, geoVel, jd);
    }
    
    // Record statistics
    pImpl->recordCalculation(corr);
    
    return corr;
}

AberrationCorrection PlanetaryAberrationCalculator::correctPlanet(
    PlanetaryBody planet,
    const Vector3D& earthPos,
    const JulianDate& jd) const
{
    // For Phase 2, we implement basic correction
    // Full planetary ephemeris integration would come in Phase 3
    
    // Placeholder: would need planetary positions from JPL or VSOP87
    // For now, return empty correction
    AberrationCorrection corr;
    corr.geometric = Vector3D(0, 0, 0);
    corr.apparent = Vector3D(0, 0, 0);
    corr.correction = Vector3D(0, 0, 0);
    
    return corr;
}

AberrationCorrection PlanetaryAberrationCalculator::correct(
    const Vector3D& geoPos,
    const Vector3D& geoVel,
    const JulianDate& jd) const
{
    AberrationCorrection corr;
    if (pImpl->order == AberrationOrder::FIRST_ORDER) {
        corr = correctFirstOrder(geoPos, geoVel);
    } else {
        corr = correctIterative(geoPos, geoVel, jd);
    }
    
    pImpl->recordCalculation(corr);
    return corr;
}

// ============================================================================
// Internal calculation methods
// ============================================================================

AberrationCorrection PlanetaryAberrationCalculator::correctFirstOrder(
    const Vector3D& geoPos,
    const Vector3D& geoVel) const
{
    AberrationCorrection corr;
    
    // Geometric position at observation time
    corr.geometric = geoPos;
    
    // Calculate light-time
    double distance = geoPos.magnitude();
    corr.lightTime = calculateLightTime(distance);
    
    // First-order aberration: apparent = geometric - v × τ
    // The object was at position (geometric - v×τ) when light was emitted
    Vector3D displacement = geoVel * corr.lightTime;
    
    // Apparent position (where we see it now)
    corr.apparent = geoPos - displacement;
    
    // Correction vector
    corr.correction = corr.apparent - corr.geometric;
    corr.magnitude = corr.correction.magnitude() * AU_KM;
    
    // Angular shift (in arcseconds)
    if (distance > 0) {
        corr.angularShift = (corr.correction.magnitude() / distance) * ARCSEC_PER_RADIAN;
    } else {
        corr.angularShift = 0.0;
    }
    
    corr.iterations = 0;  // First-order doesn't iterate
    
    return corr;
}

AberrationCorrection PlanetaryAberrationCalculator::correctIterative(
    const Vector3D& geoPos,
    const Vector3D& geoVel,
    const JulianDate& jd) const
{
    AberrationCorrection corr;
    corr.geometric = geoPos;
    
    // Iterative solution for high precision
    // Start with first-order approximation
    Vector3D apparentPos = geoPos;
    double prevLightTime = 0.0;
    
    int iter = 0;
    for (; iter < pImpl->maxIterations; ++iter) {
        // Calculate light-time for current apparent position
        double distance = apparentPos.magnitude();
        double lightTime = calculateLightTime(distance);
        
        // Check convergence
        if (std::abs(lightTime - prevLightTime) < pImpl->tolerance / (86400.0 * ARCSEC_PER_RADIAN)) {
            break;
        }
        
        // Update apparent position
        // Position when light was emitted = current - velocity × light-time
        apparentPos = geoPos - geoVel * lightTime;
        prevLightTime = lightTime;
    }
    
    corr.apparent = apparentPos;
    corr.lightTime = prevLightTime;
    corr.correction = corr.apparent - corr.geometric;
    corr.magnitude = corr.correction.magnitude() * AU_KM;
    
    double distance = geoPos.magnitude();
    if (distance > 0) {
        corr.angularShift = (corr.correction.magnitude() / distance) * ARCSEC_PER_RADIAN;
    } else {
        corr.angularShift = 0.0;
    }
    
    corr.iterations = iter + 1;
    
    return corr;
}

// ============================================================================
// Configuration methods
// ============================================================================

void PlanetaryAberrationCalculator::setOrder(AberrationOrder order) {
    pImpl->order = order;
}

AberrationOrder PlanetaryAberrationCalculator::getOrder() const {
    return pImpl->order;
}

void PlanetaryAberrationCalculator::setTolerance(double tolerance) {
    pImpl->tolerance = tolerance;
}

void PlanetaryAberrationCalculator::setMaxIterations(int maxIter) {
    pImpl->maxIterations = maxIter;
}

// ============================================================================
// Statistics
// ============================================================================

PlanetaryAberrationCalculator::Statistics 
PlanetaryAberrationCalculator::getStatistics() const
{
    Statistics stats;
    stats.totalCalculations = pImpl->totalCalcs;
    stats.iterativeCalculations = pImpl->iterativeCalcs;
    
    if (!pImpl->iterationCounts.empty()) {
        stats.avgIterations = std::accumulate(
            pImpl->iterationCounts.begin(),
            pImpl->iterationCounts.end(),
            0.0
        ) / pImpl->iterationCounts.size();
    } else {
        stats.avgIterations = 0.0;
    }
    
    if (!pImpl->corrections.empty()) {
        stats.avgCorrection = std::accumulate(
            pImpl->corrections.begin(),
            pImpl->corrections.end(),
            0.0
        ) / pImpl->corrections.size();
        
        stats.maxCorrection = *std::max_element(
            pImpl->corrections.begin(),
            pImpl->corrections.end()
        );
    } else {
        stats.avgCorrection = 0.0;
        stats.maxCorrection = 0.0;
    }
    
    return stats;
}

void PlanetaryAberrationCalculator::resetStatistics() {
    pImpl->totalCalcs = 0;
    pImpl->iterativeCalcs = 0;
    pImpl->iterationCounts.clear();
    pImpl->corrections.clear();
}

// ============================================================================
// Global convenience functions
// ============================================================================

AberrationCorrection correctAsteroidAberration(
    const Vector3D& helioPos,
    const Vector3D& helioVel,
    const Vector3D& earthPos,
    const JulianDate& jd)
{
    static PlanetaryAberrationCalculator calculator;
    return calculator.correctAsteroid(helioPos, helioVel, earthPos, jd);
}

double calculateLightTime(const Vector3D& position) {
    return PlanetaryAberrationCalculator::calculateLightTime(position);
}

bool isPlanetaryAberrationSignificant(
    double velocityKmS,
    double distanceAU,
    double accuracyKm)
{
    return PlanetaryAberrationCalculator::isSignificant(
        velocityKmS, distanceAU, accuracyKm
    );
}

} // namespace ioccultcalc
