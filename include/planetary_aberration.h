/**
 * @file planetary_aberration.h
 * @brief Planetary aberration corrections for Solar System bodies
 * 
 * Extends stellar aberration to account for finite light-time from planets
 * and asteroids. This is critical for close approaches where the delay
 * between asteroid position calculation and observation can be significant.
 * 
 * Theory:
 * - Planetary aberration is the finite speed of light correction
 * - For an asteroid at distance d, light takes τ = d/c to reach observer
 * - During this time, the asteroid moves: Δr = v × τ
 * - Geometric position ≠ apparent position
 * 
 * Impact on occultations:
 * - At 1 AU and v=20 km/s: ~100 km shift (significant!)
 * - At 2.5 AU and v=20 km/s: ~250 km shift
 * - Critical for NEAs during close approaches
 * - Important for precise timing predictions
 * 
 * References:
 * - Explanatory Supplement to the Astronomical Almanac (2014), Chapter 7
 * - JPL Horizons documentation on aberration corrections
 * - Urban & Seidelmann (2013), "Explanatory Supplement", §7.4
 * 
 * @author IOccultCalc Phase 2 Implementation
 * @date 2025
 */

#ifndef IOCCULTCALC_PLANETARY_ABERRATION_H
#define IOCCULTCALC_PLANETARY_ABERRATION_H

#include "ioccultcalc/types.h"
#include <vector>
#include <string>

namespace ioccultcalc {

/**
 * @enum AberrationOrder
 * @brief Order of aberration correction
 */
enum class AberrationOrder {
    FIRST_ORDER,    ///< Linear approximation (sufficient for most cases)
    ITERATIVE       ///< Iterative solution (high precision, slower)
};

/**
 * @enum PlanetaryBody
 * @brief Solar System bodies for which aberration can be computed
 */
enum class PlanetaryBody {
    MERCURY = 1,
    VENUS = 2,
    EARTH = 3,
    MARS = 4,
    JUPITER = 5,
    SATURN = 6,
    URANUS = 7,
    NEPTUNE = 8,
    MOON = 10,
    ASTEROID = 99  ///< Generic asteroid/small body
};

/**
 * @struct AberrationCorrection
 * @brief Result of aberration calculation
 */
struct AberrationCorrection {
    Vector3D geometric;     ///< Geometric (true) position at time t
    Vector3D apparent;      ///< Apparent position as seen by observer
    Vector3D correction;    ///< Correction vector (apparent - geometric)
    double lightTime;       ///< Light-time in days
    double magnitude;       ///< Magnitude of correction in km
    double angularShift;    ///< Angular shift in arcseconds
    int iterations;         ///< Number of iterations (if iterative)
    
    AberrationCorrection() 
        : lightTime(0.0), magnitude(0.0), angularShift(0.0), iterations(0) {}
};

/**
 * @class PlanetaryAberrationCalculator
 * @brief Calculates planetary aberration for Solar System bodies
 * 
 * This class provides methods to compute the apparent position of a planet
 * or asteroid as seen by an observer, accounting for the finite speed of light.
 * 
 * Usage:
 * ```cpp
 * PlanetaryAberrationCalculator calc;
 * 
 * // For an asteroid
 * Vector3D asteroidPos = ...; // heliocentric position at observation time
 * Vector3D asteroidVel = ...; // heliocentric velocity (AU/day)
 * Vector3D earthPos = ...;    // Earth position at observation time
 * 
 * AberrationCorrection corr = calc.correctAsteroid(
 *     asteroidPos, asteroidVel, earthPos, jd
 * );
 * 
 * Vector3D apparentPos = corr.apparent;
 * std::cout << "Light-time: " << corr.lightTime * 86400.0 << " seconds\n";
 * std::cout << "Correction: " << corr.magnitude << " km\n";
 * ```
 */
class PlanetaryAberrationCalculator {
public:
    /**
     * @brief Constructor
     * @param order Order of correction (default: first-order)
     * @param tolerance Convergence tolerance for iterative method (arcsec)
     */
    explicit PlanetaryAberrationCalculator(
        AberrationOrder order = AberrationOrder::FIRST_ORDER,
        double tolerance = 1e-6
    );
    
    ~PlanetaryAberrationCalculator();
    
    // ========================================================================
    // Main correction methods
    // ========================================================================
    
    /**
     * @brief Correct asteroid position for planetary aberration
     * 
     * This is the most common use case for occultation predictions.
     * 
     * @param helioPos Asteroid heliocentric position at observation time (AU)
     * @param helioVel Asteroid heliocentric velocity (AU/day)
     * @param earthPos Earth heliocentric position at observation time (AU)
     * @param jd Julian Date of observation
     * @return Aberration correction with geometric and apparent positions
     */
    AberrationCorrection correctAsteroid(
        const Vector3D& helioPos,
        const Vector3D& helioVel,
        const Vector3D& earthPos,
        const JulianDate& jd
    ) const;
    
    /**
     * @brief Correct planet position for planetary aberration
     * 
     * @param planet Planet identifier
     * @param earthPos Earth heliocentric position (AU)
     * @param jd Julian Date
     * @return Aberration correction
     */
    AberrationCorrection correctPlanet(
        PlanetaryBody planet,
        const Vector3D& earthPos,
        const JulianDate& jd
    ) const;
    
    /**
     * @brief Generic correction given position and velocity
     * 
     * @param geoPos Geocentric position at observation time (AU)
     * @param geoVel Geocentric velocity (AU/day)
     * @param jd Julian Date
     * @return Aberration correction
     */
    AberrationCorrection correct(
        const Vector3D& geoPos,
        const Vector3D& geoVel,
        const JulianDate& jd
    ) const;
    
    // ========================================================================
    // Utility methods
    // ========================================================================
    
    /**
     * @brief Calculate light-time for a given distance
     * @param distance Distance in AU
     * @return Light-time in days
     */
    static double calculateLightTime(double distance);
    
    /**
     * @brief Calculate light-time for a position vector
     * @param position Position vector in AU
     * @return Light-time in days
     */
    static double calculateLightTime(const Vector3D& position);
    
    /**
     * @brief Estimate magnitude of aberration correction
     * 
     * Quick estimate without full calculation:
     * Δr ≈ v × (d/c)
     * 
     * @param velocity Object velocity (AU/day)
     * @param distance Distance to object (AU)
     * @return Estimated correction magnitude in km
     */
    static double estimateCorrection(const Vector3D& velocity, double distance);
    
    /**
     * @brief Check if aberration is significant for given accuracy
     * 
     * @param velocity Object velocity (km/s)
     * @param distance Distance (AU)
     * @param accuracyThreshold Required accuracy in km
     * @return true if aberration correction is needed
     */
    static bool isSignificant(
        double velocity,
        double distance,
        double accuracyThreshold = 10.0
    );
    
    // ========================================================================
    // Configuration
    // ========================================================================
    
    /**
     * @brief Set order of aberration correction
     */
    void setOrder(AberrationOrder order);
    
    /**
     * @brief Get current order
     */
    AberrationOrder getOrder() const;
    
    /**
     * @brief Set convergence tolerance for iterative method
     * @param tolerance Tolerance in arcseconds
     */
    void setTolerance(double tolerance);
    
    /**
     * @brief Set maximum iterations for iterative method
     * @param maxIter Maximum number of iterations
     */
    void setMaxIterations(int maxIter);
    
    /**
     * @brief Get statistics from last calculation
     */
    struct Statistics {
        int totalCalculations;
        int iterativeCalculations;
        double avgIterations;
        double maxCorrection;
        double avgCorrection;
    };
    
    Statistics getStatistics() const;
    void resetStatistics();
    
    // Unified constants are now in ioccultcalc/types.h
    
private:
    class Impl;
    Impl* pImpl;
    
    // Internal calculation methods
    AberrationCorrection correctFirstOrder(
        const Vector3D& geoPos,
        const Vector3D& geoVel
    ) const;
    
    AberrationCorrection correctIterative(
        const Vector3D& geoPos,
        const Vector3D& geoVel,
        const JulianDate& jd
    ) const;
};

// ============================================================================
// Global convenience functions
// ============================================================================

/**
 * @brief Quick aberration correction for asteroid (first-order)
 * 
 * Convenience function for common use case
 */
AberrationCorrection correctAsteroidAberration(
    const Vector3D& helioPos,
    const Vector3D& helioVel,
    const Vector3D& earthPos,
    const JulianDate& jd
);

/**
 * @brief Calculate light-time distance for observer-object separation
 */
double calculateLightTime(const Vector3D& position);

/**
 * @brief Check if planetary aberration is significant
 */
bool isPlanetaryAberrationSignificant(
    double velocityKmS,
    double distanceAU,
    double accuracyKm = 10.0
);

} // namespace ioccultcalc

#endif // IOCCULTCALC_PLANETARY_ABERRATION_H
