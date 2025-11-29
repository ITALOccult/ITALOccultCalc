/**
 * @file nutation.h
 * @brief IAU 2000A/B nutation series implementation
 * 
 * This module implements the IAU 2000 nutation model for high-precision
 * celestial coordinate transformations. Nutation describes the short-period
 * oscillations in Earth's rotation axis due to gravitational torques from
 * the Moon and Sun.
 * 
 * Models implemented:
 * - IAU 2000B: 77 terms, ~1 milliarcsecond precision (recommended)
 * - IAU 2000A: 1365 terms, ~0.2 microarcsecond precision (future)
 * 
 * The IAU 2000B model provides excellent accuracy for occultation work
 * while being computationally efficient.
 * 
 * @author IOccultCalc Development Team
 * @date 2025-11-23
 * @reference IERS Conventions 2010, Chapter 5
 * @reference IAU SOFA library
 */

#ifndef NUTATION_H
#define NUTATION_H

#include <cmath>
#include <array>

namespace ioccultcalc {

/**
 * @brief Nutation model selection
 */
enum class NutationModel {
    IAU2000B,  ///< 77 terms, 1 mas precision (default)
    IAU2000A   ///< 1365 terms, 0.2 μas precision (future implementation)
};

/**
 * @brief Nutation angles (longitude and obliquity)
 */
struct NutationAngles {
    double dpsi;      ///< Nutation in longitude (radians)
    double deps;      ///< Nutation in obliquity (radians)
    
    NutationAngles() : dpsi(0.0), deps(0.0) {}
    NutationAngles(double psi, double eps) : dpsi(psi), deps(eps) {}
};

/**
 * @brief Fundamental arguments for nutation (Delaunay arguments)
 * 
 * These are used in the nutation series expansions.
 * Based on Simon et al. (1994) and Souchay et al. (1999)
 */
struct FundamentalArguments {
    double l;   ///< Mean anomaly of the Moon (radians)
    double lp;  ///< Mean anomaly of the Sun (radians)
    double F;   ///< L - Ω (Mean longitude of Moon minus longitude of ascending node)
    double D;   ///< Mean elongation of the Moon from the Sun (radians)
    double Om;  ///< Mean longitude of ascending node of Moon (radians)
    
    FundamentalArguments() : l(0), lp(0), F(0), D(0), Om(0) {}
};

/**
 * @class NutationCalculator
 * @brief Calculate nutation corrections for coordinate transformations
 */
class NutationCalculator {
public:
    /**
     * @brief Constructor
     */
    NutationCalculator() = default;
    
    /**
     * @brief Calculate nutation using IAU 2000B model (77 terms)
     * 
     * This is the recommended model for most applications including
     * occultation predictions. Provides ~1 milliarcsecond accuracy.
     * 
     * The model includes:
     * - Lunisolar effects (dominant terms)
     * - Planetary effects (Jupiter, Venus, etc.)
     * - Free core nutation (FCN)
     * 
     * @param jd_tt Julian Date in Terrestrial Time (TT)
     * @return NutationAngles containing Δψ (longitude) and Δε (obliquity)
     */
    NutationAngles calculate2000B(double jd_tt) const;
    
    /**
     * @brief Calculate nutation using simplified model
     * 
     * Fast approximation using only the largest terms (~20 terms)
     * Accuracy: ~0.1 arcsecond (suitable for quick calculations)
     * 
     * @param jd_tt Julian Date in Terrestrial Time (TT)
     * @return NutationAngles containing Δψ and Δε
     */
    NutationAngles calculateSimplified(double jd_tt) const;
    
    /**
     * @brief Calculate nutation using selected model
     * 
     * @param jd_tt Julian Date in Terrestrial Time
     * @param model Nutation model to use (default: IAU2000B)
     * @return NutationAngles
     */
    NutationAngles calculate(double jd_tt, 
                            NutationModel model = NutationModel::IAU2000B) const;
    
    /**
     * @brief Calculate mean obliquity of the ecliptic
     * 
     * Uses IAU 2000/2006 formula (Lieske et al. 1977, updated)
     * ε₀ = 84381.406" - 46.836769"T - 0.0001831"T² + ...
     * 
     * @param jd_tt Julian Date in TT
     * @return Mean obliquity in radians
     */
    static double meanObliquity(double jd_tt);
    
    /**
     * @brief Calculate true obliquity (mean + nutation)
     * 
     * ε = ε₀ + Δε
     * 
     * @param jd_tt Julian Date in TT
     * @return True obliquity in radians
     */
    double trueObliquity(double jd_tt) const;
    
    /**
     * @brief Calculate equation of the equinoxes (EE)
     * 
     * EE = Δψ cos(ε₀) + higher order terms
     * Used for converting between mean and apparent sidereal time
     * 
     * @param jd_tt Julian Date in TT
     * @return Equation of equinoxes in radians
     */
    double equationOfEquinoxes(double jd_tt) const;
    
    /**
     * @brief Get fundamental arguments (Delaunay variables)
     * 
     * Calculates the five fundamental lunisolar arguments used in
     * nutation series. Based on Simon et al. (1994).
     * 
     * @param jd_tt Julian Date in TT
     * @return FundamentalArguments structure
     */
    static FundamentalArguments getFundamentalArguments(double jd_tt);
    
    /**
     * @brief Create rotation matrix for nutation
     * 
     * Returns 3x3 rotation matrix that applies nutation correction
     * to transform from mean equator/equinox to true equator/equinox
     * 
     * @param jd_tt Julian Date in TT
     * @param matrix Output 3x3 rotation matrix
     */
    void nutationRotationMatrix(double jd_tt, double matrix[3][3]) const;
    
    /**
     * @brief Check if nutation is significant
     * 
     * Determines if nutation correction is needed for given precision
     * 
     * @param threshold_arcsec Precision threshold in arcseconds
     * @return true if nutation > threshold
     */
    static bool isSignificant(double threshold_arcsec = 1.0);

private:
    /**
     * @brief Normalize angle to [0, 2π)
     */
    static double normalizeAngle(double angle);
    
    /**
     * @brief Convert arcseconds to radians
     */
    static constexpr double arcsecToRad(double arcsec) {
        return arcsec * (M_PI / (180.0 * 3600.0));
    }
    
    /**
     * @brief Nutation series term
     */
    struct NutationTerm {
        int nl, nlp, nF, nD, nOm;  // Multipliers for fundamental arguments
        double ps, pst;             // sin coefficients (constant, T)
        double pc, pct;             // cos coefficients  (constant, T)
        double es, est;             // sin coefficients (constant, T)
        double ec, ect;             // cos coefficients (constant, T)
    };
    
    /**
     * @brief Get IAU 2000B nutation series (77 terms)
     */
    static const std::array<NutationTerm, 77>& getNutation2000BTerms();
};

/**
 * @brief Global convenience function for nutation calculation
 * 
 * Quick calculation using IAU 2000B model
 * 
 * @param jd_tt Julian Date in TT
 * @return NutationAngles
 */
NutationAngles calculateNutation(double jd_tt);

/**
 * @brief Calculate mean obliquity (convenience function)
 * 
 * @param jd_tt Julian Date in TT
 * @return Mean obliquity in radians
 */
double calculateMeanObliquity(double jd_tt);

/**
 * @brief Calculate true obliquity (convenience function)
 * 
 * @param jd_tt Julian Date in TT
 * @return True obliquity in radians
 */
double calculateTrueObliquity(double jd_tt);

} // namespace ioccultcalc

#endif // NUTATION_H
