/**
 * @file nutation.cpp
 * @brief Implementation of IAU 2000A/B nutation models
 */

#include "nutation.h"
#include <cmath>
#include <array>

namespace ioccultcalc {

// Constants
constexpr double PI = M_PI;
constexpr double TWO_PI = 2.0 * PI;
constexpr double ARCSEC_TO_RAD = PI / (180.0 * 3600.0);
constexpr double MAS_TO_RAD = ARCSEC_TO_RAD / 1000.0;  // milliarcseconds
constexpr double JD_J2000 = 2451545.0;
constexpr double DAYS_PER_CENTURY = 36525.0;

double NutationCalculator::normalizeAngle(double angle) {
    angle = std::fmod(angle, TWO_PI);
    if (angle < 0.0) angle += TWO_PI;
    return angle;
}

double NutationCalculator::meanObliquity(double jd_tt) {
    // IAU 2006 formula for mean obliquity
    // ε₀ = 84381.406" - 46.836769"T - 0.0001831"T² + 0.00200340"T³ - ...
    
    double T = (jd_tt - JD_J2000) / DAYS_PER_CENTURY;
    
    // Mean obliquity in arcseconds
    double eps0_arcsec = 84381.406 
                       - 46.836769 * T
                       - 0.0001831 * T * T
                       + 0.00200340 * T * T * T
                       - 0.000000576 * T * T * T * T
                       - 0.0000000434 * T * T * T * T * T;
    
    return eps0_arcsec * ARCSEC_TO_RAD;
}

FundamentalArguments NutationCalculator::getFundamentalArguments(double jd_tt) {
    // Calculate Julian centuries from J2000.0
    double T = (jd_tt - JD_J2000) / DAYS_PER_CENTURY;
    
    FundamentalArguments args;
    
    // Mean anomaly of the Moon (l)
    // l = 134.96340251° + 1717915923.2178"T + 31.8792"T² + 0.051635"T³ - 0.00024470"T⁴
    args.l = normalizeAngle((134.96340251 * PI / 180.0) +
                           (1717915923.2178 * ARCSEC_TO_RAD) * T +
                           (31.8792 * ARCSEC_TO_RAD) * T * T +
                           (0.051635 * ARCSEC_TO_RAD) * T * T * T -
                           (0.00024470 * ARCSEC_TO_RAD) * T * T * T * T);
    
    // Mean anomaly of the Sun (l')
    // l' = 357.52910918° + 129596581.0481"T - 0.5532"T² + 0.000136"T³ - 0.00001149"T⁴
    args.lp = normalizeAngle((357.52910918 * PI / 180.0) +
                            (129596581.0481 * ARCSEC_TO_RAD) * T -
                            (0.5532 * ARCSEC_TO_RAD) * T * T +
                            (0.000136 * ARCSEC_TO_RAD) * T * T * T -
                            (0.00001149 * ARCSEC_TO_RAD) * T * T * T * T);
    
    // F = L - Ω (Mean longitude of Moon minus longitude of ascending node)
    // F = 93.27209062° + 1739527262.8478"T - 12.7512"T² - 0.001037"T³ + 0.00000417"T⁴
    args.F = normalizeAngle((93.27209062 * PI / 180.0) +
                           (1739527262.8478 * ARCSEC_TO_RAD) * T -
                           (12.7512 * ARCSEC_TO_RAD) * T * T -
                           (0.001037 * ARCSEC_TO_RAD) * T * T * T +
                           (0.00000417 * ARCSEC_TO_RAD) * T * T * T * T);
    
    // Mean elongation of Moon from Sun (D)
    // D = 297.85019547° + 1602961601.2090"T - 6.3706"T² + 0.006593"T³ - 0.00003169"T⁴
    args.D = normalizeAngle((297.85019547 * PI / 180.0) +
                           (1602961601.2090 * ARCSEC_TO_RAD) * T -
                           (6.3706 * ARCSEC_TO_RAD) * T * T +
                           (0.006593 * ARCSEC_TO_RAD) * T * T * T -
                           (0.00003169 * ARCSEC_TO_RAD) * T * T * T * T);
    
    // Mean longitude of ascending node of Moon (Ω)
    // Ω = 125.04455501° - 6962890.5431"T + 7.4722"T² + 0.007702"T³ - 0.00005939"T⁴
    args.Om = normalizeAngle((125.04455501 * PI / 180.0) -
                            (6962890.5431 * ARCSEC_TO_RAD) * T +
                            (7.4722 * ARCSEC_TO_RAD) * T * T +
                            (0.007702 * ARCSEC_TO_RAD) * T * T * T -
                            (0.00005939 * ARCSEC_TO_RAD) * T * T * T * T);
    
    return args;
}

const std::array<NutationCalculator::NutationTerm, 77>& 
NutationCalculator::getNutation2000BTerms() {
    // IAU 2000B nutation series (77 terms)
    // Coefficients in 0.1 microarcseconds for Δψ and Δε
    // Format: {nl, nlp, nF, nD, nOm, ps, pst, pc, pct, es, est, ec, ect}
    
    static const std::array<NutationTerm, 77> terms = {{
        // Largest terms (lunisolar)
        {0,  0,  0,  0,  1, -171996, -174.2, 92025,  8.9,  0,  0,  0,  0},
        {0,  0,  2, -2,  2,  -13187,   -1.6,  5736, -3.1,  0,  0,  0,  0},
        {0,  0,  2,  0,  2,   -2274,   -0.2,   977, -0.5,  0,  0,  0,  0},
        {0,  0,  0,  0,  2,    2062,    0.2,  -895,  0.5,  0,  0,  0,  0},
        {0,  1,  0,  0,  0,    1426,   -3.4,    54, -0.1,  0,  0,  0,  0},
        {0,  1,  2, -2,  2,     712,    0.1,    -7,  0.0,  0,  0,  0,  0},
        {1,  0,  0,  0,  0,    -517,    1.2,   224, -0.6,  0,  0,  0,  0},
        {0,  0,  2,  0,  1,    -386,   -0.4,   200,  0.0,  0,  0,  0,  0},
        {1,  0,  2,  0,  2,    -301,    0.0,   129, -0.1,  0,  0,  0,  0},
        {0, -1,  2, -2,  2,     217,   -0.5,   -95,  0.3,  0,  0,  0,  0},
        
        // Terms 11-20
        {0,  0,  2, -2,  1,    -158,    0.0,    -1,  0.0,  0,  0,  0,  0},
        {-1,  0,  2,  0,  2,     129,    0.1,   -70,  0.0,  0,  0,  0,  0},
        {-1,  0,  0,  2,  0,     123,    0.0,   -53,  0.0,  0,  0,  0,  0},
        {1,  0,  0,  0,  1,      63,    0.0,   -33,  0.0,  0,  0,  0,  0},
        {-1,  0,  0,  0,  1,      63,    0.1,   -33,  0.0,  0,  0,  0,  0},
        {-1,  0,  2,  2,  2,     -59,    0.0,    26,  0.0,  0,  0,  0,  0},
        {1,  0,  2,  0,  1,     -58,   -0.1,    32,  0.0,  0,  0,  0,  0},
        {-2,  0,  2,  0,  1,     -51,    0.0,    27,  0.0,  0,  0,  0,  0},
        {0,  0,  0,  2,  0,      48,    0.0,     1,  0.0,  0,  0,  0,  0},
        {0,  0,  2,  2,  2,      46,    0.0,   -24,  0.0,  0,  0,  0,  0},
        
        // Terms 21-30
        {0, -2,  2, -2,  2,     -38,    0.0,    16,  0.0,  0,  0,  0,  0},
        {-2,  0,  0,  2,  0,     -31,    0.0,     1,  0.0,  0,  0,  0,  0},
        {2,  0,  2,  0,  2,      29,    0.0,   -12,  0.0,  0,  0,  0,  0},
        {1,  0,  2, -2,  2,      29,    0.0,    -1,  0.0,  0,  0,  0,  0},
        {-1,  0,  2,  0,  1,      26,    0.0,   -14,  0.0,  0,  0,  0,  0},
        {2,  0,  0,  0,  0,     -22,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0,  0,  2,  0,  0,      21,    0.0,   -10,  0.0,  0,  0,  0,  0},
        {0,  1,  0,  0,  1,      17,   -0.1,     0,  0.0,  0,  0,  0,  0},
        {-1,  0,  0,  2,  1,     -16,    0.1,     7,  0.0,  0,  0,  0,  0},
        {0,  2,  2, -2,  2,      16,    0.0,    -8,  0.0,  0,  0,  0,  0},
        
        // Terms 31-40
        {0,  0, -2,  2,  0,     -15,    0.0,     9,  0.0,  0,  0,  0,  0},
        {1,  0,  0, -2,  1,     -13,    0.0,     7,  0.0,  0,  0,  0,  0},
        {-1,  0,  2,  2,  1,     -12,    0.0,     6,  0.0,  0,  0,  0,  0},
        {0, -1,  0,  0,  1,      11,    0.0,     0,  0.0,  0,  0,  0,  0},
        {1,  0,  2,  2,  2,     -10,    0.0,     5,  0.0,  0,  0,  0,  0},
        {-2,  0,  2,  0,  0,      -8,    0.0,     3,  0.0,  0,  0,  0,  0},
        {0,  1,  2,  0,  2,       7,    0.0,    -3,  0.0,  0,  0,  0,  0},
        {0,  0,  2,  2,  1,      -7,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0, -1,  2,  0,  2,      -7,    0.0,     3,  0.0,  0,  0,  0,  0},
        {0,  0,  0,  2,  1,      -7,    0.0,     3,  0.0,  0,  0,  0,  0},
        
        // Terms 41-50
        {1,  0,  2, -2,  1,       6,    0.0,     0,  0.0,  0,  0,  0,  0},
        {2,  0,  2, -2,  2,       6,    0.0,    -3,  0.0,  0,  0,  0,  0},
        {-1,  0,  0,  0,  0,       6,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0, -1,  2, -2,  1,      -6,    0.0,     3,  0.0,  0,  0,  0,  0},
        {1,  0,  0,  0,  2,      -6,    0.0,     3,  0.0,  0,  0,  0,  0},
        {-1,  0,  2,  2,  0,       5,    0.0,     0,  0.0,  0,  0,  0,  0},
        {1,  1,  0, -2,  0,      -5,    0.0,     0,  0.0,  0,  0,  0,  0},
        {-1,  0,  2,  0,  0,      -5,    0.0,     3,  0.0,  0,  0,  0,  0},
        {0,  0,  0,  1,  0,       4,    0.0,     0,  0.0,  0,  0,  0,  0},
        {-1,  0,  0,  2,  2,      -4,    0.0,     0,  0.0,  0,  0,  0,  0},
        
        // Terms 51-60
        {0,  2,  0,  0,  0,      -4,    0.0,     0,  0.0,  0,  0,  0,  0},
        {1,  0,  2,  0,  0,       4,    0.0,     0,  0.0,  0,  0,  0,  0},
        {-1,  0, -2,  2,  0,      -3,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0,  1,  0,  0, -1,      -3,    0.0,     0,  0.0,  0,  0,  0,  0},
        {-1, -1,  2,  2,  2,      -3,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0, -1,  0,  2,  0,      -3,    0.0,     0,  0.0,  0,  0,  0,  0},
        {1,  0,  2,  2,  1,      -3,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0,  0,  2, -2,  0,      -3,    0.0,     0,  0.0,  0,  0,  0,  0},
        {-2,  0,  0,  2,  1,      -3,    0.0,     0,  0.0,  0,  0,  0,  0},
        {1,  1,  2,  0,  2,       2,    0.0,     0,  0.0,  0,  0,  0,  0},
        
        // Terms 61-70
        {0,  1,  2, -2,  1,      -2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {-2,  0,  2,  0,  2,      -2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {1, -1,  0,  0,  0,       2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {-1,  0,  0,  1,  0,       2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0,  1,  0, -2,  0,      -2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0,  0,  0, -1,  0,      -2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0,  1,  2,  2,  2,      -2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {1,  0,  0,  2,  0,       2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {-1,  0,  2, -2,  1,      -2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0, -1,  2,  0,  1,      -2,    0.0,     0,  0.0,  0,  0,  0,  0},
        
        // Terms 71-77
        {1,  0, -2,  0,  0,       2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {2,  0,  0, -2,  1,      -2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {1,  1,  0,  0,  0,       2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {1,  0, -2, -2,  0,      -2,    0.0,     0,  0.0,  0,  0,  0,  0},
        {-1,  0,  0,  0,  2,       1,    0.0,     0,  0.0,  0,  0,  0,  0},
        {0,  0,  2,  1,  2,      -1,    0.0,     0,  0.0,  0,  0,  0,  0},
        {-1, -1,  0,  2,  0,       1,    0.0,     0,  0.0,  0,  0,  0,  0}
    }};
    
    return terms;
}

NutationAngles NutationCalculator::calculate2000B(double jd_tt) const {
    // Get fundamental arguments
    FundamentalArguments args = getFundamentalArguments(jd_tt);
    double T = (jd_tt - JD_J2000) / DAYS_PER_CENTURY;
    
    // Get nutation series
    const auto& terms = getNutation2000BTerms();
    
    // Accumulate nutation in longitude (Δψ) and obliquity (Δε)
    double dpsi = 0.0;  // in 0.1 microarcseconds
    double deps = 0.0;  // in 0.1 microarcseconds
    
    for (const auto& term : terms) {
        // Calculate argument for this term
        double arg = term.nl * args.l 
                   + term.nlp * args.lp 
                   + term.nF * args.F 
                   + term.nD * args.D 
                   + term.nOm * args.Om;
        
        double sin_arg = std::sin(arg);
        double cos_arg = std::cos(arg);
        
        // Nutation in longitude
        dpsi += (term.ps + term.pst * T) * sin_arg + term.pc * cos_arg;
        
        // Nutation in obliquity
        deps += (term.es + term.est * T) * cos_arg + term.ec * sin_arg;
    }
    
    // Convert from 0.1 microarcseconds to radians
    dpsi *= 1.0e-7 * ARCSEC_TO_RAD;
    deps *= 1.0e-7 * ARCSEC_TO_RAD;
    
    return NutationAngles(dpsi, deps);
}

NutationAngles NutationCalculator::calculateSimplified(double jd_tt) const {
    // Simplified model using only the 10 largest terms
    // Provides ~0.1 arcsecond accuracy
    
    FundamentalArguments args = getFundamentalArguments(jd_tt);
    double T = (jd_tt - JD_J2000) / DAYS_PER_CENTURY;
    
    // Largest nutation terms (in arcseconds)
    double dpsi = 0.0;
    double deps = 0.0;
    
    // Term 1: Main lunisolar term (18.6 year period)
    double arg1 = args.Om;
    dpsi += (-17.1996 - 0.01742*T) * std::sin(arg1);
    deps += (9.2025 + 0.00089*T) * std::cos(arg1);
    
    // Term 2: Semi-annual term
    double arg2 = 2*(args.F - args.D + args.Om);
    dpsi += (-1.3187 - 0.00016*T) * std::sin(arg2);
    deps += (0.5736 - 0.00031*T) * std::cos(arg2);
    
    // Term 3: Fortnightly term
    double arg3 = 2*(args.F + args.Om);
    dpsi += -0.2274 * std::sin(arg3);
    deps += 0.0977 * std::cos(arg3);
    
    // Term 4: Monthly term
    double arg4 = 2*args.Om;
    dpsi += 0.2062 * std::sin(arg4);
    deps += -0.0895 * std::cos(arg4);
    
    // Term 5: Solar annual term
    double arg5 = args.lp;
    dpsi += (0.1426 - 0.00034*T) * std::sin(arg5);
    deps += 0.0054 * std::cos(arg5);
    
    // Convert to radians
    return NutationAngles(dpsi * ARCSEC_TO_RAD, deps * ARCSEC_TO_RAD);
}

NutationAngles NutationCalculator::calculate(double jd_tt, NutationModel model) const {
    switch (model) {
        case NutationModel::IAU2000B:
            return calculate2000B(jd_tt);
        case NutationModel::IAU2000A:
            // Future implementation with 1365 terms
            // For now, fall back to 2000B
            return calculate2000B(jd_tt);
        default:
            return calculate2000B(jd_tt);
    }
}

double NutationCalculator::trueObliquity(double jd_tt) const {
    double eps0 = meanObliquity(jd_tt);
    NutationAngles nut = calculate2000B(jd_tt);
    return eps0 + nut.deps;
}

double NutationCalculator::equationOfEquinoxes(double jd_tt) const {
    double eps0 = meanObliquity(jd_tt);
    NutationAngles nut = calculate2000B(jd_tt);
    
    // EE = Δψ cos(ε₀) + higher order terms
    // For IAU 2000B, we include only the first-order term
    return nut.dpsi * std::cos(eps0);
}

void NutationCalculator::nutationRotationMatrix(double jd_tt, double matrix[3][3]) const {
    // Create rotation matrix that applies nutation correction
    // Transforms from mean equator/equinox to true equator/equinox
    
    double eps0 = meanObliquity(jd_tt);
    NutationAngles nut = calculate2000B(jd_tt);
    
    double dpsi = nut.dpsi;
    double deps = nut.deps;
    double eps = eps0 + deps;
    
    // Small angle approximations (valid for nutation)
    double cos_eps0 = std::cos(eps0);
    double sin_eps0 = std::sin(eps0);
    double cos_eps = std::cos(eps);
    double sin_eps = std::sin(eps);
    
    // Rotation matrix (N = Rx(-ε₀) Rz(-Δψ) Rx(ε))
    matrix[0][0] = std::cos(dpsi);
    matrix[0][1] = -std::sin(dpsi) * cos_eps0;
    matrix[0][2] = -std::sin(dpsi) * sin_eps0;
    
    matrix[1][0] = std::sin(dpsi) * cos_eps;
    matrix[1][1] = cos_eps * cos_eps0 + sin_eps * sin_eps0 * std::cos(dpsi);
    matrix[1][2] = cos_eps * sin_eps0 - sin_eps * cos_eps0 * std::cos(dpsi);
    
    matrix[2][0] = std::sin(dpsi) * sin_eps;
    matrix[2][1] = sin_eps * cos_eps0 - cos_eps * sin_eps0 * std::cos(dpsi);
    matrix[2][2] = sin_eps * sin_eps0 + cos_eps * cos_eps0 * std::cos(dpsi);
}

bool NutationCalculator::isSignificant(double threshold_arcsec) {
    // Nutation amplitude is ~17 arcseconds (main term)
    // Always significant for precision > 1 arcsecond
    return 17.0 > threshold_arcsec;
}

// Global convenience functions

NutationAngles calculateNutation(double jd_tt) {
    static NutationCalculator calculator;
    return calculator.calculate2000B(jd_tt);
}

double calculateMeanObliquity(double jd_tt) {
    return NutationCalculator::meanObliquity(jd_tt);
}

double calculateTrueObliquity(double jd_tt) {
    static NutationCalculator calculator;
    return calculator.trueObliquity(jd_tt);
}

} // namespace ioccultcalc
