// Test: Convert frame BEFORE converting to Cartesian
// Strategy: 
// 1. Convert equinoctial ECLM→EQUATORIAL (transform p,q,h,k)
// 2. Then apply standard elementsToState in equatorial frame

#include <iostream>
#include <iomanip>
#include <cmath>
#include "astdys_client.h"
#include "coordinates.h"

using namespace ioccultcalc;

// Convert equinoctial elements from ecliptic to equatorial frame
struct EquinoctialEquatorial {
    double a, h, k, p, q, lambda;
};

EquinoctialEquatorial eclipticToEquatorialElements(const AstDynEquinoctialElements& ecl) {
    // Obliquity of ecliptic (J2000)
    constexpr double eps = 23.4392911 * M_PI / 180.0;
    const double cos_eps = std::cos(eps);
    const double sin_eps = std::sin(eps);
    
    // Convert from Walker et al. (2000) formula:
    // Ecliptic: (h_ecl, k_ecl, p_ecl, q_ecl)
    // Equatorial: need to rotate the orbital plane
    
    // First, extract orbital elements from equinoctial
    double e = std::sqrt(ecl.h * ecl.h + ecl.k * ecl.k);
    double omega_plus_Omega = std::atan2(ecl.h, ecl.k);
    double tan_half_i = std::sqrt(ecl.p * ecl.p + ecl.q * ecl.q);
    double Omega_ecl = std::atan2(ecl.p, ecl.q);
    
    double i_ecl = 2.0 * std::atan(tan_half_i);
    double omega_ecl = omega_plus_Omega - Omega_ecl;
    
    // Mean anomaly in ecliptic frame
    double M_ecl = ecl.lambda - omega_plus_Omega;
    
    std::cout << "\nORBITAL ELEMENTS (ECLIPTIC):\n";
    std::cout << "  e = " << e << "\n";
    std::cout << "  i = " << (i_ecl * 180.0 / M_PI) << "°\n";
    std::cout << "  Ω = " << (Omega_ecl * 180.0 / M_PI) << "°\n";
    std::cout << "  ω = " << (omega_ecl * 180.0 / M_PI) << "°\n";
    std::cout << "  M = " << (M_ecl * 180.0 / M_PI) << "°\n";
    
    // Now rotate from ecliptic to equatorial plane
    // This is a rotation about X-axis by epsilon
    // Using Euler angle transformation for orbital elements
    
    // sin(i_eq) * sin(Omega_eq) = sin(i_ecl) * sin(Omega_ecl) * cos(eps) + cos(i_ecl) * sin(eps)
    // sin(i_eq) * cos(Omega_eq) = sin(i_ecl) * cos(Omega_ecl)
    // cos(i_eq) = cos(i_ecl) * cos(eps) - sin(i_ecl) * sin(Omega_ecl) * sin(eps)
    
    double sin_i_ecl = std::sin(i_ecl);
    double cos_i_ecl = std::cos(i_ecl);
    double sin_Om_ecl = std::sin(Omega_ecl);
    double cos_Om_ecl = std::cos(Omega_ecl);
    
    double sin_i_eq_sin_Om_eq = sin_i_ecl * sin_Om_ecl * cos_eps + cos_i_ecl * sin_eps;
    double sin_i_eq_cos_Om_eq = sin_i_ecl * cos_Om_ecl;
    double cos_i_eq = cos_i_ecl * cos_eps - sin_i_ecl * sin_Om_ecl * sin_eps;
    
    double sin_i_eq = std::sqrt(1.0 - cos_i_eq * cos_i_eq);
    double i_eq = std::acos(cos_i_eq);
    double Omega_eq = std::atan2(sin_i_eq_sin_Om_eq, sin_i_eq_cos_Om_eq);
    
    // omega doesn't change in this simple rotation (approximation)
    // For rigorous treatment we'd need full rotation matrix
    double omega_eq = omega_ecl;
    
    // M doesn't change
    double M_eq = M_ecl;
    
    std::cout << "\nORBITAL ELEMENTS (EQUATORIAL):\n";
    std::cout << "  e = " << e << "\n";
    std::cout << "  i = " << (i_eq * 180.0 / M_PI) << "°\n";
    std::cout << "  Ω = " << (Omega_eq * 180.0 / M_PI) << "°\n";
    std::cout << "  ω = " << omega_eq * 180.0 / M_PI << "°\n";
    std::cout << "  M = " << M_eq * 180.0 / M_PI << "°\n";
    
    // Convert back to equinoctial in equatorial frame
    EquinoctialEquatorial eq_eq;
    eq_eq.a = ecl.a;
    
    double omega_plus_Omega_eq = omega_eq + Omega_eq;
    eq_eq.h = e * std::sin(omega_plus_Omega_eq);
    eq_eq.k = e * std::cos(omega_plus_Omega_eq);
    
    double tan_half_i_eq = std::tan(i_eq / 2.0);
    eq_eq.p = tan_half_i_eq * std::sin(Omega_eq);
    eq_eq.q = tan_half_i_eq * std::cos(Omega_eq);
    
    eq_eq.lambda = M_eq + omega_plus_Omega_eq;
    
    std::cout << "\nEQUINOCTIAL (EQUATORIAL):\n";
    std::cout << "  h = " << eq_eq.h << "\n";
    std::cout << "  k = " << eq_eq.k << "\n";
    std::cout << "  p = " << eq_eq.p << "\n";
    std::cout << "  q = " << eq_eq.q << "\n";
    std::cout << "  λ = " << (eq_eq.lambda * 180.0 / M_PI) << "°\n";
    
    return eq_eq;
}

// Standard equinoctial to Cartesian (assumes already in target frame)
void equinoctialToCartesian(const EquinoctialEquatorial& eq, double epoch_mjd,
                            double& x, double& y, double& z) {
    const double mu = 0.0002959122082855911;  // GM_Sun in AU^3/day^2
    const double n = std::sqrt(mu / (eq.a * eq.a * eq.a));
    
    // Auxiliary variables
    const double alpha = std::sqrt(1.0 - eq.h*eq.h - eq.k*eq.k);
    const double s_sq = 1.0 + eq.p*eq.p + eq.q*eq.q;
    const double w = 1.0 + eq.h * std::cos(eq.lambda) + eq.k * std::sin(eq.lambda);
    const double r = eq.a * s_sq / w;
    
    // Position
    const double f1 = eq.a / s_sq * (std::cos(eq.lambda) + eq.h + eq.k * std::sin(eq.lambda) * (eq.h + std::cos(eq.lambda)) / w);
    const double f2 = eq.a / s_sq * (std::sin(eq.lambda) + eq.k - eq.h * std::cos(eq.lambda) * (eq.k + std::sin(eq.lambda)) / w);
    
    const double p_sq_plus_q_sq = eq.p*eq.p + eq.q*eq.q;
    const double one_plus_p_sq_plus_q_sq = 1.0 + p_sq_plus_q_sq;
    
    const double denom = one_plus_p_sq_plus_q_sq;
    x = (1.0 - eq.q*eq.q + eq.p*eq.p) * f1 / denom + 2.0 * eq.p * eq.q * f2 / denom;
    y = 2.0 * eq.p * eq.q * f1 / denom + (1.0 + eq.q*eq.q - eq.p*eq.p) * f2 / denom;
    z = -2.0 * eq.p * f1 / denom + 2.0 * eq.q * f2 / denom;
}

int main() {
    try {
        // (704) Interamnia from AstDyS .eq1 file
        AstDynEquinoctialElements ecl_elements;
        ecl_elements.a = 3.0562188547464153;
        ecl_elements.h = 0.038274930727971;
        ecl_elements.k = 0.150456045083743;
        ecl_elements.p = -0.149876046796030;
        ecl_elements.q = 0.026875405559966;
        ecl_elements.lambda = 198.4783346225068 * M_PI / 180.0;  // Convert to radians
        ecl_elements.epoch = 61000.0;  // MJD
        
        std::cout << "Testing frame conversion BEFORE Cartesian conversion\n";
        std::cout << "====================================================\n";
        std::cout << "\nINPUT ELEMENTS (ECLM J2000):\n";
        std::cout << std::fixed << std::setprecision(9);
        std::cout << "  a = " << ecl_elements.a << " AU\n";
        std::cout << "  h = " << ecl_elements.h << "\n";
        std::cout << "  k = " << ecl_elements.k << "\n";
        std::cout << "  p = " << ecl_elements.p << "\n";
        std::cout << "  q = " << ecl_elements.q << "\n";
        std::cout << "  λ = " << (ecl_elements.lambda * 180.0 / M_PI) << "°\n";
        std::cout << "  epoch = MJD " << ecl_elements.epoch << "\n";
        
        // Strategy: Convert frame first, then to Cartesian
        auto eq_equatorial = eclipticToEquatorialElements(ecl_elements);
        
        // Now convert to Cartesian (in equatorial frame)
        double x, y, z;
        equinoctialToCartesian(eq_equatorial, ecl_elements.epoch, x, y, z);
        
        std::cout << "\nCARTESIAN (EQUATORIAL FRAME):\n";
        std::cout << std::setprecision(6);
        std::cout << "  r = (" << x << ", " << y << ", " << z << ") AU\n";
        
        double r = std::sqrt(x*x + y*y + z*z);
        std::cout << "  |r| = " << r << " AU\n";
        
        // Convert to RA/Dec
        double dec = std::asin(z / r) * 180.0 / M_PI;
        double ra = std::atan2(y, x) * 180.0 / M_PI;
        if (ra < 0) ra += 360.0;
        
        std::cout << "\nEQUATORIAL COORDINATES (at epoch 2025-11-21):\n";
        std::cout << std::setprecision(2);
        std::cout << "  RA  = " << ra << "°\n";
        std::cout << "  Dec = " << dec << "°\n";
        
        std::cout << "\nEXPECTED (from JPL Horizons):\n";
        std::cout << "  RA  = 56.90°\n";
        std::cout << "  Dec = +19.77°\n";
        
        double ra_error = std::abs(ra - 56.90);
        double dec_error = std::abs(dec - 19.77);
        std::cout << "\nERROR:\n";
        std::cout << "  ΔRA  = " << ra_error << "°\n";
        std::cout << "  ΔDec = " << dec_error << "°\n";
        
        if (ra_error < 1.0 && dec_error < 1.0) {
            std::cout << "\n✓ SUCCESS! Position is correct.\n";
        } else {
            std::cout << "\n✗ STILL WRONG\n";
        }
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
