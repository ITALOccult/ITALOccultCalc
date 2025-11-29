/**
 * @file test_keplerian_direct.cpp
 * @brief Test conversione diretta Keplerian→Cartesian
 */

#include <iostream>
#include <iomanip>
#include <cmath>
#include <ioccultcalc/astdys_client.h>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

// Conversione diretta Keplerian→Cartesian
void keplerianToCartesian(double a, double e, double i, double Omega, double omega, double M,
                          double& x, double& y, double& z) {
    // Risolvi equazione di Keplero
    double E = M;
    for (int iter = 0; iter < 10; iter++) {
        E = M + e * sin(E);
    }
    
    // Anomalia vera
    double nu = 2.0 * atan2(sqrt(1.0 + e) * sin(E/2.0), sqrt(1.0 - e) * cos(E/2.0));
    
    // Raggio
    double r = a * (1.0 - e * cos(E));
    
    // Coordinate nel piano orbitale
    double x_orb = r * cos(nu);
    double y_orb = r * sin(nu);
    double z_orb = 0.0;
    
    // Rotazione verso frame inerziale (formula standard)
    double cos_O = cos(Omega);
    double sin_O = sin(Omega);
    double cos_w = cos(omega);
    double sin_w = sin(omega);
    double cos_i = cos(i);
    double sin_i = sin(i);
    
    x = (cos_O * cos_w - sin_O * sin_w * cos_i) * x_orb + 
        (-cos_O * sin_w - sin_O * cos_w * cos_i) * y_orb;
        
    y = (sin_O * cos_w + cos_O * sin_w * cos_i) * x_orb + 
        (-sin_O * sin_w + cos_O * cos_w * cos_i) * y_orb;
        
    z = (sin_w * sin_i) * x_orb + (cos_w * sin_i) * y_orb;
}

int main() {
    std::cout << "\n=== DIRECT KEPLERIAN CONVERSION ===\n\n";
    
    try {
        AstDysClient client;
        auto kep = client.getRecentElements("704");
        
        std::cout << "Epoch: " << TimeUtils::jdToISO(kep.epoch) << "\n";
        std::cout << "Elements (ECLM J2000):\n";
        std::cout << "  a = " << std::fixed << std::setprecision(6) << kep.a << " AU\n";
        std::cout << "  e = " << kep.e << "\n";
        std::cout << "  i = " << (kep.i * 180.0/M_PI) << "°\n";
        std::cout << "  Ω = " << (kep.Omega * 180.0/M_PI) << "°\n";
        std::cout << "  ω = " << (kep.omega * 180.0/M_PI) << "°\n";
        std::cout << "  M = " << (kep.M * 180.0/M_PI) << "°\n\n";
        
        // Conversione diretta a cartesiano ECLITTICO
        double x_ecl, y_ecl, z_ecl;
        keplerianToCartesian(kep.a, kep.e, kep.i, kep.Omega, kep.omega, kep.M,
                            x_ecl, y_ecl, z_ecl);
        
        std::cout << "Cartesian ECLIPTIC J2000:\n";
        std::cout << "  x = " << x_ecl << " AU\n";
        std::cout << "  y = " << y_ecl << " AU\n";
        std::cout << "  z = " << z_ecl << " AU\n";
        std::cout << "  r = " << sqrt(x_ecl*x_ecl + y_ecl*y_ecl + z_ecl*z_ecl) << " AU\n\n";
        
        // Conversione a EQUATORIALE
        constexpr double eps = 23.4392911 * M_PI / 180.0;
        double cos_eps = cos(eps);
        double sin_eps = sin(eps);
        
        double x_eq = x_ecl;
        double y_eq = y_ecl * cos_eps - z_ecl * sin_eps;
        double z_eq = y_ecl * sin_eps + z_ecl * cos_eps;
        
        std::cout << "Cartesian EQUATORIAL J2000:\n";
        std::cout << "  x = " << x_eq << " AU\n";
        std::cout << "  y = " << y_eq << " AU\n";
        std::cout << "  z = " << z_eq << " AU\n\n";
        
        // Coordinate equatoriali
        double r = sqrt(x_eq*x_eq + y_eq*y_eq + z_eq*z_eq);
        double ra = atan2(y_eq, x_eq) * 180.0 / M_PI;
        if (ra < 0) ra += 360.0;
        double dec = asin(z_eq / r) * 180.0 / M_PI;
        
        std::cout << "RA/Dec:\n";
        std::cout << "  RA  = " << std::setprecision(2) << ra << "°\n";
        std::cout << "  Dec = " << dec << "°\n\n";
        
        std::cout << "Expected (Horizons 2025-11-21):\n";
        std::cout << "  RA  = 56.90°\n";
        std::cout << "  Dec = +19.77°\n\n";
        
        double err = sqrt(pow(ra - 56.90, 2) + pow(dec - 19.77, 2));
        std::cout << "Error: " << std::setprecision(1) << err << "°\n\n";
        
        std::cout << "===================================\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
}
