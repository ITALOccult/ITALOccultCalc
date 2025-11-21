#ifndef IOCCULTCALC_ORBITAL_ELEMENTS_H
#define IOCCULTCALC_ORBITAL_ELEMENTS_H

#include "types.h"
#include <string>

namespace ioccultcalc {

// Elementi orbitali equinoziali
struct EquinoctialElements {
    double a;     // Semi-major axis (AU)
    double h;     // h = e * sin(omega + Omega)
    double k;     // k = e * cos(omega + Omega)
    double p;     // p = tan(i/2) * sin(Omega)
    double q;     // q = tan(i/2) * cos(Omega)
    double lambda; // Mean longitude (radianti)
    JulianDate epoch; // Epoca degli elementi
    
    // Parametri fisici
    double H;      // Magnitudine assoluta
    double G;      // Slope parameter
    double diameter; // Diametro in km (se disponibile)
    
    std::string designation; // Designazione dell'asteroide
    std::string name;        // Nome dell'asteroide
    
    EquinoctialElements() 
        : a(0), h(0), k(0), p(0), q(0), lambda(0), 
          H(0), G(0.15), diameter(0) {}
    
    // Converte in elementi Kepleriani classici
    void toKeplerian(double& ecc, double& inc, double& omega, 
                    double& Omega, double& M) const;
    
    // Crea da elementi Kepleriani
    static EquinoctialElements fromKeplerian(double a, double ecc, double inc,
                                            double omega, double Omega, double M,
                                            const JulianDate& epoch);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ORBITAL_ELEMENTS_H
