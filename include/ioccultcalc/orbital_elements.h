#ifndef IOCCULTCALC_ORBITAL_ELEMENTS_H
#define IOCCULTCALC_ORBITAL_ELEMENTS_H

#include "types.h"
#include <string>

namespace ioccultcalc {

// Forward declaration
struct EquinoctialElements;

// Elementi orbitali Kepleriani classici
struct OrbitalElements {
    double a;      // Semi-major axis (AU)
    double e;      // Eccentricità
    double i;      // Inclinazione (radianti)
    double Omega;  // Ascending node (radianti)
    double omega;  // Argument of perihelion (radianti)
    double M;      // Mean anomaly (radianti)
    JulianDate epoch;
    
    // Parametri fisici
    double H;      // Magnitudine assoluta
    double G;      // Slope parameter
    double diameter;
    
    // Parametri non gravitazionali (Yarkovsky/outgassing) - VFCC17 model
    double A1;     // Radial non-grav parameter (AU/day²)
    double A2;     // Transverse non-grav parameter (AU/day²)
    double A3;     // Normal non-grav parameter (AU/day²)
    
    std::string designation;
    std::string name;
    
    OrbitalElements()
        : a(0), e(0), i(0), Omega(0), omega(0), M(0),
          H(0), G(0.15), diameter(0),
          A1(0), A2(0), A3(0) {}
    
    // Converte in equinoziali
    EquinoctialElements toEquinoctial() const;
    
    // Crea da equinoziali
    static OrbitalElements fromEquinoctial(const EquinoctialElements& eq);
};

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
    
    // Parametri non gravitazionali (Yarkovsky/outgassing) - VFCC17 model
    double A1;     // Radial non-grav parameter (AU/day²)
    double A2;     // Transverse non-grav parameter (AU/day²)
    double A3;     // Normal non-grav parameter (AU/day²)
    
    std::string designation; // Designazione dell'asteroide
    std::string name;        // Nome dell'asteroide
    
    EquinoctialElements() 
        : a(0), h(0), k(0), p(0), q(0), lambda(0), 
          H(0), G(0.15), diameter(0),
          A1(0), A2(0), A3(0) {}
    
    // Converte in elementi Kepleriani classici (vecchio metodo)
    void toKeplerian(double& ecc, double& inc, double& omega, 
                    double& Omega, double& M) const;
    
    // Converte in OrbitalElements Kepleriani (nuovo metodo)
    OrbitalElements toKeplerian() const;
    
    // Crea da elementi Kepleriani
    static EquinoctialElements fromKeplerian(double a, double ecc, double inc,
                                            double omega, double Omega, double M,
                                            const JulianDate& epoch);
    
    // Crea da OrbitalElements
    static EquinoctialElements fromKeplerian(const OrbitalElements& orb);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ORBITAL_ELEMENTS_H
