#include "ioccultcalc/orbital_elements.h"
#include <cmath>

namespace ioccultcalc {

void EquinoctialElements::toKeplerian(double& ecc, double& inc, double& omega,
                                     double& Omega, double& M) const {
    // Converti elementi equinoziali in Kepleriani
    
    // Eccentricità
    ecc = sqrt(h * h + k * k);
    
    // Inclinazione
    inc = 2.0 * atan(sqrt(p * p + q * q));
    
    // Longitudine del nodo ascendente
    Omega = atan2(p, q);
    if (Omega < 0) Omega += 2.0 * M_PI;
    
    // Argomento del periapside
    double omega_plus_Omega = atan2(h, k);
    if (omega_plus_Omega < 0) omega_plus_Omega += 2.0 * M_PI;
    
    omega = omega_plus_Omega - Omega;
    if (omega < 0) omega += 2.0 * M_PI;
    
    // Anomalia media
    M = lambda - omega_plus_Omega;
    while (M < 0) M += 2.0 * M_PI;
    while (M >= 2.0 * M_PI) M -= 2.0 * M_PI;
}

EquinoctialElements EquinoctialElements::fromKeplerian(double a, double ecc, double inc,
                                                      double omega, double Omega, double M,
                                                      const JulianDate& epoch) {
    EquinoctialElements elem;
    elem.a = a;
    elem.epoch = epoch;
    
    // Converti elementi Kepleriani in equinoziali
    double omega_plus_Omega = omega + Omega;
    
    elem.h = ecc * sin(omega_plus_Omega);
    elem.k = ecc * cos(omega_plus_Omega);
    elem.p = tan(inc / 2.0) * sin(Omega);
    elem.q = tan(inc / 2.0) * cos(Omega);
    elem.lambda = M + omega_plus_Omega;
    
    // Normalizza lambda a [0, 2π)
    while (elem.lambda < 0) elem.lambda += 2.0 * M_PI;
    while (elem.lambda >= 2.0 * M_PI) elem.lambda -= 2.0 * M_PI;
    
    return elem;
}

// Nuovo metodo che ritorna OrbitalElements
OrbitalElements EquinoctialElements::toKeplerian() const {
    OrbitalElements orb;
    orb.a = a;
    orb.epoch = epoch;
    orb.H = H;
    orb.G = G;
    orb.diameter = diameter;
    orb.designation = designation;
    orb.name = name;
    
    // Converti elementi
    toKeplerian(orb.e, orb.i, orb.omega, orb.Omega, orb.M);
    
    return orb;
}

EquinoctialElements EquinoctialElements::fromKeplerian(const OrbitalElements& orb) {
    return fromKeplerian(orb.a, orb.e, orb.i, orb.omega, orb.Omega, orb.M, orb.epoch);
}

EquinoctialElements OrbitalElements::toEquinoctial() const {
    return EquinoctialElements::fromKeplerian(*this);
}

OrbitalElements OrbitalElements::fromEquinoctial(const EquinoctialElements& eq) {
    return eq.toKeplerian();
}

} // namespace ioccultcalc
