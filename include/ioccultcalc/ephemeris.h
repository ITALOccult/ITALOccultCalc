#ifndef IOCCULTCALC_EPHEMERIS_H
#define IOCCULTCALC_EPHEMERIS_H

#include "orbital_elements.h"
#include "types.h"
#include <vector>

namespace ioccultcalc {

// Dati effemeridali per un istante
struct EphemerisData {
    JulianDate jd;
    EquatorialCoordinates geocentricPos; // Posizione geocentrica
    Vector3D heliocentricPos;            // Posizione eliocentrica (AU)
    Vector3D heliocentricVel;            // Velocità eliocentrica (AU/day)
    double distance;                      // Distanza dalla Terra (AU)
    double elongation;                    // Elongazione solare (gradi)
    double phase;                         // Angolo di fase (gradi)
    double magnitude;                     // Magnitudine visuale
    
    EphemerisData() : distance(0), elongation(0), phase(0), magnitude(0) {}
};

class Ephemeris {
public:
    Ephemeris();
    explicit Ephemeris(const EquinoctialElements& elements);
    
    // Imposta gli elementi orbitali
    void setElements(const EquinoctialElements& elements);
    
    // Calcola la posizione dell'asteroide per una data epoca
    EphemerisData compute(const JulianDate& jd);
    
    // Calcola effemeridi per un intervallo temporale
    std::vector<EphemerisData> computeRange(const JulianDate& startJD, 
                                           const JulianDate& endJD,
                                           double stepDays = 1.0);
    
    // Ottiene la posizione del Sole geocentrica
    static Vector3D getSunPosition(const JulianDate& jd);
    
    // Ottiene la posizione della Terra eliocentrica
    static Vector3D getEarthPosition(const JulianDate& jd);
    static Vector3D getEarthVelocity(const JulianDate& jd);
    
    // Ottiene posizione Terra con correzioni complete (aberrazione, relatività)
    static Vector3D getEarthPositionWithCorrections(const JulianDate& jd, 
                                                     const Vector3D& observerPos);
    
private:
    EquinoctialElements elements_;
    
    // Propaga l'orbita da epoca elementi a epoca target
    void propagateOrbit(const JulianDate& targetJD, 
                       Vector3D& helioPos, Vector3D& helioVel);
    
    // Risolve l'equazione di Keplero per anomalia eccentrica
    double solveKeplerEquation(double meanAnomaly, double eccentricity, 
                              double tolerance = 1e-12);
    
    // Calcola la magnitudine apparente
    double calculateMagnitude(double r, double delta, double phaseAngle);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_EPHEMERIS_H
