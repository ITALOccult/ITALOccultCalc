#ifndef IOCCULTCALC_COORDINATES_H
#define IOCCULTCALC_COORDINATES_H

#include "types.h"

namespace ioccultcalc {

class Coordinates {
public:
    // Converte coordinate equatoriali in vettore cartesiano
    static Vector3D equatorialToCartesian(const EquatorialCoordinates& eq);
    
    // Converte vettore cartesiano in coordinate equatoriali
    static EquatorialCoordinates cartesianToEquatorial(const Vector3D& vec);
    
    // Converte coordinate geografiche in vettore cartesiano terrestre (ECEF)
    static Vector3D geographicToECEF(const GeographicCoordinates& geo);
    
    // Converte vettore ECEF in coordinate geografiche
    static GeographicCoordinates ecefToGeographic(const Vector3D& ecef);
    
    // Calcola la distanza angolare tra due punti sulla sfera celeste
    static double angularSeparation(const EquatorialCoordinates& pos1,
                                   const EquatorialCoordinates& pos2);
    
    // Applica precessione delle coordinate (J2000 -> data)
    static EquatorialCoordinates applyPrecession(const EquatorialCoordinates& pos,
                                                 const JulianDate& fromEpoch,
                                                 const JulianDate& toEpoch);
    
    // Applica aberrazione annuale
    static EquatorialCoordinates applyAberration(const EquatorialCoordinates& pos,
                                                const Vector3D& earthVelocity);
    
    // Calcola l'angolo di posizione tra due oggetti celesti
    static double positionAngle(const EquatorialCoordinates& from,
                               const EquatorialCoordinates& to);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_COORDINATES_H
