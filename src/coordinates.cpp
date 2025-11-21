#include "ioccultcalc/coordinates.h"
#include <cmath>

namespace ioccultcalc {

Vector3D Coordinates::equatorialToCartesian(const EquatorialCoordinates& eq) {
    double r = eq.distance > 0 ? eq.distance : 1.0;
    return Vector3D(
        r * cos(eq.dec) * cos(eq.ra),
        r * cos(eq.dec) * sin(eq.ra),
        r * sin(eq.dec)
    );
}

EquatorialCoordinates Coordinates::cartesianToEquatorial(const Vector3D& vec) {
    double r = vec.magnitude();
    double dec = asin(vec.z / r);
    double ra = atan2(vec.y, vec.x);
    
    if (ra < 0) ra += 2.0 * M_PI;
    
    return EquatorialCoordinates(ra, dec, r);
}

Vector3D Coordinates::geographicToECEF(const GeographicCoordinates& geo) {
    // WGS84 ellipsoid parameters
    const double a = 6378.137; // km, equatorial radius
    const double f = 1.0 / 298.257223563; // flattening
    const double b = a * (1.0 - f); // polar radius
    const double e2 = 2.0 * f - f * f; // first eccentricity squared
    
    double lat = geo.latitude * DEG_TO_RAD;
    double lon = geo.longitude * DEG_TO_RAD;
    double h = geo.altitude / 1000.0; // converti in km
    
    double N = a / sqrt(1.0 - e2 * sin(lat) * sin(lat));
    
    return Vector3D(
        (N + h) * cos(lat) * cos(lon),
        (N + h) * cos(lat) * sin(lon),
        (N * (1.0 - e2) + h) * sin(lat)
    );
}

GeographicCoordinates Coordinates::ecefToGeographic(const Vector3D& ecef) {
    // WGS84 parameters
    const double a = 6378.137;
    const double f = 1.0 / 298.257223563;
    const double b = a * (1.0 - f);
    const double e2 = 2.0 * f - f * f;
    
    double p = sqrt(ecef.x * ecef.x + ecef.y * ecef.y);
    double theta = atan2(ecef.z * a, p * b);
    
    double lat = atan2(ecef.z + e2 * b * pow(sin(theta), 3),
                      p - e2 * a * pow(cos(theta), 3));
    double lon = atan2(ecef.y, ecef.x);
    
    double N = a / sqrt(1.0 - e2 * sin(lat) * sin(lat));
    double h = p / cos(lat) - N;
    
    return GeographicCoordinates(
        lon * RAD_TO_DEG,
        lat * RAD_TO_DEG,
        h * 1000.0 // converti in metri
    );
}

double Coordinates::angularSeparation(const EquatorialCoordinates& pos1,
                                     const EquatorialCoordinates& pos2) {
    // Formula haversine per maggiore precisione a piccole separazioni
    double dra = pos2.ra - pos1.ra;
    double ddec = pos2.dec - pos1.dec;
    
    double a = sin(ddec / 2.0) * sin(ddec / 2.0) +
               cos(pos1.dec) * cos(pos2.dec) *
               sin(dra / 2.0) * sin(dra / 2.0);
    
    return 2.0 * atan2(sqrt(a), sqrt(1.0 - a));
}

EquatorialCoordinates Coordinates::applyPrecession(const EquatorialCoordinates& pos,
                                                   const JulianDate& fromEpoch,
                                                   const JulianDate& toEpoch) {
    // Precessione semplificata (accurata per poche decadi)
    // Per precisione maggiore si dovrebbero usare le matrici IAU 2006
    
    double T0 = (fromEpoch.jd - 2451545.0) / 36525.0;
    double T = (toEpoch.jd - fromEpoch.jd) / 36525.0;
    
    // Parametri di precessione in arcsec
    double zeta = (2306.2181 * T + 0.30188 * T * T + 0.017998 * T * T * T) / 3600.0 * DEG_TO_RAD;
    double z = (2306.2181 * T + 1.09468 * T * T + 0.018203 * T * T * T) / 3600.0 * DEG_TO_RAD;
    double theta = (2004.3109 * T - 0.42665 * T * T - 0.041833 * T * T * T) / 3600.0 * DEG_TO_RAD;
    
    // Converti in cartesiano, applica rotazione, riconverti
    Vector3D v = equatorialToCartesian(pos);
    
    // Matrice di precessione (semplificata per angoli piccoli)
    double cosZ = cos(z), sinZ = sin(z);
    double cosTheta = cos(theta), sinTheta = sin(theta);
    double cosZeta = cos(zeta), sinZeta = sin(zeta);
    
    Vector3D vPrec(
        v.x * (cosZ * cosTheta * cosZeta - sinZ * sinZeta) -
        v.y * (cosZ * cosTheta * sinZeta + sinZ * cosZeta) -
        v.z * cosZ * sinTheta,
        
        v.x * (sinZ * cosTheta * cosZeta + cosZ * sinZeta) -
        v.y * (sinZ * cosTheta * sinZeta - cosZ * cosZeta) -
        v.z * sinZ * sinTheta,
        
        v.x * sinTheta * cosZeta -
        v.y * sinTheta * sinZeta +
        v.z * cosTheta
    );
    
    return cartesianToEquatorial(vPrec);
}

EquatorialCoordinates Coordinates::applyAberration(const EquatorialCoordinates& pos,
                                                   const Vector3D& earthVelocity) {
    // Aberrazione annuale (stella)
    // earthVelocity in AU/day
    
    Vector3D dir = equatorialToCartesian(pos).normalize();
    double v = earthVelocity.magnitude() * AU / 86400.0; // km/s
    
    // Approssimazione al primo ordine
    double k = v / C_LIGHT;
    
    Vector3D vNorm = earthVelocity.normalize();
    double cosPhi = dir.dot(vNorm);
    
    // Correzione di aberrazione
    Vector3D dirCorr = dir + vNorm * k * (1.0 - cosPhi);
    dirCorr = dirCorr.normalize();
    
    return cartesianToEquatorial(dirCorr);
}

double Coordinates::positionAngle(const EquatorialCoordinates& from,
                                 const EquatorialCoordinates& to) {
    // Angolo di posizione da "from" verso "to"
    double dra = to.ra - from.ra;
    
    double pa = atan2(sin(dra),
                     cos(from.dec) * tan(to.dec) - sin(from.dec) * cos(dra));
    
    // Normalizza a [0, 2π)
    if (pa < 0) pa += 2.0 * M_PI;
    
    return pa;
}

} // namespace ioccultcalc
