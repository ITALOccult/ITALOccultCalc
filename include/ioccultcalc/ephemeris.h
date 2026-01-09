#ifndef IOCCULTCALC_EPHEMERIS_H
#define IOCCULTCALC_EPHEMERIS_H

#include "ioccultcalc/orbital_elements.h"
#include "ioccultcalc/types.h"
#include "ioccultcalc/isp_reader.h"
#include "ioccultcalc/star_catalog.h"
#include <vector>
#include <memory>

namespace ioccultcalc {

struct EphemerisData {
    JulianDate jd;
    EquatorialCoordinates geocentricPos;
    Vector3D heliocentricPos;
    Vector3D heliocentricVel;
    double distance;
    double elongation;
    double phase;
    double magnitude;
    
    EphemerisData() : distance(0), elongation(0), phase(0), magnitude(0) {}
};

class Ephemeris {
public:
    Ephemeris();
    Ephemeris(const AstDynEquinoctialElements& elements);
    Ephemeris(std::shared_ptr<ISPReader> reader);
    Ephemeris(std::shared_ptr<ISPReader> reader, const AstDynEquinoctialElements& elements);
    
    void setElements(const AstDynEquinoctialElements& elements);
    EphemerisData compute(const JulianDate& jd);
    EphemerisData computeTopocentric(const JulianDate& jd, double lat, double lon, double alt_m);
    std::vector<EphemerisData> computeRange(const JulianDate& startJD, const JulianDate& endJD, double stepDays = 1.0);
    
    static Vector3D getEarthPosition(const JulianDate& jd);
    static Vector3D getEarthVelocity(const JulianDate& jd);
    static Vector3D getSunPosition(const JulianDate& jd);
    
    static double calculateMagnitudeHG(double H, double G, double r, double delta, double alpha_rad);

    static Vector3D applyStellarParallax(const Vector3D& starUnitVector, double parallax_mas, const Vector3D& earthHelioPos);
                                         
    static void applyProperMotion(double& ra_rad, double& dec_rad, double pm_ra_mas, double pm_dec_mas, double epoch_start_jd, double jd_target);

    bool predictOccultation(const JulianDate& jd, const StarData& star, const GeographicCoordinates& obs, double asteroid_radius_km);

    double calculateMagnitude(double r, double delta, double phaseAngle);

private:
    std::shared_ptr<ISPReader> spkReader_;
    AstDynEquinoctialElements elements_;
    
    void propagateOrbit(const JulianDate& targetJD, Vector3D& helioPos, Vector3D& helioVel);
    double solveKeplerEquation(double meanAnomaly, double eccentricity, double tolerance = 1e-12);
    double calculateObliquity(double jd) const;
    Vector3D eclipticToEquatorial(const Vector3D& posEcl, double eps) const;
    Vector3D getObserverPosition(double lat_deg, double lon_deg, double alt_m, double jd) const;
    Vector3D getEarthPositionEcl(double jd) const;
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_EPHEMERIS_H
