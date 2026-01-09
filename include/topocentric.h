/**
 * @file topocentric.h
 * @brief Topocentric coordinate corrections for observer location
 * 
 * This module implements precise topocentric corrections accounting for:
 * - Observer elevation above sea level
 * - Geodetic vs geocentric latitude (Earth ellipsoid WGS84)
 * - Earth rotation (diurnal aberration)
 * - Parallax effects
 * 
 * Critical for accurate occultation predictions, especially for:
 * - Mountain observatories (elevation > 1000m)
 * - High-latitude sites (flattening effects)
 * - Close approaches (parallax significant)
 * 
 * @author IOccultCalc Development Team
 * @date 2025-11-23
 */

#ifndef TOPOCENTRIC_H
#define TOPOCENTRIC_H

#include "ioccultcalc/types.h"
#include <cmath>
#include <string>

namespace ioccultcalc {

using Vector3D = ioccultcalc::Vector3D;

/**
 * @brief Observer location with full geodetic information
 */
struct ObserverLocation {
    double longitude_deg;          ///< Geographic longitude (degrees, + East)
    double latitude_deg;           ///< Geographic latitude (degrees, + North)
    double elevation_m;            ///< Elevation above sea level (meters)
    
    // Optional: specify if geodetic or geocentric latitude
    bool is_geodetic = true;       ///< true = geodetic (default), false = geocentric
    
    // Optional: atmospheric conditions for refraction
    double temperature_celsius = 10.0;
    double pressure_mbar = 1013.25;
    
    // Observatory metadata (optional)
    std::string name;              ///< Observatory name
    std::string code;              ///< IAU/MPC code
    
    /**
     * @brief Default constructor (geocenter)
     */
    ObserverLocation() 
        : longitude_deg(0.0), latitude_deg(0.0), elevation_m(0.0), 
          is_geodetic(true), temperature_celsius(10.0), pressure_mbar(1013.25) {}
    
    /**
     * @brief Constructor with coordinates
     */
    ObserverLocation(double lon, double lat, double elev = 0.0)
        : longitude_deg(lon), latitude_deg(lat), elevation_m(elev),
          is_geodetic(true), temperature_celsius(10.0), pressure_mbar(1013.25) {}
};

/**
 * @brief WGS84 ellipsoid parameters
 */
struct EllipsoidParameters {
    double equatorial_radius_m;     ///< Semi-major axis (a)
    double flattening;              ///< Flattening (f)
    double eccentricity_squared;    ///< e² = 2f - f²
    
    /**
     * @brief WGS84 standard ellipsoid
     */
    static EllipsoidParameters WGS84() {
        EllipsoidParameters params;
        params.equatorial_radius_m = 6378137.0;           // a = 6378.137 km
        params.flattening = 1.0 / 298.257223563;         // f = 1/298.257223563
        params.eccentricity_squared = 2.0 * params.flattening - 
                                     params.flattening * params.flattening;
        return params;
    }
    
    /**
     * @brief Get polar radius
     */
    double polarRadius() const {
        return equatorial_radius_m * (1.0 - flattening);
    }
};

/**
 * @class TopocentricConverter
 * @brief Convert between geocentric and topocentric coordinates
 */
class TopocentricConverter {
public:
    /**
     * @brief Constructor with ellipsoid parameters
     */
    explicit TopocentricConverter(const EllipsoidParameters& ellipsoid = EllipsoidParameters::WGS84());
    
    /**
     * @brief Calculate observer's geocentric position vector (ITRF frame)
     * 
     * Returns the observer's position in Earth-fixed frame (ITRF) accounting for:
     * - WGS84 ellipsoid shape
     * - Geodetic latitude (surface perpendicular)
     * - Elevation above ellipsoid
     * 
     * Result is in meters, X-Y plane is equatorial, Z along rotation axis
     * 
     * @param observer Observer location (geodetic coordinates)
     * @return Position vector in ITRF frame (meters)
     */
    Vector3D getGeocentricPosition(const ObserverLocation& observer) const;
    
    /**
     * @brief Convert geodetic latitude to geocentric latitude
     * 
     * Geodetic: angle from equatorial plane to surface normal
     * Geocentric: angle from equatorial plane to geocenter-observer line
     * 
     * Difference can be up to 11.5 arcminutes at 45° latitude
     * 
     * @param geodetic_lat_deg Geodetic latitude (degrees)
     * @return Geocentric latitude (degrees)
     */
    double geodeticToGeocentricLatitude(double geodetic_lat_deg) const;
    
    /**
     * @brief Calculate radius of curvature in prime vertical
     * 
     * N(φ) = a / sqrt(1 - e² sin²φ)
     * Used for converting geodetic to geocentric coordinates
     * 
     * @param geodetic_lat_deg Geodetic latitude (degrees)
     * @return Radius of curvature (meters)
     */
    double radiusOfCurvature(double geodetic_lat_deg) const;
    
    /**
     * @brief Convert geocentric position to topocentric
     * 
     * Transforms object position from geocentric to topocentric frame:
     * - Origin at observer location
     * - X-axis: local East
     * - Y-axis: local North
     * - Z-axis: local Zenith (up)
     * 
     * @param geocentric_pos Object position in geocentric frame (m)
     * @param observer Observer location
     * @return Position in topocentric frame (m)
     */
    Vector3D geocentricToTopocentric(const Vector3D& geocentric_pos,
                                     const ObserverLocation& observer) const;
    
    /**
     * @brief Convert topocentric position to geocentric
     * 
     * Inverse of geocentricToTopocentric
     * 
     * @param topocentric_pos Position in topocentric frame (m)
     * @param observer Observer location
     * @return Position in geocentric frame (m)
     */
    Vector3D topocentricToGeocentric(const Vector3D& topocentric_pos,
                                     const ObserverLocation& observer) const;
    
    /**
     * @brief Calculate topocentric correction vector
     * 
     * Vector from geocenter to observer in celestial frame (J2000 or ECLIPJ2000)
     * Accounts for Earth rotation at given time
     * 
     * @param observer Observer location
     * @param jd_tt Julian Date (TT)
     * @param celestial_frame "J2000" or "ECLIPJ2000"
     * @return Observer position vector in celestial frame (m)
     */
    Vector3D calculateTopocentricCorrection(const ObserverLocation& observer,
                                           double jd_tt,
                                           const std::string& celestial_frame = "ECLIPJ2000") const;

    /**
     * @brief Interseca una linea con l'elissoide WGS84
     * @param origin Origine della linea (es. posizione asteroide) in metri (ITRF)
     * @param direction Direzione della linea (vettore unitario, ITRF)
     * @return Punto di intersezione in metri (ITRF), o Vector3D(0,0,0) se nessuna intersezione
     */
    Vector3D intersectWithWGS84(const Vector3D& origin, const Vector3D& direction) const;

    /**
     * @brief Converte coordinate cartesiane ITRF in geodetiche (Lon, Lat, Alt)
     * @param itrf_pos Posizione in metri
     * @return ObserverLocation con longitude_deg, latitude_deg, elevation_m
     */
    ObserverLocation getGeodeticPosition(const Vector3D& itrf_pos) const;
    
    /**
     * @brief Calculate parallax correction
     * 
     * Difference in object position seen from geocenter vs observer location
     * Important for close objects (Moon, nearby asteroids, satellites)
     * 
     * @param geocentric_distance Object distance from geocenter (m)
     * @param observer Observer location
     * @param jd_tt Julian Date (TT)
     * @return Parallax displacement vector (m)
     */
    Vector3D calculateParallax(double geocentric_distance,
                              const ObserverLocation& observer,
                              double jd_tt) const;
    
    /**
     * @brief Calculate horizontal parallax angle
     * 
     * Maximum parallax when object is on horizon
     * π = arcsin(R_Earth / distance)
     * 
     * @param distance Object distance (m)
     * @return Horizontal parallax (degrees)
     */
    static double horizontalParallax(double distance);
    
    /**
     * @brief Calculate diurnal aberration correction
     * 
     * Aberration due to observer's velocity from Earth rotation
     * Maximum ~0.32 arcsec at equator
     * 
     * @param observer Observer location
     * @param jd_tt Julian Date (TT)
     * @return Velocity vector in celestial frame (m/s)
     */
    Vector3D calculateDiurnalAberration(const ObserverLocation& observer,
                                       double jd_tt) const;
    
    /**
     * @brief Get Earth rotation angle (ERA)
     * 
     * Earth Rotation Angle replaces GMST in IAU 2000 system
     * ERA = 2π(0.7790572732640 + 1.00273781191135448 × D_UT1)
     * where D_UT1 = JD_UT1 - 2451545.0
     * 
     * @param jd_ut1 Julian Date (UT1)
     * @return ERA in radians
     */
    static double earthRotationAngle(double jd_ut1);
    
    /**
     * @brief Get Greenwich Mean Sidereal Time (GMST)
     * 
     * Traditional method (pre-IAU 2000)
     * GMST at 0h UT = 24110.54841 + 8640184.812866 T + 0.093104 T² - 6.2e-6 T³
     * where T = centuries from J2000.0
     * 
     * @param jd_ut1 Julian Date (UT1)
     * @return GMST in radians
     */
    static double greenwichMeanSiderealTime(double jd_ut1);
    
    /**
     * @brief Get Local Sidereal Time (LST)
     * 
     * LST = GMST + longitude
     * 
     * @param jd_ut1 Julian Date (UT1)
     * @param longitude_deg Observer longitude (degrees, + East)
     * @return LST in radians
     */
    static double localSiderealTime(double jd_ut1, double longitude_deg);
    
    /**
     * @brief Rotation matrix ITRF -> Celestial frame
     * 
     * @param jd_ut1 Julian Date (UT1)
     * @param frame "J2000" or "ECLIPJ2000"
     * @return 3x3 rotation matrix
     */
    static void rotationMatrixITRFtoCelestial(double jd_ut1,
                                             const std::string& frame,
                                             double matrix[3][3]);
    
    /**
     * @brief Apply rotation matrix to vector
     */
    static Vector3D applyRotation(const double matrix[3][3], const Vector3D& vec);

private:
    EllipsoidParameters ellipsoid_;  ///< Earth ellipsoid parameters
    
    /**
     * @brief Calculate rotation matrix from ITRF to topocentric frame
     * 
     * @param observer Observer location
     * @return 3x3 rotation matrix
     */
    void rotationMatrixToTopocentric(const ObserverLocation& observer,
                                    double matrix[3][3]) const;
};

/**
 * @brief Global convenience function for topocentric correction
 * 
 * Quick calculation of observer position in celestial frame
 * 
 * @param observer Observer location
 * @param jd_tt Julian Date (TT)
 * @return Observer position vector (m)
 */
Vector3D calculateTopocentricCorrection(const ObserverLocation& observer, double jd_tt);

/**
 * @brief Calculate geodetic to geocentric latitude conversion
 * 
 * @param geodetic_lat_deg Geodetic latitude (degrees)
 * @return Geocentric latitude (degrees)
 */
double geodeticToGeocentricLatitude(double geodetic_lat_deg);

} // namespace ioccultcalc

#endif // TOPOCENTRIC_H
