/**
 * @file topocentric.cpp
 * @brief Implementation of topocentric coordinate corrections
 */

#include "topocentric.h"
#include <cmath>
#include <iostream>
#include <stdexcept>

namespace ioccultcalc {

// Use constants from types.h
constexpr double EARTH_ROTATION_RATE = TWO_PI / DAY_SEC;  // rad/s

TopocentricConverter::TopocentricConverter(const EllipsoidParameters& ellipsoid)
    : ellipsoid_(ellipsoid) {
}

double TopocentricConverter::radiusOfCurvature(double geodetic_lat_deg) const {
    // N(φ) = a / sqrt(1 - e² sin²φ)
    double lat_rad = geodetic_lat_deg * DEG_TO_RAD;
    double sin_lat = std::sin(lat_rad);
    double sin_lat_sq = sin_lat * sin_lat;
    
    return ellipsoid_.equatorial_radius_m / 
           std::sqrt(1.0 - ellipsoid_.eccentricity_squared * sin_lat_sq);
}

double TopocentricConverter::geodeticToGeocentricLatitude(double geodetic_lat_deg) const {
    // tan(φ') = (1 - e²) tan(φ)
    // where φ = geodetic, φ' = geocentric
    
    double lat_rad = geodetic_lat_deg * DEG_TO_RAD;
    double tan_geodetic = std::tan(lat_rad);
    
    double tan_geocentric = (1.0 - ellipsoid_.eccentricity_squared) * tan_geodetic;
    
    return std::atan(tan_geocentric) * RAD_TO_DEG;
}

Vector3D TopocentricConverter::getGeocentricPosition(const ObserverLocation& observer) const {
    // Convert geodetic coordinates (lon, lat, h) to geocentric Cartesian (X, Y, Z)
    // in ITRF (Earth-fixed) frame
    
    double lon_rad = observer.longitude_deg * DEG_TO_RAD;
    double lat_rad = observer.latitude_deg * DEG_TO_RAD;
    
    // If geocentric latitude given, convert to geodetic first
    double geodetic_lat_rad = lat_rad;
    if (!observer.is_geodetic) {
        // Approximate inverse: tan(φ) = tan(φ')/(1 - e²)
        double tan_geocentric = std::tan(lat_rad);
        double tan_geodetic = tan_geocentric / (1.0 - ellipsoid_.eccentricity_squared);
        geodetic_lat_rad = std::atan(tan_geodetic);
    }
    
    // Radius of curvature in prime vertical
    double N = radiusOfCurvature(geodetic_lat_rad * RAD_TO_DEG);
    
    // Geocentric Cartesian coordinates
    double cos_lat = std::cos(geodetic_lat_rad);
    double sin_lat = std::sin(geodetic_lat_rad);
    double cos_lon = std::cos(lon_rad);
    double sin_lon = std::sin(lon_rad);
    
    // X-axis points to (lon=0°, lat=0°) - Greenwich equator
    // Y-axis points to (lon=90°E, lat=0°)
    // Z-axis points to North pole
    
    double X = (N + observer.elevation_m) * cos_lat * cos_lon;
    double Y = (N + observer.elevation_m) * cos_lat * sin_lon;
    double Z = (N * (1.0 - ellipsoid_.eccentricity_squared) + observer.elevation_m) * sin_lat;
    
    return Vector3D(X, Y, Z);
}

void TopocentricConverter::rotationMatrixToTopocentric(const ObserverLocation& observer,
                                                      double matrix[3][3]) const {
    // Rotation from ITRF (geocentric) to topocentric (East-North-Up)
    // 1. Rotate around Z by (90° + lon) to align X with East
    // 2. Rotate around East by (90° - lat) to align Z with Up
    
    double lon_rad = observer.longitude_deg * DEG_TO_RAD;
    double lat_rad = observer.latitude_deg * DEG_TO_RAD;
    
    double cos_lon = std::cos(lon_rad);
    double sin_lon = std::sin(lon_rad);
    double cos_lat = std::cos(lat_rad);
    double sin_lat = std::sin(lat_rad);
    
    // Combined rotation matrix (ITRF -> Topocentric)
    // Row 0: East direction
    matrix[0][0] = -sin_lon;
    matrix[0][1] = cos_lon;
    matrix[0][2] = 0.0;
    
    // Row 1: North direction
    matrix[1][0] = -sin_lat * cos_lon;
    matrix[1][1] = -sin_lat * sin_lon;
    matrix[1][2] = cos_lat;
    
    // Row 2: Up direction (zenith)
    matrix[2][0] = cos_lat * cos_lon;
    matrix[2][1] = cos_lat * sin_lon;
    matrix[2][2] = sin_lat;
}

Vector3D TopocentricConverter::applyRotation(const double matrix[3][3], const Vector3D& vec) {
    double x = matrix[0][0] * vec.x + matrix[0][1] * vec.y + matrix[0][2] * vec.z;
    double y = matrix[1][0] * vec.x + matrix[1][1] * vec.y + matrix[1][2] * vec.z;
    double z = matrix[2][0] * vec.x + matrix[2][1] * vec.y + matrix[2][2] * vec.z;
    return Vector3D(x, y, z);
}

Vector3D TopocentricConverter::geocentricToTopocentric(const Vector3D& geocentric_pos,
                                                      const ObserverLocation& observer) const {
    // Get observer's geocentric position
    Vector3D observer_pos = getGeocentricPosition(observer);
    
    // Relative position (object - observer)
    Vector3D relative_pos = geocentric_pos - observer_pos;
    
    // Rotation matrix to topocentric frame
    double rotation_matrix[3][3];
    rotationMatrixToTopocentric(observer, rotation_matrix);
    
    // Apply rotation
    return applyRotation(rotation_matrix, relative_pos);
}

Vector3D TopocentricConverter::topocentricToGeocentric(const Vector3D& topocentric_pos,
                                                      const ObserverLocation& observer) const {
    // Inverse rotation (transpose of rotation matrix)
    double rotation_matrix[3][3];
    rotationMatrixToTopocentric(observer, rotation_matrix);
    
    // Transpose for inverse rotation
    double inverse_matrix[3][3];
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            inverse_matrix[i][j] = rotation_matrix[j][i];
        }
    }
    
    // Apply inverse rotation
    Vector3D geocentric_relative = applyRotation(inverse_matrix, topocentric_pos);
    
    // Add observer position
    Vector3D observer_pos = getGeocentricPosition(observer);
    return geocentric_relative + observer_pos;
}

double TopocentricConverter::earthRotationAngle(double jd_ut1) {
    // IAU 2000 Earth Rotation Angle
    // ERA = 2π(0.7790572732640 + 1.00273781191135448 × D_UT1)
    
    double D_UT1 = jd_ut1 - JD_J2000;
    double fraction = 0.7790572732640 + 1.00273781191135448 * D_UT1;
    
    // Reduce to [0, 2π)
    fraction = fraction - std::floor(fraction);
    
    return TWO_PI * fraction;
}

double TopocentricConverter::greenwichMeanSiderealTime(double jd_ut1) {
    // GMST at 0h UT (IAU 1982 formula, valid until ~2100)
    // GMST = 24110.54841 + 8640184.812866 T + 0.093104 T² - 6.2e-6 T³ seconds
    
    double T = (jd_ut1 - JD_J2000) / 36525.0;  // Julian centuries from J2000
    
    // GMST at 0h UT in seconds
    double gmst0_sec = 24110.54841 + 
                       8640184.812866 * T + 
                       0.093104 * T * T - 
                       6.2e-6 * T * T * T;
    
    // Add contribution from UT
    double ut_hours = (jd_ut1 - std::floor(jd_ut1 - 0.5) - 0.5) * 24.0;
    double gmst_sec = gmst0_sec + ut_hours * 3600.0 * 1.002737909350795;
    
    // Convert to radians and reduce to [0, 2π)
    double gmst_rad = (gmst_sec / 86400.0) * TWO_PI;
    gmst_rad = std::fmod(gmst_rad, TWO_PI);
    if (gmst_rad < 0.0) gmst_rad += TWO_PI;
    
    return gmst_rad;
}

double TopocentricConverter::localSiderealTime(double jd_ut1, double longitude_deg) {
    double gmst = greenwichMeanSiderealTime(jd_ut1);
    double lst = gmst + longitude_deg * DEG_TO_RAD;
    
    // Reduce to [0, 2π)
    lst = std::fmod(lst, TWO_PI);
    if (lst < 0.0) lst += TWO_PI;
    
    return lst;
}

void TopocentricConverter::rotationMatrixITRFtoCelestial(double jd_ut1,
                                                        const std::string& frame,
                                                        double matrix[3][3]) {
    // Simplified rotation: just Earth rotation (neglect precession/nutation for now)
    // For full precision, should include:
    // - Precession (J2000 -> date)
    // - Nutation
    // - Polar motion
    
    // Earth rotation angle (or GMST)
    double era = earthRotationAngle(jd_ut1);
    
    double cos_era = std::cos(era);
    double sin_era = std::sin(era);
    
    // Rotation around Z-axis (Earth rotation)
    // ITRF -> J2000 (neglecting precession/nutation)
    // R_z(-ERA) would be Celestial -> ITRF
    // R_z(+ERA) is ITRF -> Celestial
    matrix[0][0] = cos_era;
    matrix[0][1] = -sin_era; // Correct for ITRF -> Celestial
    matrix[0][2] = 0.0;
    
    matrix[1][0] = sin_era;  // Correct for ITRF -> Celestial
    matrix[1][1] = cos_era;
    matrix[1][2] = 0.0;
    
    matrix[2][0] = 0.0;
    matrix[2][1] = 0.0;
    matrix[2][2] = 1.0;
    
    // If ECLIPJ2000 requested, apply obliquity rotation
    if (frame == "ECLIPJ2000") {
        // Mean obliquity at J2000.0: ε₀ = 23°26'21.406"
        const double epsilon_J2000 = 23.439291 * DEG_TO_RAD;
        double cos_eps = std::cos(epsilon_J2000);
        double sin_eps = std::sin(epsilon_J2000);
        
        // Rotation matrix: equatorial -> ecliptic (around X-axis)
        double ecliptic_rotation[3][3];
        ecliptic_rotation[0][0] = 1.0;
        ecliptic_rotation[0][1] = 0.0;
        ecliptic_rotation[0][2] = 0.0;
        
        ecliptic_rotation[1][0] = 0.0;
        ecliptic_rotation[1][1] = cos_eps;
        ecliptic_rotation[1][2] = sin_eps;
        
        ecliptic_rotation[2][0] = 0.0;
        ecliptic_rotation[2][1] = -sin_eps;
        ecliptic_rotation[2][2] = cos_eps;
        
        // Combine rotations: result = ecliptic_rotation × matrix
        double temp[3][3];
        for (int i = 0; i < 3; ++i) {
            for (int j = 0; j < 3; ++j) {
                temp[i][j] = 0.0;
                for (int k = 0; k < 3; ++k) {
                    temp[i][j] += ecliptic_rotation[i][k] * matrix[k][j];
                }
            }
        }
        
        // Copy back
        for (int i = 0; i < 3; ++i) {
            for (int j = 0; j < 3; ++j) {
                matrix[i][j] = temp[i][j];
            }
        }
    }
}

Vector3D TopocentricConverter::calculateTopocentricCorrection(const ObserverLocation& observer,
                                                             double jd_tt,
                                                             const std::string& celestial_frame) const {
    // Get observer position in ITRF (Earth-fixed)
    Vector3D itrf_pos = getGeocentricPosition(observer);
    
    // Rotate to celestial frame
    // Note: should use UT1, but TT is close enough for this purpose (difference < 1 second)
    double jd_ut1 = jd_tt;  // Approximation: UT1 ≈ TT (ignoring ΔT)
    
    double rotation_matrix[3][3];
    rotationMatrixITRFtoCelestial(jd_ut1, celestial_frame, rotation_matrix);
    
    return applyRotation(rotation_matrix, itrf_pos);
}

Vector3D TopocentricConverter::intersectWithWGS84(const Vector3D& origin, const Vector3D& direction) const {
    // Risoluzione dell'equazione di intersezione Linea-Elissoide
    // (x/a)^2 + (y/a)^2 + (z/b)^2 = 1
    // P(k) = origin + k * direction
    double a = ellipsoid_.equatorial_radius_m;
    double b = ellipsoid_.polarRadius();
    
    double oa = origin.x / a;
    double ob = origin.y / a;
    double oc = origin.z / b;
    double da = direction.x / a;
    double db = direction.y / a;
    double dc = direction.z / b;
    
    double A = da * da + db * db + dc * dc;
    double B = 2.0 * (oa * da + ob * db + oc * dc);
    double C = oa * oa + ob * ob + oc * oc - 1.0;
    
    double discriminant = B * B - 4.0 * A * C;
    
    // Solo se molto vicino all'intersezione (debug)
    if (discriminant > -0.01) {
        std::cout << "[DEBUG] intersect A=" << A << " B=" << B << " C=" << C << " Disc=" << discriminant << std::endl;
    }
    
    if (discriminant < 0) return Vector3D(0, 0, 0); // Nessuna intersezione
    
    // Vogliamo l'intersezione più vicina all'origine (solitamente l'asteroide)
    // ma che sia "davanti" se l'origine è nello spazio.
    double k_near = (-B - std::sqrt(discriminant)) / (2.0 * A);
    double k_far = (-B + std::sqrt(discriminant)) / (2.0 * A);
    
    // Lo "Shadow Ray" va verso Terra, quindi k deve essere positivo
    double k = (k_near > 0) ? k_near : ((k_far > 0) ? k_far : -1.0);
    
    if (k < 0) return Vector3D(0, 0, 0);
    
    return origin + direction * k;
}

ObserverLocation TopocentricConverter::getGeodeticPosition(const Vector3D& itrf_pos) const {
    // Algoritmo di Bowring per conversione ITRF -> Geodetico
    double a = ellipsoid_.equatorial_radius_m;
    double b = ellipsoid_.polarRadius();
    double e2 = ellipsoid_.eccentricity_squared;
    double e_prime2 = (a * a - b * b) / (b * b);
    
    double p = std::sqrt(itrf_pos.x * itrf_pos.x + itrf_pos.y * itrf_pos.y);
    double theta = std::atan2(itrf_pos.z * a, p * b);
    
    double lat = std::atan2(itrf_pos.z + e_prime2 * b * std::pow(std::sin(theta), 3),
                           p - e2 * a * std::pow(std::cos(theta), 3));
    double lon = std::atan2(itrf_pos.y, itrf_pos.x);
    
    double sin_lat = std::sin(lat);
    double N = a / std::sqrt(1.0 - e2 * sin_lat * sin_lat);
    double alt = p / std::cos(lat) - N;
    
    ObserverLocation loc;
    loc.latitude_deg = lat * RAD_TO_DEG;
    loc.longitude_deg = lon * RAD_TO_DEG;
    loc.elevation_m = alt;
    loc.is_geodetic = true;
    
    return loc;
}

Vector3D TopocentricConverter::calculateParallax(double geocentric_distance,
                                                const ObserverLocation& observer,
                                                double jd_tt) const {
    // Parallax displacement = observer position vector
    // (scaled by geometry)
    
    Vector3D observer_pos = calculateTopocentricCorrection(observer, jd_tt);
    
    // For distant objects, parallax is simply the observer position
    // For close objects, need to account for angle
    // Here we return the full observer position vector
    
    return observer_pos;
}

double TopocentricConverter::horizontalParallax(double distance) {
    // π = arcsin(R_Earth / distance)
    // For large distances, π ≈ R_Earth / distance (small angle)
    
    double R_Earth = EARTH_RADIUS_WGS84_M;  // Equatorial radius in meters
    
    if (distance <= R_Earth) {
        throw std::invalid_argument("Distance must be > Earth radius");
    }
    
    double ratio = R_Earth / distance;
    
    if (ratio < 0.01) {
        // Small angle approximation (more accurate for small angles)
        return ratio * RAD_TO_DEG;
    } else {
        return std::asin(ratio) * RAD_TO_DEG;
    }
}

Vector3D TopocentricConverter::calculateDiurnalAberration(const ObserverLocation& observer,
                                                         double jd_tt) const {
    // Observer velocity due to Earth rotation
    // v = ω × r, where ω = Earth rotation vector, r = observer position
    
    Vector3D observer_pos_itrf = getGeocentricPosition(observer);
    
    // Earth rotation vector (along Z-axis in ITRF)
    // ω = 2π / (sidereal day) ≈ 7.292115e-5 rad/s
    Vector3D omega(0.0, 0.0, EARTH_ROTATION_RATE);
    
    // Velocity: v = ω × r
    Vector3D velocity_itrf = omega.cross(observer_pos_itrf);
    
    // Transform to celestial frame
    double jd_ut1 = jd_tt;
    double rotation_matrix[3][3];
    rotationMatrixITRFtoCelestial(jd_ut1, "ECLIPJ2000", rotation_matrix);
    
    return applyRotation(rotation_matrix, velocity_itrf);
}

// Global convenience functions

Vector3D calculateTopocentricCorrection(const ObserverLocation& observer, double jd_tt) {
    static TopocentricConverter converter;
    return converter.calculateTopocentricCorrection(observer, jd_tt);
}

double geodeticToGeocentricLatitude(double geodetic_lat_deg) {
    static TopocentricConverter converter;
    return converter.geodeticToGeocentricLatitude(geodetic_lat_deg);
}

} // namespace ioccultcalc
