#ifndef IOCCULTCALC_TYPES_H
#define IOCCULTCALC_TYPES_H

#include <string>
#include <cmath>

namespace ioccultcalc {

// --- PHYSICAL & ASTRONOMICAL CONSTANTS ---

// Mathematical
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
constexpr double PI = M_PI;
constexpr double TWO_PI = 2.0 * M_PI;
constexpr double DEG_TO_RAD = M_PI / 180.0;
constexpr double RAD_TO_DEG = 180.0 / M_PI;
constexpr double ARCSEC_TO_RAD = 4.84813681109536e-6; // 1 arcsec in radians
constexpr double ARCSEC_TO_DEG = 1.0 / 3600.0;
constexpr double ARCMIN_TO_DEG = 1.0 / 60.0;
constexpr double DEG_TO_ARCSEC = 3600.0;
constexpr double DEG_TO_ARCMIN = 60.0;
constexpr double OBLIQUITY_J2000 = 23.4392911 * DEG_TO_RAD; // ~0.4090928 rad

// Distances
constexpr double AU_KM = 149597870.7;             // 1 AU in km
constexpr double AU_M = AU_KM * 1000.0;           // 1 AU in meters
constexpr double KM_M = 1000.0;                   // 1 km in meters
constexpr double AU = AU_KM;                      // Legacy alias

// Physical
constexpr double C_LIGHT_KM_S = 299792.458;      // Speed of light km/s
constexpr double C_LIGHT_M_S = 299792458.0;      // Speed of light m/s
constexpr double C_LIGHT = C_LIGHT_KM_S;          // Legacy alias

// Thermodynamics
constexpr double ZERO_CELSIUS_KELVIN = 273.15;
constexpr double STANDARD_TEMP_KELVIN = 288.15;   // 15°C
constexpr double STANDARD_PRESSURE_MBAR = 1013.25; // 1 atm 
constexpr double STANDARD_TEMP_C = 15.0;

// Solar System
constexpr double GMS_KM3_S2 = 1.32712440041279419e11; // GM Sun km³/s²
constexpr double EARTH_RADIUS_WGS84_KM = 6378.137;
constexpr double EARTH_RADIUS_WGS84_M = 6378137.0;
constexpr double EARTH_RADIUS = EARTH_RADIUS_WGS84_KM; // Legacy alias

// WGS84 Ellipsoid
constexpr double WGS84_FLATTENING = 1.0 / 298.257223563;
constexpr double WGS84_ECCENTRICITY_SQ = 2.0 * WGS84_FLATTENING - WGS84_FLATTENING * WGS84_FLATTENING;

// Time
constexpr double DAY_SEC = 86400.0;               // Seconds per day
constexpr double JD_J2000 = 2451545.0;            // Epoch J2000.0
constexpr double DAYS_PER_CENTURY = 36525.0;      // Days per century (astronomical)
constexpr double AU_DAY_TO_KM_S = AU_KM / DAY_SEC; 
constexpr double KM_S_TO_AU_DAY = 1.0 / AU_DAY_TO_KM_S;
constexpr double LIGHT_TIME_1AU_SEC = AU_M / C_LIGHT_M_S; // ~499.00478 s
constexpr double C_LIGHT_AU_DAY = DAY_SEC / LIGHT_TIME_1AU_SEC; // ~173.14463 AU/day

// Struttura per coordinate equatoriali
struct EquatorialCoordinates {
    double ra;   // Right Ascension in radianti
    double dec;  // Declination in radianti
    double distance; // Distanza in km (opzionale)
    
    EquatorialCoordinates() : ra(0), dec(0), distance(0) {}
    EquatorialCoordinates(double r, double d, double dist = 0) 
        : ra(r), dec(d), distance(dist) {}
};

// Struttura per coordinate geografiche
struct GeographicCoordinates {
    double longitude; // in gradi
    double latitude;  // in gradi
    double altitude;  // in metri
    
    GeographicCoordinates() : longitude(0), latitude(0), altitude(0) {}
    GeographicCoordinates(double lon, double lat, double alt = 0)
        : longitude(lon), latitude(lat), altitude(alt) {}
};

// Struttura per vettori 3D
struct Vector3D {
    double x, y, z;
    
    Vector3D() : x(0), y(0), z(0) {}
    Vector3D(double x_, double y_, double z_) : x(x_), y(y_), z(z_) {}
    
    double magnitude() const {
        return std::sqrt(x*x + y*y + z*z);
    }
    
    Vector3D normalize() const {
        double mag = magnitude();
        if (mag > 0) {
            return Vector3D(x/mag, y/mag, z/mag);
        }
        return *this;
    }
    
    Vector3D operator+(const Vector3D& other) const {
        return Vector3D(x + other.x, y + other.y, z + other.z);
    }
    
    Vector3D operator-(const Vector3D& other) const {
        return Vector3D(x - other.x, y - other.y, z - other.z);
    }
    
    Vector3D operator*(double scalar) const {
        return Vector3D(x * scalar, y * scalar, z * scalar);
    }
    
    Vector3D operator/(double scalar) const {
        return Vector3D(x / scalar, y / scalar, z / scalar);
    }
    
    double operator[](int i) const {
        if (i == 0) return x;
        if (i == 1) return y;
        return z;
    }
    
    double& operator[](int i) {
        if (i == 0) return x;
        if (i == 1) return y;
        return z;
    }
    
    double dot(const Vector3D& other) const {
        return x * other.x + y * other.y + z * other.z;
    }
    
    Vector3D cross(const Vector3D& other) const {
        return Vector3D(
            y * other.z - z * other.y,
            z * other.x - x * other.z,
            x * other.y - y * other.x
        );
    }
};

// Struttura per data/ora giuliana
struct JulianDate {
    double jd;
    
    JulianDate() : jd(0) {}
    explicit JulianDate(double j) : jd(j) {}
    
    double toMJD() const { return jd - 2400000.5; }
    static JulianDate fromMJD(double mjd) { return JulianDate(mjd + 2400000.5); }
};


} // namespace ioccultcalc

#endif // IOCCULTCALC_TYPES_H
