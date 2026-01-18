#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/coordinates.h"
#include "ioccultcalc/spice_spk_reader.h"
#include "ioccultcalc/time_utils.h"
#include "ioccultcalc/types.h"
#include "ioccultcalc/star_catalog.h"
#include "astdyn/ephemeris/PlanetaryEphemeris.hpp"
#include "astdyn/ephemeris/EphemerisProvider.hpp"
#include <cmath>
#include <stdexcept>
#include <iostream>
#include <algorithm>

namespace ioccultcalc {

namespace AstEphemeris = astdyn::ephemeris;

// Bridge provider
class SpiceEphemerisProvider : public astdyn::ephemeris::EphemerisProvider {
public:
    SpiceEphemerisProvider(std::shared_ptr<ISPReader> reader) : reader_(reader) {}
    static int toNaif(astdyn::ephemeris::CelestialBody body) {
        switch (body) {
            case astdyn::ephemeris::CelestialBody::SUN:     return 10;
            case astdyn::ephemeris::CelestialBody::EARTH:   return 399;
            case astdyn::ephemeris::CelestialBody::MOON:    return 301;
            default: return (int)body; // Simplistic fallback
        }
    }
    Eigen::Vector3d getPosition(astdyn::ephemeris::CelestialBody body, double jd_tdb) override {
        if (!reader_) return Eigen::Vector3d::Zero();
        Vector3D pos = reader_->getPosition(toNaif(body), jd_tdb, 10);
        return Eigen::Vector3d(pos.x, pos.y, pos.z);
    }
    Eigen::Vector3d getVelocity(astdyn::ephemeris::CelestialBody body, double jd_tdb) override {
        if (!reader_) return Eigen::Vector3d::Zero();
        auto state = reader_->getState(toNaif(body), jd_tdb, 10);
        return Eigen::Vector3d(state.second.x, state.second.y, state.second.z);
    }
    std::string getName() const override { return "SPICE (Bridge)"; }
    double getAccuracy() const override { return 0.001; }
    bool isAvailable() const override { return reader_ && reader_->isLoaded(); }
private:
    std::shared_ptr<ISPReader> reader_;
};

static void ensureGlobalProvider(std::shared_ptr<ISPReader> reader) {
    static bool initialized = false;
    if (!initialized && reader) {
        auto provider = std::make_shared<SpiceEphemerisProvider>(reader);
        astdyn::ephemeris::PlanetaryEphemeris::setProvider(provider);
        initialized = true;
    }
}

constexpr double GAUSS_K = 0.01720209895;
constexpr double C_AU_PER_DAY = 173.1446326846693;

Ephemeris::Ephemeris() : spkReader_(nullptr) {}
Ephemeris::Ephemeris(const AstDynEquinoctialElements& elements) : spkReader_(nullptr), elements_(elements) {}
Ephemeris::Ephemeris(std::shared_ptr<ISPReader> reader) : spkReader_(reader) { ensureGlobalProvider(reader); }
Ephemeris::Ephemeris(std::shared_ptr<ISPReader> reader, const AstDynEquinoctialElements& elements) : spkReader_(reader), elements_(elements) { ensureGlobalProvider(reader); }

void Ephemeris::setElements(const AstDynEquinoctialElements& elements) { elements_ = elements; }

Vector3D Ephemeris::getEarthPosition(const JulianDate& jd) {
    auto posEigen = AstEphemeris::PlanetaryEphemeris::getPosition(AstEphemeris::CelestialBody::EARTH, jd.jd);
    return Vector3D(posEigen.x(), posEigen.y(), posEigen.z());
}

Vector3D Ephemeris::getEarthVelocity(const JulianDate& jd) {
    auto velEigen = AstEphemeris::PlanetaryEphemeris::getVelocity(AstEphemeris::CelestialBody::EARTH, jd.jd);
    return Vector3D(velEigen.x(), velEigen.y(), velEigen.z());
}

Vector3D Ephemeris::getSunPosition(const JulianDate& jd) { return Vector3D(0, 0, 0); }

EphemerisData Ephemeris::compute(const JulianDate& jd) {
    EphemerisData data;
    data.jd = jd;
    Vector3D earthPos = getEarthPosition(jd);
    double lightTime = 0.0;
    Vector3D helioPos, helioVel;
    for (int i = 0; i < 3; ++i) {
        propagateOrbit(JulianDate(jd.jd - lightTime), helioPos, helioVel);
        Vector3D relPos = helioPos - earthPos;
        lightTime = relPos.magnitude() / C_AU_PER_DAY;
    }
    data.heliocentricPos = helioPos;
    data.heliocentricVel = helioVel;
    Vector3D geocentricVec = helioPos - earthPos;
    data.distance = geocentricVec.magnitude();
    data.geocentricPos = Coordinates::cartesianToEquatorial(geocentricVec);
    Vector3D sunPosEq = earthPos * -1.0; 
    data.elongation = std::acos(std::clamp(geocentricVec.normalize().dot(sunPosEq.normalize()), -1.0, 1.0)) * RAD_TO_DEG;
    Vector3D toEarth = geocentricVec * -1.0;
    data.phase = std::acos(std::clamp(helioPos.normalize().dot(toEarth.normalize()), -1.0, 1.0)) * RAD_TO_DEG;
    data.magnitude = calculateMagnitude(helioPos.magnitude(), data.distance, data.phase);
    return data;
}

EphemerisData Ephemeris::computeTopocentric(const JulianDate& jd, double lat, double lon, double alt_m) {
    EphemerisData data = compute(jd); 
    Vector3D observerPos = Coordinates::observerPositionFromGeo(GeographicCoordinates(lon, lat, alt_m), jd);
    Vector3D topocentricVec = data.heliocentricPos - observerPos;
    data.distance = topocentricVec.magnitude();
    data.geocentricPos = Coordinates::cartesianToEquatorial(topocentricVec);
    return data;
}

std::vector<EphemerisData> Ephemeris::computeRange(const JulianDate& startJD, const JulianDate& endJD, double stepDays) {
    std::vector<EphemerisData> results;
    for (double jd = startJD.jd; jd <= endJD.jd; jd += stepDays) results.push_back(compute(JulianDate(jd)));
    return results;
}

void Ephemeris::propagateOrbit(const JulianDate& targetJD, Vector3D& helioPos, Vector3D& helioVel) {
    double dt = targetJD.jd - elements_.epoch.jd;
    double n = GAUSS_K / sqrt(elements_.a * elements_.a * elements_.a);
    double lambda_t = elements_.lambda + n * dt;
    double omega_plus_Omega = atan2(elements_.h, elements_.k);
    double M = lambda_t - omega_plus_Omega;
    double e = sqrt(elements_.h * elements_.h + elements_.k * elements_.k);
    double E = solveKeplerEquation(M, e);
    double cosNu = (cos(E) - e) / (1.0 - e * cos(E));
    double sinNu = sqrt(1.0 - e * e) * sin(E) / (1.0 - e * cos(E));
    double nu = atan2(sinNu, cosNu);
    double r = elements_.a * (1.0 - e * cos(E));
    double x_orb = r * cos(nu);
    double y_orb = r * sin(nu);
    double v_factor = GAUSS_K * sqrt(elements_.a) / r;
    double vx_orb = -v_factor * sin(E);
    double vy_orb = v_factor * sqrt(1.0 - e * e) * cos(E);
    double f = 1.0 + elements_.p * elements_.p + elements_.q * elements_.q;
    double m11 = (1.0 - elements_.p * elements_.p + elements_.q * elements_.q) / f;
    double m12 = 2.0 * elements_.p * elements_.q / f;
    double m13 = -2.0 * elements_.p / f;
    double m21 = 2.0 * elements_.p * elements_.q / f;
    double m22 = (1.0 + elements_.p * elements_.p - elements_.q * elements_.q) / f;
    double m23 = 2.0 * elements_.q / f;
    double m31 = 2.0 * elements_.p / f;
    double m32 = -2.0 * elements_.q / f;
    double m33 = (1.0 - elements_.p * elements_.p - elements_.q * elements_.q) / f;
    double cos_wp = cos(omega_plus_Omega);
    double sin_wp = sin(omega_plus_Omega);
    double x_ref = x_orb * cos_wp - y_orb * sin_wp;
    double y_ref = x_orb * sin_wp + y_orb * cos_wp;
    double z_ref = 0;
    double vx_ref = vx_orb * cos_wp - vy_orb * sin_wp;
    double vy_ref = vx_orb * sin_wp + vy_orb * cos_wp;
    double vz_ref = 0;
    double x_ecl = m11 * x_ref + m12 * y_ref + m13 * z_ref;
    double y_ecl = m21 * x_ref + m22 * y_ref + m23 * z_ref;
    double z_ecl = m31 * x_ref + m32 * y_ref + m33 * z_ref;
    double vx_ecl = m11 * vx_ref + m12 * vy_ref + m13 * vz_ref;
    double vy_ecl = m21 * vx_ref + m22 * vy_ref + m23 * vz_ref;
    double vz_ecl = m31 * vx_ref + m32 * vy_ref + m33 * vz_ref;
    constexpr double OBL_J2000 = OBLIQUITY_J2000;
    double cos_eps = std::cos(OBL_J2000);
    double sin_eps = std::sin(OBL_J2000);
    helioPos = Vector3D(x_ecl, y_ecl * cos_eps - z_ecl * sin_eps, y_ecl * sin_eps + z_ecl * cos_eps);
    helioVel = Vector3D(vx_ecl, vy_ecl * cos_eps - vz_ecl * sin_eps, vy_ecl * sin_eps + vz_ecl * cos_eps);
}

double Ephemeris::solveKeplerEquation(double M, double e, double tolerance) {
    M = std::fmod(M, TWO_PI);
    double E = (e > 0.8) ? PI : M;
    for (int i = 0; i < 100; i++) {
        double f = E - e * sin(E) - M;
        double fp = 1.0 - e * cos(E);
        if (std::abs(fp) < 1e-12) return E; 
        double d = f / fp;
        E -= d;
        if (std::abs(d) < tolerance) return E;
    }
    return E;
}

double Ephemeris::calculateMagnitudeHG(double H, double G, double r, double delta, double alpha_rad) {
    double tan_half_alpha = std::tan(alpha_rad / 2.0);
    if (tan_half_alpha < 0) tan_half_alpha = 0;
    double phi1 = std::exp(-3.33 * std::pow(std::tan(alpha_rad / 2.0), 0.63));
    double phi2 = std::exp(-1.87 * std::pow(std::tan(alpha_rad / 2.0), 1.22));
    double phase_function = (1.0 - G) * phi1 + G * phi2;
    if (phase_function <= 1e-10) phase_function = 1e-10;
    return H + 5.0 * std::log10(r * delta) - 2.5 * std::log10(phase_function);
}

double Ephemeris::calculateMagnitude(double r, double delta, double phaseAngle) {
    return calculateMagnitudeHG(elements_.H, elements_.G, r, delta, phaseAngle * DEG_TO_RAD);
}

Vector3D Ephemeris::applyStellarParallax(const Vector3D& starUnitVector, double parallax_mas, const Vector3D& earthHelioPos) {
    if (parallax_mas <= 0) return starUnitVector;
    double p_rad = (parallax_mas / 1000.0) * ARCSEC_TO_RAD;
    return (starUnitVector * (1.0 / std::sin(p_rad)) - earthHelioPos).normalize();
}

void Ephemeris::applyProperMotion(double& ra, double& dec, double pm_ra, double pm_dec, double t0, double t1) {
    double dt = (t1 - t0) / 365.25;
    const double M2R = ARCSEC_TO_RAD / 1000.0;
    ra += (pm_ra * M2R * dt) / std::cos(dec);
    dec += pm_dec * M2R * dt;
    ra = std::fmod(ra, TWO_PI);
    if (ra < 0) ra += TWO_PI;
}

bool Ephemeris::predictOccultation(const JulianDate& jd, const StarData& star, const GeographicCoordinates& obs, double radius) {
    double ra = star.position.ra, dec = star.position.dec;
    applyProperMotion(ra, dec, star.properMotion.pmra, star.properMotion.pmdec, star.epoch.jd, jd.jd);
    Vector3D sDir = applyStellarParallax(Vector3D(cos(ra)*cos(dec), sin(ra)*cos(dec), sin(dec)), star.parallax, getEarthPosition(jd));
    EphemerisData ast = computeTopocentric(jd, obs.latitude, obs.longitude, obs.altitude);
    Vector3D aDir(cos(ast.geocentricPos.ra)*cos(ast.geocentricPos.dec), sin(ast.geocentricPos.ra)*cos(ast.geocentricPos.dec), sin(ast.geocentricPos.dec));
    return std::acos(std::clamp(sDir.dot(aDir), -1.0, 1.0)) < (radius / AU) / ast.distance;
}

// Dummy stubs for unused private methods to avoid linking errors if they are called (they are not)
double Ephemeris::calculateObliquity(double jd) const { return 0; }
Vector3D Ephemeris::eclipticToEquatorial(const Vector3D& posEcl, double eps) const { return posEcl; }
Vector3D Ephemeris::getObserverPosition(double lat_deg, double lon_deg, double alt_m, double jd) const { return Vector3D(); }
Vector3D Ephemeris::getEarthPositionEcl(double jd) const { return Vector3D(); }

} // namespace ioccultcalc
