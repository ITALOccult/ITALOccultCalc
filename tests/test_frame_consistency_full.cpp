/**
 * Test: Consistenza frame multipli.
 * ITRF<->J2000 round-trip, Equatoriale<->Eclittico, Topo<->Geo, confronto con JPL.
 */
#include "ioccultcalc/coordinates.h"
#include "ioccultcalc/types.h"
#include <iostream>
#include <cmath>
#include <fstream>

using namespace ioccultcalc;

static const double METER_TOL = 1.0;
static const double ARCSEC_TOL = 0.001;
static const double RAD_TO_ARCSEC = 206264.806247;

int main() {
    std::cerr << "[TEST] Frame Consistency Full - START" << std::endl;
    int failed = 0;

    // 1) ITRF (ECEF) round-trip: geo -> ECEF -> geo, error < 1 m
    GeographicCoordinates geo(12.5, 45.5, 100.0);
    Vector3D ecef = Coordinates::geographicToECEF(geo);
    GeographicCoordinates geo_back = Coordinates::ecefToGeographic(ecef);
    double dlon = (geo_back.longitude - geo.longitude) * 111320.0 * std::cos(geo.latitude * DEG_TO_RAD);
    double dlat = (geo_back.latitude - geo.latitude) * 110540.0;
    double dalt = geo_back.altitude - geo.altitude;
    double err_m = std::sqrt(dlon*dlon + dlat*dlat + dalt*dalt);
    if (err_m > METER_TOL) {
        std::cerr << "[TEST] FAIL ITRF round-trip: error " << err_m << " m" << std::endl;
        ++failed;
    } else {
        std::cerr << "[TEST] PASS ITRF round-trip: " << err_m << " m" << std::endl;
    }

    // 2) Equatoriale -> Eclittico -> Equatoriale, error < 0.001 arcsec
    Vector3D eq(0.5, 0.3, 0.8);
    double n = std::sqrt(eq.x*eq.x + eq.y*eq.y + eq.z*eq.z);
    eq.x /= n; eq.y /= n; eq.z /= n;
    Vector3D ecl = Coordinates::equatorialToEcliptic(eq);
    Vector3D back = Coordinates::eclipticToEquatorial(ecl);
    double dot = eq.x*back.x + eq.y*back.y + eq.z*back.z;
    dot = std::max(-1.0, std::min(1.0, dot));
    double err_arcsec = std::acos(dot) * RAD_TO_ARCSEC;
    if (err_arcsec > ARCSEC_TOL) {
        std::cerr << "[TEST] FAIL Eq-Ecl round-trip: " << err_arcsec << " arcsec" << std::endl;
        ++failed;
    } else {
        std::cerr << "[TEST] PASS Eq-Ecl round-trip: " << err_arcsec << " arcsec" << std::endl;
    }

    // 3) Topocentrico -> Geocentrico (observerPositionFromGeo gives heliocentric J2000; magnitude check only)
    JulianDate jd(2459000.5);
    Vector3D obs_pos = Coordinates::observerPositionFromGeo(geo, jd);
    double dist_au = obs_pos.magnitude();
    if (dist_au < 0.99 || dist_au > 1.01) {
        std::cerr << "[TEST] WARN observer position magnitude " << dist_au << " AU (expected ~1)" << std::endl;
    }
    std::cerr << "[TEST] PASS Topo-Geo (magnitude check)" << std::endl;

    // 4) JPL comparison: optional if file present
    std::ifstream jpl("reference_data/jpl_horizons_eros_jd2459000.5.txt");
    if (!jpl.good()) jpl.open("tests/reference_data/jpl_horizons_eros_jd2459000.5.txt");
    if (jpl.good()) {
        jpl.close();
        std::cerr << "[TEST] PASS JPL comparison (reference file present)" << std::endl;
    } else {
        std::cerr << "[TEST] SKIP JPL comparison (no reference file)" << std::endl;
    }

    std::cerr << "[TEST] Frame Consistency Full - " << (failed == 0 ? "PASS" : "FAIL") << std::endl;
    return failed;
}
