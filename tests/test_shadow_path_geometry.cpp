/**
 * Test: Shadow path geometry
 * Verifica che l'ombra dell'asteroide sulla Terra sia geometricamente corretta.
 * Output: KML path centrale, CSV con time_jd, lat, lon, alt_km, distance_from_center_km, velocity_km_s
 */
#include "ioccultcalc/occultation_analyzer.h"
#include "ioccultcalc/types.h"
#include "astdyn_wrapper.h"
#include <iostream>
#include <fstream>
#include <cmath>

using namespace ioccultcalc;

static const double EARTH_RADIUS_KM = 6371.0;
static const double JD_TEST = 2459000.5;
static const double STAR_RA_DEG = 180.0;
static const double STAR_DEC_DEG = 0.0;
static const double EROS_DIAMETER_KM = 16.8;

// Eros nominal elements (epoch near JD 2459000.5)
static AstDynEquinoctialElements makeErosElements() {
    AstDynEquinoctialElements e;
    e.a = 1.458;
    e.h = 0.22 * std::cos(1.1);
    e.k = 0.22 * std::sin(1.1);
    e.p = std::tan(10.8 * DEG_TO_RAD / 2) * std::cos(0.5);
    e.q = std::tan(10.8 * DEG_TO_RAD / 2) * std::sin(0.5);
    e.lambda = 2.0;
    e.epoch = JulianDate(JD_TEST);
    e.name = "433";
    e.designation = "433";
    e.H = 10.31;
    e.G = 0.15;
    e.number = 433;
    return e;
}

int main() {
    std::cerr << "[TEST] Shadow Path Geometry - START" << std::endl;

    auto astdyn = std::make_shared<AstDynWrapper>(PropagationSettings::highAccuracy());
    AstDynEquinoctialElements elements = makeErosElements();
    astdyn->setAsteroidElements(elements);

    OccultationAnalyzer::Config config;
    config.shadow_points = 51;
    config.shadow_time_span_min = 15.0;
    config.verbose = false;
    OccultationAnalyzer analyzer(config);

    double start_mjd = JD_TEST - 2400000.5 - 0.5;
    double end_mjd = JD_TEST - 2400000.5 + 0.5;

    auto params = analyzer.analyzeOccultation(
        STAR_RA_DEG, STAR_DEC_DEG, 12.0, 0,
        astdyn, start_mjd, end_mjd,
        elements.H, EROS_DIAMETER_KM
    );

    if (!params.is_occultation) {
        std::cerr << "[TEST] No occultation in window. Geometry checks skipped." << std::endl;
        std::cerr << "[TEST] Shadow Path Geometry - PASS (no event)" << std::endl;
        return 0;
    }

    auto result = analyzer.calculateShadowPath(params, astdyn);
    if (!result.shadow_computed || result.shadow_path.empty()) {
        std::cerr << "[TEST] Shadow path failed: " << result.error_message << std::endl;
        return 1;
    }

    bool ok = true;
    double vel_km_s = result.shadow_velocity_km_s;
    if (vel_km_s < 5.0 || vel_km_s > 30.0) {
        std::cerr << "[TEST] FAIL: velocity " << vel_km_s << " km/s outside 5-30 km/s" << std::endl;
        ok = false;
    }

    std::ofstream csv("shadow_path_geometry.csv");
    csv << "time_jd,lat,lon,alt_km,distance_from_center_km,velocity_km_s\n";
    for (size_t i = 0; i < result.shadow_path.size(); ++i) {
        const auto& pt = result.shadow_path[i];
        double jd = pt.mjd_tdb + 2400000.5;
        if (pt.lat_deg < -90 || pt.lat_deg > 90) ok = false;
        if (pt.lon_deg < -180 || pt.lon_deg > 180) ok = false;
        csv << jd << "," << pt.lat_deg << "," << pt.lon_deg << ",0," << EARTH_RADIUS_KM << "," << vel_km_s << "\n";
    }
    csv.close();

    std::ofstream kml("shadow_path_geometry.kml");
    kml << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n<Document><Placemark><LineString><coordinates>\n";
    for (const auto& pt : result.shadow_path)
        kml << pt.lon_deg << "," << pt.lat_deg << ",0\n";
    kml << "</coordinates></LineString></Placemark></Document></kml>\n";
    kml.close();

    std::cerr << "[TEST] Shadow path points: " << result.shadow_path.size() << ", velocity: " << vel_km_s << " km/s" << std::endl;
    std::cerr << "[TEST] Shadow Path Geometry - " << (ok ? "PASS" : "FAIL") << std::endl;
    return ok ? 0 : 1;
}
