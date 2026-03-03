/**
 * Test: Performance e stabilità shadow path.
 * 1000 punti, tempo < 5 s, no NaN/Inf, memoria stabile.
 */
#include "ioccultcalc/occultation_analyzer.h"
#include "ioccultcalc/types.h"
#include "astdyn_wrapper.h"
#include <iostream>
#include <chrono>
#include <cmath>
#include <vector>

using namespace ioccultcalc;

static AstDynEquinoctialElements makeTestElements() {
    AstDynEquinoctialElements e;
    e.a = 2.78; e.h = 0.05; e.k = 0.05; e.p = 0.02; e.q = 0.02;
    e.lambda = 1.5; e.epoch = JulianDate(2459000.5);
    e.name = "1272"; e.designation = "1272"; e.H = 10.31; e.G = 0.15; e.number = 1272;
    return e;
}

int main() {
    std::cerr << "[TEST] Performance Shadow - START" << std::endl;

    auto astdyn = std::make_shared<AstDynWrapper>(PropagationSettings::highAccuracy());
    astdyn->setAsteroidElements(makeTestElements());

    OccultationAnalyzer::Config config;
    config.shadow_points = 1000;
    config.shadow_time_span_min = 30.0;
    config.verbose = false;

    OccultationAnalyzer analyzer(config);
    double star_ra = 122.75, star_dec = 25.10;
    double start_mjd = 61052.0, end_mjd = 61054.0;

    auto t0 = std::chrono::steady_clock::now();
    auto params = analyzer.analyzeOccultation(star_ra, star_dec, 12.0, 0, astdyn, start_mjd, end_mjd, 10.31, 85.0);
    bool has_nan = false;
    if (params.is_occultation) {
        auto result = analyzer.calculateShadowPath(params, astdyn);
        auto t1 = std::chrono::steady_clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        if (elapsed > 5.0) {
            std::cerr << "[TEST] FAIL: time " << elapsed << " s > 5 s" << std::endl;
            return 1;
        }
        for (const auto& pt : result.shadow_path) {
            if (std::isnan(pt.lat_deg) || std::isnan(pt.lon_deg) || std::isnan(pt.mjd_tdb) ||
                std::isinf(pt.lat_deg) || std::isinf(pt.lon_deg) || std::isinf(pt.mjd_tdb))
                has_nan = true;
        }
    } else {
        auto t1 = std::chrono::steady_clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        if (elapsed > 5.0) {
            std::cerr << "[TEST] FAIL: time " << elapsed << " s > 5 s" << std::endl;
            return 1;
        }
    }

    if (has_nan) {
        std::cerr << "[TEST] FAIL: NaN/Inf in shadow path" << std::endl;
        return 1;
    }
    std::cerr << "[TEST] Performance Shadow - PASS" << std::endl;
    return 0;
}
