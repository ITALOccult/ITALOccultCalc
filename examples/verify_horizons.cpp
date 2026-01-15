#include "phase2_occultation_geometry.h"
#include "ioccultcalc/spice_spk_reader.h"
#include <iostream>

using namespace ioccultcalc;

int main() {
    auto spk = std::make_shared<SPICESPKReader>();
    spk->loadFile(std::string(getenv("HOME")) + "/.ioccultcalc/ephemerides/de440.bsp");

    Phase2OccultationGeometry p2;
    p2.setSPKReader(spk);
    
    // We need to set the asteroid name/number in the wrapper first
    // so fetchHorizons knows what to query.
    p2.loadAsteroidFromDB(13477); 

    CandidateStar star;
    star.source_id = 35169528681869696;
    star.ra_deg = 2.95043559 * 15.0;
    star.dec_deg = 17.2393536;
    star.closest_approach_mjd = 61060.00657; // Jan 20 approx

    Phase2Config config;
    config.use_horizons = true; // ACTIVATED BY CONFIGURATION
    config.compute_shadow = true;

    std::cout << "\n[TEST] Running Phase 2 with use_horizons=true..." << std::endl;
    auto events = p2.calculatePreciseGeometry({star}, config);

    if (!events.empty()) {
        const auto& evt = events[0];
        std::cout << "[RESULT] Success! CA Dist: " << evt.min_dist_mas << " mas" << std::endl;
    } else {
        std::cout << "[RESULT] Failed to find event." << std::endl;
    }

    return 0;
}
