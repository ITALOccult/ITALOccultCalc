#include "astdyn_wrapper.h"
#include <iostream>
#include <iomanip>
#include <cmath>

int main() {
    using namespace ioccultcalc;
    
    // Initialize AstDyn
    auto astdyn = std::make_shared<AstDynWrapper>();
    
    // Asteroid 13477 (Utkin)
    std::string name = "13477";
    double epoch_mjd = 61041.0; // Reference epoch
    
    std::cout << "--- Propagation Test vs JPL Horizons ---" << std::endl;
    std::cout << "Asteroid: " << name << std::endl;
    std::cout << "Reference Epoch: " << epoch_mjd << std::endl;
    
    // 1. Fetch elements from Horizons at reference epoch
    if (!astdyn->loadFromHorizons(name, epoch_mjd)) {
        std::cerr << "Failed to fetch Horizons elements for reference epoch!" << std::endl;
        return 1;
    }
    
    // Get state at reference epoch from propagator (should match Horizons)
    auto state0 = astdyn->getApparentStateGeocentric(epoch_mjd);
    std::cout << "\n[T = 0] (Epoch " << epoch_mjd << ")" << std::endl;
    std::cout << "  Propagator RA:  " << std::fixed << std::setprecision(6) << state0.ra_deg << " deg" << std::endl;
    std::cout << "  Propagator Dec: " << std::fixed << std::setprecision(6) << state0.dec_deg << " deg" << std::endl;
    std::cout << "  Propagator Dist: " << std::fixed << std::setprecision(6) << state0.distance_au << " AU" << std::endl;

    // 2. Sample points until CA date
    double ca_mjd = 61060.0064; // ItalOccultCalc CA
    double occ4_mjd = 61060.1395; // Occult4 CA
    
    double test_dates[] = {61050.0, 61055.0, 61059.0, ca_mjd, occ4_mjd};
    
    for (double t : test_dates) {
        std::cout << "\n[T = " << (t - epoch_mjd) << " days] (MJD " << t << ")" << std::endl;
        
        // Propagated state
        auto state_p = astdyn->getApparentStateGeocentric(t);
        
        // Fetch truth from Horizons at this specific time
        auto astdyn_truth = std::make_shared<AstDynWrapper>();
        astdyn_truth->loadFromHorizons(name, t);
        auto state_h = astdyn_truth->getApparentStateGeocentric(t);
        
        double dra = (state_p.ra_deg - state_h.ra_deg) * 3600.0 * std::cos(state_h.dec_deg * M_PI / 180.0);
        double ddec = (state_p.dec_deg - state_h.dec_deg) * 3600.0;
        double ddist = (state_p.distance_au - state_h.distance_au);
        
        std::cout << "  Propagated: RA=" << state_p.ra_deg << " Dec=" << state_p.dec_deg << " Dist=" << state_p.distance_au << std::endl;
        std::cout << "  Horizons:   RA=" << state_h.ra_deg << " Dec=" << state_h.dec_deg << " Dist=" << state_h.distance_au << std::endl;
        std::cout << "  DIFF (mas):  dRA=" << std::fixed << std::setprecision(2) << dra*1000.0 
                  << " dDec=" << ddec*1000.0 << " dDist=" << ddist << std::endl;
    }
    
    return 0;
}
