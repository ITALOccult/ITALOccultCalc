#include <iostream>
#include <vector>
#include <iomanip>
#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/time_utils.h"
#include "astdyn_wrapper.h"
#include "ioccultcalc/occultation_engine.h"

using namespace ioccultcalc;

int main() {
    JPLHorizonsClient client;
    
    // Jan 24, 2026 00:00 UT (MJD 61064.0)
    JulianDate jd = TimeUtils::isoToJD("2026-01-24T00:00:00Z");
    double mjd = jd.jd - 2400000.5;
    
    std::cout << "--- POSITION VERIFICATION FOR 782 MONTEFIORE ---\n";
    std::cout << "Time: " << TimeUtils::jdToISO(jd) << " (MJD " << std::fixed << std::setprecision(5) << mjd << ")\n\n";
    
    // 1. Get JPL Ground Truth
    std::cout << "Step 1: Fetching JPL Horizons Ground Truth...\n";
    try {
        auto app_jpl = client.getApparentCoordinates("782", jd, "500");
        std::cout << "  JPL Apparent (Geocentric):\n";
        std::cout << "    RA:  " << (app_jpl.first * RAD_TO_DEG) << " deg\n";
        std::cout << "    Dec: " << (app_jpl.second * RAD_TO_DEG) << " deg\n\n";
        
        // 2. Local Propagation
        std::cout << "Step 2: Local Propagation with AstDynWrapper...\n";
        
        // Initialize Engine to load elements
        OccultationEngine engine;
        bool loaded = engine.loadAsteroidFromDB(782);
        if (!loaded) {
            loaded = engine.loadAsteroidFromJSON(782);
        }
        
        if (!loaded) {
            std::cerr << "  Error: Could not load elements for 782 Montefiore from DB or JSON.\n";
            return 1;
        }
        
        // Initialize Wrapper
        AstDynWrapper wrapper;
        wrapper.setAsteroidElements(engine.getCurrentElements());
        
        if (!wrapper.isInitialized()) {
            std::cerr << "  Error: Wrapper initialization failed.\n";
            return 1;
        }
        
        // Local Apparent State
        auto app_local = wrapper.getApparentStateGeocentric(mjd);
        
        std::cout << "  Local Result (AstDyn):\n";
        std::cout << "    RA:  " << (app_local.ra_deg) << " deg\n";
        std::cout << "    Dec: " << (app_local.dec_deg) << " deg\n\n";
        
        // 3. Comparison
        double dra = (app_local.ra_deg - (app_jpl.first * RAD_TO_DEG)) * 3600.0;
        double ddec = (app_local.dec_deg - (app_jpl.second * RAD_TO_DEG)) * 3600.0;
        
        std::cout << "Step 3: Comparison Results (Local - JPL):\n";
        std::cout << "  Delta RA:  " << std::setprecision(3) << dra << " arcsec\n";
        std::cout << "  Delta Dec: " << std::setprecision(3) << ddec << " arcsec\n";
        
        if (std::abs(dra) > 10.0 || std::abs(ddec) > 10.0) {
            std::cout << "\n[!] WARNING: Large discrepancy detected! (> 10 arcsec)\n";
        } else {
            std::cout << "\n[*] SUCCESS: Discrepancy is within acceptable limits for propagation.\n";
        }

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
