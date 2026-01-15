#include "ioccultcalc/asteroid_sqlite_db.h"
#include "ioccultcalc/occultation_engine.h"
#include "ioccultcalc/data_manager.h"
#include <iostream>

using namespace ioccultcalc;

int main() {
    std::cout << "[TEST] Starting AsteroidSqliteDatabase Verification..." << std::endl;

    AsteroidSqliteDatabase db;
    if (!db.isAvailable()) {
        std::cerr << "[TEST] FAILED: database not available at " << DataManager::instance().getAsteroidDatabasePath() << std::endl;
        return 1;
    }

    int count = db.getAsteroidCount();
    std::cout << "[TEST] Database contains " << count << " asteroids." << std::endl;

    // Test 1: Get Ceres (1)
    auto ceres = db.getProperties(1);
    if (ceres) {
        std::cout << "[TEST] Found Ceres: " << ceres->name << " (H=" << ceres->H << ")" << std::endl;
        std::cout << "       a=" << ceres->a << " e=" << ceres->e << " i=" << ceres->i << std::endl;
    } else {
        std::cout << "[TEST] Ceres (1) not found." << std::endl;
    }

    // Test 2: Get Orbital Elements
    auto gefion = db.getOrbitalElements(1272);
    if (gefion) {
        std::cout << "[TEST] Found Gefion (1272): " << gefion->name << std::endl;
        std::cout << "       a=" << gefion->a << " e=" << gefion->e << " i_rad=" << gefion->i << std::endl;
        std::cout << "       epoch_jd=" << gefion->epoch.jd << std::endl;
    } else {
        std::cout << "[TEST] Gefion (1272) not found." << std::endl;
    }

    // Test 3: OccultationEngine integration
    OccultationEngine engine;
    std::cout << "[TEST] Loading Gefion into OccultationEngine from DB..." << std::endl;
    if (engine.loadAsteroidFromDB(1272)) {
        auto elements = engine.getCurrentElements();
        std::cout << "[TEST] SUCCESS: Gefion loaded. a=" << elements.a << " epoch_jd=" << elements.epoch.jd << std::endl;
    } else {
        std::cout << "[TEST] FAILED: Could not load Gefion from DB." << std::endl;
    }

    std::cout << "[TEST] AsteroidSqliteDatabase Verification COMPLETE." << std::endl;
    return 0;
}
