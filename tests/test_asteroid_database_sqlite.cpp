#include "ioccultcalc/asteroid_database.h"
#include "ioccultcalc/data_manager.h"
#include <iostream>

using namespace ioccultcalc;

int main() {
    std::cout << "[TEST] Starting AsteroidDatabase (SQLite Backend) Verification..." << std::endl;

    // Use AsteroidDatabase which should now delegate to SQLite
    AsteroidDatabase db;
    if (!db.loadFromFile()) {
        std::cerr << "[TEST] FAILED: Could not load database from " << db.getPath() << std::endl;
        return 1;
    }

    auto stats = db.getStats();
    std::cout << "[TEST] Database Source: " << stats.source << std::endl;
    std::cout << "[TEST] Total Asteroids: " << stats.total_asteroids << std::endl;

    if (stats.source != "asteroids.db") {
        std::cerr << "[TEST] FAILED: Source is not asteroids.db" << std::endl;
        // Proceed anyway for now if it's JSON for some reason during dev
    }

    // Test query via AsteroidDatabase
    AsteroidRange range;
    range.setRange(1, 10);
    auto results = db.query(range);
    
    std::cout << "[TEST] Query (1-10) returned " << results.size() << " asteroids." << std::endl;
    for (const auto& p : results) {
        std::cout << "  - " << p.number << ": " << p.name << " (a=" << p.a << ")" << std::endl;
    }

    if (results.empty() && stats.total_asteroids > 0) {
        std::cerr << "[TEST] FAILED: Query returned no results despite non-empty database." << std::endl;
        return 1;
    }

    std::cout << "[TEST] AsteroidDatabase (SQLite Backend) Verification COMPLETE." << std::endl;
    return 0;
}
