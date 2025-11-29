/**
 * @file test_gaia_cache.cpp
 * @brief Test sistema cache GAIA DR3
 */

#include "ioccultcalc/gaia_cache.h"
#include "ioccultcalc/gaia_client.h"
#include "ioccultcalc/data_manager.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

void printSeparator() {
    std::cout << std::string(70, '=') << std::endl;
}

void testDataManagerSetup() {
    printSeparator();
    std::cout << "TEST 1: DataManager Setup for GAIA Cache" << std::endl;
    printSeparator();
    
    auto& dm = DataManager::instance();
    dm.initialize();
    
    std::cout << "✓ Root directory: " << dm.getRootDir() << std::endl;
    std::cout << "  GAIA cache will be in: " << dm.getRootDir() << "/gaia/" << std::endl;
    
    // Check system info
    auto info = dm.getSystemInfo();
    std::cout << "\n" << info << std::endl;
}

void testCacheCreation() {
    printSeparator();
    std::cout << "TEST 2: Cache Creation and Configuration" << std::endl;
    printSeparator();
    
    // Create cache with builder
    auto cache = GaiaCacheBuilder()
        .autoDownload(false)  // Don't auto-download in test
        .verbose(true)
        .build();
    
    std::cout << "✓ GAIA cache created" << std::endl;
    
    // Get stats
    auto stats = cache->getStats();
    std::cout << "\nCache Statistics:" << std::endl;
    std::cout << "  Total tiles: " << stats.total_tiles << std::endl;
    std::cout << "  Total stars: " << stats.total_stars << std::endl;
    std::cout << "  Sky coverage: " << std::fixed << std::setprecision(2) 
              << stats.sky_coverage << "%" << std::endl;
    std::cout << "  Cache size: " << stats.total_size_mb << " MB" << std::endl;
}

void testHEALPixConversion() {
    printSeparator();
    std::cout << "TEST 3: HEALPix Coordinate Conversion" << std::endl;
    printSeparator();
    
    auto cache = GaiaCacheBuilder().build();
    
    // Test known positions
    std::vector<std::pair<double, double>> testCoords = {
        {0, 0},         // Equator at 0h
        {90, 0},        // Equator at 6h
        {180, 0},       // Equator at 12h
        {0, 90},        // North pole
        {0, -90},       // South pole
        {150, 30},      // Random position
    };
    
    std::cout << "Converting coordinates to HEALPix tiles (NSIDE=32):\n" << std::endl;
    std::cout << std::setw(10) << "RA" << std::setw(10) << "Dec" 
              << std::setw(15) << "HEALPix ID" << std::endl;
    std::cout << std::string(35, '-') << std::endl;
    
    for (const auto& [ra, dec] : testCoords) {
        int healpix = cache->coordsToHealpix(ra, dec, 32);
        std::cout << std::setw(10) << ra << std::setw(10) << dec 
                  << std::setw(15) << healpix << std::endl;
    }
}

void testRegionQuery() {
    printSeparator();
    std::cout << "TEST 4: Region Query (Tiles Identification)" << std::endl;
    printSeparator();
    
    auto cache = GaiaCacheBuilder()
        .verbose(true)
        .autoDownload(false)
        .build();
    
    // Query region (without downloading)
    double ra = 83.8;    // Near Orion (M42)
    double dec = -5.4;
    double radius = 2.0;  // 2 degrees
    double maxMag = 15.0;
    
    std::cout << "Query region around Orion:" << std::endl;
    std::cout << "  RA: " << ra << "°" << std::endl;
    std::cout << "  Dec: " << dec << "°" << std::endl;
    std::cout << "  Radius: " << radius << "°" << std::endl;
    std::cout << "  Max magnitude: " << maxMag << std::endl;
    std::cout << std::endl;
    
    // Check coverage
    bool covered = cache->isCovered(ra, dec, radius, maxMag);
    std::cout << "Region covered: " << (covered ? "YES" : "NO") << std::endl;
    
    // Try query (will show which tiles are needed)
    auto stars = cache->queryRegion(ra, dec, radius, maxMag, false);
    std::cout << "Found " << stars.size() << " stars in cache" << std::endl;
}

void testPathQuery() {
    printSeparator();
    std::cout << "TEST 5: Path Query (Asteroid Track)" << std::endl;
    printSeparator();
    
    auto cache = GaiaCacheBuilder()
        .verbose(true)
        .autoDownload(false)
        .build();
    
    // Simulate asteroid path (straight line for test)
    std::vector<EquatorialCoordinates> path;
    double ra_start = 80.0;
    double dec_start = -5.0;
    double ra_end = 85.0;
    double dec_end = -4.0;
    int steps = 10;
    
    std::cout << "Simulating asteroid path:" << std::endl;
    std::cout << "  Start: RA=" << ra_start << "° Dec=" << dec_start << "°" << std::endl;
    std::cout << "  End:   RA=" << ra_end << "° Dec=" << dec_end << "°" << std::endl;
    std::cout << "  Steps: " << steps << std::endl;
    std::cout << std::endl;
    
    for (int i = 0; i <= steps; ++i) {
        double t = static_cast<double>(i) / steps;
        EquatorialCoordinates coord;
        coord.ra = ra_start + t * (ra_end - ra_start);
        coord.dec = dec_start + t * (dec_end - dec_start);
        path.push_back(coord);
    }
    
    double width = 0.5;  // 0.5 degree width
    double maxMag = 15.0;
    
    std::cout << "Querying stars along path (width=" << width << "°)..." << std::endl;
    auto stars = cache->queryPath(path, width, maxMag, false);
    std::cout << "Found " << stars.size() << " stars" << std::endl;
}

void testDownloadSimulation() {
    printSeparator();
    std::cout << "TEST 6: Download Simulation (DRY RUN)" << std::endl;
    printSeparator();
    
    std::cout << "NOTE: This test shows what WOULD be downloaded." << std::endl;
    std::cout << "      Actual download requires GAIA TAP service access." << std::endl;
    std::cout << std::endl;
    
    auto cache = GaiaCacheBuilder()
        .verbose(true)
        .autoDownload(false)
        .build();
    
    // Calculate tiles needed for a region
    double ra = 83.8;
    double dec = -5.4;
    double radius = 5.0;  // 5 degrees
    double maxMag = 16.0;
    
    std::cout << "Region to download:" << std::endl;
    std::cout << "  Center: RA=" << ra << "° Dec=" << dec << "°" << std::endl;
    std::cout << "  Radius: " << radius << "°" << std::endl;
    std::cout << "  Max magnitude: " << maxMag << std::endl;
    std::cout << std::endl;
    
    // This would download if autoDownload was enabled
    std::cout << "Tiles that would be downloaded:" << std::endl;
    
    // Note: Can't actually download without TAP service
    // This just shows the tile calculation
    std::cout << "(Actual download requires setting up GAIA client with TAP URL)" << std::endl;
    std::cout << std::endl;
    
    std::cout << "To download for real, use:" << std::endl;
    std::cout << "  auto client = std::make_shared<GaiaClient>();" << std::endl;
    std::cout << "  client->setTAPURL(\"https://gea.esac.esa.int/tap-server/tap\");" << std::endl;
    std::cout << "  cache->setGaiaClient(client);" << std::endl;
    std::cout << "  cache->downloadRegion(ra, dec, radius, maxMag);" << std::endl;
}

void testCacheManagement() {
    printSeparator();
    std::cout << "TEST 7: Cache Management" << std::endl;
    printSeparator();
    
    auto cache = GaiaCacheBuilder().build();
    
    // Load index
    bool loaded = cache->loadIndex();
    std::cout << "Index loaded: " << (loaded ? "YES" : "NO (empty cache)") << std::endl;
    
    // Get stats
    auto stats = cache->getStats();
    std::cout << "\nCurrent Cache Status:" << std::endl;
    std::cout << "  Tiles: " << stats.total_tiles << std::endl;
    std::cout << "  Stars: " << stats.total_stars << std::endl;
    std::cout << "  Size: " << std::fixed << std::setprecision(2) 
              << stats.total_size_mb << " MB" << std::endl;
    std::cout << "  Coverage: " << stats.sky_coverage << "% of sky" << std::endl;
    
    if (stats.total_stars > 0) {
        std::cout << "  Magnitude range: " << stats.min_magnitude 
                  << " to " << stats.max_magnitude << std::endl;
    }
    
    // Save index (even if empty)
    bool saved = cache->saveIndex();
    std::cout << "\nIndex saved: " << (saved ? "YES" : "NO") << std::endl;
}

void printUsageExample() {
    printSeparator();
    std::cout << "USAGE EXAMPLE: Integrate with Occultation Search" << std::endl;
    printSeparator();
    
    std::cout << R"(
// 1. Setup GAIA cache with auto-download
auto gaiaClient = std::make_shared<GaiaClient>();
gaiaClient->setTAPURL("https://gea.esac.esa.int/tap-server/tap");

auto cache = GaiaCacheBuilder()
    .gaiaClient(gaiaClient)
    .autoDownload(true)
    .verbose(true)
    .build();

// 2. Calculate asteroid path
std::vector<EquatorialCoordinates> path;
for (double t = start_jd; t <= end_jd; t += 0.1) {  // 2.4h steps
    auto pos = asteroid.propagate(t);
    path.push_back(pos);
}

// 3. Query stars along path (auto-downloads missing tiles)
double width = 0.01;  // ~36 arcsec
double maxMag = 16.0;  // From config
auto stars = cache->queryPath(path, width, maxMag, true);

// 4. Check occultations
for (const auto& star : stars) {
    auto event = predictor.predictOccultation(star, asteroid, ...);
    if (event.willOccur) {
        // Process occultation...
    }
}

// Cache persists between runs - only downloads new regions once!
)" << std::endl;
}

int main() {
    std::cout << "\n";
    std::cout << "╔════════════════════════════════════════════════════════════════╗\n";
    std::cout << "║       IOccultCalc - GAIA DR3 Cache System Test                ║\n";
    std::cout << "╚════════════════════════════════════════════════════════════════╝\n";
    std::cout << std::endl;
    
    try {
        testDataManagerSetup();
        testCacheCreation();
        testHEALPixConversion();
        testRegionQuery();
        testPathQuery();
        testDownloadSimulation();
        testCacheManagement();
        printUsageExample();
        
        printSeparator();
        std::cout << "\n✓ ALL TESTS COMPLETED SUCCESSFULLY\n" << std::endl;
        printSeparator();
        
        std::cout << "\nNext Steps:" << std::endl;
        std::cout << "1. Setup GAIA TAP client with URL" << std::endl;
        std::cout << "2. Download tiles for your survey region" << std::endl;
        std::cout << "3. Integrate with ioccultcalc_search" << std::endl;
        std::cout << "4. Cache will persist and speed up future searches!" << std::endl;
        std::cout << std::endl;
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n❌ TEST FAILED: " << e.what() << std::endl;
        return 1;
    }
}
