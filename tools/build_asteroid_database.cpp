/**
 * @file build_asteroid_database.cpp
 * @brief Tool per costruire database locale asteroidi da sorgenti online
 * 
 * Uso:
 *   ./build_asteroid_database --source MPC --output asteroid_db.json
 *   ./build_asteroid_database --source ASTDYS --from 1 --to 10000
 *   ./build_asteroid_database --update
 */

#include <iostream>
#include <iomanip>
#include <chrono>
#include "ioccultcalc/asteroid_database.h"
#include "ioccultcalc/asteroid_filter.h"

using namespace ioccultcalc;

void printUsage() {
    std::cout << "\n╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║  IOccultCalc Asteroid Database Builder                   ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n\n";
    
    std::cout << "Usage:\n\n";
    
    std::cout << "  Download full MPC database:\n";
    std::cout << "    ./build_asteroid_database --source MPC\n\n";
    
    std::cout << "  Download AstDyS range:\n";
    std::cout << "    ./build_asteroid_database --source ASTDYS --from 1 --to 10000\n\n";
    
    std::cout << "  Build filtered subset:\n";
    std::cout << "    ./build_asteroid_database --source MPC \\\n";
    std::cout << "      --filter \"diameter > 50\" \\\n";
    std::cout << "      --output large_asteroids.json\n\n";
    
    std::cout << "  Update existing database:\n";
    std::cout << "    ./build_asteroid_database --update\n\n";
    
    std::cout << "  Statistics:\n";
    std::cout << "    ./build_asteroid_database --stats\n\n";
    
    std::cout << "Options:\n";
    std::cout << "  --source <MPC|ASTDYS|JPL>  Data source\n";
    std::cout << "  --from <N>                 Start of range\n";
    std::cout << "  --to <N>                   End of range\n";
    std::cout << "  --filter <condition>       Filter condition (can repeat)\n";
    std::cout << "  --output <file>            Output file path\n";
    std::cout << "  --update                   Update existing database\n";
    std::cout << "  --stats                    Show database statistics\n";
    std::cout << "  --max-age <days>           Update if older than N days\n";
    std::cout << "  --help                     Show this help\n\n";
    
    std::cout << "Filter Examples:\n";
    std::cout << "  \"diameter > 50\"            Large asteroids\n";
    std::cout << "  \"H < 10\"                   Bright asteroids\n";
    std::cout << "  \"orbit_class in [MBA]\"     Main Belt only\n";
    std::cout << "  \"e < 0.3\"                  Low eccentricity\n\n";
}

void printProgress(int current, int total, const std::string& message) {
    if (total > 0) {
        int percent = (current * 100) / total;
        int barWidth = 50;
        int pos = (barWidth * current) / total;
        
        std::cout << "\r[";
        for (int i = 0; i < barWidth; ++i) {
            if (i < pos) std::cout << "█";
            else if (i == pos) std::cout << ">";
            else std::cout << " ";
        }
        std::cout << "] " << std::setw(3) << percent << "% ";
        std::cout << current << "/" << total;
        if (!message.empty()) {
            std::cout << " - " << message;
        }
        std::cout << std::flush;
    } else {
        std::cout << "\r" << message << std::flush;
    }
}

void printStats(const DatabaseStats& stats) {
    std::cout << "\n╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║              Database Statistics                          ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n\n";
    
    std::cout << "  Total asteroids:       " << stats.total_asteroids << "\n";
    std::cout << "  Numbered:              " << stats.numbered << "\n";
    std::cout << "  Unnumbered:            " << stats.unnumbered << "\n";
    std::cout << "\n";
    std::cout << "  With diameter:         " << stats.with_diameter 
              << " (" << (stats.total_asteroids > 0 ? 
                        (stats.with_diameter * 100 / stats.total_asteroids) : 0) 
              << "%)\n";
    std::cout << "  With albedo:           " << stats.with_albedo 
              << " (" << (stats.total_asteroids > 0 ? 
                        (stats.with_albedo * 100 / stats.total_asteroids) : 0) 
              << "%)\n";
    std::cout << "  With spectral type:    " << stats.with_spectral_type 
              << " (" << (stats.total_asteroids > 0 ? 
                        (stats.with_spectral_type * 100 / stats.total_asteroids) : 0) 
              << "%)\n";
    std::cout << "  With rotation period:  " << stats.with_rotation_period 
              << " (" << (stats.total_asteroids > 0 ? 
                        (stats.with_rotation_period * 100 / stats.total_asteroids) : 0) 
              << "%)\n";
    std::cout << "\n";
    std::cout << "  Last update:           " << stats.last_update << "\n";
    std::cout << "  Source:                " << stats.source << "\n";
    std::cout << "\n";
}

int main(int argc, char* argv[]) {
    // Parse arguments
    std::string source = "MPC";
    std::string outputPath = "";
    int from = -1;
    int to = -1;
    std::vector<std::string> filters;
    bool doUpdate = false;
    bool showStats = false;
    int maxAge = 30;
    
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        
        if (arg == "--help" || arg == "-h") {
            printUsage();
            return 0;
        } else if (arg == "--source" && i + 1 < argc) {
            source = argv[++i];
        } else if (arg == "--from" && i + 1 < argc) {
            from = std::stoi(argv[++i]);
        } else if (arg == "--to" && i + 1 < argc) {
            to = std::stoi(argv[++i]);
        } else if (arg == "--filter" && i + 1 < argc) {
            filters.push_back(argv[++i]);
        } else if (arg == "--output" && i + 1 < argc) {
            outputPath = argv[++i];
        } else if (arg == "--update") {
            doUpdate = true;
        } else if (arg == "--stats") {
            showStats = true;
        } else if (arg == "--max-age" && i + 1 < argc) {
            maxAge = std::stoi(argv[++i]);
        } else {
            std::cerr << "Unknown option: " << arg << "\n";
            printUsage();
            return 1;
        }
    }
    
    // Create database instance
    AsteroidDatabase db(outputPath);
    
    // Show statistics mode
    if (showStats) {
        if (!db.loadFromFile()) {
            std::cerr << "Error: Cannot load database from " << db.getPath() << "\n";
            std::cerr << "Run with --source MPC to download initial database.\n";
            return 1;
        }
        
        auto stats = db.getStats();
        printStats(stats);
        
        std::cout << "Database path: " << db.getPath() << "\n";
        std::cout << "Memory usage:  " << (db.getMemoryUsage() / 1024 / 1024) << " MB\n\n";
        
        return 0;
    }
    
    // Update mode
    if (doUpdate) {
        std::cout << "Checking for updates...\n";
        
        if (!db.loadFromFile()) {
            std::cerr << "Error: No existing database found.\n";
            std::cerr << "Run with --source MPC to download initial database.\n";
            return 1;
        }
        
        if (db.needsUpdate(maxAge)) {
            std::cout << "Database is older than " << maxAge << " days. Updating...\n";
            
            auto start = std::chrono::steady_clock::now();
            
            if (!db.downloadDatabase(DataSource::MPC_EXTENDED_JSON)) {
                std::cerr << "Error: Failed to download updates.\n";
                return 1;
            }
            
            auto end = std::chrono::steady_clock::now();
            auto duration = std::chrono::duration_cast<std::chrono::seconds>(end - start);
            
            std::cout << "\n✓ Database updated in " << duration.count() << " seconds.\n";
            
            if (!db.saveToFile()) {
                std::cerr << "Error: Failed to save database.\n";
                return 1;
            }
            
            auto stats = db.getStats();
            printStats(stats);
        } else {
            std::cout << "Database is up to date (less than " << maxAge << " days old).\n";
        }
        
        return 0;
    }
    
    // Download mode
    DataSource dataSource;
    if (source == "MPC") {
        dataSource = DataSource::MPC_EXTENDED_JSON;
    } else if (source == "ASTDYS") {
        dataSource = DataSource::ASTDYS;
    } else if (source == "JPL") {
        dataSource = DataSource::JPL_SBDB;
    } else {
        std::cerr << "Error: Unknown source: " << source << "\n";
        std::cerr << "Valid sources: MPC, ASTDYS, JPL\n";
        return 1;
    }
    
    std::cout << "╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║  Downloading Asteroid Database                            ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n\n";
    std::cout << "  Source: " << source << "\n";
    if (from > 0 && to > 0) {
        std::cout << "  Range:  " << from << " - " << to << "\n";
    }
    std::cout << "\n";
    
    auto start = std::chrono::steady_clock::now();
    
    // Download
    bool success = false;
    if (dataSource == DataSource::ASTDYS && from > 0 && to > 0) {
        int count = db.updateRange(from, to, dataSource);
        success = (count > 0);
        std::cout << "\n✓ Downloaded " << count << " asteroids.\n";
    } else {
        success = db.downloadDatabase(dataSource);
        if (success) {
            std::cout << "\n✓ Download complete.\n";
        }
    }
    
    if (!success) {
        std::cerr << "\n✗ Download failed.\n";
        return 1;
    }
    
    auto end = std::chrono::steady_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::seconds>(end - start);
    
    // Apply filters if specified
    if (!filters.empty()) {
        std::cout << "\nApplying filters...\n";
        
        AsteroidRangeBuilder builder;
        builder.from(1).to(1000000);
        
        for (const auto& filter : filters) {
            std::cout << "  - " << filter << "\n";
            builder.where(filter);
        }
        
        auto range = builder.build();
        
        // Export filtered subset
        std::string filterOutput = outputPath.empty() ? "filtered_asteroids.json" : outputPath;
        if (db.exportToJson(range, filterOutput)) {
            int count = db.count(range);
            std::cout << "✓ Exported " << count << " asteroids to " << filterOutput << "\n";
        } else {
            std::cerr << "✗ Failed to export filtered database.\n";
            return 1;
        }
    } else {
        // Save full database
        if (!db.saveToFile()) {
            std::cerr << "✗ Failed to save database.\n";
            return 1;
        }
        std::cout << "✓ Database saved to " << db.getPath() << "\n";
    }
    
    std::cout << "\n";
    
    // Show statistics
    auto stats = db.getStats();
    printStats(stats);
    
    std::cout << "Download time: " << duration.count() << " seconds\n";
    std::cout << "Database path: " << db.getPath() << "\n";
    std::cout << "Memory usage:  " << (db.getMemoryUsage() / 1024 / 1024) << " MB\n\n";
    
    std::cout << "╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║  Database ready for use!                                  ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n\n";
    
    return 0;
}
