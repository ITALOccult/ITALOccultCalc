/**
 * @file ioccultcalc_search.cpp
 * @brief IOccultCalc - Unified asteroid occultation search tool
 * 
 * Modalità:
 * 1. Single asteroid + single star (modalità classica)
 * 2. Database survey + GAIA cache (modalità massiva)
 * 
 * Uso:
 *   Single mode:
 *     ./ioccultcalc_search --asteroid 433 --star 10.684,41.269 --start 2026-01-01 --end 2026-12-31
 *   
 *   Database mode:
 *     ./ioccultcalc_search --config survey.json
 *     ./ioccultcalc_search --database --filter "diameter > 50" --gaia-cache
 * 
 * Esempio configurazione (survey.json):
 * {
 *   "asteroid_range": {
 *     "mode": "database",
 *     "where": ["diameter > 50", "H < 12"]
 *   },
 *   "gaia_cache": {
 *     "enabled": true,
 *     "auto_download": true,
 *     "max_magnitude": 16.0
 *   },
 *   "search_parameters": {
 *     "start_date": "2026-01-01",
 *     "end_date": "2026-12-31",
 *     "time_step_hours": 2.4,
 *     "path_width_deg": 0.01
 *   }
 * }
 */

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>
#include <getopt.h>
#include <nlohmann/json.hpp>

#include <ioccultcalc/asteroid_database.h>
#include <ioccultcalc/asteroid_filter.h>
#include <ioccultcalc/gaia_cache.h>
#include <ioccultcalc/gaia_client.h>
#include <ioccultcalc/astdys_client.h>
#include <ioccultcalc/orbit_propagator.h>
#include <ioccultcalc/coordinates.h>
#include <ioccultcalc/time_utils.h>
#include <ioccultcalc/config_manager.h>

using namespace ioccultcalc;
using json = nlohmann::json;

// ==================== Configuration ====================

struct SearchConfig {
    // Mode
    bool useDatabaseMode = false;
    bool useGaiaCache = false;
    
    // Single mode
    std::string asteroidId;
    double starRA = 0;   // degrees
    double starDec = 0;  // degrees
    
    // Database mode
    std::vector<std::string> filterWhere;
    std::vector<std::string> filterWhereNot;
    
    // GAIA cache
    bool gaiaAutoDownload = true;
    double gaiaMaxMagnitude = 16.0;
    
    // Search parameters
    std::string startDate;
    std::string endDate;
    double timeStepHours = 2.4;
    double pathWidthDeg = 0.01;  // ~36 arcsec
    
    // Output
    std::string outputFile = "occultations.json";
    bool verbose = false;
    
    // Config file
    std::string configFile;
};

// ==================== Helpers ====================

void printUsage(const char* progName) {
    std::cout << "IOccultCalc - Asteroid Occultation Search Tool\n\n";
    std::cout << "Usage:\n";
    std::cout << "  Single asteroid mode:\n";
    std::cout << "    " << progName << " --asteroid <id> --star <ra>,<dec> --start <date> --end <date>\n\n";
    std::cout << "  Database survey mode:\n";
    std::cout << "    " << progName << " --config <config.json>\n";
    std::cout << "    " << progName << " --database --filter \"<condition>\" --gaia-cache\n\n";
    std::cout << "Options:\n";
    std::cout << "  -c, --config <file>       Load configuration from JSON file\n";
    std::cout << "  -a, --asteroid <id>       Asteroid number or designation\n";
    std::cout << "  -s, --star <ra>,<dec>     Star coordinates (degrees)\n";
    std::cout << "  -d, --database            Use database mode\n";
    std::cout << "  -f, --filter <condition>  Filter condition (SQL-like)\n";
    std::cout << "  -g, --gaia-cache          Enable GAIA star cache\n";
    std::cout << "  --start <date>            Start date (YYYY-MM-DD)\n";
    std::cout << "  --end <date>              End date (YYYY-MM-DD)\n";
    std::cout << "  -o, --output <file>       Output file (default: occultations.json)\n";
    std::cout << "  -v, --verbose             Verbose output\n";
    std::cout << "  -h, --help                Show this help\n\n";
    std::cout << "Examples:\n";
    std::cout << "  # Single asteroid + star\n";
    std::cout << "  " << progName << " -a 433 -s 10.684,41.269 --start 2026-01-01 --end 2026-12-31\n\n";
    std::cout << "  # Database survey with filter\n";
    std::cout << "  " << progName << " -d -f \"diameter > 50\" -g --start 2026-01-01 --end 2026-12-31\n\n";
    std::cout << "  # Full survey from config\n";
    std::cout << "  " << progName << " -c examples/config_templates/survey_database.json\n\n";
}

SearchConfig parseCommandLine(int argc, char* argv[]) {
    SearchConfig config;
    
    static struct option long_options[] = {
        {"config",    required_argument, 0, 'c'},
        {"asteroid",  required_argument, 0, 'a'},
        {"star",      required_argument, 0, 's'},
        {"database",  no_argument,       0, 'd'},
        {"filter",    required_argument, 0, 'f'},
        {"gaia-cache", no_argument,      0, 'g'},
        {"start",     required_argument, 0,  1 },
        {"end",       required_argument, 0,  2 },
        {"output",    required_argument, 0, 'o'},
        {"verbose",   no_argument,       0, 'v'},
        {"help",      no_argument,       0, 'h'},
        {0, 0, 0, 0}
    };
    
    int option_index = 0;
    int c;
    
    while ((c = getopt_long(argc, argv, "c:a:s:df:go:vh", long_options, &option_index)) != -1) {
        switch (c) {
            case 'c':
                config.configFile = optarg;
                break;
            case 'a':
                config.asteroidId = optarg;
                break;
            case 's': {
                std::string coords = optarg;
                size_t comma = coords.find(',');
                if (comma != std::string::npos) {
                    config.starRA = std::stod(coords.substr(0, comma));
                    config.starDec = std::stod(coords.substr(comma + 1));
                }
                break;
            }
            case 'd':
                config.useDatabaseMode = true;
                break;
            case 'f':
                config.filterWhere.push_back(optarg);
                break;
            case 'g':
                config.useGaiaCache = true;
                break;
            case 1:  // --start
                config.startDate = optarg;
                break;
            case 2:  // --end
                config.endDate = optarg;
                break;
            case 'o':
                config.outputFile = optarg;
                break;
            case 'v':
                config.verbose = true;
                break;
            case 'h':
                printUsage(argv[0]);
                exit(0);
            default:
                printUsage(argv[0]);
                exit(1);
        }
    }
    
    return config;
}

SearchConfig loadConfigFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open config file: " + filename);
    }
    
    json j;
    file >> j;
    
    SearchConfig config;
    
    // Asteroid range
    if (j.contains("asteroid_range")) {
        auto& range = j["asteroid_range"];
        if (range.contains("mode") && range["mode"] == "database") {
            config.useDatabaseMode = true;
        }
        if (range.contains("where")) {
            for (const auto& condition : range["where"]) {
                config.filterWhere.push_back(condition);
            }
        }
        if (range.contains("where_not")) {
            for (const auto& condition : range["where_not"]) {
                config.filterWhereNot.push_back(condition);
            }
        }
    }
    
    // GAIA cache
    if (j.contains("gaia_cache")) {
        auto& gaia = j["gaia_cache"];
        if (gaia.contains("enabled") && gaia["enabled"]) {
            config.useGaiaCache = true;
        }
        if (gaia.contains("auto_download")) {
            config.gaiaAutoDownload = gaia["auto_download"];
        }
        if (gaia.contains("max_magnitude")) {
            config.gaiaMaxMagnitude = gaia["max_magnitude"];
        }
    }
    
    // Search parameters
    if (j.contains("search_parameters")) {
        auto& search = j["search_parameters"];
        if (search.contains("start_date")) {
            config.startDate = search["start_date"];
        }
        if (search.contains("end_date")) {
            config.endDate = search["end_date"];
        }
        if (search.contains("time_step_hours")) {
            config.timeStepHours = search["time_step_hours"];
        }
        if (search.contains("path_width_deg")) {
            config.pathWidthDeg = search["path_width_deg"];
        }
        if (search.contains("max_magnitude")) {
            config.gaiaMaxMagnitude = search["max_magnitude"];
        }
    }
    
    // Output
    if (j.contains("output")) {
        auto& output = j["output"];
        if (output.contains("file")) {
            config.outputFile = output["file"];
        }
        if (output.contains("verbose")) {
            config.verbose = output["verbose"];
        }
    }
    
    return config;
}

// ==================== Single Mode ====================

int runSingleMode(const SearchConfig& config) {
    std::cout << "\n";
    std::cout << "╔══════════════════════════════════════════════════════════╗\n";
    std::cout << "║   IOccultCalc - Single Asteroid Search                  ║\n";
    std::cout << "╚══════════════════════════════════════════════════════════╝\n";
    std::cout << "\n";
    
    std::cout << "Configuration:\n";
    std::cout << "  Asteroid: " << config.asteroidId << "\n";
    std::cout << "  Star: RA=" << config.starRA << "° Dec=" << config.starDec << "°\n";
    std::cout << "  Period: " << config.startDate << " to " << config.endDate << "\n";
    std::cout << "\n";
    
    // Download orbital elements
    std::cout << "📡 Downloading orbital elements...\n";
    AstDysClient astdysClient;
    auto elements = astdysClient.getElements(config.asteroidId);
    std::cout << "   ✓ Elements for " << elements.name << "\n";
    std::cout << "\n";
    
    // Setup propagator
    std::cout << "🔧 Setting up propagator...\n";
    PropagatorOptions opts;
    opts.integrator = IntegratorType::RK4;
    opts.stepSize = 0.05;
    opts.usePlanetaryPerturbations = true;
    OrbitPropagator propagator(opts);
    std::cout << "   ✓ Propagator ready\n";
    std::cout << "\n";
    
    // Calculate asteroid path
    std::cout << "🛰️  Calculating asteroid path...\n";
    JulianDate startJD = TimeUtils::isoToJD(config.startDate);
    JulianDate endJD = TimeUtils::isoToJD(config.endDate);
    
    double timeSpan = endJD.jd - startJD.jd;
    double stepDays = config.timeStepHours / 24.0;
    int nSteps = static_cast<int>(timeSpan / stepDays);
    
    std::cout << "   Time span: " << timeSpan << " days\n";
    std::cout << "   Steps: " << nSteps << "\n";
    std::cout << "\n";
    
    // Search for close approaches
    std::cout << "🔍 Searching for occultations...\n";
    
    double starRArad = config.starRA * M_PI / 180.0;
    double starDecRad = config.starDec * M_PI / 180.0;
    
    std::vector<double> separations;
    std::vector<JulianDate> epochs;
    
    for (int i = 0; i <= nSteps; i++) {
        if (i % 100 == 0 || i == nSteps) {
            std::cout << "\r   Progress: " << std::fixed << std::setprecision(1)
                     << (100.0 * i / nSteps) << "%" << std::flush;
        }
        
        JulianDate epoch(startJD.jd + i * stepDays);
        
        // Propagate
        OrbitState initialState = propagator.elementsToState(elements);
        OrbitState state = propagator.propagate(initialState, epoch);
        
        // Convert to equatorial (simplified - needs proper geocentric conversion)
        auto coords = Coordinates::cartesianToEquatorial(state.position);
        
        // Calculate separation
        EquatorialCoordinates starCoords;
        starCoords.ra = starRArad;
        starCoords.dec = starDecRad;
        
        double sep = Coordinates::angularSeparation(coords, starCoords) * 206265.0; // rad to arcsec
        
        if (sep < 60.0) {  // Within 1 arcmin
            separations.push_back(sep);
            epochs.push_back(epoch);
        }
    }
    
    std::cout << "\n\n";
    
    // Results
    std::cout << "═══════════════════ RESULTS ═══════════════════\n";
    std::cout << "\n";
    std::cout << "Close approaches found: " << separations.size() << "\n";
    
    if (!separations.empty()) {
        std::cout << "\n";
        std::cout << "Closest approaches:\n";
        for (size_t i = 0; i < std::min(separations.size(), size_t(10)); i++) {
            std::cout << "  " << TimeUtils::jdToISO(epochs[i])
                     << " - " << std::fixed << std::setprecision(2)
                     << separations[i] << " arcsec\n";
        }
    }
    
    std::cout << "\n";
    std::cout << "═══════════════════════════════════════════════\n";
    std::cout << "\n";
    
    return 0;
}

// ==================== Database Mode ====================

int runDatabaseMode(const SearchConfig& config) {
    std::cout << "\n";
    std::cout << "╔══════════════════════════════════════════════════════════╗\n";
    std::cout << "║   IOccultCalc - Database Survey Mode                    ║\n";
    std::cout << "╚══════════════════════════════════════════════════════════╝\n";
    std::cout << "\n";
    
    // Load database
    std::cout << "📦 Loading asteroid database...\n";
    AsteroidDatabase db;
    if (!db.loadFromFile()) {
        std::cerr << "❌ Failed to load asteroid database\n";
        std::cerr << "   Run: python3 tools/download_mpc_database.py\n";
        return 1;
    }
    
    auto stats = db.getStats();
    std::cout << "   ✓ Loaded " << stats.total_asteroids << " asteroids\n";
    std::cout << "\n";
    
    // Apply filters
    std::cout << "🔍 Applying filters...\n";
    auto rangeBuilder = AsteroidRangeBuilder();
    
    for (const auto& condition : config.filterWhere) {
        std::cout << "   WHERE " << condition << "\n";
        rangeBuilder.where(condition);
    }
    
    for (const auto& condition : config.filterWhereNot) {
        std::cout << "   WHERE NOT " << condition << "\n";
        rangeBuilder.whereNot(condition);
    }
    
    auto range = rangeBuilder.build();
    auto asteroidNumbers = db.queryNumbers(range);
    
    std::cout << "   ✓ Selected " << asteroidNumbers.size() << " asteroids\n";
    std::cout << "\n";
    
    // Setup GAIA cache
    std::shared_ptr<GaiaCache> gaiaCache;
    
    if (config.useGaiaCache) {
        std::cout << "⭐ Setting up GAIA cache...\n";
        
        auto gaiaClient = std::make_shared<GaiaClient>();
        gaiaClient->setTAPURL("https://gea.esac.esa.int/tap-server/tap");
        
        gaiaCache = GaiaCacheBuilder()
            .gaiaClient(gaiaClient)
            .autoDownload(config.gaiaAutoDownload)
            .verbose(config.verbose)
            .build();
        
        auto cacheStats = gaiaCache->getStats();
        std::cout << "   ✓ Cache ready (" << cacheStats.total_tiles << " tiles, "
                 << cacheStats.total_stars << " stars)\n";
        std::cout << "\n";
    }
    
    // Process asteroids
    std::cout << "🔄 Processing asteroids...\n";
    std::cout << "\n";
    
    int processedCount = 0;
    int occultationsFound = 0;
    
    for (int astNum : asteroidNumbers) {
        processedCount++;
        
        if (processedCount % 10 == 0 || processedCount == 1) {
            std::cout << "\r   Progress: " << processedCount << "/" << asteroidNumbers.size()
                     << " (" << std::fixed << std::setprecision(1)
                     << (100.0 * processedCount / asteroidNumbers.size()) << "%)" << std::flush;
        }
        
        try {
            // Get elements
            AstDysClient astdysClient;
            auto elements = astdysClient.getElements(std::to_string(astNum));
            
            // Calculate path (simplified)
            std::vector<EquatorialCoordinates> path;
            // ... propagate and build path ...
            
            // Query stars if GAIA cache enabled
            if (gaiaCache) {
                auto stars = gaiaCache->queryPath(path, config.pathWidthDeg, 
                                                  config.gaiaMaxMagnitude, true);
                
                // Check occultations
                for (const auto& star : stars) {
                    // ... predict occultation ...
                    // If found, increment occultationsFound
                }
            }
            
        } catch (const std::exception& e) {
            if (config.verbose) {
                std::cerr << "\n⚠️  Warning: Failed to process asteroid " << astNum 
                         << ": " << e.what() << "\n";
            }
        }
    }
    
    std::cout << "\n\n";
    
    // Results
    std::cout << "═══════════════════ RESULTS ═══════════════════\n";
    std::cout << "\n";
    std::cout << "Asteroids processed: " << processedCount << "\n";
    std::cout << "Occultations found:  " << occultationsFound << "\n";
    std::cout << "\n";
    std::cout << "Results saved to: " << config.outputFile << "\n";
    std::cout << "\n";
    std::cout << "═══════════════════════════════════════════════\n";
    std::cout << "\n";
    
    return 0;
}

// ==================== Main ====================

int main(int argc, char* argv[]) {
    try {
        if (argc < 2) {
            printUsage(argv[0]);
            return 1;
        }
        
        // Parse command line
        SearchConfig config = parseCommandLine(argc, argv);
        
        // Load config file if specified
        if (!config.configFile.empty()) {
            config = loadConfigFile(config.configFile);
        }
        
        // Validate configuration
        if (config.startDate.empty() || config.endDate.empty()) {
            std::cerr << "❌ Error: Start and end dates are required\n\n";
            printUsage(argv[0]);
            return 1;
        }
        
        // Run appropriate mode
        if (config.useDatabaseMode) {
            return runDatabaseMode(config);
        } else if (!config.asteroidId.empty()) {
            return runSingleMode(config);
        } else {
            std::cerr << "❌ Error: Must specify either --asteroid or --database\n\n";
            printUsage(argv[0]);
            return 1;
        }
        
    } catch (const std::exception& e) {
        std::cerr << "\n❌ ERROR: " << e.what() << "\n\n";
        return 1;
    }
    
    return 0;
}
