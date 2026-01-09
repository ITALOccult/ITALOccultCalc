#include "catalog_manager.h"
#include <fstream>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <ctime>
#include <algorithm>
#include <curl/curl.h>

namespace IOccultCalc {

// ============================================================================
// OEF2Parser Implementation
// ============================================================================

OEF2Parser::AsteroidRecord OEF2Parser::parseLine(const std::string& line) {
    AsteroidRecord rec;
    rec.catalog_id = -1;
    
    // Skip empty lines and comments
    if (line.empty() || line[0] == '%') {
        return rec;
    }
    
    // Parse space-separated fields
    std::istringstream iss(line);
    
    if (!(iss >> rec.catalog_id)) return rec;
    if (!(iss >> rec.designation)) return rec;
    if (!(iss >> rec.magnitude_h)) return rec;
    if (!(iss >> rec.slope_g)) return rec;
    if (!(iss >> rec.a)) return rec;
    if (!(iss >> rec.h)) return rec;
    if (!(iss >> rec.k)) return rec;
    if (!(iss >> rec.p)) return rec;
    if (!(iss >> rec.q)) return rec;
    if (!(iss >> rec.lambda)) return rec;
    if (!(iss >> rec.epoch_mjd)) return rec;
    
    // Set frame and reference for OEF2.0
    rec.frame = "ECLM J2000";
    rec.reference = "JPL DE441";
    
    return rec;
}

std::vector<OEF2Parser::AsteroidRecord> OEF2Parser::loadCatalog(
    const std::string& filename) {
    
    std::vector<AsteroidRecord> records;
    std::ifstream file(filename);
    
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open catalog file: " + filename);
    }
    
    std::string line;
    size_t line_count = 0;
    size_t error_count = 0;
    
    std::cout << "📖 Loading OEF2.0 catalog from: " << filename << "\n";
    
    while (std::getline(file, line)) {
        // Skip empty lines and comments
        if (line.empty() || line[0] == '%') continue;
        
        try {
            AsteroidRecord rec = parseLine(line);
            if (rec.catalog_id > 0) {
                records.push_back(rec);
                if (++line_count % 100000 == 0) {
                    std::cout << "  ✓ Loaded " << line_count << " asteroids...\n";
                }
            }
        } catch (const std::exception& e) {
            error_count++;
            if (error_count <= 10) {  // Only print first 10 errors
                std::cerr << "  ⚠️ Parse error on line " << (line_count + error_count)
                         << ": " << e.what() << "\n";
            }
        }
    }
    
    file.close();
    
    std::cout << "✅ Catalog loaded: " << records.size() << " asteroids";
    if (error_count > 0) {
        std::cout << " (" << error_count << " errors skipped)";
    }
    std::cout << "\n";
    
    return records;
}

// ============================================================================
// CatalogManager Implementation
// ============================================================================

CatalogManager::CatalogManager(
    const std::string& cache_dir,
    UpdateStrategy strategy)
    : db_(nullptr), cache_dir_(cache_dir), strategy_(strategy), is_ready_(false) {
    
    // Expand home directory if needed
    if (cache_dir_.substr(0, 2) == "~/") {
        const char* home = std::getenv("HOME");
        if (home) {
            cache_dir_ = std::string(home) + cache_dir_.substr(1);
        }
    }
    
    std::cout << "🗂️  Catalog cache directory: " << cache_dir_ << "\n";
    
    try {
        initializeDatabase();
        is_ready_ = true;
    } catch (const std::exception& e) {
        std::cerr << "❌ Failed to initialize database: " << e.what() << "\n";
        is_ready_ = false;
    }
}

CatalogManager::~CatalogManager() {
    if (db_) {
        sqlite3_close(db_);
        db_ = nullptr;
    }
}

void CatalogManager::initializeDatabase() {
    // Create cache directory if needed
    std::string create_dir = "mkdir -p " + cache_dir_;
    system(create_dir.c_str());
    
    std::string db_path = cache_dir_ + "/asteroids.db";
    
    int rc = sqlite3_open(db_path.c_str(), &db_);
    if (rc) {
        throw std::runtime_error("Cannot open database: " + db_path);
    }
    
    // Create schema
    const char* schema = R"(
        CREATE TABLE IF NOT EXISTS asteroids (
            id INTEGER PRIMARY KEY,
            catalog_id INTEGER UNIQUE,
            designation TEXT UNIQUE NOT NULL,
            magnitude_h REAL,
            slope_g REAL,
            a REAL NOT NULL,
            h REAL NOT NULL,
            k REAL NOT NULL,
            p REAL NOT NULL,
            q REAL NOT NULL,
            lambda REAL NOT NULL,
            epoch_mjd REAL NOT NULL,
            frame TEXT,
            reference TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
        
        CREATE INDEX IF NOT EXISTS idx_designation ON asteroids(designation);
        CREATE INDEX IF NOT EXISTS idx_catalog_id ON asteroids(catalog_id);
        CREATE INDEX IF NOT EXISTS idx_magnitude ON asteroids(magnitude_h);
        
        CREATE TABLE IF NOT EXISTS catalog_metadata (
            id INTEGER PRIMARY KEY,
            version TEXT,
            download_date TIMESTAMP,
            total_records INTEGER,
            file_hash TEXT,
            data_epoch REAL,
            reference_ephemeris TEXT
        );
    )";
    
    char* err_msg = nullptr;
    rc = sqlite3_exec(db_, schema, nullptr, nullptr, &err_msg);
    if (rc != SQLITE_OK) {
        std::string error = err_msg ? std::string(err_msg) : "Unknown error";
        sqlite3_free(err_msg);
        throw std::runtime_error("Schema creation failed: " + error);
    }
    
    std::cout << "✅ Database initialized: " << db_path << "\n";
}

std::string CatalogManager::getCatalogPath() {
    return cache_dir_ + "/allnum.cat";
}

bool CatalogManager::tableExists(const std::string& table_name) {
    std::string query = 
        "SELECT name FROM sqlite_master WHERE type='table' AND name='" + table_name + "';";
    
    sqlite3_stmt* stmt = nullptr;
    sqlite3_prepare_v2(db_, query.c_str(), -1, &stmt, nullptr);
    
    bool exists = (sqlite3_step(stmt) == SQLITE_ROW);
    sqlite3_finalize(stmt);
    
    return exists;
}

long CatalogManager::countRecords() {
    const char* query = "SELECT COUNT(*) FROM asteroids";
    sqlite3_stmt* stmt = nullptr;
    sqlite3_prepare_v2(db_, query, -1, &stmt, nullptr);
    
    long count = 0;
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        count = sqlite3_column_int(stmt, 0);
    }
    sqlite3_finalize(stmt);
    
    return count;
}

// Callback for CURL
static size_t writeCallback(void* contents, size_t size, size_t nmemb, FILE* userp) {
    return fwrite(contents, size, nmemb, userp);
}

bool CatalogManager::downloadFile(const std::string& url, const std::string& dest) {
    std::cout << "📥 Downloading from: " << url << "\n";
    
    FILE* outfile = fopen(dest.c_str(), "wb");
    if (!outfile) {
        std::cerr << "❌ Cannot open output file: " << dest << "\n";
        return false;
    }
    
    CURL* curl = curl_easy_init();
    if (!curl) {
        std::cerr << "❌ CURL init failed\n";
        fclose(outfile);
        return false;
    }
    
    try {
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, outfile);
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, 3600L);  // 1 hour timeout
        curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0L);
        
        CURLcode res = curl_easy_perform(curl);
        
        if (res != CURLE_OK) {
            std::cerr << "❌ Download failed: " << curl_easy_strerror(res) << "\n";
            curl_easy_cleanup(curl);
            fclose(outfile);
            return false;
        }
        
        long http_code = 0;
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);
        
        if (http_code != 200) {
            std::cerr << "❌ HTTP error: " << http_code << "\n";
            curl_easy_cleanup(curl);
            fclose(outfile);
            return false;
        }
        
        std::cout << "✅ Download completed\n";
        curl_easy_cleanup(curl);
        fclose(outfile);
        return true;
        
    } catch (const std::exception& e) {
        curl_easy_cleanup(curl);
        fclose(outfile);
        throw e;
    }
}

bool CatalogManager::loadIntoDatabase(const std::string& filepath) {
    std::lock_guard<std::mutex> lock(db_mutex_);
    
    // Load catalog
    auto records = OEF2Parser::loadCatalog(filepath);
    
    if (records.empty()) {
        std::cerr << "❌ No records to load\n";
        return false;
    }
    
    // Clear old data
    std::cout << "🗑️  Clearing old catalog data...\n";
    sqlite3_exec(db_, "DELETE FROM asteroids", nullptr, nullptr, nullptr);
    sqlite3_exec(db_, "DELETE FROM catalog_metadata", nullptr, nullptr, nullptr);
    
    const char* insert_query = R"(
        INSERT INTO asteroids 
        (catalog_id, designation, magnitude_h, slope_g, a, h, k, p, q, lambda, epoch_mjd, frame, reference)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    )";
    
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_, insert_query, -1, &stmt, nullptr);
    
    if (rc != SQLITE_OK) {
        std::cerr << "❌ SQL prepare failed: " << sqlite3_errmsg(db_) << "\n";
        return false;
    }
    
    std::cout << "💾 Loading " << records.size() << " asteroids into database...\n";
    
    sqlite3_exec(db_, "BEGIN TRANSACTION", nullptr, nullptr, nullptr);
    
    for (size_t i = 0; i < records.size(); ++i) {
        const auto& rec = records[i];
        
        sqlite3_bind_int(stmt, 1, rec.catalog_id);
        sqlite3_bind_text(stmt, 2, rec.designation.c_str(), -1, SQLITE_TRANSIENT);
        sqlite3_bind_double(stmt, 3, rec.magnitude_h);
        sqlite3_bind_double(stmt, 4, rec.slope_g);
        sqlite3_bind_double(stmt, 5, rec.a);
        sqlite3_bind_double(stmt, 6, rec.h);
        sqlite3_bind_double(stmt, 7, rec.k);
        sqlite3_bind_double(stmt, 8, rec.p);
        sqlite3_bind_double(stmt, 9, rec.q);
        sqlite3_bind_double(stmt, 10, rec.lambda);
        sqlite3_bind_double(stmt, 11, rec.epoch_mjd);
        sqlite3_bind_text(stmt, 12, rec.frame.c_str(), -1, SQLITE_TRANSIENT);
        sqlite3_bind_text(stmt, 13, rec.reference.c_str(), -1, SQLITE_TRANSIENT);
        
        rc = sqlite3_step(stmt);
        if (rc != SQLITE_DONE) {
            std::cerr << "❌ SQL step failed: " << sqlite3_errmsg(db_) << "\n";
            sqlite3_finalize(stmt);
            sqlite3_exec(db_, "ROLLBACK", nullptr, nullptr, nullptr);
            return false;
        }
        
        sqlite3_reset(stmt);
        
        if ((i + 1) % 100000 == 0) {
            std::cout << "  ✓ Inserted " << (i + 1) << "/" << records.size() << "\n";
        }
    }
    
    sqlite3_finalize(stmt);
    sqlite3_exec(db_, "COMMIT", nullptr, nullptr, nullptr);
    
    // Insert metadata
    std::time_t now = std::time(nullptr);
    char timestamp[32];
    std::strftime(timestamp, sizeof(timestamp), "%Y-%m-%d %H:%M:%S", 
                 std::localtime(&now));
    
    std::string meta_insert = 
        "INSERT INTO catalog_metadata (version, download_date, total_records, reference_ephemeris) "
        "VALUES ('OEF2.0', '" + std::string(timestamp) + "', " + 
        std::to_string(records.size()) + ", 'JPL DE441')";
    
    sqlite3_exec(db_, meta_insert.c_str(), nullptr, nullptr, nullptr);
    
    std::cout << "✅ Database updated: " << records.size() << " asteroids loaded\n";
    return true;
}

bool CatalogManager::updateCatalog(bool force) {
    std::cout << "🔄 Updating catalog...\n";
    
    // Check if update is necessary
    if (!force && isCatalogRecent()) {
        std::cout << "✅ Catalog is recent, skipping download\n";
        return true;
    }
    
    // Download from AstDyS2
    std::string remote_url = 
        "https://newton.spacedys.com/~astdys2/catalogs/allnum.cat";
    std::string local_path = getCatalogPath();
    
    try {
        if (!downloadFile(remote_url, local_path)) {
            std::cerr << "❌ Download failed\n";
            return false;
        }
        
        // Parse and load into database
        return loadIntoDatabase(local_path);
        
    } catch (const std::exception& e) {
        std::cerr << "❌ Error: " << e.what() << "\n";
        return false;
    }
}

AstDynEquinoctialElements CatalogManager::getElements(
    const std::string& designation,
    double* epoch_out) {
    
    std::lock_guard<std::mutex> lock(db_mutex_);
    
    std::string query = R"(
        SELECT a, h, k, p, q, lambda, epoch_mjd 
        FROM asteroids 
        WHERE designation = ? OR catalog_id = ?
        LIMIT 1
    )";
    
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_, query.c_str(), -1, &stmt, nullptr);
    
    if (rc != SQLITE_OK) {
        throw std::runtime_error(
            std::string("SQL prepare failed: ") + sqlite3_errmsg(db_));
    }
    
    // Try both as designation string and as integer ID
    sqlite3_bind_text(stmt, 1, designation.c_str(), -1, SQLITE_TRANSIENT);
    try {
        int id = std::stoi(designation);
        sqlite3_bind_int(stmt, 2, id);
    } catch (...) {
        sqlite3_bind_null(stmt, 2);
    }
    
    AstDynEquinoctialElements elements;
    bool found = false;
    
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        elements.a = sqlite3_column_double(stmt, 0);
        elements.h = sqlite3_column_double(stmt, 1);
        elements.k = sqlite3_column_double(stmt, 2);
        elements.p = sqlite3_column_double(stmt, 3);
        elements.q = sqlite3_column_double(stmt, 4);
        elements.lambda = sqlite3_column_double(stmt, 5);
        
        double epoch = sqlite3_column_double(stmt, 6);
        if (epoch_out) {
            *epoch_out = epoch;
        }
        
        found = true;
    }
    
    sqlite3_finalize(stmt);
    
    if (!found) {
        throw std::runtime_error("Asteroid not found: " + designation);
    }
    
    return elements;
}

std::vector<std::string> CatalogManager::searchByMagnitude(
    double mag_min,
    double mag_max) {
    
    std::lock_guard<std::mutex> lock(db_mutex_);
    
    std::string query = R"(
        SELECT designation 
        FROM asteroids 
        WHERE magnitude_h BETWEEN ? AND ?
        ORDER BY magnitude_h ASC
        LIMIT 100000
    )";
    
    sqlite3_stmt* stmt = nullptr;
    int rc = sqlite3_prepare_v2(db_, query.c_str(), -1, &stmt, nullptr);
    
    if (rc != SQLITE_OK) {
        throw std::runtime_error("SQL prepare failed");
    }
    
    sqlite3_bind_double(stmt, 1, mag_min);
    sqlite3_bind_double(stmt, 2, mag_max);
    
    std::vector<std::string> results;
    
    while (sqlite3_step(stmt) == SQLITE_ROW) {
        const char* designation = 
            reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
        if (designation) {
            results.push_back(std::string(designation));
        }
    }
    
    sqlite3_finalize(stmt);
    return results;
}

CatalogManager::CatalogStats CatalogManager::getStats() {
    std::lock_guard<std::mutex> lock(db_mutex_);
    
    CatalogStats stats = {0, 0.0, "", "", ""};
    
    // Count asteroids
    const char* count_query = "SELECT COUNT(*) FROM asteroids";
    sqlite3_stmt* stmt = nullptr;
    sqlite3_prepare_v2(db_, count_query, -1, &stmt, nullptr);
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        stats.total_asteroids = sqlite3_column_int(stmt, 0);
    }
    sqlite3_finalize(stmt);
    
    // Average magnitude
    const char* avg_query = "SELECT AVG(magnitude_h) FROM asteroids WHERE magnitude_h > 0";
    sqlite3_prepare_v2(db_, avg_query, -1, &stmt, nullptr);
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        stats.avg_magnitude = sqlite3_column_double(stmt, 0);
    }
    sqlite3_finalize(stmt);
    
    // Metadata
    const char* meta_query = 
        "SELECT version, download_date FROM catalog_metadata ORDER BY id DESC LIMIT 1";
    sqlite3_prepare_v2(db_, meta_query, -1, &stmt, nullptr);
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        const char* ver = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
        const char* date = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
        if (ver) stats.version = ver;
        if (date) stats.download_date = date;
    }
    sqlite3_finalize(stmt);
    
    return stats;
}

bool CatalogManager::isCatalogRecent(int days) {
    std::lock_guard<std::mutex> lock(db_mutex_);
    
    // Check if we have any data
    long count = countRecords();
    if (count == 0) {
        std::cout << "⚠️  Catalog is empty\n";
        return false;
    }
    
    const char* query = 
        "SELECT download_date FROM catalog_metadata ORDER BY id DESC LIMIT 1";
    sqlite3_stmt* stmt = nullptr;
    sqlite3_prepare_v2(db_, query, -1, &stmt, nullptr);
    
    bool recent = false;
    
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        const char* date_str = 
            reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
        
        if (date_str) {
            // Parse timestamp and check age
            std::tm tm = {};
            std::sscanf(date_str, "%d-%d-%d", &tm.tm_year, &tm.tm_mon, &tm.tm_mday);
            tm.tm_year -= 1900;
            tm.tm_mon -= 1;
            
            std::time_t download_time = std::mktime(&tm);
            std::time_t now = std::time(nullptr);
            
            int age_days = (now - download_time) / (24 * 3600);
            
            std::cout << "📅 Catalog age: " << age_days << " days "
                      << "(threshold: " << days << " days)\n";
            
            recent = (age_days < days);
        }
    }
    
    sqlite3_finalize(stmt);
    return recent;
}

} // namespace IOccultCalc
