/**
 * @file build_allnum_database.cpp
 * @brief Tool per creare/aggiornare database SQLite da allnum.cat
 * 
 * Features:
 * - Scarica allnum.cat da AstDyS
 * - Parsa formato OEF2.0 con elementi kepleriani
 * - Inserisce in SQLite con tracciamento data
 * - Verifica se database è vecchio (> 30 giorni) e aggiorna
 * 
 * Usage:
 *   ./build_allnum_database [--force] [--db-path PATH] [--max-age DAYS]
 */

#include "ioccultcalc/orbital_elements.h"
#include "ioccultcalc/data_manager.h"
#include "ioccultcalc/types.h"
#include <sqlite3.h>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <ctime>
#include <cmath>
#include <vector>
#include <string>
#include <cstring>
#include <cstdlib>

using namespace ioccultcalc;

class AllnumDatabase {
private:
    sqlite3* db_;
    std::string db_path_;
    
    static int callback(void* data, int argc, char** argv, char** azColName) {
        return 0;
    }
    
public:
    AllnumDatabase(const std::string& db_path) : db_(nullptr), db_path_(db_path) {
        int rc = sqlite3_open(db_path.c_str(), &db_);
        if (rc) {
            throw std::runtime_error("Cannot open database: " + db_path + 
                                   " - " + sqlite3_errmsg(db_));
        }
        initializeSchema();
    }
    
    ~AllnumDatabase() {
        if (db_) {
            sqlite3_close(db_);
        }
    }
    
    void initializeSchema() {
        const char* schema = R"(
            CREATE TABLE IF NOT EXISTS allnum_asteroids (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                number INTEGER UNIQUE NOT NULL,
                designation TEXT UNIQUE NOT NULL,
                epoch_mjd REAL NOT NULL,
                a REAL NOT NULL,
                e REAL NOT NULL,
                i REAL NOT NULL,
                Omega REAL NOT NULL,
                omega REAL NOT NULL,
                M REAL NOT NULL,
                H REAL,
                G REAL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
            
            CREATE INDEX IF NOT EXISTS idx_number ON allnum_asteroids(number);
            CREATE INDEX IF NOT EXISTS idx_designation ON allnum_asteroids(designation);
            CREATE INDEX IF NOT EXISTS idx_epoch ON allnum_asteroids(epoch_mjd);
            CREATE INDEX IF NOT EXISTS idx_magnitude ON allnum_asteroids(H);
            
            CREATE TABLE IF NOT EXISTS allnum_metadata (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                download_date TIMESTAMP NOT NULL,
                total_records INTEGER NOT NULL,
                file_url TEXT,
                file_format TEXT,
                data_epoch_mjd REAL,
                last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
        )";
        
        char* err_msg = nullptr;
        int rc = sqlite3_exec(db_, schema, nullptr, nullptr, &err_msg);
        if (rc != SQLITE_OK) {
            std::string error = err_msg ? std::string(err_msg) : "Unknown error";
            sqlite3_free(err_msg);
            throw std::runtime_error("Schema creation failed: " + error);
        }
    }
    
    bool needsUpdate(int max_age_days = 30) {
        const char* sql = R"(
            SELECT download_date, total_records 
            FROM allnum_metadata 
            ORDER BY download_date DESC 
            LIMIT 1
        )";
        
        sqlite3_stmt* stmt;
        int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
        if (rc != SQLITE_OK) {
            return true; // Se errore, assume che serve update
        }
        
        if (sqlite3_step(stmt) != SQLITE_ROW) {
            sqlite3_finalize(stmt);
            return true; // Nessun record, serve update
        }
        
        // Leggi data download
        const char* date_str = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
        int total_records = sqlite3_column_int(stmt, 1);
        
        sqlite3_finalize(stmt);
        
        if (total_records == 0) {
            return true; // Database vuoto
        }
        
        // Parse data (formato: YYYY-MM-DD HH:MM:SS)
        std::tm tm = {};
        std::istringstream ss(date_str);
        ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");
        
        if (ss.fail()) {
            return true; // Data invalida, serve update
        }
        
        std::time_t download_time = std::mktime(&tm);
        std::time_t now = std::time(nullptr);
        double days_diff = std::difftime(now, download_time) / (24.0 * 3600.0);
        
        return days_diff > max_age_days;
    }
    
    void clearDatabase() {
        const char* sql = "DELETE FROM allnum_asteroids; DELETE FROM allnum_metadata;";
        char* err_msg = nullptr;
        int rc = sqlite3_exec(db_, sql, nullptr, nullptr, &err_msg);
        if (rc != SQLITE_OK) {
            std::string error = err_msg ? std::string(err_msg) : "Unknown error";
            sqlite3_free(err_msg);
            throw std::runtime_error("Failed to clear database: " + error);
        }
    }
    
    void insertAsteroid(const OrbitalElements& elem, int number) {
        const char* sql = R"(
            INSERT OR REPLACE INTO allnum_asteroids 
            (number, designation, epoch_mjd, a, e, i, Omega, omega, M, H, G, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
        )";
        
        sqlite3_stmt* stmt;
        int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
        if (rc != SQLITE_OK) {
            throw std::runtime_error("Failed to prepare statement: " + 
                                   std::string(sqlite3_errmsg(db_)));
        }
        
        sqlite3_bind_int(stmt, 1, number);
        sqlite3_bind_text(stmt, 2, elem.designation.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_double(stmt, 3, elem.epoch.toMJD());
        sqlite3_bind_double(stmt, 4, elem.a);
        sqlite3_bind_double(stmt, 5, elem.e);
        sqlite3_bind_double(stmt, 6, elem.i);  // già in radianti
        sqlite3_bind_double(stmt, 7, elem.Omega);  // già in radianti
        sqlite3_bind_double(stmt, 8, elem.omega);  // già in radianti
        sqlite3_bind_double(stmt, 9, elem.M);  // già in radianti
        sqlite3_bind_double(stmt, 10, elem.H);
        sqlite3_bind_double(stmt, 11, elem.G);
        
        rc = sqlite3_step(stmt);
        if (rc != SQLITE_DONE) {
            std::string error = sqlite3_errmsg(db_);
            sqlite3_finalize(stmt);
            throw std::runtime_error("Failed to insert asteroid: " + error);
        }
        
        sqlite3_finalize(stmt);
    }
    
    void insertMetadata(int total_records, const std::string& url, 
                       double data_epoch_mjd) {
        const char* sql = R"(
            INSERT INTO allnum_metadata 
            (download_date, total_records, file_url, file_format, data_epoch_mjd)
            VALUES (datetime('now'), ?, ?, 'OEF2.0', ?)
        )";
        
        sqlite3_stmt* stmt;
        int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
        if (rc != SQLITE_OK) {
            throw std::runtime_error("Failed to prepare metadata statement");
        }
        
        sqlite3_bind_int(stmt, 1, total_records);
        sqlite3_bind_text(stmt, 2, url.c_str(), -1, SQLITE_STATIC);
        sqlite3_bind_double(stmt, 3, data_epoch_mjd);
        
        rc = sqlite3_step(stmt);
        if (rc != SQLITE_DONE) {
            std::string error = sqlite3_errmsg(db_);
            sqlite3_finalize(stmt);
            throw std::runtime_error("Failed to insert metadata: " + error);
        }
        
        sqlite3_finalize(stmt);
    }
    
    int getRecordCount() {
        const char* sql = "SELECT COUNT(*) FROM allnum_asteroids";
        sqlite3_stmt* stmt;
        int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
        if (rc != SQLITE_OK) {
            return 0;
        }
        
        if (sqlite3_step(stmt) == SQLITE_ROW) {
            int count = sqlite3_column_int(stmt, 0);
            sqlite3_finalize(stmt);
            return count;
        }
        
        sqlite3_finalize(stmt);
        return 0;
    }
};

// Parse single line from allnum.cat (OEF2.0 format)
OrbitalElements parseAllnumLine(const std::string& line, int& number) {
    if (line.length() < 190) {
        throw std::runtime_error("Line too short");
    }
    
    // Extract number from quotes
    size_t firstQuote = line.find('\'');
    size_t secondQuote = line.find('\'', firstQuote + 1);
    if (secondQuote == std::string::npos) {
        throw std::runtime_error("Invalid line format");
    }
    
    std::string number_str = line.substr(firstQuote + 1, secondQuote - firstQuote - 1);
    number = std::stoi(number_str);
    
    OrbitalElements elem;
    elem.designation = number_str;
    
    // Parse using fixed-width positions (OEF2.0 format)
    // Epoch (MJD) - positions 15-27
    std::string mjd_str = line.substr(15, 13);
    double mjd = std::stod(mjd_str);
    elem.epoch.jd = mjd + 2400000.5;
    
    // a (AU) - positions 30-52 (scientific format)
    std::string a_str = line.substr(30, 23);
    elem.a = std::stod(a_str);
    
    // e - positions 55-77 (scientific format)
    std::string e_str = line.substr(55, 23);
    elem.e = std::stod(e_str);
    
    // i (inclination) - positions 80-102 (scientific format, in DEGREES)
    std::string i_str = line.substr(80, 23);
    double i_deg = std::stod(i_str);
    elem.i = i_deg * M_PI / 180.0; // Convert to radians
    
    // Omega - positions 105-127 (scientific format, in DEGREES)
    std::string Omega_str = line.substr(105, 23);
    double Omega_deg = std::stod(Omega_str);
    elem.Omega = Omega_deg * M_PI / 180.0; // Convert to radians
    
    // omega - positions 130-152 (scientific format, in DEGREES)
    std::string omega_str = line.substr(130, 23);
    double omega_deg = std::stod(omega_str);
    elem.omega = omega_deg * M_PI / 180.0; // Convert to radians
    
    // M (mean anomaly) - positions 155-177 (scientific format, in DEGREES)
    std::string M_str = line.substr(155, 23);
    double M_deg = std::stod(M_str);
    elem.M = M_deg * M_PI / 180.0; // Convert to radians
    
    // H - positions 178-183
    std::string H_str = line.substr(178, 6);
    if (!H_str.empty() && H_str.find_first_not_of(" \t") != std::string::npos) {
        elem.H = std::stod(H_str);
    } else {
        elem.H = 15.0; // default
    }
    
    // G - positions 185-189
    std::string G_str = line.substr(185, 5);
    if (!G_str.empty() && G_str.find_first_not_of(" \t") != std::string::npos) {
        elem.G = std::stod(G_str);
    } else {
        elem.G = 0.15; // default
    }
    
    return elem;
}

double parseAllnumFile(const std::string& filepath, AllnumDatabase& db) {
    std::ifstream file(filepath);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + filepath);
    }
    
    std::string line;
    bool foundHeader = false;
    int count = 0;
    double data_epoch_mjd = 0.0;
    
    std::cout << "Parsing allnum.cat...\n";
    
    while (std::getline(file, line)) {
        if (line.find("END_OF_HEADER") != std::string::npos) {
            foundHeader = true;
            continue;
        }
        if (!foundHeader) continue;
        
        // Skip comment lines
        if (line.empty() || line[0] == '!') continue;
        
        // Parse line: 'number' epoch a e i Omega omega M H G flag
        if (line.find("'") == 0) {
            try {
                int number;
                OrbitalElements elem = parseAllnumLine(line, number);
                
                // Store epoch for metadata (all records have same epoch)
                if (count == 0) {
                    data_epoch_mjd = elem.epoch.toMJD();
                }
                
                db.insertAsteroid(elem, number);
                count++;
                
                if (count % 10000 == 0) {
                    std::cout << "  Processed " << count << " asteroids...\n";
                }
            } catch (const std::exception& e) {
                // Skip invalid lines
                continue;
            }
        }
    }
    
    std::cout << "✓ Parsed " << count << " asteroids\n";
    return data_epoch_mjd;
}

int main(int argc, char* argv[]) {
    bool force = false;
    std::string db_path;
    int max_age_days = 30;
    
    // Parse arguments
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg == "--force") {
            force = true;
        } else if (arg == "--db-path" && i + 1 < argc) {
            db_path = argv[++i];
        } else if (arg == "--max-age" && i + 1 < argc) {
            max_age_days = std::stoi(argv[++i]);
        } else if (arg == "--help" || arg == "-h") {
            std::cout << "Usage: " << argv[0] << " [options]\n"
                      << "Options:\n"
                      << "  --force          Force update even if database is recent\n"
                      << "  --db-path PATH   Specify database path\n"
                      << "  --max-age DAYS   Maximum age in days before update (default: 30)\n"
                      << "  --help           Show this help\n";
            return 0;
        }
    }
    
    // Default database path
    if (db_path.empty()) {
        DataManager dm;
        db_path = dm.getDatabasePath() + "/allnum.db";
    }
    
    try {
        std::cout << "📊 Allnum Database Builder\n";
        std::cout << "Database path: " << db_path << "\n\n";
        
        AllnumDatabase db(db_path);
        
        // Check if update needed
        if (!force && !db.needsUpdate(max_age_days)) {
            int count = db.getRecordCount();
            std::cout << "✓ Database is up-to-date (" << count << " records)\n";
            std::cout << "  Use --force to update anyway\n";
            return 0;
        }
        
        std::cout << "Downloading allnum.cat from AstDyS...\n";
        std::string url = "https://newton.spacedys.com/~astdys2/catalogs/allnum.cat";
        std::string temp_file = "/tmp/allnum.cat";
        
        // Download using curl
        std::string cmd = "curl -s --max-time 300 -o " + temp_file + " \"" + url + "\"";
        int ret = system(cmd.c_str());
        if (ret != 0) {
            throw std::runtime_error("Failed to download allnum.cat from " + url);
        }
        
        // Check if file was downloaded
        std::ifstream check_file(temp_file);
        if (!check_file.good()) {
            throw std::runtime_error("Downloaded file is empty or invalid");
        }
        check_file.close();
        
        std::cout << "✓ Downloaded allnum.cat\n";
        std::cout << "Building database...\n";
        
        // Clear old data
        db.clearDatabase();
        
        // Parse and insert
        double data_epoch_mjd = parseAllnumFile(temp_file, db);
        
        // Insert metadata
        int total_records = db.getRecordCount();
        db.insertMetadata(total_records, url, data_epoch_mjd);
        
        std::cout << "\n✅ Database built successfully!\n";
        std::cout << "   Total records: " << total_records << "\n";
        std::cout << "   Database: " << db_path << "\n";
        
        // Cleanup
        std::remove(temp_file.c_str());
        
    } catch (const std::exception& e) {
        std::cerr << "❌ Error: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
