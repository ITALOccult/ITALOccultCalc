/**
 * @file build_allnum_database.cpp
 * @brief Tool ibrido per creare database SQLite da AstDyS (allnum.cat) e MPC (JSON)
 * 
 * Logic:
 * 1. Scarica mpcorb_extended.json.gz (MPC) -> Estrae Nomi e metadati fisici (H, G)
 * 2. Scarica allnum.cat (AstDyS) -> Estrae elementi orbitali MEDI (ECL J2000)
 * 3. Inserisce record AstDyS come primari (MEAN_ASTDYS)
 * 4. Inserisce record MPC rimanenti come secondari (OSCULATING)
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
#include <cstdlib>
#include <set>
#include <map>
#include <memory>
#include <nlohmann/json.hpp>

using namespace ioccultcalc;
using json = nlohmann::json;

struct MPCObject {
    std::string designation;
    std::string number_str;
    std::string name;
    double H = 15.0;
    double G = 0.15;
    double epoch = 0;
    double a = 0, e = 0, i = 0, node = 0, peri = 0, M = 0;
    bool already_processed = false;
};

class AllnumDatabase {
private:
    sqlite3* db_;
    
public:
    AllnumDatabase(const std::string& db_path) : db_(nullptr) {
        int rc = sqlite3_open(db_path.c_str(), &db_);
        if (rc) throw std::runtime_error("Cannot open database: " + std::string(sqlite3_errmsg(db_)));
        initializeSchema();
    }
    
    ~AllnumDatabase() { if (db_) sqlite3_close(db_); }
    
    void initializeSchema() {
        // Drop old table to ensure schema update if structure changed
        sqlite3_exec(db_, "DROP TABLE IF EXISTS allnum_asteroids;", nullptr, nullptr, nullptr);
        
        const char* schema = R"(
            CREATE TABLE IF NOT EXISTS allnum_asteroids (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                number INTEGER UNIQUE,
                designation TEXT NOT NULL,
                name TEXT,
                epoch_mjd REAL NOT NULL,
                a REAL NOT NULL, e REAL NOT NULL, i REAL NOT NULL,
                node_long REAL NOT NULL, peri_arg REAL NOT NULL, M REAL NOT NULL,
                H REAL, G REAL,
                frame_type TEXT DEFAULT 'ECLIPTIC_J2000',
                element_type TEXT DEFAULT 'OSCULATING',
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
            CREATE INDEX IF NOT EXISTS idx_number ON allnum_asteroids(number);
            CREATE INDEX IF NOT EXISTS idx_designation ON allnum_asteroids(designation);
            CREATE TABLE IF NOT EXISTS allnum_metadata (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                download_date TIMESTAMP NOT NULL,
                total_records INTEGER NOT NULL,
                file_url TEXT,
                file_format TEXT,
                data_epoch_mjd REAL
            );
        )";
        char* err_msg = nullptr;
        int rc = sqlite3_exec(db_, schema, nullptr, nullptr, &err_msg);
        if (rc != SQLITE_OK) {
            std::string err = err_msg ? err_msg : "Unknown error";
            std::cerr << "Schema error: " << err << std::endl;
            if (err_msg) sqlite3_free(err_msg);
            throw std::runtime_error("Failed to initialize schema");
        }
    }
    
    void clearDatabase() {
        sqlite3_exec(db_, "DELETE FROM allnum_asteroids; DELETE FROM allnum_metadata;", nullptr, nullptr, nullptr);
    }

    void begin() { sqlite3_exec(db_, "BEGIN TRANSACTION;", nullptr, nullptr, nullptr); }
    void commit() { sqlite3_exec(db_, "COMMIT;", nullptr, nullptr, nullptr); }

    void insertAsteroid(const OrbitalElements& elem, int number) {
        const char* sql = R"(
            INSERT OR REPLACE INTO allnum_asteroids 
            (number, designation, name, epoch_mjd, a, e, i, node_long, peri_arg, M, H, G, frame_type, element_type, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
        )";
        
        sqlite3_stmt* stmt;
        int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
        if (rc != SQLITE_OK) {
            throw std::runtime_error("Prepare failed: " + std::string(sqlite3_errmsg(db_)));
        }
        
        if (number > 0) sqlite3_bind_int(stmt, 1, number); else sqlite3_bind_null(stmt, 1);
        sqlite3_bind_text(stmt, 2, elem.designation.c_str(), -1, SQLITE_TRANSIENT);
        sqlite3_bind_text(stmt, 3, (elem.name.empty() ? "" : elem.name.c_str()), -1, SQLITE_TRANSIENT);
        sqlite3_bind_double(stmt, 4, elem.epoch.toMJD());
        sqlite3_bind_double(stmt, 5, elem.a);
        sqlite3_bind_double(stmt, 6, elem.e);
        sqlite3_bind_double(stmt, 7, elem.i);
        sqlite3_bind_double(stmt, 8, elem.Omega);
        sqlite3_bind_double(stmt, 9, elem.omega);
        sqlite3_bind_double(stmt, 10, elem.M);
        sqlite3_bind_double(stmt, 11, elem.H);
        sqlite3_bind_double(stmt, 12, elem.G);
        
        std::string frame_str = (elem.frame == FrameType::EQUATORIAL_ICRF) ? "EQUATORIAL_ICRF" : "ECLIPTIC_J2000";
        std::string type_str = (elem.type == ElementType::MEAN_ASTDYS) ? "MEAN_ASTDYS" : "OSCULATING";
        
        sqlite3_bind_text(stmt, 13, frame_str.c_str(), -1, SQLITE_TRANSIENT);
        sqlite3_bind_text(stmt, 14, type_str.c_str(), -1, SQLITE_TRANSIENT);
        
        rc = sqlite3_step(stmt);
        if (rc != SQLITE_DONE) {
            std::cerr << "Insert failed for " << elem.designation << " (" << elem.name << "): " << sqlite3_errmsg(db_) << std::endl;
        }
        sqlite3_finalize(stmt);
    }
};

bool parseAllnumLine(const std::string& line, int& number, OrbitalElements& elem) {
    if (line.length() < 180 || line[0] != '\'') return false;
    
    size_t secondQuote = line.find('\'', 1);
    if (secondQuote == std::string::npos) return false;
    
    std::string num_str = line.substr(1, secondQuote - 1);
    try {
        number = std::stoi(num_str);
    } catch (...) { number = -1; }
    
    elem.designation = num_str;
    
    try {
        elem.epoch.jd = std::stod(line.substr(15, 13)) + 2400000.5;
        elem.a = std::stod(line.substr(30, 23));
        elem.e = std::stod(line.substr(55, 23));
        double rad = M_PI / 180.0;
        elem.i = std::stod(line.substr(80, 23)) * rad;
        elem.Omega = std::stod(line.substr(105, 23)) * rad;
        elem.omega = std::stod(line.substr(130, 23)) * rad;
        elem.M = std::stod(line.substr(155, 23)) * rad;
        
        elem.H = std::stod(line.substr(178, 6));
        elem.G = std::stod(line.substr(185, 5));
        
        elem.frame = FrameType::ECLIPTIC_J2000;
        elem.type = ElementType::MEAN_ASTDYS;
        return true;
    } catch (...) { return false; }
}

int main(int argc, char* argv[]) {
    std::string db_path = DataManager::instance().getDatabaseDir() + "/allnum.db";
    
    try {
        std::cout << "🚀 Hybrid Database Builder (AstDyS MEAN + MPC INFO)\n";
        AllnumDatabase db(db_path);
        
        // 1. Download and Parse JSON for Names/Metadata
        std::string json_raw = "/tmp/mpcorb_extended.json";
        std::cout << "Indexing MPC JSON for names and metadata..." << std::endl;
        // In local environment we assume the file is already there or download it
        // curl -L -s -o /tmp/mpcorb_extended.json.gz "..." && gunzip -f /tmp/mpcorb_extended.json.gz
        
        std::ifstream ifs(json_raw);
        if (!ifs.is_open()) {
             std::cout << "Downloading MPC JSON..." << std::endl;
             std::string json_gz = "/tmp/mpcorb_extended.json.gz";
             system(("curl -L -s -o " + json_gz + " \"https://minorplanetcenter.net/Extended_Files/mpcorb_extended.json.gz\"").c_str());
             system(("gunzip -f " + json_gz).c_str());
             ifs.open(json_raw);
        }
        
        json mp_data;
        ifs >> mp_data;
        ifs.close();
        
        std::vector<std::shared_ptr<MPCObject>> mpc_list;
        std::map<std::string, std::shared_ptr<MPCObject>> mpc_by_desig;
        std::map<std::string, std::shared_ptr<MPCObject>> mpc_by_num;
        
        for (const auto& item : mp_data) {
            auto obj = std::make_shared<MPCObject>();
            obj->designation = item["Principal_desig"].get<std::string>();
            obj->H = item.value("H", 15.0);
            obj->G = item.value("G", 0.15);
            if (item.contains("Name") && !item["Name"].is_null()) {
                obj->name = item["Name"].get<std::string>();
            }
            if (item.contains("Number") && !item["Number"].is_null()) {
                std::string num_raw = item["Number"].get<std::string>();
                if (num_raw.size() > 2) {
                    obj->number_str = num_raw.substr(1, num_raw.size() - 2);
                    mpc_by_num[obj->number_str] = obj;
                }
            }
            mpc_by_desig[obj->designation] = obj;
            
            // Backup elements if not found in AstDyS
            obj->epoch = item["Epoch"].get<double>();
            obj->a = item["a"].get<double>();
            obj->e = item["e"].get<double>();
            obj->i = item.value("i", 0.0);
            obj->node = item.value("Node", 0.0);
            obj->peri = item.value("Peri", 0.0);
            obj->M = item.value("M", 0.0);
            
            mpc_list.push_back(obj);
        }
        
        // 2. Parse allnum.cat (AstDyS Mean Elements)
        std::cout << "Downloading AstDyS allnum.cat..." << std::endl;
        std::string allnum_file = "/tmp/allnum.cat";
        system(("curl -s -o " + allnum_file + " \"https://newton.spacedys.com/~astdys2/catalogs/allnum.cat\"").c_str());
        
        std::cout << "Building Database (Inserting MEAN elements from AstDyS)..." << std::endl;
        db.clearDatabase();
        db.begin();
        
        std::ifstream afs(allnum_file);
        std::string line;
        bool header = true;
        int count = 0;
        
        while (std::getline(afs, line)) {
            if (header) {
                if (line.find("END_OF_HEADER") != std::string::npos) header = false;
                continue;
            }
            
            int num;
            OrbitalElements elem;
            if (parseAllnumLine(line, num, elem)) {
                // Try to find matching name in MPC
                std::shared_ptr<MPCObject> match = nullptr;
                if (num > 0 && mpc_by_num.count(std::to_string(num))) {
                    match = mpc_by_num[std::to_string(num)];
                } else if (mpc_by_desig.count(elem.designation)) {
                    match = mpc_by_desig[elem.designation];
                }
                
                if (match) {
                    if (!match->name.empty()) {
                        elem.name = "(" + std::to_string(num) + ") " + match->name;
                    } else {
                        elem.name = "(" + std::to_string(num) + ")";
                    }
                    match->already_processed = true;
                } else {
                    elem.name = "(" + std::to_string(num) + ")";
                }
                
                db.insertAsteroid(elem, num);
                count++;
                if (count % 10000 == 0) std::cout << "  Processed " << count << " AstDyS Mean records...\n";
            }
        }
        afs.close();
        
        // 3. Add remaining from MPC (Osculating Elements)
        std::cout << "Adding remaining MPC records (Osculating elements not in AstDyS)..." << std::endl;
        int mpc_added = 0;
        for (const auto& obj : mpc_list) {
            if (obj->already_processed) continue;
            
            OrbitalElements elem;
            elem.designation = obj->designation;
            int number = -1;
            if (!obj->number_str.empty()) number = std::stoi(obj->number_str);
            
            if (!obj->name.empty()) {
                elem.name = (number > 0) ? "(" + std::to_string(number) + ") " + obj->name : obj->name;
            } else {
                elem.name = (number > 0) ? "(" + std::to_string(number) + ")" : obj->designation;
            }
            
            elem.epoch.jd = obj->epoch;
            double rad = M_PI / 180.0;
            elem.a = obj->a;
            elem.e = obj->e;
            elem.i = obj->i * rad;
            elem.Omega = obj->node * rad;
            elem.omega = obj->peri * rad;
            elem.M = obj->M * rad;
            elem.H = obj->H;
            elem.G = obj->G;
            
            elem.frame = FrameType::ECLIPTIC_J2000;
            elem.type = ElementType::OSCULATING;
            
            db.insertAsteroid(elem, number);
            mpc_added++;
        }
        
        db.commit();
        std::cout << "\n✅ SUCCESS!\n";
        std::cout << "  AstDyS (Elementi MEDI): " << count << "\n";
        std::cout << "  MPC (Elementi OSCULANTI): " << mpc_added << "\n";
        std::cout << "  Totale oggetti: " << (count + mpc_added) << "\n";
        
        std::remove(json_raw.c_str());
        std::remove(allnum_file.c_str());
        
    } catch (const std::exception& e) {
        std::cerr << "❌ ERROR: " << e.what() << std::endl;
        return 1;
    }
    return 0;
}
