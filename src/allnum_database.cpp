/**
 * @file allnum_database.cpp
 * @brief Implementazione AllnumDatabaseReader per lettura database SQLite
 */

#include "ioccultcalc/allnum_database.h"
#include "ioccultcalc/types.h"
#include <iostream>
#include <cmath>

namespace ioccultcalc {

AllnumDatabaseReader::AllnumDatabaseReader() {
    std::string default_path = DataManager::instance().getDatabaseDir() + "/allnum.db";
    initializeDatabase(default_path);
}

AllnumDatabaseReader::AllnumDatabaseReader(const std::string& db_path) {
    initializeDatabase(db_path);
}

AllnumDatabaseReader::~AllnumDatabaseReader() {
    if (db_) {
        sqlite3_close(db_);
        db_ = nullptr;
    }
}

void AllnumDatabaseReader::initializeDatabase(const std::string& db_path) {
    db_path_ = db_path;
    is_ready_ = false;
    db_ = nullptr;
    
    int rc = sqlite3_open(db_path.c_str(), &db_);
    if (rc != SQLITE_OK) {
        // Database non esiste o non accessibile - non è un errore fatale
        if (db_) {
            sqlite3_close(db_);
            db_ = nullptr;
        }
        return;
    }
    
    // Verifica che le tabelle esistano
    const char* check_sql = "SELECT COUNT(*) FROM allnum_asteroids";
    sqlite3_stmt* stmt;
    rc = sqlite3_prepare_v2(db_, check_sql, -1, &stmt, nullptr);
    if (rc == SQLITE_OK) {
        if (sqlite3_step(stmt) == SQLITE_ROW) {
            is_ready_ = true;
        }
        sqlite3_finalize(stmt);
    }
    
    if (!is_ready_ && db_) {
        sqlite3_close(db_);
        db_ = nullptr;
    }
}

std::optional<OrbitalElements> AllnumDatabaseReader::getElement(int number) {
    return getElement(std::to_string(number));
}

std::optional<OrbitalElements> AllnumDatabaseReader::getElement(const std::string& designation) {
    if (!is_ready_ || !db_) {
        return std::nullopt;
    }
    
    // Converti designation a numero se possibile
    int number = 0;
    try {
        number = std::stoi(designation);
    } catch (...) {
        return std::nullopt;
    }
    
    const char* sql = R"(
        SELECT number, designation, name, epoch_mjd, a, e, i, node_long, peri_arg, M, H, G, frame_type, element_type
        FROM allnum_asteroids
        WHERE number = ?
        LIMIT 1
    )";
    
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        std::cerr << "[AllnumDatabaseReader] Prepare error: " << sqlite3_errmsg(db_) << "\n";
        return std::nullopt;
    }
    
    sqlite3_bind_int(stmt, 1, number);
    
    std::optional<OrbitalElements> result;
    
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        OrbitalElements elem;
        
        // Designation e Name
        const char* desig_str = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
        const char* name_str = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));
        
        elem.designation = desig_str ? desig_str : designation;
        std::string raw_name = name_str ? name_str : elem.designation;
        
        // Pulizia nome: se contiene il numero tra parentesi, rimuovilo
        // Es: "(34713) Yesiltas" -> "Yesiltas"
        size_t startBracket = raw_name.find('(');
        size_t endBracket = raw_name.find(')');
        if (startBracket != std::string::npos && endBracket != std::string::npos && endBracket > startBracket) {
            std::string numInBrackets = raw_name.substr(startBracket + 1, endBracket - startBracket - 1);
            // Se il numero tra parentesi corrisponde alla designazione, lo rimuoviamo
            if (numInBrackets == elem.designation || numInBrackets == designation) {
                raw_name.erase(startBracket, endBracket - startBracket + 1);
            }
        }
        
        // Rimuovi spazi extra iniziali/finali
        raw_name.erase(0, raw_name.find_first_not_of(" \t"));
        size_t last = raw_name.find_last_not_of(" \t");
        if (last != std::string::npos) raw_name.erase(last + 1);
        
        elem.name = raw_name;
        
        // Epoch (MJD) -> JD
        double epoch_mjd = sqlite3_column_double(stmt, 3);
        elem.epoch.jd = epoch_mjd + 2400000.5;
        
        // Elementi orbitali (già in radianti nel database allnum.db)
        elem.a = sqlite3_column_double(stmt, 4);
        elem.e = sqlite3_column_double(stmt, 5);
        elem.i = sqlite3_column_double(stmt, 6);
        elem.Omega = sqlite3_column_double(stmt, 7);
        elem.omega = sqlite3_column_double(stmt, 8);
        elem.M = sqlite3_column_double(stmt, 9);
        
        // H e G
        elem.H = sqlite3_column_double(stmt, 10);
        elem.G = sqlite3_column_double(stmt, 11);
        
        // Metadata di frame e tipo
        const char* frame_sql = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 12));
        const char* type_sql = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 13));
        
        if (frame_sql) {
            std::string frame(frame_sql);
            if (frame == "EQUATORIAL_ICRF") elem.frame = FrameType::EQUATORIAL_ICRF;
            else elem.frame = FrameType::ECLIPTIC_J2000;
        }
        
        if (type_sql) {
            std::string type(type_sql);
            if (type == "MEAN_ASTDYS") elem.type = ElementType::MEAN_ASTDYS;
            else elem.type = ElementType::OSCULATING;
        }
        
        result = elem;
    }
    
    sqlite3_finalize(stmt);
    return result;
}

int AllnumDatabaseReader::getRecordCount() const {
    if (!is_ready_ || !db_) {
        return 0;
    }
    
    const char* sql = "SELECT COUNT(*) FROM allnum_asteroids";
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return 0;
    }
    
    int count = 0;
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        count = sqlite3_column_int(stmt, 0);
    }
    
    sqlite3_finalize(stmt);
    return count;
}

std::string AllnumDatabaseReader::getLastUpdateDate() const {
    if (!is_ready_ || !db_) {
        return "";
    }
    
    const char* sql = R"(
        SELECT download_date 
        FROM allnum_metadata 
        ORDER BY download_date DESC 
        LIMIT 1
    )";
    
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return "";
    }
    
    std::string date;
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        const char* date_str = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
        if (date_str) {
            date = date_str;
        }
    }
    
    sqlite3_finalize(stmt);
    return date;
}

} // namespace ioccultcalc
