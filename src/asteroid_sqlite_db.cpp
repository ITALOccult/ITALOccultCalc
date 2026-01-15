#include "ioccultcalc/asteroid_sqlite_db.h"
#include "ioccultcalc/data_manager.h"
#include <iostream>
#include <sstream>

namespace ioccultcalc {

AsteroidSqliteDatabase::AsteroidSqliteDatabase() {
    std::string path = DataManager::instance().getDatabaseDir() + "/asteroids.db";
    initializeDatabase(path);
}

AsteroidSqliteDatabase::AsteroidSqliteDatabase(const std::string& dbPath) {
    initializeDatabase(dbPath);
}

AsteroidSqliteDatabase::~AsteroidSqliteDatabase() {
    if (db_) {
        sqlite3_close(db_);
        db_ = nullptr;
    }
}

void AsteroidSqliteDatabase::initializeDatabase(const std::string& path) {
    dbPath_ = path;
    db_ = nullptr;
    
    int rc = sqlite3_open_v2(path.c_str(), &db_, SQLITE_OPEN_READONLY, nullptr);
    if (rc != SQLITE_OK) {
        if (db_) {
            sqlite3_close(db_);
            db_ = nullptr;
        }
    }
}

std::optional<AsteroidProperties> AsteroidSqliteDatabase::getProperties(int mpcNumber) {
    if (!db_) return std::nullopt;

    const char* sql = R"(
        SELECT 
            a.mpc_number, a.primary_name, a.discovery_designation,
            o.semimajor_axis_au, o.eccentricity, o.inclination_deg, 
            o.longitude_asc_node_deg, o.argument_perihelion_deg, o.mean_anomaly_deg,
            o.epoch_mjd,
            p.diameter_km, p.H_mag, p.albedo
        FROM asteroids a
        LEFT JOIN orbits o ON a.id = o.asteroid_id
        LEFT JOIN physical p ON a.id = p.asteroid_id
        WHERE a.mpc_number = ?
        ORDER BY o.created_at DESC, p.created_at DESC
        LIMIT 1
    )";

    sqlite3_stmt* stmt;
    if (sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr) != SQLITE_OK) {
        return std::nullopt;
    }

    sqlite3_bind_int(stmt, 1, mpcNumber);

    std::optional<AsteroidProperties> result;
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        result = mapRowToProperties(stmt);
    }

    sqlite3_finalize(stmt);
    return result;
}

std::optional<OrbitalElements> AsteroidSqliteDatabase::getOrbitalElements(int mpcNumber) {
    auto props = getProperties(mpcNumber);
    if (!props) return std::nullopt;

    OrbitalElements elem;
    elem.number = props->number;
    elem.designation = props->designation;
    elem.name = props->name;
    elem.a = props->a;
    elem.e = props->e;
    elem.i = props->i * (M_PI / 180.0);
    elem.Omega = props->om * (M_PI / 180.0);
    elem.omega = props->w * (M_PI / 180.0);
    elem.M = props->ma * (M_PI / 180.0);
    elem.epoch.jd = props->epoch + 2400000.5;
    elem.H = props->H;
    elem.diameter = props->diameter;
    
    // Default metadata from DB
    elem.frame = FrameType::ECLIPTIC_J2000;
    elem.type = ElementType::MEAN_ASTDYS; // Assuming ASTDYS is the source as seen in earlier check
    
    return elem;
}

bool AsteroidSqliteDatabase::hasAsteroid(int mpcNumber) {
    if (!db_) return false;
    const char* sql = "SELECT 1 FROM asteroids WHERE mpc_number = ? LIMIT 1";
    sqlite3_stmt* stmt;
    if (sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr) != SQLITE_OK) return false;
    sqlite3_bind_int(stmt, 1, mpcNumber);
    bool found = (sqlite3_step(stmt) == SQLITE_ROW);
    sqlite3_finalize(stmt);
    return found;
}

int AsteroidSqliteDatabase::getAsteroidCount() {
    if (!db_) return 0;
    const char* sql = "SELECT COUNT(*) FROM asteroids";
    sqlite3_stmt* stmt;
    if (sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr) != SQLITE_OK) return 0;
    int count = 0;
    if (sqlite3_step(stmt) == SQLITE_ROW) count = sqlite3_column_int(stmt, 0);
    sqlite3_finalize(stmt);
    return count;
}

AsteroidProperties AsteroidSqliteDatabase::mapRowToProperties(sqlite3_stmt* stmt) {
    AsteroidProperties props;
    props.number = sqlite3_column_int(stmt, 0);
    
    const char* name = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
    if (name) props.name = name;
    
    const char* desig = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));
    if (desig) props.designation = desig;

    // Elements
    props.a = sqlite3_column_double(stmt, 3);
    props.e = sqlite3_column_double(stmt, 4);
    props.i = sqlite3_column_double(stmt, 5);
    props.om = sqlite3_column_double(stmt, 6);
    props.w = sqlite3_column_double(stmt, 7);
    props.ma = sqlite3_column_double(stmt, 8);
    props.epoch = sqlite3_column_double(stmt, 9);
    
    if (sqlite3_column_type(stmt, 10) != SQLITE_NULL) {
        props.diameter = sqlite3_column_double(stmt, 10);
        props.has_diameter = true;
    }
    
    props.H = sqlite3_column_double(stmt, 11);
    
    if (sqlite3_column_type(stmt, 12) != SQLITE_NULL) {
        props.albedo = sqlite3_column_double(stmt, 12);
        props.has_albedo = true;
    }

    return props;
}

std::vector<AsteroidProperties> AsteroidSqliteDatabase::query(const AsteroidRange& range) {
    std::vector<AsteroidProperties> results;
    if (!db_) return results;

    // For now, simpler implementation: load all and filter manually 
    // OR implement SQL where clause builder.
    // Given the request, let's start with basic loading.
    
    // To be efficient, we should at least use the range if possible.
    std::stringstream sql;
    sql << R"(
        SELECT 
            a.mpc_number, a.primary_name, a.discovery_designation,
            o.semimajor_axis_au, o.eccentricity, o.inclination_deg, 
            o.longitude_asc_node_deg, o.argument_perihelion_deg, o.mean_anomaly_deg,
            o.epoch_mjd,
            p.diameter_km, p.H_mag, p.albedo
        FROM asteroids a
        LEFT JOIN orbits o ON a.id = o.asteroid_id
        LEFT JOIN physical p ON a.id = p.asteroid_id
    )";
    
    if (range.getFrom() > 0 && range.getTo() > 0) {
        sql << " WHERE a.mpc_number BETWEEN " << range.getFrom() << " AND " << range.getTo();
    }
    
    sqlite3_stmt* stmt;
    if (sqlite3_prepare_v2(db_, sql.str().c_str(), -1, &stmt, nullptr) != SQLITE_OK) {
        return results;
    }

    while (sqlite3_step(stmt) == SQLITE_ROW) {
        auto props = mapRowToProperties(stmt);
        if (range.matches(props)) {
            results.push_back(props);
        }
    }

    sqlite3_finalize(stmt);
    return results;
}

} // namespace ioccultcalc
