#ifndef IOCCULTCALC_ASTEROID_SQLITE_DB_H
#define IOCCULTCALC_ASTEROID_SQLITE_DB_H

#include "ioccultcalc/asteroid_filter.h"
#include "ioccultcalc/orbital_elements.h"
#include <sqlite3.h>
#include <string>
#include <vector>
#include <optional>
#include <memory>

namespace ioccultcalc {

/**
 * @class AsteroidSqliteDatabase
 * @brief Reader per il database SQLite asteroids.db (schema v3)
 * 
 * Gestisce l'accesso al database unificato degli asteroidi che contiene:
 * - Dati anagrafici (numero, nome, designazione)
 * - Elementi orbitali (da diverse fonti: ASTDYS, JPL, MPC)
 * - Proprietà fisiche (diametro, albedo, H, G)
 */
class AsteroidSqliteDatabase {
public:
    AsteroidSqliteDatabase();
    explicit AsteroidSqliteDatabase(const std::string& dbPath);
    ~AsteroidSqliteDatabase();

    /**
     * @brief Ottieni proprietà fisiche e orbitali di un asteroide
     * @param mpcNumber Numero MPC dell'asteroide
     * @return Proprietà dell'asteroide se trovato
     */
    std::optional<AsteroidProperties> getProperties(int mpcNumber);

    /**
     * @brief Ottieni elementi orbitali kepleriani
     * @param mpcNumber Numero MPC
     * @return Elementi orbitali se trovati
     */
    std::optional<OrbitalElements> getOrbitalElements(int mpcNumber);

    /**
     * @brief Verifica se l'asteroide è presente nel database
     */
    bool hasAsteroid(int mpcNumber);

    /**
     * @brief Ottieni il numero totale di asteroidi nel DB
     */
    int getAsteroidCount();

    /**
     * @brief Esegue una query filtrata (simile alla versione JSON)
     */
    std::vector<AsteroidProperties> query(const AsteroidRange& range);

    /**
     * @brief Verifica disponibilità DB
     */
    bool isAvailable() const { return db_ != nullptr; }

private:
    sqlite3* db_;
    std::string dbPath_;

    void initializeDatabase(const std::string& path);
    
    // Mapping helpers
    AsteroidProperties mapRowToProperties(sqlite3_stmt* stmt);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ASTEROID_SQLITE_DB_H
