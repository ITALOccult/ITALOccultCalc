#ifndef IOCCULTCALC_CATALOG_MANAGER_H
#define IOCCULTCALC_CATALOG_MANAGER_H

#include <string>
#include <vector>
#include <memory>
#include <mutex>
#include <sqlite3.h>
#include "orbit_elements.h"
#include "julian_date.h"

namespace IOccultCalc {

/**
 * @class OEF2Parser
 * @brief Parser per il formato OEF2.0 del catalogo AstDyS2
 * 
 * OEF2.0 (Orbfit Equinoctial Format 2.0) ECLM J2000
 * Frame: Ecliptic Mean J2000
 * Reference: JPL DE441 ephemerides
 */
class OEF2Parser {
public:
    struct AsteroidRecord {
        int catalog_id;
        std::string designation;
        double magnitude_h;      // Absolute magnitude
        double slope_g;          // Slope parameter (for phase function)
        double a;                // Semi-major axis [AU]
        double h;                // e×sin(ϖ)
        double k;                // e×cos(ϖ)
        double p;                // tan(i/2)×sin(Ω)
        double q;                // tan(i/2)×cos(Ω)
        double lambda;           // Mean longitude [radians]
        double epoch_mjd;        // Epoch [MJD]
        std::string frame;       // "ECLM J2000"
        std::string reference;   // "JPL DE441"
    };
    
    /**
     * @brief Parse singola riga OEF2.0
     */
    static AsteroidRecord parseLine(const std::string& line);
    
    /**
     * @brief Carica catalogo completo da file
     */
    static std::vector<AsteroidRecord> loadCatalog(const std::string& filename);
};

/**
 * @class CatalogManager
 * @brief Gestore del catalogo locale offline OEF2.0
 * 
 * Features:
 * - Download automatico da AstDyS2
 * - Storage locale SQLite (O(1) lookup)
 * - Auto-update settimanale/mensile
 * - Thread-safe con mutex
 * - Fallback offline completo
 */
class CatalogManager {
public:
    enum UpdateStrategy {
        OFFLINE_ONLY,      ///< Non scarica mai, usa cache
        CHECK_WEEKLY,      ///< Controlla update ogni 7 giorni
        CHECK_MONTHLY,     ///< Controlla update ogni 30 giorni (DEFAULT)
        AUTO_UPDATE        ///< Scarica se nuova versione disponibile
    };
    
    /**
     * @brief Constructor
     * @param cache_dir Directory per storage catalogo (default: ~/.ioccultcalc/catalogs)
     * @param strategy Strategia auto-update
     */
    explicit CatalogManager(
        const std::string& cache_dir = "~/.ioccultcalc/catalogs",
        UpdateStrategy strategy = CHECK_MONTHLY
    );
    
    /**
     * @brief Destructor - chiude database
     */
    ~CatalogManager();
    
    /**
     * @brief Scarica e carica il catalogo completo
     * @param force Forza download anche se cache recente
     * @return true se successo
     */
    bool updateCatalog(bool force = false);
    
    /**
     * @brief Carica elementi orbitali per un asteroide
     * @param designation Designazione (es: "17030", "433", "2025 AB123")
     * @param epoch_out [OUT] Epoca degli elementi (MJD)
     * @return AstDynEquinoctialElements in frame ECLM J2000
     * @throw std::runtime_error se asteroide non trovato
     * 
     * Performance: O(1) lookup via indice SQLite
     */
    AstDynEquinoctialElements getElements(
        const std::string& designation,
        double* epoch_out = nullptr
    );
    
    /**
     * @brief Ricerca asteroidi per magnitudine
     * @param mag_min Magnitudine minima (brightest)
     * @param mag_max Magnitudine massima (dimmest)
     * @return Vettore designazioni
     */
    std::vector<std::string> searchByMagnitude(
        double mag_min,
        double mag_max
    );
    
    /**
     * @brief Statistiche catalogo
     */
    struct CatalogStats {
        size_t total_asteroids;
        double avg_magnitude;
        std::string version;
        std::string download_date;
        std::string data_epoch;
    };
    
    /**
     * @brief Ottieni statistiche
     */
    CatalogStats getStats();
    
    /**
     * @brief Controlla se catalogo è recente
     * @param days Soglia in giorni
     * @return true se ultimo download < days
     */
    bool isCatalogRecent(int days = 7);
    
    /**
     * @brief Ritorna true se database è disponibile (non vuoto)
     */
    bool isReady() const { return is_ready_; }

private:
    sqlite3* db_;
    std::string cache_dir_;
    UpdateStrategy strategy_;
    std::mutex db_mutex_;
    bool is_ready_;
    
    void initializeDatabase();
    std::string getCatalogPath();
    bool downloadFile(const std::string& url, const std::string& dest);
    bool loadIntoDatabase(const std::string& filepath);
    bool tableExists(const std::string& table_name);
    long countRecords();
};

} // namespace IOccultCalc

#endif // IOCCULTCALC_CATALOG_MANAGER_H
