/**
 * @file asteroid_database.h
 * @brief Database locale di proprietà fisiche e orbitali degli asteroidi
 * @author IOccultCalc Team
 * @date 2025-11-22
 * 
 * Gestisce un database locale in JSON con:
 * - Elementi orbitali da MPC/AstDyS
 * - Proprietà fisiche (diametro, albedo, H)
 * - Classificazione spettrale e orbitale
 * - Aggiornamento automatico da fonti online
 */

#ifndef IOCCULTCALC_ASTEROID_DATABASE_H
#define IOCCULTCALC_ASTEROID_DATABASE_H

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <nlohmann/json.hpp>
#include "asteroid_filter.h"
#include "asteroid_sqlite_db.h"

namespace ioccultcalc {

/**
 * @brief Sorgente dati per il database
 */
enum class DataSource {
    MPC_MPCORB,           // MPC MPCORB.DAT (tutti gli asteroidi)
    MPC_EXTENDED_JSON,    // MPC mpcorb_extended.json.gz (con proprietà fisiche)
    ASTDYS,               // AstDyS (numbered asteroids)
    JPL_SBDB,             // JPL Small-Body Database Query
    LOCAL_CACHE           // Cache locale già scaricata
};

/**
 * @brief Statistiche del database
 */
struct DatabaseStats {
    int total_asteroids;
    int with_diameter;
    int with_albedo;
    int with_spectral_type;
    int with_rotation_period;
    int numbered;
    int unnumbered;
    std::string last_update;
    std::string source;
    
    DatabaseStats() : total_asteroids(0), with_diameter(0), with_albedo(0),
                     with_spectral_type(0), with_rotation_period(0),
                     numbered(0), unnumbered(0) {}
};

/**
 * @brief Database di asteroidi con filtraggio
 */
class AsteroidDatabase {
public:
    /**
     * @brief Costruttore con path al file database
     * @param dbPath Path al file JSON del database (default: ~/.ioccultcalc/asteroid_db.json)
     */
    explicit AsteroidDatabase(const std::string& dbPath = "");
    
    // ========================================================================
    // Caricamento e aggiornamento dati
    // ========================================================================
    
    /**
     * @brief Scarica database completo da sorgente online
     * @param source Sorgente dati da utilizzare
     * @param maxAsteroids Numero massimo di asteroidi (-1 = tutti)
     * @return true se successo
     */
    bool downloadDatabase(DataSource source = DataSource::MPC_EXTENDED_JSON, 
                         int maxAsteroids = -1);
    
    /**
     * @brief Carica database da file locale
     * @return true se successo
     */
    bool loadFromFile();
    
    /**
     * @brief Carica database da file specifico
     * @param path Path del file
     * @return true se successo
     */
    bool loadFromFile(const std::string& path);
    
    /**
     * @brief Salva database su file locale
     * @return true se successo
     */
    bool saveToFile();
    
    /**
     * @brief Salva database su file specifico
     * @param path Path del file
     * @return true se successo
     */
    bool saveToFile(const std::string& path);
    
    /**
     * @brief Aggiungi asteroide al database
     * @param props Proprietà dell'asteroide
     */
    void addAsteroid(const AsteroidProperties& props);
    
    /**
     * @brief Aggiorna singolo asteroide
     * @param number Numero asteroide
     * @param source Sorgente da cui scaricare
     * @return true se successo
     */
    bool updateAsteroid(int number, DataSource source = DataSource::ASTDYS);
    
    /**
     * @brief Aggiorna range di asteroidi
     * @param from Inizio range
     * @param to Fine range
     * @param source Sorgente da cui scaricare
     * @return Numero asteroidi aggiornati con successo
     */
    int updateRange(int from, int to, DataSource source = DataSource::ASTDYS);
    
    // ========================================================================
    // Query e filtraggio
    // ========================================================================
    
    /**
     * @brief Ottieni lista asteroidi che matchano il filtro
     * @param range Range con filtri WHERE/WHERENOT
     * @return Vettore di proprietà asteroidi che soddisfano i filtri
     */
    std::vector<AsteroidProperties> query(const AsteroidRange& range) const;
    
    /**
     * @brief Ottieni proprietà di un singolo asteroide
     * @param number Numero asteroide
     * @return Proprietà asteroide (o default se non trovato)
     */
    AsteroidProperties getProperties(int number) const;
    
    /**
     * @brief Verifica se asteroide è nel database
     * @param number Numero asteroide
     * @return true se presente
     */
    bool hasAsteroid(int number) const;
    
    /**
     * @brief Conta asteroidi che matchano il filtro
     * @param range Range con filtri
     * @return Numero asteroidi che soddisfano i filtri
     */
    int count(const AsteroidRange& range) const;
    
    /**
     * @brief Ottieni solo numeri asteroidi che matchano (senza caricare tutte le proprietà)
     * @param range Range con filtri
     * @return Vettore di numeri asteroidi
     */
    std::vector<int> queryNumbers(const AsteroidRange& range) const;
    
    // ========================================================================
    // Statistiche e informazioni
    // ========================================================================
    
    /**
     * @brief Ottieni statistiche del database
     * @return Struttura con statistiche
     */
    DatabaseStats getStats() const;
    
    /**
     * @brief Verifica se database necessita aggiornamento
     * @param maxAgeDays Età massima in giorni (default: 30)
     * @return true se più vecchio di maxAgeDays
     */
    bool needsUpdate(int maxAgeDays = 30) const;
    
    /**
     * @brief Ottieni path del file database
     * @return Path completo al file
     */
    std::string getPath() const { return dbPath_; }
    
    /**
     * @brief Ottieni dimensione database in memoria
     * @return Bytes occupati
     */
    size_t getMemoryUsage() const;
    
    // ========================================================================
    // Utilità
    // ========================================================================
    
    /**
     * @brief Esporta subset del database in JSON
     * @param range Range da esportare
     * @param outputPath Path file output
     * @return true se successo
     */
    bool exportToJson(const AsteroidRange& range, const std::string& outputPath) const;
    
    /**
     * @brief Importa dati da file JSON esterno
     * @param inputPath Path file input
     * @param merge Se true, fa merge con dati esistenti, altrimenti sostituisce
     * @return Numero record importati
     */
    int importFromJson(const std::string& inputPath, bool merge = true);
    
    /**
     * @brief Pulisci database (rimuovi tutti i dati)
     */
    void clear();
    
    /**
     * @brief Ottieni path default per il database
     * @return Path: ~/.ioccultcalc/asteroid_db.json
     */
    static std::string getDefaultPath();
    
private:
    std::string dbPath_;
    std::map<int, AsteroidProperties> asteroids_;
    std::unique_ptr<AsteroidSqliteDatabase> sqliteDb_;
    DatabaseStats stats_;
    std::string lastUpdateDate_;
    
    // Parsing helpers
    AsteroidProperties parseMPCExtendedLine(const std::string& line);
    AsteroidProperties parseMPCExtendedJson(const nlohmann::json& j);
    void updateStats();
    
    // Download helpers
    bool downloadMPCExtendedJson(int maxAsteroids);
    bool downloadFromAstDyS(int from, int to);
    bool downloadFromJPL(int number);
    
    // Utility
    std::string getCurrentDate() const;
    bool extractGzipFile(const std::string& gzPath, const std::string& outPath);
};

/**
 * @brief Builder per query complesse al database
 */
class DatabaseQueryBuilder {
public:
    DatabaseQueryBuilder(const AsteroidDatabase& db);
    
    // Range
    DatabaseQueryBuilder& from(int start);
    DatabaseQueryBuilder& to(int end);
    DatabaseQueryBuilder& numbers(const std::vector<int>& list);
    
    // Filtri WHERE
    DatabaseQueryBuilder& where(const std::string& condition);
    DatabaseQueryBuilder& whereDiameter(double min, double max = 1e9);
    DatabaseQueryBuilder& whereHMagnitude(double min, double max = 99);
    DatabaseQueryBuilder& whereOrbitClass(const std::vector<std::string>& classes);
    
    // Filtri WHERENOT
    DatabaseQueryBuilder& whereNot(const std::string& condition);
    
    // Sorting
    DatabaseQueryBuilder& orderByDiameter(bool descending = true);
    DatabaseQueryBuilder& orderByH(bool descending = false);
    DatabaseQueryBuilder& orderByNumber(bool descending = false);
    
    // Limiti
    DatabaseQueryBuilder& limit(int count);
    DatabaseQueryBuilder& offset(int skip);
    
    // Esecuzione
    std::vector<AsteroidProperties> execute() const;
    std::vector<int> executeNumbers() const;
    int count() const;
    
private:
    const AsteroidDatabase& db_;
    AsteroidRange range_;
    int limit_;
    int offset_;
    std::string orderBy_;
    bool descending_;
    
    void applyOrdering(std::vector<AsteroidProperties>& results) const;
};

/**
 * @brief Gestore download con progress callback
 */
class DatabaseDownloader {
public:
    using ProgressCallback = std::function<void(int current, int total, const std::string& message)>;
    
    DatabaseDownloader();
    
    /**
     * @brief Scarica MPC MPCORB extended JSON
     * @param outputPath Dove salvare il file
     * @param callback Callback per progresso (opzionale)
     * @return true se successo
     */
    bool downloadMPCExtended(const std::string& outputPath, 
                            ProgressCallback callback = nullptr);
    
    /**
     * @brief Scarica subset da AstDyS
     * @param from Inizio range
     * @param to Fine range
     * @param outputPath Dove salvare il file
     * @param callback Callback per progresso (opzionale)
     * @return Numero asteroidi scaricati
     */
    int downloadAstDySRange(int from, int to, const std::string& outputPath,
                           ProgressCallback callback = nullptr);
    
    /**
     * @brief Cancella download in corso
     */
    void cancel();
    
private:
    bool cancelled_;
    std::string downloadUrl(const std::string& url);
    bool parseAstDySCatalog(const std::string& catalogPath, int from, int to,
                           nlohmann::json& output, ProgressCallback callback);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ASTEROID_DATABASE_H
