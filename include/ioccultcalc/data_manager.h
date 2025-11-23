/**
 * @file data_manager.h
 * @brief Gestione centralizzata di tutti i file scaricati (database, effemeridi, IERS, ecc.)
 * @author IOccultCalc Team
 * @date 2025-11-22
 * 
 * Gestisce la directory ~/.ioccultcalc/ con:
 * - asteroid_db.json (database asteroidi)
 * - ephemerides/ (file SPK JPL)
 * - iers/ (file IERS Earth orientation)
 * - cache/ (cache temporanea)
 * - logs/ (log files)
 */

#ifndef IOCCULTCALC_DATA_MANAGER_H
#define IOCCULTCALC_DATA_MANAGER_H

#include <string>
#include <vector>
#include <map>

namespace ioccultcalc {

/**
 * @brief Gestore centralizzato directory dati
 */
class DataManager {
public:
    /**
     * @brief Ottieni istanza singleton
     */
    static DataManager& instance();
    
    /**
     * @brief Inizializza directory struttura (crea se non esiste)
     * @return true se successo
     */
    bool initialize();
    
    // ========================================================================
    // Path Management
    // ========================================================================
    
    /**
     * @brief Ottieni root directory
     * @return Path: ~/.ioccultcalc/
     */
    std::string getRootDir() const;
    
    /**
     * @brief Ottieni directory database asteroidi
     * @return Path: ~/.ioccultcalc/database/
     */
    std::string getDatabaseDir() const;
    
    /**
     * @brief Ottieni path database asteroidi principale
     * @return Path: ~/.ioccultcalc/database/asteroid_db.json
     */
    std::string getAsteroidDatabasePath() const;
    
    /**
     * @brief Ottieni directory effemeridi JPL
     * @return Path: ~/.ioccultcalc/ephemerides/
     */
    std::string getEphemeridesDir() const;
    
    /**
     * @brief Ottieni path file SPK specifico
     * @param name Nome file (es: "de441.bsp")
     * @return Path completo
     */
    std::string getEphemerisPath(const std::string& name) const;
    
    /**
     * @brief Ottieni directory dati IERS
     * @return Path: ~/.ioccultcalc/iers/
     */
    std::string getIERSDir() const;
    
    /**
     * @brief Ottieni path file IERS specifico
     * @param name Nome file (es: "finals2000A.all")
     * @return Path completo
     */
    std::string getIERSPath(const std::string& name) const;
    
    /**
     * @brief Ottieni directory cache temporanea
     * @return Path: ~/.ioccultcalc/cache/
     */
    std::string getCacheDir() const;
    
    /**
     * @brief Ottieni directory log
     * @return Path: ~/.ioccultcalc/logs/
     */
    std::string getLogsDir() const;
    
    /**
     * @brief Ottieni directory configurazioni
     * @return Path: ~/.ioccultcalc/config/
     */
    std::string getConfigDir() const;
    
    // ========================================================================
    // File Management
    // ========================================================================
    
    /**
     * @brief Verifica se file esiste
     * @param path Path completo o relativo alla root
     * @return true se esiste
     */
    bool fileExists(const std::string& path) const;
    
    /**
     * @brief Ottieni dimensione file
     * @param path Path al file
     * @return Dimensione in bytes, 0 se non esiste
     */
    size_t getFileSize(const std::string& path) const;
    
    /**
     * @brief Ottieni età file in giorni
     * @param path Path al file
     * @return Giorni dall'ultima modifica, -1 se non esiste
     */
    int getFileAgeDays(const std::string& path) const;
    
    /**
     * @brief Cancella file
     * @param path Path al file
     * @return true se successo
     */
    bool deleteFile(const std::string& path);
    
    /**
     * @brief Pulisci cache (cancella file più vecchi di N giorni)
     * @param maxAgeDays Età massima in giorni (default: 30)
     * @return Numero file cancellati
     */
    int cleanCache(int maxAgeDays = 30);
    
    // ========================================================================
    // Disk Usage
    // ========================================================================
    
    /**
     * @brief Ottieni spazio disco usato totale
     * @return Bytes occupati
     */
    size_t getTotalDiskUsage() const;
    
    /**
     * @brief Ottieni spazio disco per categoria
     * @return Map categoria -> bytes
     */
    std::map<std::string, size_t> getDiskUsageByCategory() const;
    
    /**
     * @brief Ottieni lista di tutti i file gestiti
     * @return Vettore di path relativi
     */
    std::vector<std::string> listAllFiles() const;
    
    // ========================================================================
    // Setup & Maintenance
    // ========================================================================
    
    /**
     * @brief Verifica integrità directory e file
     * @return true se tutto OK
     */
    bool verifyIntegrity();
    
    /**
     * @brief Ripara struttura directory (ricrea mancanti)
     * @return true se successo
     */
    bool repairStructure();
    
    /**
     * @brief Ottieni informazioni stato sistema
     * @return Stringa multi-linea con info
     */
    std::string getSystemInfo() const;
    
private:
    DataManager();
    ~DataManager() = default;
    DataManager(const DataManager&) = delete;
    DataManager& operator=(const DataManager&) = delete;
    
    std::string rootDir_;
    bool initialized_;
    
    bool createDirectory(const std::string& path);
    std::string expandHome(const std::string& path) const;
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_DATA_MANAGER_H
