/**
 * @file jpl_ephemeris.h
 * @brief Interfaccia per effemeridi planetarie JPL DE (Development Ephemerides)
 * 
 * Supporta JPL DE441 (2021) - la più recente e accurata:
 * - Copertura: 13200 BCE - 17191 CE
 * - Precisione: <1 km per pianeti interni su secoli
 * - Precisione: <100 m per pianeti esterni su decenni
 * - Include: Sole, 8 pianeti, Luna, Plutone, 343 asteroidi principali
 * - Parametri fisici: GM aggiornati, masse asteroidali
 * 
 * Le effemeridi JPL DE sono basate su:
 * - Osservazioni radar planetarie
 * - Telemetria spacecraft (Voyager, Cassini, New Horizons, etc.)
 * - Osservazioni ottiche storiche
 * - Timing pulsar (per SSB)
 * - Correzioni relativistiche complete (PN formulation)
 * 
 * Formato file:
 * - Binary SPK (SPICE kernel) format
 * - Chebyshev polynomial coefficients
 * - Interpolazione efficiente
 */

#ifndef IOCCULTCALC_JPL_EPHEMERIS_H
#define IOCCULTCALC_JPL_EPHEMERIS_H

#include "ioccultcalc/types.h"
#include <string>
#include <vector>
#include <memory>
#include <map>

namespace ioccultcalc {

/**
 * @enum JPLBody
 * @brief Corpi celesti disponibili nelle effemeridi JPL
 */
enum class JPLBody {
    MERCURY = 1,
    VENUS = 2,
    EARTH = 3,
    MARS = 4,
    JUPITER = 5,
    SATURN = 6,
    URANUS = 7,
    NEPTUNE = 8,
    PLUTO = 9,
    MOON = 10,
    SUN = 11,
    SOLAR_SYSTEM_BARYCENTER = 0,
    EARTH_MOON_BARYCENTER = 3  // EMB
};

/**
 * @enum JPLVersion
 * @brief Versioni supportate delle effemeridi JPL
 */
enum class JPLVersion {
    DE430,  // 2013-2021, copertura 1550-2650
    DE431,  // Long-term, copertura 13200 BCE - 17191 CE
    DE440,  // 2020, alta precisione spacecraft
    DE441   // 2021, most recent, recommended (default)
};

/**
 * @struct JPLEphemerisState
 * @brief Stato completo (posizione + velocità) da effemeridi JPL
 */
struct JPLEphemerisState {
    JulianDate epoch;
    Vector3D position;      // km (barycentric)
    Vector3D velocity;      // km/day (barycentric)
    JPLBody body;
    JPLVersion version;
    
    JPLEphemerisState();
    
    // Converte unità da km a AU
    Vector3D positionAU() const;
    Vector3D velocityAU_day() const;
};

/**
 * @struct JPLConstants
 * @brief Costanti fisiche dalle effemeridi JPL
 */
struct JPLConstants {
    double AU;              // Unità astronomica (km)
    double c;               // Velocità della luce (km/s)
    double GM_Sun;          // Parametro gravitazionale Sole (km³/s²)
    double GM_Earth;        // Terra + Luna
    double GM_Moon;         // Solo Luna
    double GM_planets[9];   // Mercurio-Plutone
    double EMRAT;           // Earth/Moon mass ratio
    double J2_Sun;          // Oblateness solare
    
    JPLConstants();
    
    // Carica da file DE
    static JPLConstants loadFromDE(JPLVersion version);
};

/**
 * @class JPLEphemerisReader
 * @brief Lettura effemeridi JPL in formato SPK/BSP
 * 
 * Implementa lettura e interpolazione dei file binari JPL:
 * - Formato SPICE SPK (Spacecraft/Planet Kernel)
 * - Chebyshev polynomial interpolation
 * - Efficiente caching dei coefficienti
 */
class JPLEphemerisReader {
public:
    JPLEphemerisReader();
    explicit JPLEphemerisReader(JPLVersion version);
    ~JPLEphemerisReader();
    
    /**
     * @brief Carica file effemeridi JPL
     * 
     * @param filepath Path al file .bsp (binary SPK)
     * @return true se caricamento riuscito
     */
    bool loadFile(const std::string& filepath);
    
    /**
     * @brief Scarica automaticamente file DE da NASA NAIF
     * 
     * @param version Versione da scaricare
     * @param cachePath Directory cache locale (default: ~/.ioccultcalc/jpl/)
     * @return true se download riuscito
     */
    bool downloadDE(JPLVersion version, 
                    const std::string& cachePath = "");
    
    /**
     * @brief Calcola posizione e velocità di un corpo
     * 
     * @param body Corpo celeste
     * @param jd Epoca (Julian Date TDB)
     * @return Stato (posizione + velocità) barycentric
     */
    JPLEphemerisState getState(JPLBody body, double jd) const;
    
    /**
     * @brief Calcola solo posizione (più veloce di getState)
     * 
     * @param body Corpo
     * @param jd Epoca
     * @return Posizione barycentric (km)
     */
    Vector3D getPosition(JPLBody body, double jd) const;
    
    /**
     * @brief Calcola solo velocità
     * 
     * @param body Corpo
     * @param jd Epoca
     * @return Velocità barycentric (km/day)
     */
    Vector3D getVelocity(JPLBody body, double jd) const;
    
    /**
     * @brief Ottiene posizione relativa (body rispetto a center)
     * 
     * @param body Corpo target
     * @param center Corpo centro (default: SSB)
     * @param jd Epoca
     * @return Posizione relativa (km)
     */
    Vector3D getRelativePosition(JPLBody body, 
                                 JPLBody center,
                                 double jd) const;
    
    /**
     * @brief Ottiene costanti fisiche
     * 
     * @return Costanti (AU, GM, etc.) dalla versione caricata
     */
    JPLConstants getConstants() const;
    
    /**
     * @brief Verifica se epoca è nel range coperto
     * 
     * @param jd Epoca
     * @return true se dentro coverage
     */
    bool isCovered(double jd) const;
    
    /**
     * @brief Ottiene intervallo temporale coperto
     * 
     * @param[out] jdStart Inizio copertura
     * @param[out] jdEnd Fine copertura
     */
    void getCoverage(double& jdStart, double& jdEnd) const;
    
    /**
     * @brief Ottiene versione caricata
     */
    JPLVersion getVersion() const { return version_; }
    
    /**
     * @brief Verifica se file è caricato
     */
    bool isLoaded() const { return loaded_; }
    
    /**
     * @brief Pre-carica coefficienti per intervallo
     * 
     * Ottimizzazione: pre-legge blocchi di coefficienti
     * 
     * @param jdStart Inizio intervallo
     * @param jdEnd Fine intervallo
     */
    void precache(double jdStart, double jdEnd);
    
    /**
     * @brief Pulisce cache
     */
    void clearCache();
    
    /**
     * @brief Ottiene informazioni sul file caricato
     */
    std::string getFileInfo() const;
    
private:
    JPLVersion version_;
    bool loaded_;
    
    // File info
    std::string filepath_;
    double jdStart_;
    double jdEnd_;
    
    // Costanti fisiche
    JPLConstants constants_;
    
    // Chebyshev coefficients storage
    struct ChebyshevRecord {
        double jdStart;
        double jdEnd;
        int nCoeff;
        int nSets;
        std::vector<double> coefficients;
    };
    
    // Cache dei record per body
    mutable std::map<std::pair<JPLBody, int>, ChebyshevRecord> recordCache_;
    
    // Metodi interni
    ChebyshevRecord readRecord(JPLBody body, double jd) const;
    Vector3D interpolateChebyshev(const ChebyshevRecord& record, 
                                  double jd,
                                  bool velocityToo) const;
    void evaluateChebyshev(double x, int n, 
                          std::vector<double>& T,
                          std::vector<double>& Tdot) const;
    
    // SPK file handling
    class SPKFile;
    std::unique_ptr<SPKFile> spkFile_;
};

/**
 * @class JPLEphemerisManager
 * @brief Manager high-level per effemeridi JPL
 * 
 * Gestisce:
 * - Download automatico files
 * - Selezione versione appropriata
 * - Caching intelligente
 * - Fallback tra versioni
 */
class JPLEphemerisManager {
public:
    /**
     * @brief Ottiene istanza singleton
     */
    static JPLEphemerisManager& getInstance();
    
    /**
     * @brief Inizializza con versione preferita
     * 
     * @param version Versione DE da usare (default: DE441)
     * @param autoDownload Se true, scarica automaticamente se mancante
     * @return true se inizializzazione riuscita
     */
    bool initialize(JPLVersion version = JPLVersion::DE441,
                   bool autoDownload = true);
    
    /**
     * @brief Ottiene reader corrente
     */
    JPLEphemerisReader& getReader();
    const JPLEphemerisReader& getReader() const;
    
    /**
     * @brief Imposta directory cache
     * 
     * @param path Directory per file .bsp
     */
    void setCacheDirectory(const std::string& path);
    
    /**
     * @brief Ottiene directory cache corrente
     */
    std::string getCacheDirectory() const;
    
    /**
     * @brief Verifica quali versioni sono disponibili localmente
     */
    std::vector<JPLVersion> getAvailableVersions() const;
    
    /**
     * @brief Ottiene path del file per una versione
     * 
     * @param version Versione
     * @return Path completo al file .bsp
     */
    std::string getFilePath(JPLVersion version) const;
    
    /**
     * @brief Scarica versione se non presente
     * 
     * @param version Versione da scaricare
     * @return true se già presente o download riuscito
     */
    bool ensureAvailable(JPLVersion version);
    
private:
    JPLEphemerisManager();
    ~JPLEphemerisManager();
    
    // Non copiabile
    JPLEphemerisManager(const JPLEphemerisManager&) = delete;
    JPLEphemerisManager& operator=(const JPLEphemerisManager&) = delete;
    
    std::unique_ptr<JPLEphemerisReader> reader_;
    std::string cacheDir_;
    JPLVersion currentVersion_;
};

/**
 * @namespace JPLUtils
 * @brief Utility functions per effemeridi JPL
 */
namespace JPLUtils {
    /**
     * @brief Converte nome corpo in JPLBody enum
     */
    JPLBody bodyFromName(const std::string& name);
    
    /**
     * @brief Converte JPLBody in nome
     */
    std::string nameFromBody(JPLBody body);
    
    /**
     * @brief Converte versione in stringa
     */
    std::string versionString(JPLVersion version);
    
    /**
     * @brief Ottiene URL download per versione
     */
    std::string getDownloadURL(JPLVersion version);
    
    /**
     * @brief Stima dimensione file (MB)
     */
    double estimateFileSize(JPLVersion version);
    
    /**
     * @brief Verifica integrità file (checksum)
     */
    bool verifyFile(const std::string& filepath, JPLVersion version);
}

} // namespace ioccultcalc

#endif // IOCCULTCALC_JPL_EPHEMERIS_H
