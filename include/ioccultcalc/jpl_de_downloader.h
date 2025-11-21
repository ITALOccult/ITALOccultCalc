/**
 * @file jpl_de_downloader.h
 * @brief Downloader per file JPL Development Ephemerides (DE)
 * 
 * Scarica i file SPK (SPICE Kernel) da server JPL/NAIF
 */

#ifndef IOCCULTCALC_JPL_DE_DOWNLOADER_H
#define IOCCULTCALC_JPL_DE_DOWNLOADER_H

#include <string>
#include <vector>
#include <functional>

namespace ioccultcalc {

/**
 * @brief Informazioni su un file JPL DE disponibile
 */
struct JPLDEFileInfo {
    std::string name;           // es: "de441.bsp"
    std::string version;        // es: "DE441"
    std::string url;            // URL download
    double coverageStart;       // JD inizio
    double coverageEnd;         // JD fine
    size_t sizeBytes;           // Dimensione file
    std::string description;
    
    JPLDEFileInfo() : coverageStart(0), coverageEnd(0), sizeBytes(0) {}
};

/**
 * @brief Downloader file JPL DE
 */
class JPLDEDownloader {
public:
    JPLDEDownloader();
    ~JPLDEDownloader();
    
    /**
     * @brief Lista file JPL DE disponibili
     * @return Vector di file disponibili
     */
    std::vector<JPLDEFileInfo> listAvailableFiles();
    
    /**
     * @brief Scarica un file JPL DE
     * 
     * @param version Versione (es: "DE441", "DE440")
     * @param outputPath Percorso output (default: ~/.ioccultcalc/ephemerides/)
     * @param progressCallback Callback per progresso (opzionale)
     * @return Percorso file scaricato
     * @throws std::runtime_error se download fallisce
     */
    std::string downloadFile(const std::string& version,
                            const std::string& outputPath = "",
                            std::function<void(size_t, size_t)> progressCallback = nullptr);
    
    /**
     * @brief Verifica se file esiste localmente
     * 
     * @param version Versione
     * @param searchPath Percorso ricerca (default: ~/.ioccultcalc/ephemerides/)
     * @return Percorso file se esiste, stringa vuota altrimenti
     */
    std::string findLocalFile(const std::string& version,
                             const std::string& searchPath = "");
    
    /**
     * @brief Scarica file se non esiste, altrimenti restituisce percorso
     * 
     * @param version Versione
     * @return Percorso file (locale o scaricato)
     */
    std::string ensureFileAvailable(const std::string& version);
    
    /**
     * @brief Imposta directory cache
     * 
     * Default: ~/.ioccultcalc/ephemerides/
     */
    void setCacheDirectory(const std::string& path);
    
    /**
     * @brief Imposta URL base server
     * 
     * Default: https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/
     */
    void setBaseURL(const std::string& url);
    
    /**
     * @brief Verifica integrità file (checksum)
     * 
     * @param filePath Percorso file
     * @return true se integrità OK
     */
    bool verifyFileIntegrity(const std::string& filePath);
    
private:
    class Impl;
    std::unique_ptr<Impl> pImpl;
};

/**
 * @brief Ottieni percorso directory cache default
 * 
 * @return ~/.ioccultcalc/ephemerides/ (creata se non esiste)
 */
std::string getDefaultCacheDirectory();

/**
 * @brief Ottieni dimensione file
 * 
 * @param filePath Percorso file
 * @return Dimensione in bytes, 0 se non esiste
 */
size_t getFileSize(const std::string& filePath);

} // namespace ioccultcalc

#endif // IOCCULTCALC_JPL_DE_DOWNLOADER_H
