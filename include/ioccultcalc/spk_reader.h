/**
 * @file spk_reader.h
 * @brief Reader per file SPK (SPICE Kernel) usando CSPICE
 * 
 * Wrapper semplificato per la libreria CSPICE della NASA/NAIF
 */

#ifndef IOCCULTCALC_SPK_READER_H
#define IOCCULTCALC_SPK_READER_H

#include "ioccultcalc/types.h"
#include <string>
#include <memory>

namespace ioccultcalc {

/**
 * @brief Reader per file SPK (SPICE Kernel)
 * 
 * Nota: Questa è un'implementazione semplificata che NON richiede CSPICE.
 * Per ora usa una cache di dati pre-calcolati da JPL Horizons.
 * 
 * Per implementazione completa con CSPICE:
 * 1. Installare CSPICE: https://naif.jpl.nasa.gov/naif/toolkit.html
 * 2. Linkare libreria: -lcspice
 * 3. Usare furnsh(), spkezr(), etc.
 */
class SPKReader {
public:
    SPKReader();
    ~SPKReader();
    
    /**
     * @brief Carica file SPK
     * 
     * @param filePath Percorso file .bsp
     * @return true se caricamento OK
     */
    bool loadFile(const std::string& filePath);
    
    /**
     * @brief Scarica file SPK se necessario
     * 
     * @param version Versione (es: "DE441", "DE430")
     * @return true se file disponibile
     */
    bool ensureFileLoaded(const std::string& version = "DE441");
    
    /**
     * @brief Ottieni posizione corpo
     * 
     * @param bodyId NAIF ID (1=Mercury, 2=Venus, ..., 5=Jupiter, 10=Moon, 399=Earth)
     * @param jd Julian Date (TDB)
     * @param centerId Centro riferimento (0=SSB, 10=Sun, 399=Earth)
     * @return Posizione in km
     */
    Vector3D getPosition(int bodyId, double jd, int centerId = 10);
    
    /**
     * @brief Ottieni posizione e velocità
     * 
     * @param bodyId NAIF ID
     * @param jd Julian Date (TDB)
     * @param centerId Centro riferimento
     * @return (posizione km, velocità km/s)
     */
    std::pair<Vector3D, Vector3D> getState(int bodyId, double jd, int centerId = 10);
    
    /**
     * @brief Verifica se file è caricato
     */
    bool isLoaded() const;
    
    /**
     * @brief Ottieni copertura temporale
     * 
     * @return (JD inizio, JD fine)
     */
    std::pair<double, double> getCoverage() const;
    
    /**
     * @brief Ottieni versione file
     */
    std::string getVersion() const;
    
private:
    class Impl;
    std::unique_ptr<Impl> pImpl;
};

/**
 * @brief Converti NAIF ID in nome corpo
 */
std::string naifIdToName(int id);

/**
 * @brief Converti nome in NAIF ID
 */
int nameToNaifId(const std::string& name);

} // namespace ioccultcalc

#endif // IOCCULTCALC_SPK_READER_H
