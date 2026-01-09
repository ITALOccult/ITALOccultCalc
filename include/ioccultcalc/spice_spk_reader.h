/**
 * @file spice_spk_reader.h
 * @brief Lettore file SPK usando NAIF SPICE Toolkit
 * 
 * Permette di leggere file SPK standard (es. asteroidi) usando CSPICE
 */

#ifndef IOCCULTCALC_SPICE_SPK_READER_H
#define IOCCULTCALC_SPICE_SPK_READER_H

#include "ioccultcalc/types.h"
#include "ioccultcalc/isp_reader.h"
#include <string>
#include <vector>
#include <memory>
#include "ioccultcalc/coordinates.h"

namespace ioccultcalc {

/**
 * @brief Lettore file SPK usando CSPICE
 * 
 * Supporta file SPK standard (format SPICE) per asteroidi e altri corpi
 */
class SPICESPKReader : public ISPReader {
public:
    SPICESPKReader();
    ~SPICESPKReader();
    
    // Non copiabile
    SPICESPKReader(const SPICESPKReader&) = delete;
    SPICESPKReader& operator=(const SPICESPKReader&) = delete;
    
    /**
     * @brief Carica file SPK
     * 
     * @param filepath Percorso file SPK
     * @return true se caricato con successo
     */
    bool loadFile(const std::string& filepath);
    
    /**
     * @brief Scarica file se necessario
     * 
     * @param name Nome file (es: "codes_300ast_20100725.bsp")
     * @return true se disponibile
     */
    bool ensureFileLoaded(const std::string& name = "codes_300ast_20100725.bsp");
    
    /**
     * @brief Ottieni posizione corpo
     * 
     * @param bodyId NAIF ID (2000001 = Ceres, 2000004 = Vesta, ...)
     * @param jd Julian Date (TDB)
     * @param centerId Centro riferimento (0=SSB, 10=Sun)
     * @return Posizione in AU
     */
    Vector3D getPosition(int bodyId, double jd, int centerId = 10);
    
    /**
     * @brief Ottieni posizione e velocità
     * 
     * @param bodyId NAIF ID
     * @param jd Julian Date (TDB)
     * @param centerId Centro riferimento
     * @return (posizione AU, velocità AU/day)
     */
    std::pair<Vector3D, Vector3D> getState(int bodyId, double jd, int centerId = 10);
    
    /**
     * @brief Verifica se file è caricato
     */
    bool isLoaded() const;
    
    /**
     * @brief Lista corpi disponibili nel file
     */
    std::vector<int> getAvailableBodies() const;
    
    /**
     * @brief Chiudi file
     */
    void close();
    
private:
    class Impl;
    std::unique_ptr<Impl> pImpl;
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_SPICE_SPK_READER_H
