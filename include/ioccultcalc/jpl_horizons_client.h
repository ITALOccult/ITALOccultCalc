/**
 * @file jpl_horizons_client.h
 * @brief Client per JPL Horizons API
 * 
 * Interfaccia per scaricare effemeridi da JPL Horizons per confronto
 * e validazione della propagazione orbitale.
 */

#ifndef IOCCULTCALC_JPL_HORIZONS_CLIENT_H
#define IOCCULTCALC_JPL_HORIZONS_CLIENT_H

#include "types.h"
#include "time_utils.h"
#include <string>
#include <vector>
#include <memory>

namespace ioccultcalc {

/**
 * @brief Dati effemeridi da JPL Horizons
 */
struct HorizonsEphemeris {
    JulianDate epoch;
    Vector3D position;      // AU (heliocentrico)
    Vector3D velocity;      // AU/day (heliocentrico)
    double ra;              // radianti (geocentrico)
    double dec;             // radianti (geocentrico)
    double distance;        // AU (distanza Terra)
    double magnitude;       // mag apparente
    std::string targetName;
    
    HorizonsEphemeris() : ra(0), dec(0), distance(0), magnitude(0) {}
};

/**
 * @brief Opzioni per query JPL Horizons
 */
struct HorizonsQuery {
    std::string target;           // Es: "433" (Eros), "Ceres", "2024 AA"
    JulianDate startTime;
    JulianDate stopTime;
    double stepSize;              // giorni
    std::string center;           // "@sun" (helio), "500@10" (geo), "500" (geocentro)
    bool includeVectors;          // Include posizione/velocità cartesiane
    bool includeRADec;            // Include RA/Dec apparenti
    
    HorizonsQuery() 
        : stepSize(1.0), center("@sun"), 
          includeVectors(true), includeRADec(true) {}
};

/**
 * @brief Client per JPL Horizons
 * 
 * Scarica effemeridi precise da JPL Horizons per confronto
 * con propagazioni locali.
 * 
 * Note:
 * - Usa l'API batch di Horizons (telnet-style interface via HTTP)
 * - Richiede connessione internet
 * - Rate limit: ~1 query/secondo
 * 
 * Esempi di target ID:
 * - Asteroidi numerati: "433" (Eros)
 * - Nomi: "Ceres", "Vesta"
 * - Designazioni: "2024 AA"
 * - Pianeti: "599" (Giove), "399" (Terra)
 */
class JPLHorizonsClient {
public:
    JPLHorizonsClient();
    ~JPLHorizonsClient();
    
    /**
     * @brief Scarica effemeridi per un oggetto
     * 
     * @param query Parametri della query
     * @return Vector di effemeridi
     * @throws std::runtime_error se query fallisce
     */
    std::vector<HorizonsEphemeris> getEphemerides(const HorizonsQuery& query);
    
    /**
     * @brief Scarica singola effemerìde a una data
     * 
     * @param target ID oggetto (es: "433")
     * @param epoch Epoca richiesta
     * @param center Centro di riferimento ("@sun", "500")
     * @return Effemerìde
     */
    HorizonsEphemeris getEphemeris(const std::string& target,
                                   const JulianDate& epoch,
                                   const std::string& center = "@sun");
    
    /**
     * @brief Scarica vettori di stato (posizione/velocità)
     * 
     * @param target ID oggetto
     * @param epoch Epoca
     * @param center Centro ("@sun" per heliocentrico)
     * @return Posizione e velocità in AU e AU/day
     */
    std::pair<Vector3D, Vector3D> getStateVectors(const std::string& target,
                                                   const JulianDate& epoch,
                                                   const std::string& center = "@sun");
    
    /**
     * @brief Scarica coordinate apparenti (RA/Dec)
     * 
     * @param target ID oggetto
     * @param epoch Epoca osservativa
     * @param observerCode Codice osservatorio MPC (default "500" = geocentro)
     * @return (RA, Dec) in radianti
     */
    std::pair<double, double> getApparentCoordinates(const std::string& target,
                                                     const JulianDate& epoch,
                                                     const std::string& observerCode = "500");
    
    /**
     * @brief Imposta URL base di Horizons
     * 
     * Default: https://ssd.jpl.nasa.gov/api/horizons.api
     */
    void setBaseURL(const std::string& url);
    
    /**
     * @brief Imposta timeout richieste HTTP (secondi)
     */
    void setTimeout(int seconds);
    
    /**
     * @brief Imposta email per identificazione (opzionale ma consigliato)
     * 
     * JPL Horizons richiede identificazione per uso intensivo
     */
    void setEmail(const std::string& email);
    
    /**
     * @brief Verifica disponibilità oggetto
     * 
     * @param target ID oggetto
     * @return true se oggetto trovato
     */
    bool isTargetAvailable(const std::string& target);
    
private:
    class Impl;
    std::unique_ptr<Impl> pImpl;
    
    // Parsing risposta Horizons
    std::vector<HorizonsEphemeris> parseHorizonsOutput(const std::string& response);
    std::pair<Vector3D, Vector3D> parseVectors(const std::string& response);
    std::pair<double, double> parseRADec(const std::string& response);
    
    // Costruzione query Horizons
    std::string buildQuery(const HorizonsQuery& query);
    std::string buildVectorQuery(const std::string& target,
                                 const JulianDate& start,
                                 const JulianDate& stop,
                                 const std::string& center);
};

/**
 * @brief Confronta stato propagato con JPL Horizons
 * 
 * Utility per validare accuratezza propagatore
 * 
 * @param localPosition Posizione calcolata localmente (AU)
 * @param localVelocity Velocità calcolata (AU/day)
 * @param horizonsPosition Posizione da JPL (AU)
 * @param horizonsVelocity Velocità da JPL (AU/day)
 * @return Errore in km (posizione) e mm/s (velocità)
 */
std::pair<double, double> compareWithHorizons(
    const Vector3D& localPosition,
    const Vector3D& localVelocity,
    const Vector3D& horizonsPosition,
    const Vector3D& horizonsVelocity
);

} // namespace ioccultcalc

#endif // IOCCULTCALC_JPL_HORIZONS_CLIENT_H
