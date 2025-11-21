/**
 * @file star_catalog.h
 * @brief Gestione cataloghi stellari con moto proprio e parallasse
 * 
 * Estende GaiaClient con:
 * - Propagazione posizioni con moto proprio completo
 * - Correzione parallasse annuale
 * - Velocità radiale per correzioni Doppler
 * - Query ottimizzate per occultazioni
 */

#ifndef IOCCULTCALC_STAR_CATALOG_H
#define IOCCULTCALC_STAR_CATALOG_H

#include "ioccultcalc/types.h"
#include "ioccultcalc/gaia_client.h"
#include <string>
#include <vector>

namespace ioccultcalc {

/**
 * @struct ProperMotion
 * @brief Moto proprio completo
 */
struct ProperMotion {
    double pmra;      // mas/yr in RA*cos(dec)
    double pmdec;     // mas/yr in Dec
    double pmra_error;   // mas/yr
    double pmdec_error;  // mas/yr
    double pmra_pmdec_corr; // Correlazione [-1,1]
    
    /**
     * @brief Calcola moto proprio totale
     * @return Velocità angolare (mas/yr)
     */
    double totalProperMotion() const;
    
    /**
     * @brief Calcola angolo posizione moto proprio
     * @return Angolo (gradi, nord=0, est=90)
     */
    double positionAngle() const;
};

/**
 * @struct StarData
 * @brief Dati stellari completi per calcolo preciso
 */
struct StarData {
    // Identificazione
    int64_t source_id;        // Gaia DR3 source_id
    std::string designation;  // Gaia DR3 designation
    
    // Posizione (ICRS, epoca riferimento)
    EquatorialCoordinates position;  // RA, Dec (rad)
    JulianDate epoch;                // Epoca riferimento (J2016.0 per DR3)
    
    // Moto proprio
    ProperMotion properMotion;
    
    // Parallasse
    double parallax;          // mas
    double parallax_error;    // mas
    
    // Velocità radiale
    double radialVelocity;    // km/s (positiva se allontana)
    double radialVelocity_error; // km/s
    bool hasRadialVelocity;
    
    // Fotometria
    double G_mag;             // Magnitudine G
    double BP_mag;            // Magnitudine BP
    double RP_mag;            // Magnitudine RP
    
    // Qualità astrometrica
    double astrometric_excess_noise;
    int astrometric_n_good_obs;
    double ruwe;              // Renormalized Unit Weight Error
    
    /**
     * @brief Propaga posizione a nuova epoca
     * Include moto proprio, parallasse annuale, aberrazione
     * 
     * @param targetEpoch Epoca target
     * @param observerPos Posizione osservatore eliocentric (AU)
     * @return Posizione apparente corretta
     */
    EquatorialCoordinates propagateToEpoch(
        const JulianDate& targetEpoch,
        const Vector3D& observerPos = Vector3D{0,0,0}) const;
    
    /**
     * @brief Calcola distanza dalla parallasse
     * @return Distanza in parsec (o -1 se parallasse negativa/invalida)
     */
    double distanceFromParallax() const;
    
    /**
     * @brief Calcola moto proprio in coordinate cartesiane
     * @return Velocità tangenziale (km/s)
     */
    Vector3D tangentialVelocity() const;
    
    /**
     * @brief Calcola velocità spaziale totale
     * @return Velocità 3D (km/s)
     */
    Vector3D spaceVelocity() const;
    
    /**
     * @brief Verifica qualità astrometrica
     * @return true se stella affidabile per astrometria
     */
    bool isAstrometricQualityGood() const;
    
    /**
     * @brief Stima diametro angolare
     * Usa relazione mag-diametro per stelle di sequenza principale
     * @return Diametro (mas)
     */
    double estimateAngularDiameter() const;
};

/**
 * @class StarCatalog
 * @brief Gestione catalogo stellare con funzioni avanzate
 */
class StarCatalog {
public:
    StarCatalog();
    ~StarCatalog();
    
    /**
     * @brief Query stelle per occultazione
     * 
     * Ricerca ottimizzata: considera moto proprio per predire
     * posizioni future e cercare in regione corretta
     * 
     * @param centerRA RA centro ricerca (deg)
     * @param centerDec Dec centro ricerca (deg)
     * @param radius Raggio ricerca (deg)
     * @param targetEpoch Epoca target (per moto proprio)
     * @param maxMagnitude Magnitudine limite
     * @param minQuality Qualità minima astrometrica
     * @return Vettore stelle con posizioni propagate
     */
    std::vector<StarData> queryForOccultation(
        double centerRA,
        double centerDec,
        double radius,
        const JulianDate& targetEpoch,
        double maxMagnitude = 15.0,
        bool minQuality = true);
    
    /**
     * @brief Query stelle lungo traccia
     * 
     * Cerca stelle lungo un path (es. traccia asteroide)
     * utile per identificare occultazioni multiple
     * 
     * @param path Vettore coordinate lungo traccia
     * @param searchWidth Larghezza banda ricerca (deg)
     * @param targetEpoch Epoca
     * @param maxMagnitude Mag limite
     * @return Stelle candidate
     */
    std::vector<StarData> queryAlongPath(
        const std::vector<EquatorialCoordinates>& path,
        double searchWidth,
        const JulianDate& targetEpoch,
        double maxMagnitude = 15.0);
    
    /**
     * @brief Trova closest approach asteroide-stella
     * 
     * Calcola quando e a che distanza un asteroide passa
     * più vicino a una stella, considerando moti propri
     * 
     * @param asteroidEphemeris Effemeridi asteroide (vettore tempo/pos)
     * @param star Dati stella
     * @return Coppia: tempo closest approach e distanza minima (arcsec)
     */
    std::pair<JulianDate, double> findClosestApproach(
        const std::vector<std::pair<JulianDate, EquatorialCoordinates>>& asteroidEphemeris,
        const StarData& star);
    
    /**
     * @brief Calcola probabilità occultazione
     * 
     * Stima probabilità basata su:
     * - Distanza minima asteroide-stella
     * - Diametro asteroide
     * - Incertezza orbitale
     * - Diametro stella
     * 
     * @param closestDistance Distanza minima (arcsec)
     * @param asteroidDiameter Diametro asteroide (km)
     * @param asteroidDistance Distanza asteroide (AU)
     * @param orbitalUncertainty Incertezza 1-sigma (arcsec)
     * @param starDiameter Diametro stella (mas)
     * @return Probabilità [0,1]
     */
    static double estimateOccultationProbability(
        double closestDistance,
        double asteroidDiameter,
        double asteroidDistance,
        double orbitalUncertainty,
        double starDiameter = 0.0);
    
    /**
     * @brief Cache locale stelle
     * Salva risultati query per uso offline
     */
    void enableLocalCache(const std::string& cacheDir);
    void cacheResults(const std::vector<StarData>& stars, 
                     const std::string& queryID);
    std::vector<StarData> loadFromCache(const std::string& queryID);
    
private:
    class Impl;
    Impl* pImpl;
};

/**
 * @class ProperMotionCorrection
 * @brief Correzioni precise per moto proprio
 */
class ProperMotionCorrection {
public:
    /**
     * @brief Applica moto proprio rigoroso
     * 
     * Include correzioni prospettiva, curva tura, foreshortening
     * secondo Stumpff (1985) e USNO Circular 179
     * 
     * @param pos0 Posizione epoca 0
     * @param pm Moto proprio
     * @param parallax Parallasse (mas)
     * @param rv Velocità radiale (km/s)
     * @param epoch0 Epoca iniziale
     * @param epoch1 Epoca finale
     * @return Posizione corretta a epoch1
     */
    static EquatorialCoordinates applyRigorous(
        const EquatorialCoordinates& pos0,
        const ProperMotion& pm,
        double parallax,
        double radialVelocity,
        const JulianDate& epoch0,
        const JulianDate& epoch1);
    
    /**
     * @brief Applica moto proprio lineare (semplificato)
     * Valido per stelle lontane (parallasse piccola)
     */
    static EquatorialCoordinates applyLinear(
        const EquatorialCoordinates& pos0,
        const ProperMotion& pm,
        const JulianDate& epoch0,
        const JulianDate& epoch1);
    
    /**
     * @brief Calcola correzione prospettiva (foreshortening)
     * Correzione dovuta al moto radiale che cambia la parallasse
     */
    static double perspectiveCorrection(
        double parallax,
        double radialVelocity,
        double timeInterval); // anni
    
    /**
     * @brief Calcola correzione curvatura traiettoria
     * Per stelle vicine con moto proprio grande
     */
    static EquatorialCoordinates curvatureCorrection(
        const EquatorialCoordinates& pos,
        const ProperMotion& pm,
        double parallax,
        double timeInterval); // anni
};

/**
 * @class ParallaxCorrection
 * @brief Correzioni parallasse annuale e diurna
 */
class ParallaxCorrection {
public:
    /**
     * @brief Calcola correzione parallasse annuale
     * 
     * Converte da baricentro sistema solare a geocentro
     * 
     * @param starPos Posizione baricentrica stella
     * @param parallax Parallasse (mas)
     * @param earthPos Posizione Terra eliocentric (AU)
     * @return Correzione coordinate (arcsec)
     */
    static EquatorialCoordinates annualParallax(
        const EquatorialCoordinates& starPos,
        double parallax,
        const Vector3D& earthPos);
    
    /**
     * @brief Calcola correzione parallasse diurna
     * 
     * Converte da geocentro a topocentro
     * 
     * @param starPos Posizione geocentrica
     * @param observerPos Posizione osservatore da centro Terra (km)
     * @param starDistance Distanza stella (AU)
     * @return Correzione coordinate (arcsec)
     */
    static EquatorialCoordinates diurnalParallax(
        const EquatorialCoordinates& starPos,
        const Vector3D& observerPos,
        double starDistance);
    
    /**
     * @brief Calcola parallasse combinata (annuale + diurna)
     */
    static EquatorialCoordinates totalParallax(
        const EquatorialCoordinates& starPos,
        double parallax,
        const Vector3D& earthPos,
        const Vector3D& observerPos);
};

/**
 * @struct StarCatalogStatistics
 * @brief Statistiche su catalogo/query
 */
struct StarCatalogStatistics {
    int totalStars;
    int starsWithProperMotion;
    int starsWithParallax;
    int starsWithRadialVelocity;
    int starsGoodAstrometry;
    
    double meanMagnitude;
    double meanProperMotion;
    double meanParallax;
    
    void print() const;
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_STAR_CATALOG_H
