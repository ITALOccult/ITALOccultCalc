/**
 * @file asteroid_shape.h
 * @brief Modello forma asteroide per calcolo shadow preciso
 * 
 * Implementa:
 * - Ellissoide triassiale (a, b, c)
 * - Modello forma da lightcurve
 * - Database dimensioni/forme da DAMIT, SBNDB
 * - Calcolo ombra/penombra con raggio effettivo dipendente da viewing angle
 */

#ifndef IOCCULTCALC_ASTEROID_SHAPE_H
#define IOCCULTCALC_ASTEROID_SHAPE_H

#include "ioccultcalc/types.h"
#include "ioccultcalc/orbital_elements.h"
#include <string>

namespace ioccultcalc {

/**
 * @struct TriaxialEllipsoid
 * @brief Ellissoide triassiale per forma asteroide
 */
struct TriaxialEllipsoid {
    double a;  // Semiasse maggiore (km)
    double b;  // Semiasse intermedio (km)
    double c;  // Semiasse minore (km)
    
    // Polo di rotazione (J2000 equatoriale)
    double poleLambda;  // Longitudine eclittica polo (deg)
    double poleBeta;    // Latitudine eclittica polo (deg)
    
    // Periodo rotazione
    double rotationPeriod;  // ore
    
    // Angolo posizione a epoca (per calcolare orientamento)
    double phi0;        // Angolo rotazione a epoch0 (deg)
    JulianDate epoch0;  // Epoca riferimento
    
    /**
     * @brief Calcola raggio effettivo in direzione data
     * @param viewDir Direzione osservazione (unitaria)
     * @return Raggio effettivo (km)
     */
    double effectiveRadius(const Vector3D& viewDir) const;
    
    /**
     * @brief Calcola area proiettata
     * @param viewDir Direzione osservazione
     * @return Area cross-section (km²)
     */
    double projectedArea(const Vector3D& viewDir) const;
    
    /**
     * @brief Calcola orientamento a un dato tempo
     * @param jd Julian Date
     * @return Matrice rotazione body-fixed -> inertial
     */
    std::vector<std::vector<double>> rotationMatrix(const JulianDate& jd) const;
};

/**
 * @struct AsteroidShape
 * @brief Descrizione completa forma asteroide
 */
struct AsteroidShape {
    std::string designation;
    std::string name;
    
    // Ellissoide triassiale
    TriaxialEllipsoid ellipsoid;
    
    // Diametro equivalente (sferico)
    double equivalentDiameter;  // km
    
    // Incertezza dimensioni
    double diameterUncertainty; // km
    
    // Albedo geometrico
    double geometricAlbedo;
    
    // Source dei dati
    std::string dataSource;  // "DAMIT", "SBNDB", "occultation", "estimate"
    
    /**
     * @brief Crea shape da diametro sferico
     */
    static AsteroidShape fromDiameter(double diameter);
    
    /**
     * @brief Crea shape da magnitudine assoluta
     * Usa relazione H-diameter con albedo assunto
     */
    static AsteroidShape fromAbsoluteMagnitude(
        double H, double assumedAlbedo = 0.14);
};

/**
 * @class AsteroidShapeDatabase
 * @brief Database forme asteroidi da varie fonti
 */
class AsteroidShapeDatabase {
public:
    /**
     * @brief Carica shape da database
     * Cerca in ordine: DAMIT, SBNDB, catalogo occultazioni
     * 
     * @param designation Designazione asteroide
     * @return Shape se trovata, altrimenti stima da H
     */
    static AsteroidShape loadShape(const std::string& designation);
    
    /**
     * @brief Verifica se shape è disponibile
     */
    static bool hasShape(const std::string& designation);
    
    /**
     * @brief Download shape da DAMIT
     */
    static AsteroidShape downloadFromDAMIT(const std::string& designation);
    
    /**
     * @brief Download da JPL Small-Body Database
     */
    static AsteroidShape downloadFromSBNDB(const std::string& designation);
};

/**
 * @struct BesselianElements
 * @brief Elementi Besseliani per calcolo occultazione
 * 
 * Sistema di coordinate centrato sull'ombra dell'asteroide,
 * con asse z verso la stella
 */
struct BesselianElements {
    JulianDate t0;      // Epoca centrale
    
    // Coordinate ombra sul piano fondamentale (perpendicolare a stella)
    double x, y;        // Posizione centro ombra (km)
    double dx, dy;      // Velocità ombra (km/s)
    double d2x, d2y;    // Accelerazione ombra (km/s²)
    
    // Raggio ombra (considerando forma asteroide)
    double radius;      // km
    double radiusUmbra; // km (ombra totale)
    double radiusPenumbra; // km (ombra parziale)
    
    // Direzione stella (J2000)
    EquatorialCoordinates starPos;
    
    // Matrice rotazione: (X,Y,Z)_fundamental -> (x,y,z)_equatorial
    std::vector<std::vector<double>> rotationMatrix;
    
    /**
     * @brief Calcola posizione osservatore nel piano fondamentale
     * @param observerPos Posizione osservatore geocentrico (km)
     * @param time Tempo (JD)
     * @return Coordinate (ξ, η) nel piano fondamentale (km)
     */
    Vector3D observerInFundamentalPlane(
        const Vector3D& observerPos,
        const JulianDate& time) const;
    
    /**
     * @brief Calcola distanza minima osservatore-centro ombra
     * @param observerTrack Traccia osservatore nel piano (vettore posizioni)
     * @return Distanza minima (km) e tempo di closest approach
     */
    std::pair<double, JulianDate> closestApproach(
        const std::vector<std::pair<JulianDate, Vector3D>>& observerTrack) const;
    
    /**
     * @brief Determina se osservatore è nell'ombra
     * @param xi Coordinata ξ nel piano fondamentale
     * @param eta Coordinata η nel piano fondamentale
     * @param time Tempo
     * @return true se in ombra totale (umbra)
     */
    bool isInUmbra(double xi, double eta, const JulianDate& time) const;
    
    /**
     * @brief Determina se osservatore è in penombra
     */
    bool isInPenumbra(double xi, double eta, const JulianDate& time) const;
    
    /**
     * @brief Calcola frazione disco stellare coperta
     * @param xi Coordinata ξ
     * @param eta Coordinata η
     * @param time Tempo
     * @param starDiameter Diametro stella (mas)
     * @return Frazione coperta [0,1]
     */
    double occultationFraction(double xi, double eta,
                              const JulianDate& time,
                              double starDiameter = 0.0) const;
};

/**
 * @class BesselianCalculator
 * @brief Calcola elementi Besseliani per occultazione
 * 
 * Metodo classico usato per eclissi solari, adattato per occultazioni
 * asteroidali. Più accurato di semplice proiezione geometrica.
 */
class BesselianCalculator {
public:
    /**
     * @brief Calcola elementi Besseliani a un'epoca
     * 
     * @param asteroidPos Posizione asteroide geocentrico (AU)
     * @param asteroidVel Velocità asteroide (AU/giorno)
     * @param starPos Posizione stella (coordinate equatoriali)
     * @param asteroidShape Forma asteroide
     * @param epoch Epoca calcolo
     * @return Elementi Besseliani
     */
    static BesselianElements compute(
        const Vector3D& asteroidPos,
        const Vector3D& asteroidVel,
        const EquatorialCoordinates& starPos,
        const AsteroidShape& asteroidShape,
        const JulianDate& epoch);
    
    /**
     * @brief Calcola elementi per intervallo temporale
     * @param times Vettore epoche
     * @return Vettore elementi Besseliani
     */
    static std::vector<BesselianElements> computeSeries(
        const Vector3D& asteroidPos,
        const Vector3D& asteroidVel,
        const EquatorialCoordinates& starPos,
        const AsteroidShape& asteroidShape,
        const std::vector<JulianDate>& times);
    
    /**
     * @brief Calcola shadow path sulla Terra
     * 
     * Usa elementi Besseliani per tracciare path preciso
     * Include curvatura, accelerazione, forma elliossoidale
     * 
     * @param elements Elementi Besseliani centrali
     * @param duration Durata totale da calcolare (ore)
     * @param timeStep Passo temporale (secondi)
     * @return Vettore coordinate geografiche lungo path
     */
    static std::vector<GeographicCoordinates> computeShadowPath(
        const BesselianElements& elements,
        double duration,
        double timeStep = 1.0);
    
    /**
     * @brief Calcola bande incertezza su shadow path
     * 
     * Usa covarianza elementi orbitali per calcolare
     * envelope di incertezza su path
     * 
     * @param elements Elementi Besseliani
     * @param covariance Matrice covarianza orbitale (6x6)
     * @param sigma Livello confidenza (default: 1.0 = 1-sigma)
     * @return Coppia di path: limite nord e limite sud
     */
    static std::pair<std::vector<GeographicCoordinates>,
                     std::vector<GeographicCoordinates>>
    computeUncertaintyEnvelope(
        const BesselianElements& elements,
        const std::vector<std::vector<double>>& covariance,
        double sigma = 1.0);
};

/**
 * @class ShadowPathPrecise
 * @brief Calcolo shadow path con massima precisione
 * 
 * Combina tutti gli effetti:
 * - VSOP87 completo per Terra
 * - Perturbazioni planetarie su asteroide
 * - Correzioni relativistiche
 * - Precessione/nutazione IAU2000A
 * - Forma triassiale asteroide
 * - Metodo Besseliano
 */
class ShadowPathPrecise {
public:
    ShadowPathPrecise();
    ~ShadowPathPrecise();
    
    /**
     * @brief Configura calcolo
     */
    void setAsteroidElements(const EquinoctialElements& elements);
    void setAsteroidShape(const AsteroidShape& shape);
    void setStarPosition(const EquatorialCoordinates& star);
    void setEpoch(const JulianDate& centralEpoch);
    
    /**
     * @brief Abilita opzioni precisione
     */
    void enablePlanetaryPerturbations(bool enable);
    void enableRelativisticCorrections(bool enable);
    void enableNutationCorrection(bool enable);
    void useNumericalIntegration(bool enable);
    
    /**
     * @brief Calcola shadow path
     * @param duration Durata totale (ore)
     * @param timeStep Passo (secondi)
     * @return Path geografico con metadati
     */
    struct PathPoint {
        JulianDate time;
        GeographicCoordinates location;
        double shadowWidth;      // km (1-sigma)
        double shadowSpeed;      // km/s
        double sunAltitude;      // gradi
        double moonDistance;     // gradi dalla Luna
        double occultationDuration; // secondi
        bool inUmbra;           // vs penombra
    };
    
    std::vector<PathPoint> compute(double duration, double timeStep = 1.0);
    
    /**
     * @brief Calcola incertezze su path
     */
    void setCovarianceMatrix(
        const std::vector<std::vector<double>>& covariance);
    
    std::pair<std::vector<GeographicCoordinates>,
              std::vector<GeographicCoordinates>>
    computeUncertaintyBands(double sigma = 1.0);
    
private:
    class Impl;
    Impl* pImpl;
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ASTEROID_SHAPE_H
