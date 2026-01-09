#ifndef IOCCULTCALC_OCCULTATION_PREDICTOR_H
#define IOCCULTCALC_OCCULTATION_PREDICTOR_H

#include "orbital_elements.h"
#include "ephemeris.h"
#include "gaia_client.h"
#include "types.h"
#include "isp_reader.h"
#include <string>
#include <vector>
#include <memory>
#include <Eigen/Dense>
#include "ioccultcalc/output_manager.h"

namespace ioccultcalc {

/**
 * @struct ShadowPathPoint
 * @brief Punto sulla traccia dell'ombra terrestre
 */
struct ShadowPathPoint {
    JulianDate time;
    GeographicCoordinates location;
    double duration;        // secondi
    double centerlineDistance; // km
    
    ShadowPathPoint() : duration(0), centerlineDistance(0) {}
};

/**
 * @struct OccultationEvent
 * @brief Risultato raffinato di una previsione di occultazione
 * Hereditary from OutputEvent for legacy and output compatibility.
 */
struct OccultationEvent : public OutputEvent {
    std::string eventId;
    AstDynEquinoctialElements asteroid;
    GaiaStar star;
    
    JulianDate timeCA;              // Istante di massimo avvicinamento (TDB)
    double closeApproachDistance;   // Separazione minima (arcsec)
    double positionAngle;           // Angolo di posizione (gradi)
    double probability;             // Probabilità (SVD-based)
    double maxDuration;             // Durata massima (sec)
    double pathWidth;               // Larghezza ombra (diametro asteroide km)
    double magnitudeDrop;           // Calo di luce (HG model)
    
    std::vector<ShadowPathPoint> shadowPath;
    
    // Incertezza 1-sigma (km)
    double uncertaintyNorth;
    double uncertaintySouth;
    double asteroidDistanceAu;
    
    // Parametri geometrici B-Plane
    double besselianX;
    double besselianY;
    double besselianDX; // km/s
    double besselianDY; // km/s
    
    // Compatibility with legacy code (shadow names)
    // Most fields are already in OutputEvent (asteroid_number, star_id, etc.)
    
    double observerLongitude;
    double observerLatitude;
    double observerAltitude;
    
    OccultationEvent() 
        : closeApproachDistance(0), positionAngle(0), 
          probability(0), maxDuration(0), pathWidth(0), magnitudeDrop(0),
          uncertaintyNorth(0), uncertaintySouth(0), asteroidDistanceAu(0),
          besselianX(0), besselianY(0), besselianDX(0), besselianDY(0),
          observerLongitude(0), observerLatitude(0), observerAltitude(0) {}
};

/**
 * @class OccultationPredictor
 * @brief Motore di ricerca e previsione ad alta precisione
 */
class OccultationPredictor {
public:
    OccultationPredictor(std::shared_ptr<ISPReader> reader = nullptr);
    ~OccultationPredictor();
    
    void setAsteroid(const AstDynEquinoctialElements& elements);
    void loadAsteroidFromAstDyS(const std::string& designation);
    
    std::vector<OccultationEvent> findOccultations(
        const JulianDate& startJD,
        const JulianDate& endJD,
        double maxMagnitude = 14.0,
        double searchRadius = 0.2,
        double minProbability = 0.001
    );
    
    // Alias per compatibilità (deprecated)
    std::vector<OccultationEvent> find_occultations(
        const JulianDate& startJD,
        const JulianDate& endJD,
        double maxMagnitude = 14.0,
        double searchRadius = 0.2,
        double minProbability = 0.001) {
        return findOccultations(startJD, endJD, maxMagnitude, searchRadius, minProbability);
    }
    
    // Previsione puntuale (Alta Precisione)
    OccultationEvent predictOccultation(const GaiaStar& star, 
                                       const JulianDate& approximateTime);
    
    void setAsteroidDiameter(double diameter);
    
    // Metodo di compatibilità (deprecated)
    void setOrbitalUncertainty(double sigma_km) { (void)sigma_km; }
    
    // Genera la traccia dell'ombra terrestre
    std::vector<ShadowPathPoint> generateShadowPath(const OccultationEvent& event, 
                                                   const std::vector<std::vector<double>>& covariance, 
                                                   double windowMinutes = 30.0);

private:
    class Impl;
    std::unique_ptr<Impl> pImpl;
    
    // Algoritmo: PrepareStarForDate (PM + Stellar Parallax)
    static Vector3D prepareStarForDate(const GaiaStar& star, const JulianDate& jd, const Vector3D& earthHelioPos);
    
    // Algoritmo: Precision Search Loop (RA15 + Light-Time)
    JulianDate findPrecisionCA(const GaiaStar& star, const JulianDate& startJD, const JulianDate& endJD);
    
    // Algoritmo: Uncertainty Analysis (Covariance -> B-Plane)
    double calculateProbabilitySVD(const OccultationEvent& event, const std::vector<std::vector<double>>& covariance);
    
    // Algoritmo: PredictEventUncertainty (Time and Path error)
    struct UncertaintyProfile {
        double timeUncertainty; // secondi
        double pathUncertainty; // km
    };
    UncertaintyProfile predictEventUncertainty(const OccultationEvent& event, const std::vector<std::vector<double>>& covariance);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_OCCULTATION_PREDICTOR_H
