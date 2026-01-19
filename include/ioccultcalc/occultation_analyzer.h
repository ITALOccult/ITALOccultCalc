#ifndef IOCCULTCALC_OCCULTATION_ANALYZER_H
#define IOCCULTCALC_OCCULTATION_ANALYZER_H

#include "types.h"
#include <vector>
#include <functional>
#include <optional>
#include <Eigen/Dense>

namespace ioccultcalc {

// Forward declarations
class AstDynWrapper;

/**
 * @struct OccultationParameters
 * @brief Parametri base dell'occultazione (senza shadow path)
 */
struct OccultationParameters {
    bool is_occultation;              ///< True se c'è occultazione
    double closest_approach_arcsec;   ///< Distanza minima in arcsec
    double closest_approach_mjd;      ///< Tempo del CA (MJD TDB)
    double asteroid_diameter_km;      ///< Diametro asteroide
    double asteroid_distance_au;      ///< Distanza asteroide
    double apparent_diameter_arcsec;  ///< Diametro apparente
    double estimated_duration_sec;    ///< Durata stimata
    double impact_parameter;          ///< Parametro d'impatto (0=centrale, 1=radente)
    
    // Stella
    uint64_t star_id;
    double star_ra_deg;
    double star_dec_deg;
    double star_magnitude;
    
    // Asteroide al CA
    double asteroid_ra_deg;
    double asteroid_dec_deg;
    double asteroid_velocity_arcsec_per_sec;
};

/**
 * @struct AnalyzerShadowPoint
 * @brief Punto sulla traccia dell'ombra
 */
struct AnalyzerShadowPoint {
    double mjd_tdb;
    double lat_deg;
    double lon_deg;
    
    // Limiti geometrici
    double geonorth_lat_deg;
    double geonorth_lon_deg;
    double geosouth_lat_deg;
    double geosouth_lon_deg;
    
    // Limiti 1-sigma
    double north_lat_deg;
    double north_lon_deg;
    double south_lat_deg;
    double south_lon_deg;
    
    double star_alt_deg;
};

/**
 * @struct BesselianElements
 * @brief Elementi di Bessel
 */
struct BesselianElements {
    double x, y;           ///< Coordinate piano fondamentale (raggi terrestri)
    double dx, dy;         ///< Derivate (raggi terrestri/ora)
    double L1, L2;         ///< Raggi coni ombra/penombra (raggi terrestri)
    double d, mu;          ///< Declinazione e angolo orario asse ombra (rad)
    double f1, f2;         ///< Angoli coni (rad)
    double epoch_mjd_tdb;  ///< Epoca elementi (MJD TDB)
};

/**
 * @struct ShadowPathResult
 * @brief Risultato completo con shadow path
 */
struct ShadowPathResult {
    OccultationParameters parameters;  ///< Parametri base
    
    bool shadow_computed;              ///< True se shadow path calcolata
    std::string error_message;         ///< Eventuale errore
    
    BesselianElements bessel;
    double shadow_velocity_km_s;
    double max_duration_sec;
    
    double subsolar_lat_deg;
    double subsolar_lon_deg;
    double substellar_lat_deg;
    double substellar_lon_deg;
    
    std::vector<AnalyzerShadowPoint> shadow_path;
    
    double calculation_time_sec;
    int num_propagations;
};

/**
 * @class OccultationAnalyzer
 * @brief Classe unificata per analisi occultazioni
 * 
 * Fornisce tre livelli di analisi:
 * 1. isOccultation() - Check veloce (1 propagazione)
 * 2. analyzeOccultation() - Parametri completi (1-2 propagazioni)
 * 3. calculateShadowPath() - Traccia completa (21+ propagazioni)
 */
class OccultationAnalyzer {
public:
    /**
     * @struct Config
     * @brief Configurazione analisi
     */
    struct Config {
        // Shadow path
        int shadow_points;
        double shadow_time_span_min;
        bool compute_geometric_limits;
        bool compute_sigma_limits;
        
        // Generale
        double search_tolerance;
        bool verbose;
        
        // Constructor with defaults
        Config() 
            : shadow_points(21)
            , shadow_time_span_min(10.0)
            , compute_geometric_limits(true)
            , compute_sigma_limits(true)
            , search_tolerance(1e-9)
            , verbose(false)
        {}
    };
    
    OccultationAnalyzer(const Config& config = Config());
    ~OccultationAnalyzer() = default;
    
    // ========== LIVELLO 1: Check Veloce ==========
    
    /**
     * @brief Verifica rapida se c'è occultazione
     * @return true se distanza minima < diametro apparente
     */
    bool isOccultation(
        double star_ra_deg,
        double star_dec_deg,
        std::shared_ptr<AstDynWrapper> astdyn,
        double search_start_mjd,
        double search_end_mjd,
        double asteroid_H,
        double asteroid_diameter_km = 0.0
    );
    
    // ========== LIVELLO 2: Parametri Completi ==========
    
    /**
     * @brief Analizza occultazione e calcola parametri completi
     * @return Parametri dell'occultazione (senza shadow path)
     */
    OccultationParameters analyzeOccultation(
        double star_ra_deg,
        double star_dec_deg,
        double star_mag,
        uint64_t star_id,
        std::shared_ptr<AstDynWrapper> astdyn,
        double search_start_mjd,
        double search_end_mjd,
        double asteroid_H,
        double asteroid_diameter_km = 0.0
    );
    
    // ========== LIVELLO 3: Shadow Path Completa ==========
    
    /**
     * @brief Calcola shadow path completa sulla Terra
     * @param params Parametri da analyzeOccultation()
     * @param astdyn Wrapper AstDyn per propagazioni
     * @return Risultato con shadow path
     */
    ShadowPathResult calculateShadowPath(
        const OccultationParameters& params,
        std::shared_ptr<AstDynWrapper> astdyn
    );
    
    // ========== Metodi di Utilità Statici ==========
    
    static double calculateApparentDiameter(double diameter_km, double distance_au);
    static double estimateDiameter(double H, double albedo = 0.15);
    static double angularDistance(double ra1_deg, double dec1_deg, 
                                  double ra2_deg, double dec2_deg);
    static double calculateShadowVelocity(const BesselianElements& bessel);
    
    // Configurazione
    void setConfig(const Config& config) { config_ = config; }
    const Config& getConfig() const { return config_; }

private:
    Config config_;
    
    // Metodi comuni interni
    double findClosestApproachTime(
        std::function<double(double)> distance_func,
        double start_mjd,
        double end_mjd
    );
    
    double estimateApparentVelocity(
        std::function<std::pair<double, double>(double)> ra_dec_getter,
        double mjd,
        double dt = 60.0 / 86400.0
    );
    
    BesselianElements calculateBesselianElements(
        const OccultationParameters& params,
        std::shared_ptr<AstDynWrapper> astdyn
    );
    
    std::optional<AnalyzerShadowPoint> projectShadowPoint(
        const BesselianElements& bessel,
        double mjd_tdb
    );
    
    std::optional<AnalyzerShadowPoint> calculateShadowPointAtTime(
        double mjd,
        const OccultationParameters& params,
        std::shared_ptr<AstDynWrapper> astdyn
    );
    
    void calculateGeometricLimits(
        AnalyzerShadowPoint& point,
        const Eigen::Vector3d& asteroid_pos_itrf,
        const Eigen::Vector3d& star_dir_itrf,
        double asteroid_radius_m
    );
    
    void calculateSigmaLimits(
        AnalyzerShadowPoint& point,
        const Eigen::Vector3d& asteroid_pos_itrf,
        const Eigen::Vector3d& star_dir_itrf,
        const Eigen::Vector3d& velocity_itrf,
        const Eigen::Matrix<double, 6, 6>& covariance
    );
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_OCCULTATION_ANALYZER_H
