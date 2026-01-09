/**
 * @file phase2_occultation_geometry.h
 * @brief Phase 2: Calcolo geometria precisa occultazione
 * @date 4 Dicembre 2025
 * 
 * OBIETTIVO:
 * ===========
 * Data la lista di candidati da Phase 1, calcola con precisione massima:
 * - Percorso dell'ombra sulla superficie terrestre
 * - Istante esatto del closest approach
 * - Durata massima dell'occultazione
 * - Chord length e position angle
 * - Ellisse di incertezza
 * 
 * STRATEGIA:
 * ===========
 * Per ogni candidato:
 * 1. Crea path ultra-denso attorno al CA (±5 min, step 1-5 sec)
 * 2. Usa propagazione precisa (RKF78 con perturbazioni planetarie)
 * 3. Corregge parallasse e aberrazione
 * 4. Proietta ombra su ellissoide terrestre (WGS84)
 * 5. Calcola parametri osservativi per diverse località
 * 
 * DIFFERENZE DA PHASE 1:
 * ======================
 * - Intervallo temporale: ±5 minuti (vs 24 ore)
 * - Risoluzione: 1-5 secondi (vs 1.5 ore)
 * - Perturbazioni: TUTTE attive (vs disabilitate)
 * - Correzioni: parallasse, aberrazione, proper motion
 * - Output: geometria completa per report
 */

#ifndef PHASE2_OCCULTATION_GEOMETRY_H
#define PHASE2_OCCULTATION_GEOMETRY_H

#include <vector>
#include <string>
#include <memory>
#include <nlohmann/json.hpp>
#include "ioccultcalc/astdyn_wrapper.h"
#include "phase1_candidate_screening.h"
#include "ioccultcalc/spice_spk_reader.h"

// Forward declarations
namespace astdyn {
    namespace propagation {
        struct KeplerianElements;
        class Propagator;
    }
}

// Include completo per usare KeplerianElements nei membri struct
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/propagation/OrbitalElements.hpp"

namespace ioc {
namespace gaia {
    class GaiaStar;
}
}

namespace ioccultcalc {

// ═══════════════════════════════════════════════════════════════
// STRUTTURE DATI
// ═══════════════════════════════════════════════════════════════

/**
 * @brief Punto sul percorso dell'ombra sulla Terra (Fase 2)
 */
struct Phase2ShadowPathPoint {
    double time_mjd_utc;        ///< Istante (MJD UTC)
    double latitude_deg;         ///< Latitudine (WGS84)
    double longitude_deg;        ///< Longitudine (WGS84)
    double speed_km_s;          ///< Velocità dell'ombra (km/s)
    double position_angle_deg;   ///< Position angle (gradi Est da Nord)
};

/**
 * @brief Geometria dell'occultazione per un osservatore
 */
struct ObserverGeometry {
    std::string site_name;           ///< Nome località
    double latitude_deg;              ///< Latitudine osservatore
    double longitude_deg;             ///< Longitudine osservatore
    double elevation_m;               ///< Elevazione sul livello del mare
    
    double time_ca_mjd_utc;          ///< Istante closest approach (UTC)
    double miss_distance_mas;         ///< Miss distance (milliarcsec)
    double max_duration_sec;          ///< Durata massima occultazione
    double sun_altitude_deg;          ///< Altitudine Sole (per visibilità)
    double moon_separation_deg;       ///< Separazione angolare Luna
    double target_altitude_deg;       ///< Altitudine target
    double target_azimuth_deg;        ///< Azimut target
    
    bool is_in_shadow_path;          ///< True se osservatore nel path
    double distance_from_centerline_km; ///< Distanza dalla linea centrale
};

/**
 * @brief Ellisse di incertezza 1-sigma
 */
struct UncertaintyEllipse {
    double semi_major_axis_mas = 0.0;   ///< Semi-asse maggiore (mas)
    double semi_minor_axis_mas = 0.0;   ///< Semi-asse minore (mas)
    double position_angle_deg = 0.0;    ///< Angolo di posizione (deg)
    double time_uncertainty_sec = 0.0;  ///< Incertezza temporale (sec)
    
    UncertaintyEllipse() = default;
};

/**
 * @brief Risultati precisi dell'occultazione (Fase 2)
 */
struct Phase2OccultationEvent {
    // Identificazione
    uint64_t star_source_id = 0;         ///< Gaia source_id
    std::string asteroid_name;        ///< Nome asteroide
    int asteroid_number = 0;              ///< Numero asteroide
    
    // Parametri fisici
    double diameter_km = 0.0;
    double h_mag = 0.0;
    double albedo = 0.0;
    
    // Dati stella
    double star_ra_deg = 0.0;               ///< RA stella J2000
    double star_dec_deg = 0.0;              ///< Dec stella J2000
    double star_magnitude = 0.0;            ///< Magnitudine G
    double star_pm_ra_mas_yr = 0.0;        ///< Proper motion RA
    double star_pm_dec_mas_yr = 0.0;       ///< Proper motion Dec
    
    // Geometria globale
    double time_ca_mjd_utc = 0.0;          ///< Istante CA geocentrico (UTC)
    double closest_approach_mas = 0.0;      ///< Minima distanza geocentrica (mas)
    double max_duration_sec = 0.0;          ///< Durata massima teorica
    double chord_length_km = 0.0;           ///< Lunghezza chord
    double shadow_width_km = 0.0;           ///< Larghezza ombra (diametro asteroide proiettato)
    double shadow_velocity_kms = 0.0;       ///< Velocità dell'ombra sul piano fondamentale (km/s)
    double position_angle_deg = 0.0;        ///< PA del moto relativo
    double mag_drop = 0.0;                 ///< Calo di magnitudine
    std::string utc_string;                ///< Stringa oraria UTC
    
    // Path dell'ombra sulla Terra
    std::vector<Phase2ShadowPathPoint> shadow_path;  ///< Traccia sulla superficie
    double path_length_km = 0.0;            ///< Lunghezza totale percorso
    double path_duration_sec = 0.0;         ///< Durata totale attraversamento Terra
    
    // Geometria per osservatori specifici
    std::vector<ObserverGeometry> observer_predictions;
    
    // Incertezza
    UncertaintyEllipse uncertainty;
    
    // Dati propagazione
    double asteroid_distance_au = 0.0;      ///< Distanza asteroide dalla Terra
    double star_distance_au = 0.0;          ///< Distanza stella (se nota)
    double sun_target_elongation_deg = 0.0; ///< Elongazione dal Sole
    
    // Quality flags
    bool high_confidence = false;             ///< True se geometria affidabile
    double snr = 0.0;                       ///< Signal-to-noise ratio stima
    std::string notes;                ///< Note aggiuntive

    Phase2OccultationEvent() = default;
};

// ═══════════════════════════════════════════════════════════════
// CONFIGURAZIONE PHASE 2
// ═══════════════════════════════════════════════════════════════

/**
 * @brief Configurazione calcolo Phase 2
 */
struct Phase2Config {
    // ORBITAL FITTING (nuova funzionalità)
    bool refine_orbit_from_observations = false; ///< Scarica RWO e rifà fit orbitale (DISATTIVATO per ora)
    std::string mpc_code = "";                    ///< Codice MPC asteroide (es: "17030")
    int observation_arc_days = 365;               ///< Arco osservativo per fit (giorni)
    bool use_all_available_observations = false;  ///< Usa tutte osservazioni disponibili
    int max_observations_for_fit = 20;            ///< Numero massimo osservazioni da usare (le più recenti)
    
    // AstDyS integration (nuovo)
    bool auto_download_astdys = false;             ///< Scarica automaticamente .eq1/.rwo
    bool use_astdys_covariance_no_fit = false;    ///< Usa covarianza da .eq1 senza fit
    
    // Parametri fitting
    bool fit_planetary_perturbations = true;      ///< Include perturbazioni nel fit
    bool fit_relativistic_effects = true;         ///< Include relatività nel fit
    bool fit_asteroid_perturbations = false;      ///< Perturbazioni da Ceres, Vesta, etc
    double fit_tolerance = 1e-12;                 ///< Tolleranza integrazione fit
    int max_fit_iterations = 50;                  ///< Iterazioni massime fit
    double convergence_threshold = 1e-9;          ///< Soglia convergenza fit
    
    // Parametri temporali
    double time_window_minutes = 5.0;    ///< Finestra attorno CA (±minuti)
    double time_step_seconds = 1.0;      ///< Risoluzione temporale (sec)
    
    // Parametri propagazione finale
    bool use_planetary_perturbations = true;  ///< Abilita perturbazioni
    bool use_relativistic_effects = true;     ///< Effetti relativistici
    double integrator_tolerance = 1e-12;      ///< Tolleranza integrazione
    
    // Correzioni astrometriche
    bool apply_parallax = true;          ///< Correggi parallasse
    bool apply_aberration = true;        ///< Correggi aberrazione
    bool apply_proper_motion = true;     ///< Applica proper motion stella
    double proper_motion_epoch = 2016.0; ///< Epoca proper motion Gaia
    
    // Output
    bool compute_uncertainty = true;     ///< Calcola ellisse incertezza
    bool compute_shadow_path = true;     ///< Calcola path su Terra
    int shadow_path_points = 100;        ///< Numero punti path
    
    // Osservatori
    std::vector<ObserverGeometry> observer_sites;  ///< Località da calcolare
    
    // Debug/Output
    bool verbose = true;                           ///< Stampa output dettagliato

    // === DEBUG FLAGS (User requested keywords) ===
    bool debug_asteroid_equ = false;           ///< Asteroid_Phase2_EQU
    bool debug_asteroid_ecl = false;           ///< Asteroid_Phase2_ECL
    bool debug_asteroid_vec = false;           ///< Asteroid_Phase2_VEC
    bool debug_elements_equ = false;           ///< Asteroid_Phase2_ELMENTS_EQU
    bool debug_elements_ecl = false;           ///< Asteroid_Phase2_ELMENTS_Ecl
    bool debug_star_position = false;          ///< StarPosition
    std::string debug_json_path = "";
};

/**
 * @brief Risultati fitting orbitale
 */
struct OrbitalFitResults {
    bool fit_successful = false;
    int num_observations_used = 0;
    double rms_residuals_arcsec = 0.0;
    double max_residual_arcsec = 0.0;
    double chi_squared = 0.0;
    int iterations_performed = 0;
    astdyn::propagation::KeplerianElements refined_elements;
    Eigen::MatrixXd covariance_matrix;  ///< Matrice covarianza 6x6
    std::string fit_notes;
};

/**
 * @brief Risultati Phase 2
 */
struct Phase2Results {
    std::vector<Phase2OccultationEvent> events;  ///< Eventi calcolati
    int successful_calculations = 0;
    int failed_calculations = 0;
    double total_computation_time_ms = 0.0;
    std::string error_messages;
    
    // Risultati fitting orbitale (se abilitato)
    bool orbit_refined = false;
    OrbitalFitResults orbital_fit;
};

// ═══════════════════════════════════════════════════════════════
// CLASSE PRINCIPALE
// ═══════════════════════════════════════════════════════════════

/**
 * @brief Calcola geometria precisa delle occultazioni
 * 
 * Prende i candidati da Phase1 e calcola:
 * - Geometria dettagliata dell'evento
 * - Path dell'ombra sulla Terra
 * - Predizioni per osservatori specifici
 */
class Phase2OccultationGeometry {
public:
    Phase2OccultationGeometry();
    explicit Phase2OccultationGeometry(std::shared_ptr<ISPReader> reader);
    ~Phase2OccultationGeometry();
    
    // Non copiabile
    Phase2OccultationGeometry(const Phase2OccultationGeometry&) = delete;
    Phase2OccultationGeometry& operator=(const Phase2OccultationGeometry&) = delete;
    
    /**
     * @brief Carica elementi orbitali da file EQ1
     * @param eq1_path Percorso file
     * @return true se successo
     */
    bool loadAsteroidFromEQ1(const std::string& eq1_path);
    
    /**
     * @brief Carica elementi orbitali dal database JSON locale
     * @param asteroid_number Numero dell'asteroide (es: 17030)
     * @param json_path Path al file JSON (default: ~/.ioccultcalc/data/all_numbered_asteroids.json)
     * @return true se caricamento riuscito
     */
    bool loadAsteroidFromJSON(int asteroid_number, const std::string& json_path = "");
    
    /**
     * @brief Imposta elementi orbitali direttamente
     */
    void setOrbitalElements(const astdyn::propagation::KeplerianElements& elements,
                            const std::string& name = "",
                            FrameType frame = FrameType::ECLIPTIC_J2000,
                            ElementType type = ElementType::OSCULATING);
    
    /**
     * @brief Imposta parametri fisici dell'asteroide
     * @param diameter_km Diametro in km
     * @param abs_mag Magnitudine assoluta (H)
     * @param slope_param Parametro slope (G)
     */
    void setPhysicalParameters(double diameter_km, double abs_mag, double slope_param);
    
    /**
     * @brief Ottiene elementi orbitali correnti
     */
    const astdyn::propagation::KeplerianElements& getOrbitalElements() const;
    
    /**
     * @brief Calcola geometria per lista candidati
     * 
     * @param candidates Lista candidati da Phase 1
     * @param config Configurazione calcolo
     * @return Risultati con eventi calcolati
     */
    Phase2Results calculateGeometry(
        const std::vector<ioccultcalc::CandidateStar>& candidates,
        const Phase2Config& config);
    
    /**
     * @brief Calcola i dettagli di un singolo evento
     */
    Phase2OccultationEvent calculateSingleEvent(
        const ioccultcalc::CandidateStar& candidate,
        const Phase2Config& config);
    
    /**
     * @brief Aggiunge sito osservatorio alle predizioni
     */
    void addObserverSite(const std::string& name, 
                        double lat_deg, double lon_deg, double elev_m);
    
    /**
     * @brief Pulisce lista osservatori
     */
    void clearObserverSites();
    
private:
    class Impl;
    std::unique_ptr<Impl> pimpl_;
};

} // namespace ioccultcalc

#endif // PHASE2_OCCULTATION_GEOMETRY_H
