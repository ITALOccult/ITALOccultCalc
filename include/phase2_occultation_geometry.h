/**
 * @file phase2_occultation_geometry.h
 * @brief Header per FASE 2: Geometria precisa e calcolo ombra
 */

#pragma once

#include <memory>
#include <vector>
#include "phase1_candidate_screening.h" // Per CandidateStar
#include "astdyn_wrapper.h"

namespace ioccultcalc {

struct ShadowPoint {
    double mjd_tdb;
    double lat_deg;
    double lon_deg;
    double star_alt_deg;
};

struct Phase2OccultationEvent {
    long long star_id;
    double t_ca_mjd;      // Tempo del Closest Approach
    double min_dist_mas;  // Distanza minima in milliarcosecondi
    double duration_sec;  // Durata stimata occultazione
    std::vector<ShadowPoint> shadow_path;
    bool is_valid = false;
    
    // Additional fields for CA report
    std::string utc_string;
    double mjd_tdb;
    double star_ra_deg;
    double star_dec_deg;
    double asteroid_ra_deg;
    double asteroid_dec_deg;
    double closest_approach_arcsec;
    double asteroid_distance_au;
    
    // Besselian elements at CA
    double besselian_x;
    double besselian_y;
    double besselian_dx;
    double besselian_dy;
    
    // Substellar/Subsolar points
    double substellar_lon_deg;
    double substellar_lat_deg;
    double subsolar_lon_deg;
    double subsolar_lat_deg;
    
    // Apparent positions
    double star_app_ra_deg;
    double star_app_dec_deg;
};

struct Phase2Config {
    double search_window_sec = 600.0; // +/- 10 minuti intorno al CA di Fase 1
    double precision_goal_mas = 0.1;
    bool compute_shadow = true;
    bool refine_orbit = false;   // Se vero, esegue il fit con le ultime N osservazioni
    int last_n_obs = 50;         // Numero di osservazioni da usare per il fit
    bool use_horizons = false;   // Se vero, scarica elementi osculanti da JPL Horizons all'epoca CA
};

class Phase2OccultationGeometry {
public:
    Phase2OccultationGeometry();
    ~Phase2OccultationGeometry();

    /**
     * @brief Processa i candidati di Fase 1 per trovare la geometria esatta
     */
    std::vector<Phase2OccultationEvent> calculatePreciseGeometry(
        const std::vector<CandidateStar>& candidates,
        const Phase2Config& config);

    // Compatibilità API
    /**
     * @brief Carica elementi orbitali dal database JSON locale
     */
    bool loadAsteroidFromJSON(int asteroid_number, const std::string& json_path = "");
    bool loadAsteroidFromDB(int asteroid_number);
    bool loadAsteroidFromEQ1(int asteroid_number, const std::string& eq1_path);
    bool setAsteroidElements(const class AstDynEquinoctialElements& elements);

    void setAstDynWrapper(std::shared_ptr<AstDynWrapper> wrapper);
    void setSPKReader(std::shared_ptr<class ISPReader> reader);

private:
    class Impl;
    std::unique_ptr<Impl> pimpl_;
};

} // namespace ioccultcalc
