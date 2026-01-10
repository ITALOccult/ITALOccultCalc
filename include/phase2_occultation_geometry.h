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
};

struct Phase2Config {
    double search_window_sec = 600.0; // +/- 10 minuti intorno al CA di Fase 1
    double precision_goal_mas = 0.1;
    bool compute_shadow = true;
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
    bool loadAsteroidFromEQ1(int asteroid_number, const std::string& eq1_path);

    void setAstDynWrapper(std::shared_ptr<AstDynWrapper> wrapper);
    void setSPKReader(std::shared_ptr<class ISPReader> reader);

private:
    class Impl;
    std::unique_ptr<Impl> pimpl_;
};

} // namespace ioccultcalc
