/**
 * @file phase1_candidate_screening.h
 * @brief Header per FASE 1: Screening stelle candidate (Clean API)
 */

#pragma once

#include <memory>
#include <vector>
#include <string>
#include "astdyn_wrapper.h" // Necessario per i tipi di AstDyn
#include "ioc_gaialib/types.h" // Per GaiaStar e CelestialPoint

namespace ioc { namespace gaia { class UnifiedGaiaCatalog; } }

namespace ioccultcalc {

// Strutture dati minimali per lo screening
struct CandidateStar {
    long long source_id;
    double ra_deg;
    double dec_deg;
    double phot_g_mean_mag;
    double closest_approach_arcsec;
    /** Epoca del closest approach in MJD TDB (coerente con Phase1Config e con getApparentStateGeocentric). */
    double closest_approach_mjd;
    double pmra;
    double pmdec;
    double parallax;
};

/** Configurazione Fase 1. Tutte le epoche in MJD TDB (Tempo Dinamico Baricentrico). */
struct Phase1Config {
    std::string asteroid_name;
    double start_mjd_tdb;  ///< Inizio finestra (MJD TDB)
    double end_mjd_tdb;    ///< Fine finestra (MJD TDB)
    double corridor_width_deg = 0.5;
    double max_magnitude = 18.0;
    double threshold_arcsec = 10.0; // Soglia per il CA iniziale
};

struct Phase1Results {
    std::vector<CandidateStar> candidates;
    int num_stars_in_corridor = 0;
    double propagation_time_ms = 0;
};

class Phase1CandidateScreening {
public:
    Phase1CandidateScreening();
    ~Phase1CandidateScreening();

    // Caricamento elementi (Interfaccia pulita)
    bool loadAsteroidFromJSON(int number, const std::string& path = "");
    bool loadAsteroidFromDB(int number);
    bool loadAsteroidFromEQ1(int number, const std::string& eq1_path);
    bool setAsteroidElements(const class AstDynEquinoctialElements& elements);
    
    /**
     * @brief Esegue lo screening completo usando AstDyn e GaiaLib Native
     */
    Phase1Results screenCandidates(const Phase1Config& config);

    // Getters/Setters per il catalogo Gaia
    void setCatalog(ioc::gaia::UnifiedGaiaCatalog* catalog);
    void setSPKReader(std::shared_ptr<class ISPReader> reader);
    void setVerbose(int level);

private:
    class Impl;
    std::unique_ptr<Impl> pimpl_;

    // Funzione interna per il check veloce del CA
    bool checkInitialCA(CandidateStar& star, 
                        const std::vector<ioc::gaia::CelestialPoint>& path, 
                        double threshold,
                        double start_mjd,
                        double step_days);
};

} // namespace ioccultcalc
