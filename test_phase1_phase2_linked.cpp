/**
 * @file test_phase1_phase2_linked.cpp
 * @brief Test integrazione Phase 1 → Phase 2
 * 
 * Flusso completo:
 * 1. Phase 1: Carica elementi orbitali da JSON
 * 2. Phase 1: Cerca stelle candidate nel corridor
 * 3. Phase 2: Usa STESSI elementi orbitali
 * 4. Phase 2: Analizza SOLO le stelle candidate trovate in Phase 1
 * 
 * @date 5 Dicembre 2025
 */

#include "phase1_candidate_screening.h"
#include "phase2_occultation_geometry.h"
#include "ioc_gaialib/unified_gaia_catalog.h"

#include <iostream>
#include <iomanip>
#include <cmath>

using namespace ioccultcalc;

int main() {
    std::cout << "\n╔════════════════════════════════════════════════════════════════╗\n";
    std::cout << "║     TEST INTEGRAZIONE PHASE 1 → PHASE 2                     ║\n";
    std::cout << "║     Asteroide 17030 Sierks - 28 Novembre 2025               ║\n";
    std::cout << "╚════════════════════════════════════════════════════════════════╝\n";
    
    try {
        // Inizializza catalogo Gaia
        std::cout << "\n[0] Inizializzazione catalogo Gaia...\n";
        
        std::string home = std::string(getenv("HOME"));
        std::string catalog_path = home + "/.catalog/gaia_mag18_v2_multifile";
        
        std::string json_config = R"({
            "catalog_type": "multifile_v2",
            "multifile_directory": ")" + catalog_path + R"("
        })";
        
        if (!ioc::gaia::UnifiedGaiaCatalog::initialize(json_config)) {
            throw std::runtime_error("Impossibile inizializzare catalogo Gaia");
        }
        std::cout << "✓ Catalogo Gaia inizializzato\n";
        
        // ═══════════════════════════════════════════════════════════════
        // PHASE 1: SCREENING CANDIDATE
        // ═══════════════════════════════════════════════════════════════
        
        std::cout << "\n" << std::string(80, '=') << "\n";
        std::cout << "[1] PHASE 1: SCREENING CANDIDATE\n";
        std::cout << std::string(80, '=') << "\n\n";
        
        Phase1CandidateScreening phase1;
        
        // Carica elementi orbitali da JSON
        std::cout << "Caricamento elementi orbitali da database JSON (asteroide 17030)...\n";
        if (!phase1.loadAsteroidFromJSON(17030)) {
            std::cerr << "ERRORE: Impossibile caricare elementi orbitali!\n";
            return 1;
        }
        
        // Ottieni gli elementi per usarli in Phase 2
        auto orbital_elements = phase1.getOrbitalElements();
        
        std::cout << "\nElementi orbitali caricati (frame eclittica J2000):\n";
        std::cout << "  a = " << std::fixed << std::setprecision(6) 
                  << orbital_elements.semi_major_axis << " AU\n";
        std::cout << "  e = " << orbital_elements.eccentricity << "\n";
        std::cout << "  i = " << orbital_elements.inclination * 180.0 / M_PI << "°\n";
        std::cout << "  Ω = " << orbital_elements.longitude_ascending_node * 180.0 / M_PI << "°\n";
        std::cout << "  ω = " << orbital_elements.argument_perihelion * 180.0 / M_PI << "°\n";
        std::cout << "  M = " << orbital_elements.mean_anomaly * 180.0 / M_PI << "°\n";
        std::cout << "  epoch = " << orbital_elements.epoch_mjd_tdb << " MJD TDB\n";
        
        // Configura Phase 1
        Phase1Config config1;
        config1.start_mjd_tdb = 61007.0;  // 28 Nov 2025 00:00 UT
        config1.end_mjd_tdb = 61008.0;    // 29 Nov 2025 00:00 UT
        config1.path_interval_seconds = 30;
        config1.corridor_width_deg = 0.0083;  // 30 arcsec
        config1.closest_approach_threshold_arcsec = 15.0;
        config1.max_magnitude = 18.0;
        
        std::cout << "\nConfigurazionePhase 1:\n";
        std::cout << "  Periodo: 28-29 Nov 2025\n";
        std::cout << "  Corridor width: 30 arcsec\n";
        std::cout << "  CA threshold: 15 arcsec\n";
        
        // Esegui screening
        std::cout << "\nRicerca stelle candidate...\n";
        auto results1 = phase1.screenCandidates(config1);
        
        std::cout << "\n--- Risultati Phase 1 ---\n";
        std::cout << "Path points:        " << results1.num_path_points << "\n";
        std::cout << "Stelle nel corridor: " << results1.num_stars_in_corridor << "\n";
        std::cout << "Candidate filtrate: " << results1.num_candidates_filtered << "\n";
        std::cout << "Tempo totale:       " << std::fixed << std::setprecision(2) 
                  << (results1.propagation_time_ms + results1.corridor_query_time_ms + 
                      results1.closest_approach_calc_time_ms) << " ms\n";
        
        // Mostra candidate
        std::cout << "\n--- Stelle candidate da analizzare con Phase 2 ---\n";
        for (size_t i = 0; i < results1.candidates.size(); i++) {
            const auto& c = results1.candidates[i];
            std::cout << i+1 << ") Gaia DR3 " << c.source_id << "\n";
            std::cout << "   RA=" << std::setprecision(4) << c.ra_deg << "° ";
            std::cout << "Dec=" << c.dec_deg << "°\n";
            std::cout << "   Gmag=" << std::setprecision(2) << c.phot_g_mean_mag;
            std::cout << "  CA=" << std::setprecision(3) << c.closest_approach_arcsec << "\"\n";
        }
        
        if (results1.candidates.empty()) {
            std::cout << "\n⚠ Nessuna candidata trovata in Phase 1. Fine test.\n";
            return 0;
        }
        
        // ═══════════════════════════════════════════════════════════════
        // PHASE 2: GEOMETRIA PRECISA
        // ═══════════════════════════════════════════════════════════════
        
        std::cout << "\n" << std::string(80, '=') << "\n";
        std::cout << "[2] PHASE 2: GEOMETRIA PRECISA OCCULTAZIONE\n";
        std::cout << std::string(80, '=') << "\n\n";
        
        Phase2OccultationGeometry phase2;
        
        // ★ CRUCIALE: Usa gli STESSI elementi orbitali della Phase 1
        std::cout << "Passaggio elementi orbitali da Phase 1 a Phase 2...\n";
        phase2.setOrbitalElements(orbital_elements);
        std::cout << "✓ Elementi orbitali assegnati a Phase 2\n";
        
        // Configura Phase 2
        Phase2Config config2;
        config2.time_window_minutes = 5.0;    // ±5 min attorno CA
        config2.time_step_seconds = 1.0;      // 1 sec risoluzione
        config2.use_planetary_perturbations = true;
        config2.use_relativistic_effects = true;
        config2.apply_parallax = true;
        config2.apply_aberration = true;
        config2.apply_proper_motion = true;
        config2.refine_orbit_from_observations = false;  // NO fitting
        
        std::cout << "\nConfigurazione Phase 2:\n";
        std::cout << "  Finestra temporale: ±5 min attorno CA\n";
        std::cout << "  Risoluzione: 1 sec\n";
        std::cout << "  Perturbazioni: SI\n";
        std::cout << "  Correzioni: parallasse, aberrazione, proper motion\n";
        
        // Aggiungi osservatori italiani
        phase2.addObserverSite("Roma", 41.9028, 12.4964, 21);
        phase2.addObserverSite("Milano", 45.4642, 9.1900, 120);
        phase2.addObserverSite("Bologna", 44.4949, 11.3426, 54);
        
        // ★ CRUCIALE: Calcola geometria SOLO per le candidate di Phase 1
        std::cout << "\nAnalisi " << results1.candidates.size() << " stelle candidate...\n";
        
        auto results2 = phase2.calculateGeometry(results1.candidates, config2);
        
        std::cout << "\n--- Risultati Phase 2 ---\n";
        std::cout << "Eventi calcolati con successo: " << results2.successful_calculations << "\n";
        std::cout << "Errori: " << results2.failed_calculations << "\n";
        std::cout << "Tempo totale: " << std::fixed << std::setprecision(1) 
                  << results2.total_computation_time_ms << " ms\n";
        
        // Mostra dettagli di ogni evento
        std::cout << "\n" << std::string(80, '=') << "\n";
        std::cout << "DETTAGLI EVENTI DI OCCULTAZIONE\n";
        std::cout << std::string(80, '=') << "\n";
        
        for (size_t i = 0; i < results2.events.size(); i++) {
            const auto& evt = results2.events[i];
            
            std::cout << "\n[EVENTO " << i+1 << "] " << std::string(60, '=') << "\n";
            
            std::cout << "Stella: Gaia DR3 " << evt.star_source_id << "\n";
            std::cout << "RA/Dec: " << std::setprecision(6) << evt.star_ra_deg << "° / " 
                      << evt.star_dec_deg << "°\n";
            std::cout << "Magnitudine G: " << std::setprecision(2) << evt.star_magnitude << "\n";
            
            std::cout << "\nGeometria occultazione:\n";
            std::cout << "  Closest approach (geocentrico): " << std::setprecision(2) 
                      << evt.closest_approach_mas << " mas\n";
            std::cout << "  Durata massima: " << std::setprecision(1) << evt.max_duration_sec << " sec\n";
            std::cout << "  Position angle: " << std::setprecision(1) << evt.position_angle_deg << "°\n";
            std::cout << "  Epoch (MJD UTC): " << std::setprecision(6) << evt.time_ca_mjd_utc << "\n";
            
            if (!evt.observer_predictions.empty()) {
                std::cout << "\nPredizioni per osservatori:\n";
                for (const auto& obs : evt.observer_predictions) {
                    std::cout << "  " << obs.site_name << ":\n";
                    std::cout << "    In shadow path: " << (obs.is_in_shadow_path ? "SI" : "NO") << "\n";
                    if (obs.is_in_shadow_path) {
                        std::cout << "    Distanza dalla linea centrale: " 
                                  << obs.distance_from_centerline_km << " km\n";
                    }
                    std::cout << "    Altitudine target: " << obs.target_altitude_deg << "°\n";
                    std::cout << "    Azimut target: " << obs.target_azimuth_deg << "°\n";
                }
            }
            
            std::cout << "╚" << std::string(72, '=') << "╝\n";
        }
        
        std::cout << "\n" << std::string(80, '=') << "\n";
        std::cout << "✅ TEST INTEGRAZIONE PHASE 1 → PHASE 2 COMPLETATO CON SUCCESSO!\n";
        std::cout << std::string(80, '=') << "\n\n";
        
    } catch (const std::exception& e) {
        std::cerr << "✗ ERRORE: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
