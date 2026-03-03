// Esempio di utilizzo della classe OccultationAnalyzer

#include "ioccultcalc/occultation_analyzer.h"
#include "astdyn_wrapper.h"
#include <iostream>
#include <memory>

using namespace ioccultcalc;

int main() {
    // Setup AstDyn wrapper (esempio)
    auto astdyn = std::make_shared<AstDynWrapper>();
    // ... inizializza astdyn con elementi orbitali ...
    
    // Configurazione analyzer
    OccultationAnalyzer::Config config;
    config.shadow_points = 21;
    config.shadow_time_span_min = 10.0;
    config.verbose = true;
    
    OccultationAnalyzer analyzer(config);
    
    // Dati stella
    double star_ra = 122.75;   // gradi
    double star_dec = 25.10;   // gradi
    double star_mag = 12.5;
    uint64_t star_id = 681067227714680448;
    
    // Finestra di ricerca
    double start_mjd = 61052.0;
    double end_mjd = 61054.0;
    
    // Parametri asteroide
    double asteroid_H = 13.0;
    double asteroid_diameter_km = 0.0;  // 0 = calcola da H
    
    // ========== LIVELLO 1: Check Veloce ==========
    std::cout << "=== LIVELLO 1: Check Veloce ===" << std::endl;
    
    bool is_occ = analyzer.isOccultation(
        star_ra, star_dec,
        astdyn,
        start_mjd, end_mjd,
        asteroid_H, asteroid_diameter_km
    );
    
    std::cout << "Is occultation? " << (is_occ ? "YES" : "NO") << std::endl;
    
    if (!is_occ) {
        std::cout << "No occultation detected. Exiting." << std::endl;
        return 0;
    }
    
    // ========== LIVELLO 2: Parametri Completi ==========
    std::cout << "\n=== LIVELLO 2: Parametri Completi ===" << std::endl;
    
    auto params = analyzer.analyzeOccultation(
        star_ra, star_dec, star_mag, star_id,
        astdyn,
        start_mjd, end_mjd,
        asteroid_H, asteroid_diameter_km
    );
    
    std::cout << "Closest Approach: " << params.closest_approach_arcsec << " arcsec" << std::endl;
    std::cout << "CA Time (MJD): " << params.closest_approach_mjd << std::endl;
    std::cout << "Apparent Diameter: " << params.apparent_diameter_arcsec << " arcsec" << std::endl;
    std::cout << "Impact Parameter: " << params.impact_parameter << std::endl;
    std::cout << "Estimated Duration: " << params.estimated_duration_sec << " sec" << std::endl;
    std::cout << "Asteroid Velocity: " << params.asteroid_velocity_arcsec_per_sec << " \"/sec" << std::endl;
    
    // ========== LIVELLO 3: Shadow Path Completa ==========
    std::cout << "\n=== LIVELLO 3: Shadow Path Completa ===" << std::endl;
    
    auto shadow_result = analyzer.calculateShadowPath(params, astdyn);
    
    if (shadow_result.shadow_computed) {
        std::cout << "Shadow path computed successfully!" << std::endl;
        std::cout << "Shadow velocity: " << shadow_result.shadow_velocity_km_s << " km/s" << std::endl;
        std::cout << "Max duration: " << shadow_result.max_duration_sec << " sec" << std::endl;
        std::cout << "Number of points: " << shadow_result.shadow_path.size() << std::endl;
        std::cout << "Calculation time: " << shadow_result.calculation_time_sec << " sec" << std::endl;
        std::cout << "Propagations: " << shadow_result.num_propagations << std::endl;
        
        // Stampa primi 3 punti
        std::cout << "\nFirst 3 shadow points:" << std::endl;
        for (size_t i = 0; i < std::min(size_t(3), shadow_result.shadow_path.size()); ++i) {
            const auto& pt = shadow_result.shadow_path[i];
            std::cout << "  Point " << i << ": Lat=" << pt.lat_deg 
                      << " Lon=" << pt.lon_deg << std::endl;
        }
    } else {
        std::cout << "Shadow path calculation failed: " 
                  << shadow_result.error_message << std::endl;
    }
    
    return 0;
}
