// Test rapido per OccultationAnalyzer con Besselian elements
#include "ioccultcalc/occultation_analyzer.h"
#include "astdyn_wrapper.h"
#include <iostream>
#include <memory>

using namespace ioccultcalc;

int main() {
    std::cout << "=== Test OccultationAnalyzer - Besselian Shadow Path ===" << std::endl;
    
    // Setup AstDyn wrapper per asteroide 1272 (Gefion)
    auto astdyn = std::make_shared<AstDynWrapper>();
    
    // Carica elementi da file (esempio)
    // astdyn->loadFromEQ1File("1272.eq1");
    
    // Configurazione analyzer
    OccultationAnalyzer::Config config;
    config.shadow_points = 11;  // Test con pochi punti
    config.shadow_time_span_min = 5.0;  // ±5 min
    config.verbose = true;
    
    OccultationAnalyzer analyzer(config);
    
    // Dati stella (esempio da verify_1272)
    double star_ra = 122.75;
    double star_dec = 25.10;
    double star_mag = 12.5;
    uint64_t star_id = 681067227714680448;
    
    // Parametri asteroide
    double asteroid_H = 10.31;
    double asteroid_diameter_km = 173.0;  // Gefion
    
    // Finestra di ricerca
    double start_mjd = 61052.0;
    double end_mjd = 61054.0;
    
    std::cout << "\n--- LIVELLO 2: Analisi Parametri ---" << std::endl;
    
    auto params = analyzer.analyzeOccultation(
        star_ra, star_dec, star_mag, star_id,
        astdyn,
        start_mjd, end_mjd,
        asteroid_H, asteroid_diameter_km
    );
    
    std::cout << "Is occultation: " << (params.is_occultation ? "YES" : "NO") << std::endl;
    std::cout << "Closest approach: " << params.closest_approach_arcsec << " arcsec" << std::endl;
    std::cout << "CA time (MJD): " << params.closest_approach_mjd << std::endl;
    std::cout << "Apparent diameter: " << params.apparent_diameter_arcsec << " arcsec" << std::endl;
    std::cout << "Estimated duration: " << params.estimated_duration_sec << " sec" << std::endl;
    
    if (params.is_occultation) {
        std::cout << "\n--- LIVELLO 3: Shadow Path (Besselian) ---" << std::endl;
        
        auto shadow_result = analyzer.calculateShadowPath(params, astdyn);
        
        if (shadow_result.shadow_computed) {
            std::cout << "✅ Shadow path computed successfully!" << std::endl;
            std::cout << "Bessel elements:" << std::endl;
            std::cout << "  x = " << shadow_result.bessel.x << " Earth radii" << std::endl;
            std::cout << "  y = " << shadow_result.bessel.y << " Earth radii" << std::endl;
            std::cout << "  dx = " << shadow_result.bessel.dx << " ER/hr" << std::endl;
            std::cout << "  dy = " << shadow_result.bessel.dy << " ER/hr" << std::endl;
            std::cout << "  L1 = " << shadow_result.bessel.L1 << " Earth radii" << std::endl;
            std::cout << "  d = " << shadow_result.bessel.d * 180.0 / M_PI << " deg" << std::endl;
            std::cout << "  mu = " << shadow_result.bessel.mu * 180.0 / M_PI << " deg" << std::endl;
            
            std::cout << "\nShadow velocity: " << shadow_result.shadow_velocity_km_s << " km/s" << std::endl;
            std::cout << "Max duration: " << shadow_result.max_duration_sec << " sec" << std::endl;
            std::cout << "Number of points: " << shadow_result.shadow_path.size() << std::endl;
            std::cout << "Calculation time: " << shadow_result.calculation_time_sec << " sec" << std::endl;
            std::cout << "Propagations: " << shadow_result.num_propagations << " (vs 21+ with Kepler)" << std::endl;
            
            if (!shadow_result.shadow_path.empty()) {
                std::cout << "\nFirst 3 shadow points:" << std::endl;
                for (size_t i = 0; i < std::min(size_t(3), shadow_result.shadow_path.size()); ++i) {
                    const auto& pt = shadow_result.shadow_path[i];
                    std::cout << "  Point " << i << ": Lat=" << pt.lat_deg 
                              << "° Lon=" << pt.lon_deg << "° MJD=" << pt.mjd_tdb << std::endl;
                }
            }
        } else {
            std::cout << "❌ Shadow path failed: " << shadow_result.error_message << std::endl;
        }
    }
    
    return 0;
}
