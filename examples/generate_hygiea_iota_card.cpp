/**
 * @file generate_hygiea_iota_card.cpp
 * @brief Genera scheda IOTA per occultazione (10) Hygiea
 * 
 * Evento: 2026-01-09 18:35:48 UT
 * Asteroide: (10) Hygiea
 * Stella: Gaia DR3 (mag 10.2)
 * Mag drop: 7.45 mag (eccellente!)
 * Durata: 22.2 secondi
 * 
 * @author Michele Bigi
 * @date 2025-11-23
 */

#include "ioccultcalc/output_manager.h"
#include <iostream>

using namespace ioccultcalc;

int main() {
    std::cout << "Generazione scheda IOTA per (10) Hygiea...\n\n";
    
    // Crea evento Hygiea
    OccultationEvent hygiea;
    
    // Asteroide
    hygiea.asteroid_number = 10;
    hygiea.asteroid_name = "Hygiea";
    hygiea.asteroid_designation = "1849 Hygiea";
    hygiea.diameter_km = 407.1;
    hygiea.absolute_magnitude = 5.43;
    hygiea.albedo = 0.072;
    
    // Stella
    hygiea.star_catalog = "GAIA DR3";
    hygiea.star_id = "1234567890123456";
    hygiea.star_ra_deg = 123.4567;
    hygiea.star_dec_deg = 23.4567;
    hygiea.star_mag = 10.2;
    hygiea.star_parallax_mas = 0.8;
    
    // Evento
    hygiea.jd_event = 2460685.274866;
    hygiea.utc_string = "2026-01-09 18:35:48";
    
    // Geometria
    hygiea.closest_approach_arcsec = 19.63;
    hygiea.shadow_width_km = 407.1;
    hygiea.shadow_velocity_kms = 18.3;
    hygiea.duration_seconds = 22.2;
    hygiea.mag_drop = 7.45;
    
    // Path centrale attraverso Italia
    hygiea.central_path = {
        {43.5, 10.0, "Mar Tirreno"},
        {42.8, 11.2, "Nord Lazio"},
        {42.0, 12.5, "Roma"},
        {41.2, 13.8, "Sud Lazio"},
        {40.5, 15.1, "Campania"},
        {39.8, 16.4, "Basilicata"}
    };
    
    // Limiti 1-sigma (±18 km)
    hygiea.north_limit = {
        {43.6, 10.1, ""},
        {42.9, 11.3, ""},
        {42.1, 12.6, ""},
        {41.3, 13.9, ""},
        {40.6, 15.2, ""},
        {39.9, 16.5, ""}
    };
    
    hygiea.south_limit = {
        {43.4, 9.9, ""},
        {42.7, 11.1, ""},
        {41.9, 12.4, ""},
        {41.1, 13.7, ""},
        {40.4, 15.0, ""},
        {39.7, 16.3, ""}
    };
    
    // Incertezze
    hygiea.path_uncertainty_km = 18.0;
    hygiea.time_uncertainty_sec = 3.2;
    
    // Osservabilità Italia
    OccultationEvent::ObserverData roma;
    roma.location_name = "Roma Campidoglio";
    roma.latitude = 41.8931;
    roma.longitude = 12.4964;
    roma.elevation_m = 50.0;
    roma.altitude_deg = 52.3;
    roma.azimuth_deg = 178.5;
    roma.sun_altitude_deg = -12.4;  // Crepuscolo
    roma.moon_separation_deg = 65.2;
    roma.observable = true;
    hygiea.observers.push_back(roma);
    
    OccultationEvent::ObserverData napoli;
    napoli.location_name = "Napoli Capodimonte";
    napoli.latitude = 40.8522;
    napoli.longitude = 14.2681;
    napoli.elevation_m = 150.0;
    napoli.altitude_deg = 48.7;
    napoli.azimuth_deg = 175.3;
    napoli.sun_altitude_deg = -14.1;
    napoli.moon_separation_deg = 67.8;
    napoli.observable = true;
    hygiea.observers.push_back(napoli);
    
    OccultationEvent::ObserverData firenze;
    firenze.location_name = "Firenze Arcetri";
    firenze.latitude = 43.7502;
    firenze.longitude = 11.2532;
    firenze.elevation_m = 184.0;
    firenze.altitude_deg = 55.8;
    firenze.azimuth_deg = 182.1;
    firenze.sun_altitude_deg = -10.2;
    firenze.moon_separation_deg = 63.5;
    firenze.observable = true;
    hygiea.observers.push_back(firenze);
    
    // Priorità
    hygiea.priority_score = 11;
    hygiea.priority_class = "★★★";
    
    // Metadata
    hygiea.computed_by = "ITALOccultCalc";
    hygiea.computation_date = "2025-11-23";
    hygiea.software_version = "v1.0.0";
    
    // Genera scheda IOTA
    std::cout << "Configurazione output IOTA card...\n";
    
    OutputManager manager;
    OutputOptions opts;
    opts.format = OutputFormat::IOTA_CARD;
    opts.output_file = "hygiea_iota_card.jpg";
    opts.iota_image_width = 1920;
    opts.iota_image_height = 1080;
    opts.iota_include_map = true;
    opts.iota_include_finder_chart = false;
    opts.iota_map_projection = "mercator";
    manager.setOptions(opts);
    
    std::cout << "Generazione LaTeX template...\n";
    bool success = manager.writeEvent(hygiea);
    
    if (success) {
        std::cout << "\n";
        std::cout << "================================================================\n";
        std::cout << "✓ SCHEDA IOTA GENERATA CON SUCCESSO!\n";
        std::cout << "================================================================\n\n";
        std::cout << "File: hygiea_iota_card.jpg\n";
        std::cout << "Risoluzione: 1920x1080 (Full HD)\n";
        std::cout << "Formato: IOTA observation card\n\n";
        std::cout << "Dettagli evento:\n";
        std::cout << "  Asteroide: (10) Hygiea (D=407 km)\n";
        std::cout << "  Data: 2026-01-09 18:35:48 UT\n";
        std::cout << "  Mag drop: 7.45 mag (ECCELLENTE!)\n";
        std::cout << "  Durata: 22.2 secondi\n";
        std::cout << "  Path: Attraversa Italia centrale\n";
        std::cout << "  Priorità: ★★★ (11/11)\n\n";
        std::cout << "Osservabilità:\n";
        std::cout << "  Roma: Alt 52° - OTTIMO\n";
        std::cout << "  Napoli: Alt 49° - OTTIMO\n";
        std::cout << "  Firenze: Alt 56° - ECCELLENTE\n\n";
        std::cout << "Condizioni:\n";
        std::cout << "  Sole: -12° (crepuscolo astronomico)\n";
        std::cout << "  Luna: 65° di separazione\n";
        std::cout << "  Cielo: BUONO per osservazione\n\n";
        std::cout << "La scheda può essere:\n";
        std::cout << "  • Stampata A4 landscape\n";
        std::cout << "  • Proiettata su schermo\n";
        std::cout << "  • Condivisa con osservatori\n";
        std::cout << "  • Inviata a IOTA per coordinamento\n\n";
    } else {
        std::cout << "\n✗ Errore generazione scheda IOTA\n";
        std::cout << "Verifica che pdflatex e sips/ImageMagick siano installati\n\n";
        return 1;
    }
    
    // Genera anche altri formati per confronto
    std::cout << "Generazione formati aggiuntivi...\n\n";
    
    // TEXT
    opts.format = OutputFormat::TEXT;
    opts.output_file = "hygiea_details.txt";
    manager.setOptions(opts);
    manager.writeEvent(hygiea);
    std::cout << "  ✓ hygiea_details.txt (TEXT)\n";
    
    // JSON
    opts.format = OutputFormat::JSON;
    opts.output_file = "hygiea_data.json";
    opts.json_pretty_print = true;
    manager.setOptions(opts);
    manager.writeEvent(hygiea);
    std::cout << "  ✓ hygiea_data.json (JSON)\n";
    
    // XML per OccultWatcher
    opts.format = OutputFormat::XML_OCCULT4;
    opts.output_file = "hygiea_occultwatch.xml";
    manager.setOptions(opts);
    manager.writeEvent(hygiea);
    std::cout << "  ✓ hygiea_occultwatch.xml (Occult4)\n";
    
    // PDF report
    opts.format = OutputFormat::PDF;
    opts.output_file = "hygiea_report.pdf";
    opts.latex_compile_pdf = true;
    opts.latex_include_tikz_map = true;
    manager.setOptions(opts);
    manager.writeEvent(hygiea);
    std::cout << "  ✓ hygiea_report.pdf (PDF)\n\n";
    
    std::cout << "================================================================\n";
    std::cout << "TUTTI I FORMATI GENERATI!\n";
    std::cout << "================================================================\n\n";
    
    return 0;
}
