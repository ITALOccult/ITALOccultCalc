/**
 * @file test_output_formats.cpp
 * @brief Test sistema output multi-formato
 * 
 * Genera esempi di output in tutti i 5 formati supportati:
 * 1. TEXT - File di testo leggibile
 * 2. LATEX/PDF - Report scientifico
 * 3. XML - Formato Occult4 per OccultWatcher Cloud
 * 4. JSON - Formato strutturato
 * 5. JPG - Scheda grafica IOTA
 * 
 * @author Michele Bigi
 * @date 2025-11-23
 */

#include "ioccultcalc/output_manager.h"
#include "ioccultcalc/occultation_predictor.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

// ============================================================================
// DATI TEST
// ============================================================================

OutputEvent createTestEvent() {
    OutputEvent evt;
    
    // Asteroide: (324) Bamberga
    evt.asteroid_number = 324;
    evt.asteroid_name = "Bamberga";
    evt.asteroid_designation = "1892 A";
    evt.diameter_km = 228.0;
    evt.absolute_magnitude = 6.82;
    evt.albedo = 0.063;
    
    // Stella: TYC 5865-00764-1
    evt.star_catalog = "TYC";
    evt.star_id = "5865-00764-1";
    evt.star_ra_deg = 56.8234;
    evt.star_dec_deg = 8.4567;
    evt.star_mag = 7.5;
    evt.star_parallax_mas = 1.2;
    
    // Evento: 2025-12-08 22:44:13 UT
    evt.jd_event = 2460653.44737;
    evt.utc_string = "2025-12-08 22:44:13";
    
    // Geometria
    evt.closest_approach_arcsec = 0.12;
    evt.shadow_width_km = 228.0;
    evt.shadow_velocity_kms = 19.8;
    evt.duration_seconds = 11.5;
    evt.mag_drop = 3.07;
    
    // Path centrale (Italia)
    evt.central_path = {
        {45.5, 9.2, "Milano"},
        {44.5, 11.3, "Bologna"},
        {43.8, 11.2, "Firenze"},
        {41.9, 12.5, "Roma"},
        {40.8, 14.3, "Napoli"}
    };
    
    // Limiti 1-sigma (±12 km)
    evt.north_limit = {
        {45.6, 9.3, ""},
        {44.6, 11.4, ""},
        {43.9, 11.3, ""},
        {42.0, 12.6, ""},
        {40.9, 14.4, ""}
    };
    
    evt.south_limit = {
        {45.4, 9.1, ""},
        {44.4, 11.2, ""},
        {43.7, 11.1, ""},
        {41.8, 12.4, ""},
        {40.7, 14.2, ""}
    };
    
    // Incertezze
    evt.path_uncertainty_km = 12.0;
    evt.time_uncertainty_sec = 2.5;
    
    // Osservabilità
    OccultationEvent::ObserverData obs_roma;
    obs_roma.location_name = "Roma Campidoglio";
    obs_roma.latitude = 41.8931;
    obs_roma.longitude = 12.4964;
    obs_roma.elevation_m = 50.0;
    obs_roma.altitude_deg = 71.3;
    obs_roma.azimuth_deg = 135.7;
    obs_roma.sun_altitude_deg = -45.2;
    obs_roma.moon_separation_deg = 78.5;
    obs_roma.observable = true;
    evt.observers.push_back(obs_roma);
    
    OccultationEvent::ObserverData obs_milano;
    obs_milano.location_name = "Milano Brera";
    obs_milano.latitude = 45.4654;
    obs_milano.longitude = 9.1859;
    obs_milano.elevation_m = 122.0;
    obs_milano.altitude_deg = 68.9;
    obs_milano.azimuth_deg = 138.2;
    obs_milano.sun_altitude_deg = -43.8;
    obs_milano.moon_separation_deg = 79.1;
    obs_milano.observable = true;
    evt.observers.push_back(obs_milano);
    
    // Priorità
    evt.priority_score = 11;
    evt.priority_class = "★★★";
    
    // Metadata
    evt.computed_by = "ITALOccultCalc";
    evt.computation_date = "2025-11-23";
    evt.software_version = "v1.0.0";
    
    return evt;
}

std::vector<OutputEvent> createTestEvents() {
    std::vector<OutputEvent> events;
    
    // Evento 1: Bamberga (già definito)
    events.push_back(createTestEvent());
    
    // Evento 2: (10) Hygiea
    OutputEvent evt2;
    evt2.asteroid_number = 10;
    evt2.asteroid_name = "Hygiea";
    evt2.asteroid_designation = "1849 Hygiea";
    evt2.diameter_km = 407.1;
    evt2.absolute_magnitude = 5.43;
    evt2.albedo = 0.072;
    
    evt2.star_catalog = "GAIA";
    evt2.star_id = "DR3 1234567890123456";
    evt2.star_ra_deg = 123.4567;
    evt2.star_dec_deg = 23.4567;
    evt2.star_mag = 10.2;
    evt2.star_parallax_mas = 0.8;
    
    evt2.jd_event = 2460685.274866;
    evt2.utc_string = "2026-01-09 18:35:48";
    
    evt2.closest_approach_arcsec = 19.63;
    evt2.shadow_width_km = 407.1;
    evt2.shadow_velocity_kms = 18.3;
    evt2.duration_seconds = 22.2;
    evt2.mag_drop = 7.45;
    
    evt2.central_path = {
        {42.5, 10.5, "Corsica"},
        {41.5, 12.8, "Lazio"},
        {40.2, 15.1, "Campania"}
    };
    
    evt2.path_uncertainty_km = 18.0;
    evt2.time_uncertainty_sec = 3.2;
    
    evt2.priority_score = 9;
    evt2.priority_class = "★★★";
    
    evt2.computed_by = "ITALOccultCalc";
    evt2.computation_date = "2025-11-23";
    evt2.software_version = "v1.0.0";
    
    events.push_back(evt2);
    
    // Evento 3: (4) Vesta
    OutputEvent evt3;
    evt3.asteroid_number = 4;
    evt3.asteroid_name = "Vesta";
    evt3.asteroid_designation = "1807 FA";
    evt3.diameter_km = 525.4;
    evt3.absolute_magnitude = 3.20;
    evt3.albedo = 0.423;
    
    evt3.star_catalog = "HIP";
    evt3.star_id = "45678";
    evt3.star_ra_deg = 234.5678;
    evt3.star_dec_deg = -12.3456;
    evt3.star_mag = 8.9;
    evt3.star_parallax_mas = 2.1;
    
    evt3.jd_event = 2460700.125000;
    evt3.utc_string = "2026-01-24 15:00:00";
    
    evt3.closest_approach_arcsec = 5.23;
    evt3.shadow_width_km = 525.4;
    evt3.shadow_velocity_kms = 21.5;
    evt3.duration_seconds = 24.4;
    evt3.mag_drop = 5.12;
    
    evt3.central_path = {
        {38.1, 13.4, "Sicilia"},
        {37.5, 15.1, "Sicilia"}
    };
    
    evt3.path_uncertainty_km = 22.0;
    evt3.time_uncertainty_sec = 2.8;
    
    evt3.priority_score = 7;
    evt3.priority_class = "★★";
    
    evt3.computed_by = "ITALOccultCalc";
    evt3.computation_date = "2025-11-23";
    evt3.software_version = "v1.0.0";
    
    events.push_back(evt3);
    
    return events;
}

// ============================================================================
// TEST FORMATI
// ============================================================================

void testTextFormat() {
    std::cout << "\n";
    std::cout << "================================================================\n";
    std::cout << "TEST 1: TEXT FORMAT\n";
    std::cout << "================================================================\n\n";
    
    OutputManager manager;
    OutputOptions opts;
    opts.format = OutputFormat::TEXT;
    opts.output_file = "test_output_text.txt";
    opts.text_include_header = true;
    opts.text_include_map_ascii = true;
    opts.text_width_columns = 80;
    manager.setOptions(opts);
    
    auto events = createTestEvents();
    bool success = manager.writeEvents(events);
    
    std::cout << "Output file: test_output_text.txt\n";
    std::cout << "Status: " << (success ? "✓ SUCCESS" : "✗ FAILED") << "\n";
    std::cout << "Events: " << events.size() << "\n\n";
}

void testLatexFormat() {
    std::cout << "================================================================\n";
    std::cout << "TEST 2: LATEX FORMAT\n";
    std::cout << "================================================================\n\n";
    
    OutputManager manager;
    OutputOptions opts;
    opts.format = OutputFormat::LATEX;
    opts.output_file = "test_output_latex.tex";
    opts.latex_compile_pdf = false;  // Solo LaTeX source
    opts.latex_include_tikz_map = true;
    opts.latex_include_uncertainty_plot = true;
    manager.setOptions(opts);
    
    auto events = createTestEvents();
    bool success = manager.writeEvents(events);
    
    std::cout << "Output file: test_output_latex.tex\n";
    std::cout << "Status: " << (success ? "✓ SUCCESS" : "✗ FAILED") << "\n";
    std::cout << "Events: " << events.size() << "\n";
    std::cout << "Note: PDF compilation disabled for test\n\n";
}

void testPdfFormat() {
    std::cout << "================================================================\n";
    std::cout << "TEST 3: PDF FORMAT\n";
    std::cout << "================================================================\n\n";
    
    OutputManager manager;
    OutputOptions opts;
    opts.format = OutputFormat::PDF;
    opts.output_file = "test_output_report.pdf";
    opts.latex_compile_pdf = true;
    opts.latex_include_tikz_map = true;
    manager.setOptions(opts);
    
    auto events = createTestEvents();
    bool success = manager.writeEvents(events);
    
    std::cout << "Output file: test_output_report.pdf\n";
    std::cout << "Status: " << (success ? "✓ SUCCESS" : "✗ FAILED") << "\n";
    std::cout << "Events: " << events.size() << "\n\n";
}

void testXmlFormat() {
    std::cout << "================================================================\n";
    std::cout << "TEST 4: XML FORMAT (Occult4 compatible)\n";
    std::cout << "================================================================\n\n";
    
    OutputManager manager;
    OutputOptions opts;
    opts.format = OutputFormat::XML_OCCULT4;
    opts.output_file = "test_output_occult4.xml";
    opts.xml_schema_version = "Occult4.2.0";
    opts.xml_include_uncertainties = true;
    manager.setOptions(opts);
    
    auto events = createTestEvents();
    bool success = manager.writeEvents(events);
    
    std::cout << "Output file: test_output_occult4.xml\n";
    std::cout << "Status: " << (success ? "✓ SUCCESS" : "✗ FAILED") << "\n";
    std::cout << "Events: " << events.size() << "\n";
    std::cout << "Schema: Occult4.2.0\n";
    std::cout << "Import to: OccultWatcher Cloud\n\n";
}

void testJsonFormat() {
    std::cout << "================================================================\n";
    std::cout << "TEST 5: JSON FORMAT\n";
    std::cout << "================================================================\n\n";
    
    OutputManager manager;
    OutputOptions opts;
    opts.format = OutputFormat::JSON;
    opts.output_file = "test_output_structured.json";
    opts.json_pretty_print = true;
    opts.json_indent_spaces = 2;
    opts.json_include_metadata = true;
    manager.setOptions(opts);
    
    auto events = createTestEvents();
    bool success = manager.writeEvents(events);
    
    std::cout << "Output file: test_output_structured.json\n";
    std::cout << "Status: " << (success ? "✓ SUCCESS" : "✗ FAILED") << "\n";
    std::cout << "Events: " << events.size() << "\n";
    std::cout << "Format: Pretty-printed with metadata\n\n";
}

void testIotaCardFormat() {
    std::cout << "================================================================\n";
    std::cout << "TEST 6: IOTA CARD FORMAT (JPG)\n";
    std::cout << "================================================================\n\n";
    
    OutputManager manager;
    OutputOptions opts;
    opts.format = OutputFormat::IOTA_CARD;
    opts.output_file = "test_output_iota_card.jpg";
    opts.iota_image_width = 1920;
    opts.iota_image_height = 1080;
    opts.iota_include_map = true;
    opts.iota_include_finder_chart = true;
    manager.setOptions(opts);
    
    auto event = createTestEvent();
    bool success = manager.writeEvent(event);
    
    std::cout << "Output file: test_output_iota_card.jpg\n";
    std::cout << "Status: " << (success ? "✓ SUCCESS" : "✗ FAILED") << "\n";
    std::cout << "Resolution: 1920x1080\n";
    std::cout << "Style: IOTA observation card\n\n";
}

void testConfigurableOutput() {
    std::cout << "================================================================\n";
    std::cout << "TEST 7: CONFIGURABLE FROM JSON\n";
    std::cout << "================================================================\n\n";
    
    ConfigManager config;
    try {
        config.loadFromJson("preset_output_config.json");
        
        OutputManager manager(config);
        auto events = createTestEvents();
        bool success = manager.writeEvents(events);
        
        std::cout << "Config file: preset_output_config.json\n";
        std::cout << "Output: According to config settings\n";
        std::cout << "Status: " << (success ? "✓ SUCCESS" : "✗ FAILED") << "\n\n";
        
    } catch (const std::exception& e) {
        std::cout << "✗ Config not found or invalid\n";
        std::cout << "  Using default TEXT format instead\n\n";
        
        OutputManager manager;
        auto events = createTestEvents();
        manager.writeEvents(events, "test_output_default.txt");
    }
}

// ============================================================================
// MAIN
// ============================================================================

int main() {
    std::cout << "\n";
    std::cout << "╔═══════════════════════════════════════════════════════════════╗\n";
    std::cout << "║        TEST OUTPUT MANAGER - MULTI-FORMAT SYSTEM              ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════════╝\n";
    
    try {
        testTextFormat();
        testLatexFormat();
        testPdfFormat();
        testXmlFormat();
        testJsonFormat();
        testIotaCardFormat();
        testConfigurableOutput();
        
        std::cout << "================================================================\n";
        std::cout << "SUMMARY\n";
        std::cout << "================================================================\n\n";
        
        std::cout << "Generated files:\n";
        std::cout << "  1. test_output_text.txt          - TEXT format\n";
        std::cout << "  2. test_output_latex.tex         - LATEX source\n";
        std::cout << "  3. test_output_report.pdf        - PDF report\n";
        std::cout << "  4. test_output_occult4.xml       - XML (Occult4)\n";
        std::cout << "  5. test_output_structured.json   - JSON\n";
        std::cout << "  6. test_output_iota_card.jpg     - IOTA card\n\n";
        
        std::cout << "Use cases:\n";
        std::cout << "  TEXT   -> Quick review, email, printout\n";
        std::cout << "  LATEX  -> Scientific papers, detailed analysis\n";
        std::cout << "  PDF    -> Distribution, publication\n";
        std::cout << "  XML    -> Import to OccultWatcher Cloud\n";
        std::cout << "  JSON   -> Web APIs, databases, automation\n";
        std::cout << "  JPG    -> IOTA submissions, social media\n\n";
        
        std::cout << "✓ All tests completed!\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n✗ ERROR: " << e.what() << "\n\n";
        return 1;
    }
}
