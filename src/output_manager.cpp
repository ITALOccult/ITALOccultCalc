/**
 * @file output_manager.cpp
 * @brief Implementazione sistema output multi-formato
 */

#include "ioccultcalc/output_manager.h"
#include "ioccultcalc/time_utils.h"
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <algorithm>
#include <cstdlib>

namespace ioccultcalc {

// ============================================================================
// COSTRUTTORI
// ============================================================================

OutputManager::OutputManager() {
    // Opzioni di default
    options_.format = OutputFormat::TEXT;
    options_.output_file = "occultation_results.txt";
    options_.append_mode = false;
    
    options_.text_include_header = true;
    options_.text_include_map_ascii = true;
    options_.text_width_columns = 80;
    
    options_.latex_compile_pdf = true;
    options_.latex_include_tikz_map = true;
    options_.latex_include_uncertainty_plot = true;
    options_.latex_document_class = "article";
    
    options_.xml_schema_version = "Occult4.2.0";
    options_.xml_include_uncertainties = true;
    
    options_.json_pretty_print = true;
    options_.json_indent_spaces = 2;
    options_.json_include_metadata = true;
    
    options_.iota_image_width = 1920;
    options_.iota_image_height = 1080;
    options_.iota_include_map = true;
    options_.iota_include_finder_chart = true;
    options_.iota_map_projection = "mercator";
    
    options_.path_resolution_km = 100.0;
    options_.include_italy_highlight = true;
}

OutputManager::OutputManager(const ConfigManager& config) : OutputManager() {
    configure(config);
}

OutputManager::~OutputManager() = default;

// ============================================================================
// CONFIGURAZIONE
// ============================================================================

void OutputManager::configure(const ConfigManager& config) {
    try {
        auto outputSection = config.getSection(ConfigSection::OUTPUT);
        if (!outputSection) return;
        
        // Formato
        std::string formatStr = outputSection->getParameter("format")->asString();
        if (formatStr == "TEXT") options_.format = OutputFormat::TEXT;
        else if (formatStr == "LATEX") options_.format = OutputFormat::LATEX;
        else if (formatStr == "PDF") options_.format = OutputFormat::PDF;
        else if (formatStr == "XML") options_.format = OutputFormat::XML_OCCULT4;
        else if (formatStr == "JSON") options_.format = OutputFormat::JSON;
        else if (formatStr == "IOTA_CARD") options_.format = OutputFormat::IOTA_CARD;
        
        // File output
        options_.output_file = outputSection->getParameter("file")->asString();
        
        // Opzioni formato-specifiche
        if (outputSection->hasParameter("latex_compile_pdf")) {
            options_.latex_compile_pdf = outputSection->getParameter("latex_compile_pdf")->asBool();
        }
        
        if (outputSection->hasParameter("json_pretty_print")) {
            options_.json_pretty_print = outputSection->getParameter("json_pretty_print")->asBool();
        }
        
        if (outputSection->hasParameter("include_map")) {
            options_.latex_include_tikz_map = outputSection->getParameter("include_map")->asBool();
        }
        
    } catch (const std::exception& e) {
        // Usa defaults
    }
}

void OutputManager::setOptions(const OutputOptions& options) {
    options_ = options;
}

// ============================================================================
// WRITE EVENTS
// ============================================================================

bool OutputManager::writeEvent(const OccultationEvent& event,
                               const std::string& output_file) {
    return writeEvents({event}, output_file);
}

bool OutputManager::writeEvents(const std::vector<OccultationEvent>& events,
                               const std::string& output_file) {
    if (events.empty()) return false;
    
    std::string filename = output_file.empty() ? options_.output_file : output_file;
    
    switch (options_.format) {
        case OutputFormat::TEXT:
            return writeTextFormat(events, filename);
        
        case OutputFormat::LATEX:
            return writeLatexFormat(events, filename);
        
        case OutputFormat::PDF: {
            std::string tex_file = filename;
            if (tex_file.size() > 4 && tex_file.substr(tex_file.size()-4) == ".pdf") {
                tex_file = tex_file.substr(0, tex_file.size()-4) + ".tex";
            } else {
                tex_file += ".tex";
            }
            if (!writeLatexFormat(events, tex_file)) return false;
            if (options_.latex_compile_pdf) {
                return compilePDF(tex_file);
            }
            return true;
        }
        
        case OutputFormat::XML_OCCULT4:
            return writeXmlOccult4(events, filename);
        
        case OutputFormat::JSON:
            return writeJsonFormat(events, filename);
        
        case OutputFormat::IOTA_CARD:
            if (events.size() == 1) {
                return writeIotaCard(events[0], filename);
            } else {
                // Multi-card: un file per evento
                bool success = true;
                for (size_t i = 0; i < events.size(); i++) {
                    std::string base = filename;
                    size_t dot = base.rfind('.');
                    if (dot != std::string::npos) {
                        base = base.substr(0, dot) + "_" + std::to_string(i+1) + base.substr(dot);
                    } else {
                        base += "_" + std::to_string(i+1) + ".jpg";
                    }
                    success = success && writeIotaCard(events[i], base);
                }
                return success;
            }
        
        default:
            return false;
    }
}

bool OutputManager::writeSummary(const std::vector<OccultationEvent>& events,
                                const std::string& summary_file) {
    // Sempre in formato TEXT per summary
    OutputFormat orig_format = options_.format;
    options_.format = OutputFormat::TEXT;
    
    bool result = writeTextFormat(events, summary_file);
    
    options_.format = orig_format;
    return result;
}

// ============================================================================
// TEXT FORMAT
// ============================================================================

bool OutputManager::writeTextFormat(const std::vector<OccultationEvent>& events,
                                   const std::string& filename) {
    std::ofstream file(filename);
    if (!file.is_open()) return false;
    
    // Header
    if (options_.text_include_header) {
        file << std::string(options_.text_width_columns, '=') << "\n";
        file << "ITALOCCULTCALC - ASTEROID OCCULTATION PREDICTIONS\n";
        file << std::string(options_.text_width_columns, '=') << "\n";
        file << "Generated: " << events[0].computation_date << "\n";
        file << "Software: " << events[0].software_version << "\n";
        file << "Total events: " << events.size() << "\n";
        file << std::string(options_.text_width_columns, '=') << "\n\n";
    }
    
    // Eventi
    for (size_t i = 0; i < events.size(); i++) {
        const auto& evt = events[i];
        
        file << "EVENT #" << (i+1) << " - Priority: " << priorityToStars(evt.priority_score) 
             << " (" << evt.priority_score << "/11)\n";
        file << std::string(options_.text_width_columns, '-') << "\n\n";
        
        // Asteroide
        file << "ASTEROID:\n";
        file << "  (" << evt.asteroid_number << ") " << evt.asteroid_name;
        if (!evt.asteroid_designation.empty()) {
            file << " = " << evt.asteroid_designation;
        }
        file << "\n";
        file << "  Diameter: " << std::fixed << std::setprecision(1) 
             << evt.diameter_km << " km\n";
        file << "  H magnitude: " << std::setprecision(2) << evt.absolute_magnitude << "\n";
        file << "  Albedo: " << std::setprecision(3) << evt.albedo << "\n\n";
        
        // Stella
        file << "STAR:\n";
        file << "  Catalog: " << evt.star_catalog << " " << evt.star_id << "\n";
        file << "  RA: " << formatCoordinate(evt.star_ra_deg, false) << "\n";
        file << "  Dec: " << formatCoordinate(evt.star_dec_deg, true) << "\n";
        file << "  Magnitude: " << std::setprecision(2) << evt.star_mag << "\n\n";
        
        // Evento
        file << "OCCULTATION:\n";
        file << "  Time (UTC): " << evt.utc_string << "\n";
        file << "  JD: " << std::fixed << std::setprecision(6) << evt.jd_event << "\n";
        file << "  Magnitude drop: " << std::setprecision(2) << evt.mag_drop << " mag\n";
        file << "  Duration: " << std::setprecision(1) << evt.duration_seconds << " seconds\n";
        file << "  Shadow width: " << std::setprecision(1) << evt.shadow_width_km << " km\n";
        file << "  Shadow velocity: " << std::setprecision(2) << evt.shadow_velocity_kms << " km/s\n";
        file << "  Closest approach: " << std::setprecision(3) << evt.closest_approach_arcsec 
             << " arcsec\n\n";
        
        // Incertezze
        file << "UNCERTAINTIES:\n";
        file << "  Path: ±" << std::setprecision(1) << evt.path_uncertainty_km << " km\n";
        file << "  Time: ±" << std::setprecision(2) << evt.time_uncertainty_sec << " seconds\n\n";
        
        // Path centrale (primi/ultimi punti)
        if (!evt.central_path.empty()) {
            file << "CENTRAL PATH:\n";
            file << "  Start: " << formatCoordinate(evt.central_path.front().latitude, true)
                 << ", " << formatCoordinate(evt.central_path.front().longitude, false);
            if (!evt.central_path.front().location_name.empty()) {
                file << " (" << evt.central_path.front().location_name << ")";
            }
            file << "\n";
            
            file << "  End:   " << formatCoordinate(evt.central_path.back().latitude, true)
                 << ", " << formatCoordinate(evt.central_path.back().longitude, false);
            if (!evt.central_path.back().location_name.empty()) {
                file << " (" << evt.central_path.back().location_name << ")";
            }
            file << "\n\n";
        }
        
        // Osservatori
        if (!evt.observers.empty()) {
            file << "OBSERVABILITY:\n";
            for (const auto& obs : evt.observers) {
                file << "  " << obs.location_name << ":\n";
                file << "    Altitude: " << std::setprecision(1) << obs.altitude_deg << "°\n";
                file << "    Azimuth: " << std::setprecision(1) << obs.azimuth_deg << "°\n";
                file << "    Sun altitude: " << std::setprecision(1) << obs.sun_altitude_deg << "°\n";
                file << "    Observable: " << (obs.observable ? "YES" : "NO") << "\n";
            }
            file << "\n";
        }
        
        // ASCII map
        if (options_.text_include_map_ascii) {
            file << "MAP:\n";
            file << generateAsciiMap(evt, options_.text_width_columns - 4, 20);
            file << "\n";
        }
        
        file << std::string(options_.text_width_columns, '=') << "\n\n";
    }
    
    file.close();
    return true;
}

// ============================================================================
// LATEX FORMAT
// ============================================================================

bool OutputManager::writeLatexFormat(const std::vector<OccultationEvent>& events,
                                    const std::string& filename) {
    std::ofstream file(filename);
    if (!file.is_open()) return false;
    
    // Preamble
    file << "\\documentclass[11pt,a4paper]{" << options_.latex_document_class << "}\n";
    file << "\\usepackage[utf8]{inputenc}\n";
    file << "\\usepackage[T1]{fontenc}\n";
    file << "\\usepackage[italian,english]{babel}\n";
    file << "\\usepackage{geometry}\n";
    file << "\\usepackage{graphicx}\n";
    file << "\\usepackage{amsmath}\n";
    file << "\\usepackage{booktabs}\n";
    file << "\\usepackage{hyperref}\n";
    
    if (options_.latex_include_tikz_map) {
        file << "\\usepackage{tikz}\n";
        file << "\\usetikzlibrary{arrows,shapes,positioning}\n";
    }
    
    file << "\\geometry{margin=2cm}\n\n";
    
    file << "\\title{Asteroid Occultation Predictions}\n";
    file << "\\author{ITALOccultCalc}\n";
    file << "\\date{" << events[0].computation_date << "}\n\n";
    
    file << "\\begin{document}\n\n";
    file << "\\maketitle\n\n";
    
    // Summary table
    file << "\\section*{Summary}\n\n";
    file << "Total predictions: " << events.size() << "\\\\\n";
    file << "Generated by: " << events[0].computed_by << "\\\\\n";
    file << "Software: " << events[0].software_version << "\n\n";
    
    file << "\\begin{table}[h]\n";
    file << "\\centering\n";
    file << "\\begin{tabular}{clcccc}\n";
    file << "\\toprule\n";
    file << "\\# & Asteroid & Date (UTC) & Mag Drop & Duration & Priority \\\\\n";
    file << "\\midrule\n";
    
    for (size_t i = 0; i < events.size(); i++) {
        const auto& evt = events[i];
        file << (i+1) << " & (" << evt.asteroid_number << ") " << evt.asteroid_name
             << " & " << evt.utc_string.substr(0, 10)
             << " & " << std::fixed << std::setprecision(1) << evt.mag_drop
             << " & " << std::setprecision(1) << evt.duration_seconds << "s"
             << " & " << priorityToStars(evt.priority_score) << " \\\\\n";
    }
    
    file << "\\bottomrule\n";
    file << "\\end{tabular}\n";
    file << "\\caption{Predicted occultation events}\n";
    file << "\\end{table}\n\n";
    
    file << "\\clearpage\n\n";
    
    // Dettaglio eventi
    for (size_t i = 0; i < events.size(); i++) {
        const auto& evt = events[i];
        
        file << "\\section{Event \\#" << (i+1) << ": (" << evt.asteroid_number 
             << ") " << evt.asteroid_name << "}\n\n";
        
        file << "Priority: " << priorityToStars(evt.priority_score) 
             << " (" << evt.priority_score << "/11)\n\n";
        
        // Tabella parametri
        file << "\\subsection{Event Parameters}\n\n";
        file << "\\begin{table}[h]\n";
        file << "\\begin{tabular}{ll}\n";
        file << "\\toprule\n";
        file << "\\multicolumn{2}{c}{\\textbf{Asteroid}} \\\\\n";
        file << "\\midrule\n";
        file << "Number & " << evt.asteroid_number << " \\\\\n";
        file << "Name & " << evt.asteroid_name << " \\\\\n";
        file << "Diameter & " << std::fixed << std::setprecision(1) << evt.diameter_km << " km \\\\\n";
        file << "H magnitude & " << std::setprecision(2) << evt.absolute_magnitude << " \\\\\n";
        file << "\\midrule\n";
        file << "\\multicolumn{2}{c}{\\textbf{Star}} \\\\\n";
        file << "\\midrule\n";
        file << "Catalog & " << evt.star_catalog << " " << evt.star_id << " \\\\\n";
        file << "RA (J2000) & " << formatCoordinate(evt.star_ra_deg, false) << " \\\\\n";
        file << "Dec (J2000) & " << formatCoordinate(evt.star_dec_deg, true) << " \\\\\n";
        file << "Magnitude & " << std::setprecision(2) << evt.star_mag << " \\\\\n";
        file << "\\midrule\n";
        file << "\\multicolumn{2}{c}{\\textbf{Occultation}} \\\\\n";
        file << "\\midrule\n";
        file << "Time (UTC) & " << evt.utc_string << " \\\\\n";
        file << "JD & " << std::fixed << std::setprecision(6) << evt.jd_event << " \\\\\n";
        file << "Magnitude drop & " << std::setprecision(2) << evt.mag_drop << " mag \\\\\n";
        file << "Duration & " << std::setprecision(1) << evt.duration_seconds << " s \\\\\n";
        file << "Shadow width & " << std::setprecision(1) << evt.shadow_width_km << " km \\\\\n";
        file << "Shadow velocity & " << std::setprecision(2) << evt.shadow_velocity_kms << " km/s \\\\\n";
        file << "Path uncertainty & $\\pm$" << std::setprecision(1) << evt.path_uncertainty_km << " km \\\\\n";
        file << "Time uncertainty & $\\pm$" << std::setprecision(2) << evt.time_uncertainty_sec << " s \\\\\n";
        file << "\\bottomrule\n";
        file << "\\end{tabular}\n";
        file << "\\end{table}\n\n";
        
        // TikZ map
        if (options_.latex_include_tikz_map && !evt.central_path.empty()) {
            file << "\\subsection{Ground Track}\n\n";
            file << generateTikzMap(evt);
            file << "\n";
        }
        
        if (i < events.size() - 1) {
            file << "\\clearpage\n\n";
        }
    }
    
    file << "\\end{document}\n";
    file.close();
    
    return true;
}

bool OutputManager::compilePDF(const std::string& tex_file) {
    // Trova pdflatex
    std::string pdflatex = "/usr/local/texlive/2025/bin/universal-darwin/pdflatex";
    
    // Controlla se esiste
    std::ifstream test(pdflatex);
    if (!test.good()) {
        // Prova percorso standard
        pdflatex = "pdflatex";
    }
    test.close();
    
    // Compila (2 passaggi per references)
    std::string cmd = pdflatex + " -interaction=nonstopmode " + tex_file + " > /dev/null 2>&1";
    int result1 = std::system(cmd.c_str());
    int result2 = std::system(cmd.c_str());
    
    return (result1 == 0 && result2 == 0);
}

// ============================================================================
// XML OCCULT4 FORMAT
// ============================================================================

bool OutputManager::writeXmlOccult4(const std::vector<OccultationEvent>& events,
                                   const std::string& filename) {
    std::ofstream file(filename);
    if (!file.is_open()) return false;
    
    // XML Header
    file << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    file << "<OccultationPredictions xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n";
    file << "                        version=\"" << options_.xml_schema_version << "\">\n\n";
    
    // Metadata
    file << "  <Metadata>\n";
    file << "    <Generator>" << events[0].computed_by << "</Generator>\n";
    file << "    <Software>" << events[0].software_version << "</Software>\n";
    file << "    <GenerationDate>" << events[0].computation_date << "</GenerationDate>\n";
    file << "    <EventCount>" << events.size() << "</EventCount>\n";
    file << "  </Metadata>\n\n";
    
    // Eventi
    file << "  <Events>\n";
    for (const auto& evt : events) {
        file << "    <Event>\n";
        
        // Asteroid
        file << "      <Asteroid>\n";
        file << "        <Number>" << evt.asteroid_number << "</Number>\n";
        file << "        <Name>" << evt.asteroid_name << "</Name>\n";
        file << "        <Diameter unit=\"km\">" << std::fixed << std::setprecision(2) 
             << evt.diameter_km << "</Diameter>\n";
        file << "        <AbsoluteMagnitude>" << std::setprecision(2) 
             << evt.absolute_magnitude << "</AbsoluteMagnitude>\n";
        file << "      </Asteroid>\n";
        
        // Star
        file << "      <Star>\n";
        file << "        <Catalog>" << evt.star_catalog << "</Catalog>\n";
        file << "        <ID>" << evt.star_id << "</ID>\n";
        file << "        <RA unit=\"degrees\">" << std::setprecision(8) 
             << evt.star_ra_deg << "</RA>\n";
        file << "        <Dec unit=\"degrees\">" << std::setprecision(8) 
             << evt.star_dec_deg << "</Dec>\n";
        file << "        <Magnitude>" << std::setprecision(2) 
             << evt.star_mag << "</Magnitude>\n";
        file << "      </Star>\n";
        
        // Event
        file << "      <Occultation>\n";
        file << "        <TimeUTC>" << evt.utc_string << "</TimeUTC>\n";
        file << "        <JulianDate>" << std::setprecision(6) 
             << evt.jd_event << "</JulianDate>\n";
        file << "        <MagnitudeDrop>" << std::setprecision(2) 
             << evt.mag_drop << "</MagnitudeDrop>\n";
        file << "        <Duration unit=\"seconds\">" << std::setprecision(2) 
             << evt.duration_seconds << "</Duration>\n";
        file << "        <ShadowWidth unit=\"km\">" << std::setprecision(1) 
             << evt.shadow_width_km << "</ShadowWidth>\n";
        file << "        <ShadowVelocity unit=\"km/s\">" << std::setprecision(2) 
             << evt.shadow_velocity_kms << "</ShadowVelocity>\n";
        
        if (options_.xml_include_uncertainties) {
            file << "        <PathUncertainty unit=\"km\">" << std::setprecision(1) 
                 << evt.path_uncertainty_km << "</PathUncertainty>\n";
            file << "        <TimeUncertainty unit=\"seconds\">" << std::setprecision(2) 
                 << evt.time_uncertainty_sec << "</TimeUncertainty>\n";
        }
        
        file << "      </Occultation>\n";
        
        // Path
        if (!evt.central_path.empty()) {
            file << "      <CentralPath>\n";
            for (const auto& pt : evt.central_path) {
                file << "        <Point lat=\"" << std::setprecision(6) << pt.latitude
                     << "\" lon=\"" << std::setprecision(6) << pt.longitude << "\"/>\n";
            }
            file << "      </CentralPath>\n";
        }
        
        // Priority
        file << "      <Priority>\n";
        file << "        <Score>" << evt.priority_score << "</Score>\n";
        file << "        <Class>" << evt.priority_class << "</Class>\n";
        file << "      </Priority>\n";
        
        file << "    </Event>\n\n";
    }
    file << "  </Events>\n";
    
    file << "</OccultationPredictions>\n";
    file.close();
    
    return true;
}

// ============================================================================
// JSON FORMAT
// ============================================================================

bool OutputManager::writeJsonFormat(const std::vector<OccultationEvent>& events,
                                   const std::string& filename) {
    std::ofstream file(filename);
    if (!file.is_open()) return false;
    
    std::string indent = options_.json_pretty_print ? 
        std::string(options_.json_indent_spaces, ' ') : "";
    std::string indent2 = indent + indent;
    std::string indent3 = indent2 + indent;
    
    file << "{\n";
    
    // Metadata
    if (options_.json_include_metadata) {
        file << indent << "\"metadata\": {\n";
        file << indent2 << "\"generator\": \"" << events[0].computed_by << "\",\n";
        file << indent2 << "\"software\": \"" << events[0].software_version << "\",\n";
        file << indent2 << "\"date_generated\": \"" << events[0].computation_date << "\",\n";
        file << indent2 << "\"event_count\": " << events.size() << "\n";
        file << indent << "},\n";
    }
    
    // Eventi
    file << indent << "\"events\": [\n";
    
    for (size_t i = 0; i < events.size(); i++) {
        const auto& evt = events[i];
        
        file << indent2 << "{\n";
        
        // Asteroid
        file << indent3 << "\"asteroid\": {\n";
        file << indent3 << indent << "\"number\": " << evt.asteroid_number << ",\n";
        file << indent3 << indent << "\"name\": \"" << evt.asteroid_name << "\",\n";
        file << indent3 << indent << "\"diameter_km\": " << std::fixed 
             << std::setprecision(2) << evt.diameter_km << ",\n";
        file << indent3 << indent << "\"absolute_magnitude\": " << std::setprecision(2) 
             << evt.absolute_magnitude << "\n";
        file << indent3 << "},\n";
        
        // Star
        file << indent3 << "\"star\": {\n";
        file << indent3 << indent << "\"catalog\": \"" << evt.star_catalog << "\",\n";
        file << indent3 << indent << "\"id\": \"" << evt.star_id << "\",\n";
        file << indent3 << indent << "\"ra_deg\": " << std::setprecision(8) 
             << evt.star_ra_deg << ",\n";
        file << indent3 << indent << "\"dec_deg\": " << std::setprecision(8) 
             << evt.star_dec_deg << ",\n";
        file << indent3 << indent << "\"magnitude\": " << std::setprecision(2) 
             << evt.star_mag << "\n";
        file << indent3 << "},\n";
        
        // Occultation
        file << indent3 << "\"occultation\": {\n";
        file << indent3 << indent << "\"time_utc\": \"" << evt.utc_string << "\",\n";
        file << indent3 << indent << "\"jd\": " << std::setprecision(6) 
             << evt.jd_event << ",\n";
        file << indent3 << indent << "\"mag_drop\": " << std::setprecision(2) 
             << evt.mag_drop << ",\n";
        file << indent3 << indent << "\"duration_sec\": " << std::setprecision(2) 
             << evt.duration_seconds << ",\n";
        file << indent3 << indent << "\"shadow_width_km\": " << std::setprecision(1) 
             << evt.shadow_width_km << ",\n";
        file << indent3 << indent << "\"shadow_velocity_kms\": " << std::setprecision(2) 
             << evt.shadow_velocity_kms << ",\n";
        file << indent3 << indent << "\"path_uncertainty_km\": " << std::setprecision(1) 
             << evt.path_uncertainty_km << ",\n";
        file << indent3 << indent << "\"time_uncertainty_sec\": " << std::setprecision(2) 
             << evt.time_uncertainty_sec << "\n";
        file << indent3 << "},\n";
        
        // Priority
        file << indent3 << "\"priority\": {\n";
        file << indent3 << indent << "\"score\": " << evt.priority_score << ",\n";
        file << indent3 << indent << "\"class\": \"" << evt.priority_class << "\"\n";
        file << indent3 << "}\n";
        
        file << indent2 << "}";
        if (i < events.size() - 1) file << ",";
        file << "\n";
    }
    
    file << indent << "]\n";
    file << "}\n";
    
    file.close();
    return true;
}

// ============================================================================
// IOTA CARD
// ============================================================================

bool OutputManager::writeIotaCard(const OccultationEvent& event,
                                 const std::string& filename) {
    // Genera script LaTeX per carta IOTA
    std::string tex_file = filename;
    if (tex_file.size() > 4 && tex_file.substr(tex_file.size()-4) == ".jpg") {
        tex_file = tex_file.substr(0, tex_file.size()-4) + ".tex";
    } else {
        tex_file += ".tex";
    }
    
    std::ofstream file(tex_file);
    if (!file.is_open()) return false;
    
    file << "\\documentclass[landscape]{article}\n";
    file << "\\usepackage[paperwidth=10in,paperheight=7.5in,margin=0.5in]{geometry}\n";
    file << "\\usepackage{tikz,graphicx,amsmath}\n";
    file << "\\usepackage{helvet}\n";
    file << "\\renewcommand{\\familydefault}{\\sfdefault}\n";
    file << "\\pagestyle{empty}\n";
    file << "\\begin{document}\n\n";
    
    file << "\\begin{tikzpicture}[remember picture,overlay]\n";
    file << "  \\node[anchor=north west] at (current page.north west) {\n";
    file << "    \\begin{minipage}{10in}\n";
    
    // Header IOTA-style
    file << "      \\begin{center}\n";
    file << "        {\\Huge\\bfseries ASTEROID OCCULTATION}\\\\\n";
    file << "        \\vspace{0.2cm}\n";
    file << "        {\\LARGE (" << event.asteroid_number << ") " << event.asteroid_name 
         << " occults " << event.star_catalog << " " << event.star_id << "}\\\\\n";
    file << "        \\vspace{0.1cm}\n";
    file << "        {\\Large " << event.utc_string << " UT}\n";
    file << "      \\end{center}\n";
    file << "      \\vspace{0.3cm}\n";
    
    // Due colonne
    file << "      \\begin{minipage}[t]{0.48\\linewidth}\n";
    file << "        {\\large\\bfseries Event Details}\\\\\n";
    file << "        \\vspace{0.2cm}\n";
    file << "        \\begin{tabular}{ll}\n";
    file << "          Magnitude drop: & {\\Large\\bfseries " << std::fixed 
         << std::setprecision(1) << event.mag_drop << " mag} \\\\\n";
    file << "          Duration: & {\\Large\\bfseries " << std::setprecision(1) 
         << event.duration_seconds << " sec} \\\\\n";
    file << "          Shadow width: & " << std::setprecision(0) 
         << event.shadow_width_km << " km \\\\\n";
    file << "          Shadow speed: & " << std::setprecision(1) 
         << event.shadow_velocity_kms << " km/s \\\\\n";
    file << "          Path uncertainty: & $\\pm$" << std::setprecision(0) 
         << event.path_uncertainty_km << " km \\\\\n";
    file << "        \\end{tabular}\n";
    file << "      \\end{minipage}\n";
    file << "      \\hfill\n";
    file << "      \\begin{minipage}[t]{0.48\\linewidth}\n";
    file << "        {\\large\\bfseries Star Data}\\\\\n";
    file << "        \\vspace{0.2cm}\n";
    file << "        \\begin{tabular}{ll}\n";
    file << "          Star mag: & {\\Large\\bfseries " << std::setprecision(1) 
         << event.star_mag << "} \\\\\n";
    file << "          RA (J2000): & " << formatCoordinate(event.star_ra_deg, false) << " \\\\\n";
    file << "          Dec (J2000): & " << formatCoordinate(event.star_dec_deg, true) << " \\\\\n";
    file << "          Priority: & {\\Large " << priorityToStars(event.priority_score) 
         << "} (" << event.priority_score << "/11) \\\\\n";
    file << "        \\end{tabular}\n";
    file << "      \\end{minipage}\n";
    
    file << "    \\end{minipage}\n";
    file << "  };\n";
    file << "\\end{tikzpicture}\n\n";
    
    file << "\\end{document}\n";
    file.close();
    
    // Compila a PDF
    if (!compilePDF(tex_file)) return false;
    
    // Converti PDF -> JPG usando ImageMagick/sips
    std::string pdf_file = tex_file.substr(0, tex_file.size()-4) + ".pdf";
    std::string cmd = "sips -s format jpeg -s formatOptions 90 " + pdf_file + 
                     " --out " + filename + " > /dev/null 2>&1";
    int result = std::system(cmd.c_str());
    
    return (result == 0);
}

// ============================================================================
// UTILITY
// ============================================================================

std::string OutputManager::generateAsciiMap(const OccultationEvent& event,
                                           int width, int height) {
    std::stringstream map;
    
    if (event.central_path.empty()) {
        map << "(No path data available)\n";
        return map.str();
    }
    
    // Trova bounds
    double minLat = 90, maxLat = -90, minLon = 180, maxLon = -180;
    for (const auto& pt : event.central_path) {
        minLat = std::min(minLat, pt.latitude);
        maxLat = std::max(maxLat, pt.latitude);
        minLon = std::min(minLon, pt.longitude);
        maxLon = std::max(maxLon, pt.longitude);
    }
    
    // Aggiungi margine
    double latMargin = (maxLat - minLat) * 0.1;
    double lonMargin = (maxLon - minLon) * 0.1;
    minLat -= latMargin; maxLat += latMargin;
    minLon -= lonMargin; maxLon += lonMargin;
    
    // Crea griglia ASCII
    std::vector<std::string> grid(height, std::string(width, ' '));
    
    // Disegna bordo
    for (int x = 0; x < width; x++) {
        grid[0][x] = '-';
        grid[height-1][x] = '-';
    }
    for (int y = 0; y < height; y++) {
        grid[y][0] = '|';
        grid[y][width-1] = '|';
    }
    
    // Disegna path
    for (const auto& pt : event.central_path) {
        int x = (int)((pt.longitude - minLon) / (maxLon - minLon) * (width - 3)) + 1;
        int y = (int)((maxLat - pt.latitude) / (maxLat - minLat) * (height - 3)) + 1;
        
        if (x >= 1 && x < width-1 && y >= 1 && y < height-1) {
            grid[y][x] = '*';
        }
    }
    
    // Output
    for (const auto& row : grid) {
        map << "  " << row << "\n";
    }
    
    map << "  Lat: " << std::fixed << std::setprecision(1) << minLat 
        << "° to " << maxLat << "°\n";
    map << "  Lon: " << std::fixed << std::setprecision(1) << minLon 
        << "° to " << maxLon << "°\n";
    
    return map.str();
}

std::string OutputManager::generateTikzMap(const OccultationEvent& event) {
    std::stringstream tikz;
    
    if (event.central_path.empty()) return "";
    
    tikz << "\\begin{figure}[h]\n";
    tikz << "\\centering\n";
    tikz << "\\begin{tikzpicture}[scale=0.8]\n";
    
    // Trova bounds
    double minLat = 90, maxLat = -90, minLon = 180, maxLon = -180;
    for (const auto& pt : event.central_path) {
        minLat = std::min(minLat, pt.latitude);
        maxLat = std::max(maxLat, pt.latitude);
        minLon = std::min(minLon, pt.longitude);
        maxLon = std::max(maxLon, pt.longitude);
    }
    
    double latMargin = (maxLat - minLat) * 0.15;
    double lonMargin = (maxLon - minLon) * 0.15;
    minLat -= latMargin; maxLat += latMargin;
    minLon -= lonMargin; maxLon += lonMargin;
    
    double scaleX = 12.0 / (maxLon - minLon);
    double scaleY = 8.0 / (maxLat - minLat);
    double scale = std::min(scaleX, scaleY);
    
    // Griglia geografica
    tikz << "  % Griglia\n";
    tikz << "  \\draw[gray,very thin] (" << ((minLon - minLon) * scale) << "," 
         << ((minLat - minLat) * scale) << ") rectangle (" 
         << ((maxLon - minLon) * scale) << "," << ((maxLat - minLat) * scale) << ");\n";
    
    // Path centrale
    tikz << "  % Central path\n";
    tikz << "  \\draw[red,very thick] ";
    for (size_t i = 0; i < event.central_path.size(); i++) {
        const auto& pt = event.central_path[i];
        double x = (pt.longitude - minLon) * scale;
        double y = (pt.latitude - minLat) * scale;
        tikz << "(" << std::fixed << std::setprecision(2) << x << "," << y << ")";
        if (i < event.central_path.size() - 1) tikz << " -- ";
    }
    tikz << ";\n";
    
    // Limiti incertezza (se presenti)
    if (!event.north_limit.empty()) {
        tikz << "  % North limit\n";
        tikz << "  \\draw[red,dotted] ";
        for (size_t i = 0; i < event.north_limit.size(); i++) {
            const auto& pt = event.north_limit[i];
            double x = (pt.longitude - minLon) * scale;
            double y = (pt.latitude - minLat) * scale;
            tikz << "(" << std::fixed << std::setprecision(2) << x << "," << y << ")";
            if (i < event.north_limit.size() - 1) tikz << " -- ";
        }
        tikz << ";\n";
    }
    
    if (!event.south_limit.empty()) {
        tikz << "  % South limit\n";
        tikz << "  \\draw[red,dotted] ";
        for (size_t i = 0; i < event.south_limit.size(); i++) {
            const auto& pt = event.south_limit[i];
            double x = (pt.longitude - minLon) * scale;
            double y = (pt.latitude - minLat) * scale;
            tikz << "(" << std::fixed << std::setprecision(2) << x << "," << y << ")";
            if (i < event.south_limit.size() - 1) tikz << " -- ";
        }
        tikz << ";\n";
    }
    
    // Punti notevoli
    if (!event.central_path.empty()) {
        const auto& start = event.central_path.front();
        const auto& end = event.central_path.back();
        
        double x1 = (start.longitude - minLon) * scale;
        double y1 = (start.latitude - minLat) * scale;
        double x2 = (end.longitude - minLon) * scale;
        double y2 = (end.latitude - minLat) * scale;
        
        tikz << "  \\filldraw[blue] (" << x1 << "," << y1 << ") circle (2pt) node[above] {Start};\n";
        tikz << "  \\filldraw[blue] (" << x2 << "," << y2 << ") circle (2pt) node[above] {End};\n";
    }
    
    tikz << "\\end{tikzpicture}\n";
    tikz << "\\caption{Ground track with 1-$\\sigma$ uncertainty limits (dotted lines)}\n";
    tikz << "\\end{figure}\n";
    
    return tikz.str();
}

std::string OutputManager::formatDateTime(double jd) {
    int year, month, day, hour, minute;
    double second;
    JulianDate julian;
    julian.jd = jd;
    TimeUtils::jdToCalendar(julian, year, month, day, hour, minute, second);
    
    std::stringstream ss;
    ss << year << "-" << std::setfill('0') << std::setw(2) << month << "-"
       << std::setw(2) << day << " " << std::setw(2) << hour << ":"
       << std::setw(2) << minute << ":" << std::setw(2) << (int)second;
    return ss.str();
}

std::string OutputManager::formatCoordinate(double coord, bool is_latitude) {
    char sign = (coord >= 0) ? '+' : '-';
    coord = std::abs(coord);
    
    int deg = (int)coord;
    double frac = (coord - deg) * 60.0;
    int min = (int)frac;
    double sec = (frac - min) * 60.0;
    
    std::stringstream ss;
    ss << sign << std::setfill('0') << std::setw(2) << deg << "°"
       << std::setw(2) << min << "'" << std::fixed << std::setprecision(2)
       << std::setw(5) << sec << "\"";
    
    return ss.str();
}

std::string OutputManager::priorityToStars(int score) {
    if (score >= 8) return "★★★";
    if (score >= 5) return "★★";
    if (score >= 3) return "★";
    return "☆";
}

// ============================================================================
// FACTORY
// ============================================================================

std::unique_ptr<OutputManager> OutputManagerFactory::create(const ConfigManager& config) {
    return std::make_unique<OutputManager>(config);
}

std::unique_ptr<OutputManager> OutputManagerFactory::createFromJson(const std::string& json_file) {
    ConfigManager config;
    config.loadFromJson(json_file);
    return create(config);
}

} // namespace ioccultcalc
