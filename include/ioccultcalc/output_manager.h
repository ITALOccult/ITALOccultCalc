/**
 * @file output_manager.h
 * @brief Sistema di output multi-formato per risultati occultazioni
 * 
 * Supporta 5 formati configurabili:
 * 1. TEXT - File di testo leggibile
 * 2. LATEX/PDF - Report scientifico con TikZ maps
 * 3. XML - Formato Occult4 per import in OccultWatcher Cloud
 * 4. JSON - Formato strutturato per API/web
 * 5. JPG - Schede grafiche stile IOTA
 * 
 * @author Michele Bigi
 * @date 2025-11-23
 */

#ifndef IOCCULTCALC_OUTPUT_MANAGER_H
#define IOCCULTCALC_OUTPUT_MANAGER_H

#include "ioccultcalc/types.h"
#include "ioccultcalc/config_manager.h"
#include <string>
#include <vector>
#include <memory>

namespace ioccultcalc {

/**
 * @brief Formato output disponibili
 */
enum class OutputFormat {
    TEXT,           ///< File di testo semplice (.txt)
    LATEX,          ///< LaTeX source (.tex)
    PDF,            ///< PDF compilato da LaTeX (.pdf)
    XML_OCCULT4,    ///< XML compatibile Occult4 (.xml)
    JSON,           ///< JSON strutturato (.json)
    IOTA_CARD,      ///< Scheda grafica IOTA (.jpg)
    ASTNUM_LIST,    ///< Lista numeri asteroidi con occultazioni (.txt)
    A4_VERTICAL_CARD ///< Scheda verticale A4 grande (.pdf)
};

/**
 * @brief Struttura dati occultazione per output
 */
struct OutputEvent {
    // Identificazione
    int asteroid_number;
    std::string asteroid_name;
    std::string asteroid_designation;
    
    // Parametri fisici asteroide
    double diameter_km;
    double absolute_magnitude;
    double albedo;
    
    // Stella
    std::string star_catalog;      // "TYC", "GAIA", "HIP", etc.
    std::string star_id;
    double star_ra_deg;
    double star_dec_deg;
    double star_mag;
    double star_parallax_mas;
    
    // Tempo evento
    double jd_event;               // Julian Date
    std::string utc_string;        // "2026-01-15 23:45:12"
    
    // Geometria occultazione
    double closest_approach_arcsec;
    double shadow_width_km;
    double shadow_velocity_kms;
    double duration_seconds;
    double mag_drop;
    
    // Path centrale
    struct PathPoint {
        double latitude;
        double longitude;
        std::string location_name;
        PathPoint(double lat = 0, double lon = 0, const std::string& loc = "")
            : latitude(lat), longitude(lon), location_name(loc) {}
    };
    std::vector<PathPoint> central_path;
    
    // Limiti Nord/Sud (1-sigma)
    std::vector<PathPoint> north_limit;
    std::vector<PathPoint> south_limit;
    
    // Incertezze
    double path_uncertainty_km;
    double time_uncertainty_sec;
    
    // Osservabilità
    struct ObserverData {
        std::string location_name;
        double latitude;
        double longitude;
        double elevation_m;
        double altitude_deg;
        double azimuth_deg;
        double sun_altitude_deg;
        double moon_separation_deg;
        bool observable;
    };
    std::vector<ObserverData> observers;
    
    // Priorità (0-11 scala italiana)
    int priority_score;
    std::string priority_class;    // "★★★", "★★", "★", "☆"
    
    // Metadati
    std::string computed_by;
    std::string computation_date;
    std::string software_version;

    struct ComparativePath {
        std::string name;
        std::string color;
        std::vector<PathPoint> points;
    };
    std::vector<ComparativePath> comparative_paths;
};

/**
 * @brief Opzioni configurabili output
 */
struct OutputOptions {
    // Generale
    OutputFormat format;
    std::string output_file;
    bool append_mode;
    
    // TEXT
    bool text_include_header;
    bool text_include_map_ascii;
    int text_width_columns;
    
    // LATEX/PDF
    bool latex_compile_pdf;
    bool latex_include_tikz_map;
    bool latex_include_uncertainty_plot;
    std::string latex_document_class;  // "article", "report"
    
    // XML
    std::string xml_schema_version;    // "Occult4.2.0"
    bool xml_include_uncertainties;
    
    // JSON
    bool json_pretty_print;
    int json_indent_spaces;
    bool json_include_metadata;
    
    // IOTA Card
    int iota_image_width;
    int iota_image_height;
    bool iota_include_map;
    bool iota_include_finder_chart;
    std::string iota_map_projection;   // "mercator", "orthographic"
    
    // Path rendering
    double path_resolution_km;         // Distanza tra punti path
    bool include_italy_highlight;
    std::vector<std::string> highlight_countries;
};

/**
 * @brief Gestore output multi-formato
 */
class OutputManager {
public:
    OutputManager();
    explicit OutputManager(const ConfigManager& config);
    ~OutputManager();
    
    /**
     * @brief Configura opzioni da ConfigManager
     */
    void configure(const ConfigManager& config);
    
    /**
     * @brief Imposta opzioni manualmente
     */
    void setOptions(const OutputOptions& options);
    
    /**
     * @brief Ottieni opzioni correnti
     */
    OutputOptions getOptions() const { return options_; }
    
    /**
     * @brief Genera output per singolo evento
     * @param event Dati evento occultazione
     * @param output_file File di output (override opzioni)
     * @return true se successo
     */
    bool writeEvent(const OutputEvent& event, 
                   const std::string& output_file = "");
    
    /**
     * @brief Genera output per lista eventi
     * @param events Lista eventi occultazione
     * @param output_file File di output (override opzioni)
     * @return true se successo
     */
    bool writeEvents(const std::vector<OutputEvent>& events,
                    const std::string& output_file = "");
    
    /**
     * @brief Genera report riepilogativo
     * @param events Lista eventi
     * @param summary_file File riepilogo
     * @return true se successo
     */
    bool writeSummary(const std::vector<OutputEvent>& events,
                     const std::string& summary_file);
    
    /**
     * @brief Genera scheda A4 verticale (20/40/40)
     * @param event Dati evento
     * @param filename Nome file output (.pdf o .tex)
     * @return true se successo
     */
    bool writeEventCardA4(const OutputEvent& event,
                         const std::string& filename);

private:
    OutputOptions options_;
    
    // Implementazioni formato-specifiche
    bool writeTextFormat(const std::vector<OutputEvent>& events,
                        const std::string& filename);
    
    bool writeLatexFormat(const std::vector<OutputEvent>& events,
                         const std::string& filename);
    
    bool compilePDF(const std::string& tex_file);
    
    bool writeXmlOccult4(const std::vector<OutputEvent>& events,
                        const std::string& filename);
    
    bool writeJsonFormat(const std::vector<OutputEvent>& events,
                        const std::string& filename);
    
    bool writeIotaCard(const OutputEvent& event,
                      const std::string& filename);
               /**
     * @brief Genera una lista di asteroidi con eventi in formato testo
     */
    bool writeAstNumList(const std::vector<OutputEvent>& events,
                        const std::string& filename);
                         
    /**
     * @brief Genera l'immagine della mappa dell'occultazione (Ground Map)
     * Usa IOC_Earth se disponibile.
     */
    std::string generateGroundMapImage(const OutputEvent& event, 
                                     int width, int height, 
                                     const std::string& output_path);

    // Utility rendering
    std::string generateAsciiMap(const OutputEvent& event,
                                int width, int height);
    
    std::string generateTikzMap(const OutputEvent& event);
    
    std::string formatDateTime(double jd);
    std::string formatCoordinate(double coord, bool is_latitude);
    std::string priorityToStars(int score);
};

/**
 * @brief Factory per creazione OutputManager da configurazione
 */
class OutputManagerFactory {
public:
    static std::unique_ptr<OutputManager> create(const ConfigManager& config);
    static std::unique_ptr<OutputManager> createFromJson(const std::string& json_file);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_OUTPUT_MANAGER_H
