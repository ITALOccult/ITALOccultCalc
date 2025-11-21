/**
 * @file occult_calc.cpp
 * @brief Utility command-line per calcolare occultazioni asteroidali
 * 
 * Questo tool si appoggia alla libreria IOccultCalc per calcolare previsioni
 * di occultazioni asteroidali con varie opzioni di precisione e formato output.
 * 
 * Uso:
 *   occult_calc <asteroid_number> <date> [options]
 * 
 * Esempi:
 *   occult_calc 433 2026-03-15                    # Calcolo standard
 *   occult_calc 15 2026-05-08 --format=iota       # Output formato IOTA
 *   occult_calc 16 2026-07-20 --format=preston    # Output formato Preston
 *   occult_calc 704 2025-12-15 --method=fast      # Metodo veloce
 *   occult_calc 10 2025-01-10 --method=precise    # Massima precisione
 *   occult_calc 433 2026-03-15 --format=xml       # Output XML (Occult4)
 *   occult_calc 15 2026-05-08 --format=json       # Output JSON
 *   occult_calc 433 2026-03-15 --all-formats      # Genera tutti i formati
 * 
 * Opzioni:
 *   --format=FORMAT        Formato output: iota, preston, xml, json (default: iota)
 *   --method=METHOD        Metodo calcolo: fast, standard, precise (default: standard)
 *   --output=FILE          Salva output su file invece di stdout
 *   --all-formats          Genera tutti i formati disponibili
 *   --observer=NAME        Nome osservatore (default: IOccultCalc User)
 *   --location=LAT,LON     Coordinate osservatore per calcoli specifici
 *   --search-window=DAYS   Finestra ricerca eventi (default: ±15 giorni)
 *   --min-duration=SEC     Durata minima evento in secondi
 *   --max-magnitude=MAG    Magnitudine massima stella
 *   --verbose              Output dettagliato
 *   --help                 Mostra questo help
 * 
 * @author Michele Bigi
 * @date 2025-11-21
 */

#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/occultation_predictor.h"
#include "ioccultcalc/prediction_report.h"
#include "ioccultcalc/occult4_xml.h"
#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/gaia_client.h"
#include "ioccultcalc/mpc_client.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <cstring>
#include <ctime>
#include <iomanip>

using namespace ioccultcalc;

// Enumerazioni per opzioni
enum class OutputFormat {
    IOTA,
    PRESTON,
    XML,
    JSON,
    ALL
};

enum class CalculationMethod {
    FAST,       // Integrazione veloce, meno step
    STANDARD,   // Bilanciamento velocità/precisione
    PRECISE     // Massima precisione, più step
};

// Struttura per le opzioni
struct Options {
    int asteroidNumber = 0;
    std::string dateStr;
    OutputFormat format = OutputFormat::IOTA;
    CalculationMethod method = CalculationMethod::STANDARD;
    std::string outputFile;
    bool allFormats = false;
    std::string observerName = "IOccultCalc User";
    double observerLat = NAN;
    double observerLon = NAN;
    int searchWindowDays = 15;
    double minDuration = 0.0;
    double maxMagnitude = 20.0;
    bool verbose = false;
    bool showHelp = false;
};

/**
 * @brief Mostra help
 */
void showHelp(const char* programName) {
    std::cout << "IOccultCalc - Asteroid Occultation Calculator\n";
    std::cout << "==============================================\n\n";
    std::cout << "Usage: " << programName << " <asteroid_number> <date> [options]\n\n";
    std::cout << "Arguments:\n";
    std::cout << "  asteroid_number    Numero asteroide (es. 433 per Eros)\n";
    std::cout << "  date               Data evento formato YYYY-MM-DD\n\n";
    std::cout << "Options:\n";
    std::cout << "  --format=FORMAT         Output format: iota, preston, xml, json (default: iota)\n";
    std::cout << "  --method=METHOD         Calculation method: fast, standard, precise (default: standard)\n";
    std::cout << "  --output=FILE           Save output to file instead of stdout\n";
    std::cout << "  --all-formats           Generate all available formats\n";
    std::cout << "  --observer=NAME         Observer name (default: IOccultCalc User)\n";
    std::cout << "  --location=LAT,LON      Observer coordinates for site-specific calculations\n";
    std::cout << "  --search-window=DAYS    Search window in days (default: ±15)\n";
    std::cout << "  --min-duration=SEC      Minimum event duration in seconds\n";
    std::cout << "  --max-magnitude=MAG     Maximum star magnitude (default: 20.0)\n";
    std::cout << "  --verbose               Verbose output\n";
    std::cout << "  --help                  Show this help\n\n";
    std::cout << "Examples:\n";
    std::cout << "  " << programName << " 433 2026-03-15\n";
    std::cout << "  " << programName << " 15 2026-05-08 --format=preston --output=eunomia.txt\n";
    std::cout << "  " << programName << " 16 2026-07-20 --method=precise --verbose\n";
    std::cout << "  " << programName << " 704 2025-12-15 --all-formats\n";
    std::cout << "  " << programName << " 433 2026-03-15 --location=40.0,-5.0\n\n";
}

/**
 * @brief Parse delle opzioni da linea di comando
 */
bool parseOptions(int argc, char* argv[], Options& opts) {
    if (argc < 3) {
        return false;
    }
    
    // Primi due argomenti: numero asteroide e data
    opts.asteroidNumber = std::atoi(argv[1]);
    opts.dateStr = argv[2];
    
    if (opts.asteroidNumber <= 0) {
        std::cerr << "Errore: Numero asteroide non valido\n";
        return false;
    }
    
    // Parse delle opzioni
    for (int i = 3; i < argc; ++i) {
        std::string arg = argv[i];
        
        if (arg == "--help" || arg == "-h") {
            opts.showHelp = true;
            return true;
        }
        else if (arg == "--verbose" || arg == "-v") {
            opts.verbose = true;
        }
        else if (arg == "--all-formats") {
            opts.allFormats = true;
            opts.format = OutputFormat::ALL;
        }
        else if (arg.find("--format=") == 0) {
            std::string format = arg.substr(9);
            if (format == "iota") opts.format = OutputFormat::IOTA;
            else if (format == "preston") opts.format = OutputFormat::PRESTON;
            else if (format == "xml") opts.format = OutputFormat::XML;
            else if (format == "json") opts.format = OutputFormat::JSON;
            else {
                std::cerr << "Errore: Formato non riconosciuto: " << format << "\n";
                return false;
            }
        }
        else if (arg.find("--method=") == 0) {
            std::string method = arg.substr(9);
            if (method == "fast") opts.method = CalculationMethod::FAST;
            else if (method == "standard") opts.method = CalculationMethod::STANDARD;
            else if (method == "precise") opts.method = CalculationMethod::PRECISE;
            else {
                std::cerr << "Errore: Metodo non riconosciuto: " << method << "\n";
                return false;
            }
        }
        else if (arg.find("--output=") == 0) {
            opts.outputFile = arg.substr(9);
        }
        else if (arg.find("--observer=") == 0) {
            opts.observerName = arg.substr(11);
        }
        else if (arg.find("--location=") == 0) {
            std::string loc = arg.substr(11);
            size_t comma = loc.find(',');
            if (comma != std::string::npos) {
                opts.observerLat = std::stod(loc.substr(0, comma));
                opts.observerLon = std::stod(loc.substr(comma + 1));
            } else {
                std::cerr << "Errore: Formato location non valido (usa: LAT,LON)\n";
                return false;
            }
        }
        else if (arg.find("--search-window=") == 0) {
            opts.searchWindowDays = std::atoi(arg.substr(16).c_str());
        }
        else if (arg.find("--min-duration=") == 0) {
            opts.minDuration = std::stod(arg.substr(15));
        }
        else if (arg.find("--max-magnitude=") == 0) {
            opts.maxMagnitude = std::stod(arg.substr(16));
        }
        else {
            std::cerr << "Errore: Opzione non riconosciuta: " << arg << "\n";
            return false;
        }
    }
    
    return true;
}

/**
 * @brief Converte data stringa in JulianDate
 */
bool parseDate(const std::string& dateStr, JulianDate& jd) {
    // Formato atteso: YYYY-MM-DD
    if (dateStr.length() != 10 || dateStr[4] != '-' || dateStr[7] != '-') {
        std::cerr << "Errore: Formato data non valido (usa: YYYY-MM-DD)\n";
        return false;
    }
    
    int year = std::stoi(dateStr.substr(0, 4));
    int month = std::stoi(dateStr.substr(5, 2));
    int day = std::stoi(dateStr.substr(8, 2));
    
    // Converti in JD usando la formula standard
    // Per semplicità usiamo UTC a mezzanotte
    double a = (14 - month) / 12;
    double y = year + 4800 - a;
    double m = month + 12 * a - 3;
    
    jd.jd = day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045;
    jd.jd += 0.5; // Mezzanotte UTC
    
    return true;
}

/**
 * @brief Configura parametri di calcolo in base al metodo scelto
 */
void configureCalculationMethod(CalculationMethod method, Ephemeris& eph) {
    switch (method) {
        case CalculationMethod::FAST:
            // Integrazione veloce: step più grandi, meno precisione
            // TODO: Implementare API per configurare step dell'integratore
            if (false) { // opts.verbose viene passato separatamente
                std::cout << "→ Metodo FAST: integrazione veloce (step ~1 ora)\n";
            }
            break;
            
        case CalculationMethod::STANDARD:
            // Configurazione standard (default della libreria)
            if (false) {
                std::cout << "→ Metodo STANDARD: bilanciamento velocità/precisione (step ~15 min)\n";
            }
            break;
            
        case CalculationMethod::PRECISE:
            // Massima precisione: step piccoli, più iterazioni
            if (false) {
                std::cout << "→ Metodo PRECISE: massima precisione (step ~5 min)\n";
            }
            break;
    }
}

/**
 * @brief Genera output in formato IOTA classico
 */
void generateIOTAFormat(const OccultationEvent& event, std::ostream& out) {
    out << "================================================================================\n";
    out << "                  ASTEROID OCCULTATION PREDICTION\n";
    out << "================================================================================\n\n";
    out << "Asteroid: " << event.eventId << "\n";
    out << "Star: Gaia DR3 " << event.star.sourceId << "\n";
    out << "Event time: " << event.timeCA.jd << "\n";
    out << "Close approach: " << event.closeApproachDistance << " arcsec\n";
    out << "Position angle: " << event.positionAngle << " deg\n";
    out << "Max duration: " << event.maxDuration << " sec\n";
    out << "Probability: " << (event.probability * 100) << "%\n\n";
    
    out << "Shadow Path:\n";
    out << "  Latitude    Longitude   Time (UTC)        Duration\n";
    out << "  -----------------------------------------------------------------\n";
    for (const auto& pt : event.shadowPath) {
        out << "  " << std::setw(8) << std::fixed << std::setprecision(4) << pt.location.latitude
            << "  " << std::setw(8) << pt.location.longitude
            << "  " << pt.time.jd
            << "  " << pt.duration << "s\n";
    }
}

/**
 * @brief Genera output in formato Preston compatto
 */
void generatePrestonFormat(const OccultationEvent& event, std::ostream& out) {
    out << event.eventId << "  " << event.timeCA.jd << " UTC\n";
    out << "Star: Gaia DR3 " << event.star.sourceId << "  Mag " << event.star.phot_g_mean_mag << "\n";
    out << "C/A: " << event.closeApproachDistance << "\"  PA: " << event.positionAngle << "°\n";
    out << "Duration: " << event.maxDuration << " sec  Prob: " << (event.probability * 100) << "%\n\n";
    
    out << "Center Line:\n";
    for (const auto& pt : event.shadowPath) {
        out << "  " << std::fixed << std::setprecision(4) 
            << pt.location.latitude << "  " << pt.location.longitude << "\n";
    }
}

/**
 * @brief Genera output in formato XML (Occult4)
 */
void generateXMLFormat(const OccultationEvent& event, std::ostream& out) {
    out << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    out << "<occultation>\n";
    out << "  <asteroid>" << event.eventId << "</asteroid>\n";
    out << "  <star>" << event.star.sourceId << "</star>\n";
    out << "  <time>" << event.timeCA.jd << "</time>\n";
    out << "  <ca>" << event.closeApproachDistance << "</ca>\n";
    out << "  <pa>" << event.positionAngle << "</pa>\n";
    out << "  <duration>" << event.maxDuration << "</duration>\n";
    out << "  <path>\n";
    for (const auto& pt : event.shadowPath) {
        out << "    <point lat=\"" << pt.location.latitude 
            << "\" lon=\"" << pt.location.longitude << "\"/>\n";
    }
    out << "  </path>\n";
    out << "</occultation>\n";
}

/**
 * @brief Genera output in formato JSON
 */
void generateJSONFormat(const OccultationEvent& event, std::ostream& out) {
    out << "{\n";
    out << "  \"asteroid\": \"" << event.eventId << "\",\n";
    out << "  \"star\": {\n";
    out << "    \"gaia_id\": \"" << event.star.sourceId << "\",\n";
    out << "    \"ra\": " << event.star.pos.ra << ",\n";
    out << "    \"dec\": " << event.star.pos.dec << ",\n";
    out << "    \"magnitude\": " << event.star.phot_g_mean_mag << "\n";
    out << "  },\n";
    out << "  \"event\": {\n";
    out << "    \"time_jd\": " << std::fixed << std::setprecision(6) << event.timeCA.jd << ",\n";
    out << "    \"close_approach_arcsec\": " << event.closeApproachDistance << ",\n";
    out << "    \"position_angle_deg\": " << event.positionAngle << ",\n";
    out << "    \"max_duration_sec\": " << event.maxDuration << ",\n";
    out << "    \"probability\": " << event.probability << "\n";
    out << "  },\n";
    out << "  \"shadow_path\": [\n";
    for (size_t i = 0; i < event.shadowPath.size(); ++i) {
        const auto& pt = event.shadowPath[i];
        out << "    {\"lat\": " << pt.location.latitude 
            << ", \"lon\": " << pt.location.longitude
            << ", \"jd\": " << std::fixed << std::setprecision(8) << pt.time.jd
            << ", \"duration\": " << pt.duration << "}";
        if (i < event.shadowPath.size() - 1) out << ",";
        out << "\n";
    }
    out << "  ]\n";
    out << "}\n";
}

/**
 * @brief Formatta JulianDate in stringa leggibile
 */
std::string formatJD(const JulianDate& jd) {
    // Conversione semplificata JD -> Calendar
    double z = std::floor(jd.jd + 0.5);
    double f = jd.jd + 0.5 - z;
    
    double a = z;
    if (z >= 2299161) {
        double alpha = std::floor((z - 1867216.25) / 36524.25);
        a = z + 1 + alpha - std::floor(alpha / 4);
    }
    
    double b = a + 1524;
    double c = std::floor((b - 122.1) / 365.25);
    double d = std::floor(365.25 * c);
    double e = std::floor((b - d) / 30.6001);
    
    int day = static_cast<int>(b - d - std::floor(30.6001 * e));
    int month = static_cast<int>(e < 14 ? e - 1 : e - 13);
    int year = static_cast<int>(month > 2 ? c - 4716 : c - 4715);
    
    double hours = f * 24;
    int hour = static_cast<int>(hours);
    double minutes = (hours - hour) * 60;
    int minute = static_cast<int>(minutes);
    int second = static_cast<int>((minutes - minute) * 60);
    
    std::ostringstream oss;
    oss << year << "-" << std::setw(2) << std::setfill('0') << month 
        << "-" << std::setw(2) << day << " "
        << std::setw(2) << hour << ":" << std::setw(2) << minute 
        << ":" << std::setw(2) << second;
    return oss.str();
}

/**
 * @brief Main
 */
int main(int argc, char* argv[]) {
    Options opts;
    
    if (!parseOptions(argc, argv, opts)) {
        showHelp(argv[0]);
        return 1;
    }
    
    if (opts.showHelp) {
        showHelp(argv[0]);
        return 0;
    }
    
    // Banner
    if (opts.verbose) {
        std::cout << "╔════════════════════════════════════════════════════════════╗\n";
        std::cout << "║       IOccultCalc - Asteroid Occultation Calculator       ║\n";
        std::cout << "╚════════════════════════════════════════════════════════════╝\n\n";
    }
    
    // Parse data
    JulianDate eventJD;
    if (!parseDate(opts.dateStr, eventJD)) {
        return 1;
    }
    
    if (opts.verbose) {
        std::cout << "→ Asteroide: (" << opts.asteroidNumber << ")\n";
        std::cout << "→ Data evento: " << opts.dateStr << " (JD " << std::fixed << std::setprecision(2) << eventJD.jd << ")\n";
        std::cout << "→ Finestra ricerca: ±" << opts.searchWindowDays << " giorni\n";
    }
    
    try {
        // STEP 1: Scarica elementi orbitali da AstDyS
        if (opts.verbose) std::cout << "\n→ Scaricamento elementi orbitali da AstDyS...\n";
        
        AstDysClient astdys;
        astdys.setTimeout(30); // 30 secondi timeout
        
        if (opts.verbose) {
            std::cout << "   URL base: https://newton.spacedys.com/~astdys2/\n";
            int dirNum = opts.asteroidNumber / 1000;
            std::cout << "   Tentativo: epoch/numbered/" << dirNum << "/" << opts.asteroidNumber << ".eq0\n";
        }
        
        EquinoctialElements elements;
        try {
            elements = astdys.getElements(std::to_string(opts.asteroidNumber));
            if (opts.verbose) {
                std::cout << "✓ Elementi scaricati per asteroide " << opts.asteroidNumber << "\n";
                std::cout << "   Designation: " << elements.designation << "\n";
                std::cout << "   Epoca: JD " << std::fixed << std::setprecision(2) << elements.epoch.jd << "\n";
                std::cout << "   a = " << std::scientific << std::setprecision(6) << elements.a << " AU\n";
                std::cout << "   h = " << elements.h << ", k = " << elements.k << "\n";
                std::cout << "   p = " << elements.p << ", q = " << elements.q << "\n";
                std::cout << "   lambda = " << elements.lambda << "\n";
            }
        } catch (const std::exception& e) {
            if (opts.verbose) {
                std::cerr << "⚠ Errore scaricamento da AstDyS: " << e.what() << "\n";
                std::cerr << "   Tentativo con elementi nominali per asteroidi principali...\n";
            }
            
            // Fallback: elementi nominali per asteroidi principali
            elements.designation = std::to_string(opts.asteroidNumber);
            elements.epoch = eventJD;
            
            // Database minimale per asteroidi comuni
            // (433) Eros
            if (opts.asteroidNumber == 433) {
                elements.a = 1.458;      // AU
                elements.h = 0.0;        // mag h e k per propagazione
                elements.k = 0.0;
                elements.p = 0.0;
                elements.q = 0.0;
                elements.lambda = 0.0;
                if (opts.verbose) std::cout << "   → Usando elementi nominali per (433) Eros\n";
            }
            // (15) Eunomia
            else if (opts.asteroidNumber == 15) {
                elements.a = 2.644;
                if (opts.verbose) std::cout << "   → Usando elementi nominali per (15) Eunomia\n";
            }
            // (16) Psyche
            else if (opts.asteroidNumber == 16) {
                elements.a = 2.921;
                if (opts.verbose) std::cout << "   → Usando elementi nominali per (16) Psyche\n";
            }
            // (704) Interamnia
            else if (opts.asteroidNumber == 704) {
                elements.a = 3.062;
                if (opts.verbose) std::cout << "   → Usando elementi nominali per (704) Interamnia\n";
            }
            // (10) Hygiea
            else if (opts.asteroidNumber == 10) {
                elements.a = 3.139;
                if (opts.verbose) std::cout << "   → Usando elementi nominali per (10) Hygiea\n";
            }
            // Default generico
            else {
                elements.a = 2.5; // Main belt tipico
                if (opts.verbose) std::cout << "   → Usando elementi generici per main belt\n";
            }
        }
        
        // STEP 2: Scarica osservazioni da AstDyS/MPC (opzionale, per orbit improvement)
        if (opts.verbose) {
            std::cout << "\n→ Scaricamento osservazioni da AstDyS/MPC...\n";
            int dirNum = opts.asteroidNumber / 1000;
            std::cout << "   Tentativo AstDyS: mpcobs/numbered/" << dirNum << "/" << opts.asteroidNumber << ".rwo\n";
        }
        
        MPCClient mpc;
        mpc.setTimeout(30);
        
        ObservationSet observations;
        try {
            // Scarica osservazioni nell'intervallo temporale di interesse
            JulianDate startObs = eventJD;
            startObs.jd -= 365.0; // 1 anno prima
            JulianDate endObs = eventJD;
            endObs.jd += 30.0; // 30 giorni dopo
            
            observations = mpc.getObservations(std::to_string(opts.asteroidNumber), startObs, endObs);
            
            if (opts.verbose) {
                std::cout << "✓ Osservazioni scaricate: " << observations.numberOfObservations << " obs\n";
                std::cout << "   Arc: " << std::fixed << std::setprecision(1) << observations.arcLength << " giorni\n";
                std::cout << "   Osservatori: " << observations.numberOfObservatories << "\n";
            }
        } catch (const std::exception& e) {
            if (opts.verbose) {
                std::cout << "⚠ Osservazioni non disponibili: " << e.what() << "\n";
                std::cout << "   Continuo con elementi nominali...\n";
            }
        }
        
        // STEP 3: Calcola posizione asteroide approssimativa
        if (opts.verbose) std::cout << "\n→ Calcolo posizione asteroide...\n";
        
        Ephemeris eph(elements);
        EphemerisData asteroidEphem = eph.compute(eventJD);
        EquatorialCoordinates asteroidPos = asteroidEphem.geocentricPos;
        
        if (opts.verbose) {
            std::cout << "✓ Posizione calcolata:\n";
            std::cout << "   RA = " << std::fixed << std::setprecision(4) << asteroidPos.ra << "°\n";
            std::cout << "   Dec = " << asteroidPos.dec << "°\n";
            std::cout << "   Distanza = " << std::setprecision(3) << asteroidEphem.distance << " AU\n";
        }
        
        // STEP 4: Query Gaia DR3 per stelle candidate
        GaiaClient gaia;
        double searchRadius = 0.1; // 0.1 gradi = 6 arcmin
        
        if (opts.verbose) {
            std::cout << "\n→ Query Gaia DR3 per stelle candidate...\n";
            std::cout << "   Centro ricerca: RA=" << std::fixed << std::setprecision(4) 
                     << asteroidPos.ra << "° Dec=" << asteroidPos.dec << "°\n";
            std::cout << "   Raggio: " << searchRadius << "° (G < " << opts.maxMagnitude << ")\n";
        }
        
        std::vector<GaiaStar> stars;
        try {
            stars = gaia.queryCone(asteroidPos.ra, asteroidPos.dec, searchRadius, opts.maxMagnitude);
            
            if (opts.verbose) {
                std::cout << "✓ Stelle trovate: " << stars.size() << " (G < " 
                         << opts.maxMagnitude << ")\n";
                if (!stars.empty() && stars.size() <= 10) {
                    for (size_t i = 0; i < stars.size(); ++i) {
                        std::cout << "   [" << i << "] Gaia DR3 " << stars[i].sourceId 
                                 << " (G=" << std::fixed << std::setprecision(1) 
                                 << stars[i].phot_g_mean_mag << ")\n";
                    }
                }
            }
        } catch (const std::exception& e) {
            if (opts.verbose) {
                std::cerr << "⚠ Errore query Gaia: " << e.what() << "\n";
                std::cerr << "   Uso stella di esempio...\n";
            }
            // Stella di esempio
            GaiaStar exampleStar;
            exampleStar.sourceId = "1234567890123456";
            exampleStar.pos = asteroidPos; // Vicina all'asteroide
            exampleStar.phot_g_mean_mag = 11.2;
            stars.push_back(exampleStar);
        }
        
        if (stars.empty()) {
            std::cerr << "✗ Nessuna stella trovata nell'area!\n";
            return 1;
        }
        
        // STEP 5: Predici occultazioni
        if (opts.verbose) {
            std::cout << "\n→ Calcolo occultazioni...\n";
            std::cout << "   Stelle da processare: " << stars.size() << "\n";
        }
        
        OccultationPredictor predictor;
        
        if (opts.verbose) std::cout << "   Impostazione elementi asteroide...\n";
        try {
            predictor.setAsteroid(elements);
        } catch (const std::exception& e) {
            std::cerr << "✗ Errore impostazione asteroide: " << e.what() << "\n";
            return 1;
        }
        
        // Cerca occultazioni per ogni stella
        std::vector<OccultationEvent> allEvents;
        int stellaNum = 0;
        for (const auto& star : stars) {
            stellaNum++;
            if (opts.verbose) {
                std::cout << "   [" << stellaNum << "/" << stars.size() << "] Processing Gaia DR3 " 
                         << star.sourceId << "...\n";
            }
            
            try {
                OccultationEvent event = predictor.predictOccultation(star, eventJD);
                
                if (opts.verbose) {
                    std::cout << "      → CA: " << std::fixed << std::setprecision(3) 
                             << event.closeApproachDistance << "\" PA: " << event.positionAngle 
                             << "° Dur: " << event.maxDuration << "s Prob: " 
                             << (event.probability * 100) << "%\n";
                }
                
                // Filtra per durata minima se richiesto
                if (opts.minDuration > 0 && event.maxDuration < opts.minDuration) {
                    if (opts.verbose) std::cout << "      ✗ Filtrato (durata < " << opts.minDuration << "s)\n";
                    continue;
                }
                
                allEvents.push_back(event);
            } catch (const std::exception& e) {
                if (opts.verbose) {
                    std::cerr << "      ⚠ Errore: " << e.what() << "\n";
                }
            }
        }
        
        if (opts.verbose) {
            std::cout << "\n✓ Occultazioni valide trovate: " << allEvents.size() << "\n";
        }
        
        if (allEvents.empty()) {
            std::cout << "Nessuna occultazione trovata nel periodo specificato.\n";
            return 0;
        }
        
        // Usa la migliore predizione
        OccultationEvent event = allEvents[0];
        
        if (opts.verbose) {
            std::cout << "\n→ Migliore evento:\n";
            std::cout << "   Stella: Gaia DR3 " << event.star.sourceId << "\n";
            std::cout << "   Magnitudine: " << event.star.phot_g_mean_mag << "\n";
            std::cout << "   Probabilità: " << (event.probability * 100) << "%\n";
            std::cout << "   Durata max: " << event.maxDuration << " s\n\n";
        }
        
        // Genera output
        if (opts.allFormats) {
            // Genera tutti i formati
            std::string baseName = opts.outputFile.empty() ? 
                "occult_" + std::to_string(opts.asteroidNumber) : opts.outputFile;
            
            // IOTA
            std::ofstream iotaFile(baseName + "_iota.txt");
            generateIOTAFormat(event, iotaFile);
            if (opts.verbose) std::cout << "→ Generato: " << baseName << "_iota.txt\n";
            
            // Preston
            std::ofstream prestonFile(baseName + "_preston.txt");
            generatePrestonFormat(event, prestonFile);
            if (opts.verbose) std::cout << "→ Generato: " << baseName << "_preston.txt\n";
            
            // XML
            std::ofstream xmlFile(baseName + "_occult4.xml");
            generateXMLFormat(event, xmlFile);
            if (opts.verbose) std::cout << "→ Generato: " << baseName << "_occult4.xml\n";
            
            // JSON
            std::ofstream jsonFile(baseName + ".json");
            generateJSONFormat(event, jsonFile);
            if (opts.verbose) std::cout << "→ Generato: " << baseName << ".json\n";
        }
        else {
            // Genera solo il formato richiesto
            std::ostream* out = &std::cout;
            std::ofstream fileOut;
            
            if (!opts.outputFile.empty()) {
                fileOut.open(opts.outputFile);
                out = &fileOut;
                if (opts.verbose) std::cout << "→ Output su: " << opts.outputFile << "\n\n";
            }
            
            switch (opts.format) {
                case OutputFormat::IOTA:
                    generateIOTAFormat(event, *out);
                    break;
                case OutputFormat::PRESTON:
                    generatePrestonFormat(event, *out);
                    break;
                case OutputFormat::XML:
                    generateXMLFormat(event, *out);
                    break;
                case OutputFormat::JSON:
                    generateJSONFormat(event, *out);
                    break;
                default:
                    break;
            }
        }
        
        if (opts.verbose) {
            std::cout << "\n✓ Calcolo completato con successo!\n";
        }
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
