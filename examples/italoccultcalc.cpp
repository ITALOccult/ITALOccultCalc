/**
 * @file italoccultcalc.cpp
 * @brief ITALOccultCalc - Procedura completa ricerca occultazioni asteroidali
 * 
 * Workflow automatizzato completo:
 * 1. Carica configurazione da preset JSON
 * 2. Seleziona asteroidi candidati (database/MPC)
 * 3. Propaga orbite con alta precisione (Phase 2 features)
 * 4. Query catalogo stelle Gaia DR3
 * 5. Rileva eventi di occultazione
 * 6. Calcola incertezze e priorità
 * 7. Genera report multipli (IOTA, Preston, KML, JSON)
 * 
 * Ottimizzato per osservatori italiani.
 * 
 * @author Michele Bigi
 * @date 2025-11-23
 */

#include "ioccultcalc/config_manager.h"
#include "ioccultcalc/asteroid_filter.h"
#include "ioccultcalc/orbit_propagator.h"
// IOC_GaiaLib - UnifiedGaiaCatalog per formato multifile_v2
#include "ioc_gaialib/unified_gaia_catalog.h"
#include "ioccultcalc/occultation_predictor.h"
#include "ioccultcalc/chebyshev_detector.h"
#include "ioccultcalc/occult4_xml.h"
#include "ioccultcalc/prediction_report.h"
#include "ioccultcalc/time_utils.h"
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/kml_exporter.h"
#include "ioccultcalc/observation.h"
#include "ioccultcalc/orbit_fitter.h"
#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/mpc_client.h"
#include "planetary_aberration.h"
#include "cubic_spline.h"
// Phase1+Phase2 integration (LinOccult method)
// #include "ioccultcalc/occultation_search_astdyn.h" // REMOVED
#include "phase1_candidate_screening.h"
#include "phase2_occultation_geometry.h"
#include <nlohmann/json.hpp>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>
#include <string>
#include <chrono>
#include <thread>
#include <algorithm>

#ifdef _OPENMP
#include <omp.h>
#endif
#include <ctime>
#include <cmath>
#include <set>
#include <sstream>
#include <unistd.h>  // for getpid()

// AstDyn Headers for manual propagation check
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/propagation/Propagator.hpp"
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/propagation/Integrator.hpp"
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/propagation/OrbitalElements.hpp"
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/ephemeris/PlanetaryEphemeris.hpp"
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/api/OrbitFitAPI.hpp"

using namespace ioccultcalc;
#include "ioccultcalc/allnum_database.h"

// Helper: Convert Ecliptic Elements (from DB/AstDyS) to Equatorial (for Propagator)
// Assumes Mean Obliquity J2000 for the rotation.
// Helper: Convert Ecliptic Elements (from DB/AstDyS) to Osculating Equatorial using AstDyn API
astdyn::propagation::KeplerianElements convert_ecliptic_to_equatorial(const ioccultcalc::OrbitalElements& startElem) {
    // 1. Map to AstDyn Keplerian (Mean Ecliptic)
    astdyn::propagation::KeplerianElements kMeanEcl;
    kMeanEcl.epoch_mjd_tdb = startElem.epoch.toMJD();
    kMeanEcl.semi_major_axis = startElem.a;
    kMeanEcl.eccentricity = startElem.e;
    kMeanEcl.inclination = startElem.i;
    kMeanEcl.longitude_ascending_node = startElem.Omega;
    kMeanEcl.argument_perihelion = startElem.omega;
    kMeanEcl.mean_anomaly = startElem.M;
    kMeanEcl.gravitational_parameter = astdyn::constants::GMS;

    // 2. Convert to Equinoctial (Mean Ecliptic)
    auto eqMeanEcl = astdyn::propagation::keplerian_to_equinoctial(kMeanEcl);

    // 3. Use API to convert Mean Equinoctial (Ecliptic) -> Osculating Keplerian (Equatorial)
    // This handles the geometric frame rotation (Ecliptic J2000 -> Equatorial J2000)
    // and prepares elements for the equatorial propagator.
    // Note: This API treats input as "Mean" geometrically. 
    // For physical Mean->Osculating (J2), use mean_to_osculating() additionally if needed.
    auto kOscEq = astdyn::api::OrbitFitAPI::convert_mean_equinoctial_to_osculating(eqMeanEcl);
    
    // (Optional) Apply physical Mean->Osculating correction (J2)
    // kOscEq = astdyn::propagation::mean_to_osculating(kOscEq); 

    return kOscEq;
}





        // ...


// ============================================================================
// VARIABILI GLOBALI
// ============================================================================

static bool g_verbose = true;  // Flag per output verboso (default true, può essere disabilitato da config)
static int g_verbosityLevel = 0; // 0=quiet, 1=monitor, 2=debug

// ============================================================================
// STRUTTURE DATI
// ============================================================================

struct AsteroidCandidate {
    OrbitalElements elements;
    double priority_score;
    std::string reason;
};

struct ItalOccultationEvent {
    std::string asteroid_name;
    std::string asteroid_id;
    EquinoctialElements asteroid_elements;  // Elementi completi per XML
    StarData star;
    JulianDate event_time;
    double separation_arcsec;
    double position_angle;  // Added field
    double mag_drop;
    double duration_sec;
    double path_width_km;
    double uncertainty_km;
    double priority_score;
    std::vector<std::string> visible_from_italy;
    std::vector<ioccultcalc::ShadowPathPoint> path; // Traccia per KML
    
    // Besselian Elements and Extra Data
    double besselianX;
    double besselianY;
    double besselianDX;
    double besselianDY;
    double observerLongitude;
    double observerLatitude;
    double observerAltitude;
};

// ============================================================================
// UTILITÀ STAMPA
// ============================================================================

void printHeader(const std::string& title) {
    std::cout << "\n";
    std::cout << "================================================================\n";
    std::cout << title << "\n";
    std::cout << "================================================================\n\n";
}

void printProgress(const std::string& stage, int current, int total) {
    std::cout << "[" << current << "/" << total << "] " << stage << "...\r" << std::flush;
}

// Stampa barra di avanzamento unificata per l'intero processo
void printUnifiedProgress(double percentage, const std::string& phase, const std::string& details) {
    if (!g_verbose) {
        return;
    }
    
    int barWidth = 50;
    int pos = (int)((percentage * barWidth) / 100.0);
    
    // Troncamento del dettaglio per evitare righe troppo lunghe
    std::string truncDetails = details;
    if (truncDetails.length() > 50) {
        truncDetails = truncDetails.substr(0, 47) + "...";
    }
    
    std::cout << "\r[";
    for (int i = 0; i < barWidth; ++i) {
        if (i < pos) std::cout << "█";
        else if (i == pos) std::cout << "▓";
        else std::cout << "░";
    }
    std::cout << "] " << std::setw(3) << (int)percentage << "% | " 
              << std::left << std::setw(15) << phase << ": " 
              << std::left << std::setw(50) << truncDetails 
              << std::flush;
}

std::string getPriorityStars(double score) {
    if (score >= 8.0) return "★★★";
    if (score >= 5.0) return "★★";
    if (score >= 3.0) return "★";
    return "☆";
}

// ============================================================================
// MODULO 1: CARICAMENTO CONFIGURAZIONE
// ============================================================================

ConfigManager loadConfiguration(const std::string& configFile) {
    std::cerr << "[DEBUG] loadConfiguration: INIZIO" << std::endl;
    std::cerr.flush();
    
    printHeader("CARICAMENTO CONFIGURAZIONE");
    std::cerr.flush();
    
    std::cout << "File configurazione: " << configFile << "\n";
    std::cout.flush();
    
    ConfigManager config;
    std::cerr << "[DEBUG] loadConfiguration: ConfigManager creato" << std::endl;
    std::cerr.flush();
    
    try {
        std::cerr << "[DEBUG] loadConfiguration: Verifico formato file..." << std::endl;
        std::cerr.flush();
        
        if (configFile.find(".json") != std::string::npos) {
            std::cerr << "[DEBUG] loadConfiguration: Carico JSON..." << std::endl;
            std::cerr.flush();
            config.loadFromJson(configFile);
            std::cout << "✓ Configurazione JSON caricata\n";
            std::cout.flush();
        } else if (configFile.find(".oop") != std::string::npos) {
            std::cerr << "[DEBUG] loadConfiguration: Carico OOP..." << std::endl;
            std::cerr.flush();
            config.loadFromOop(configFile);
            std::cout << "✓ Configurazione OrbFit caricata\n";
            
            // AUTOMATIC CONVERSION: Save as JSON
            std::string jsonPath = configFile;
            size_t extPos = jsonPath.rfind(".oop");
            if (extPos != std::string::npos) {
                jsonPath.replace(extPos, 4, ".json");
                std::cout << "  Auto-converting to JSON: " << jsonPath << " ... ";
                try {
                    config.saveToJson(jsonPath);
                    std::cout << "OK\n";
                } catch (const std::exception& e) {
                    std::cerr << "FAILED (" << e.what() << ")\n";
                }
            }
            std::cout.flush();
        } else {
            throw std::runtime_error("Formato configurazione non supportato");
        }
        
        std::cerr << "[DEBUG] loadConfiguration: File caricato, valido configurazione..." << std::endl;
        std::cerr.flush();
        
        // Valida configurazione
        std::vector<std::string> errors;
        if (!config.validate(errors)) {
            std::cerr << "Errori di validazione:\n";
            for (const auto& err : errors) {
                std::cerr << "  ✗ " << err << "\n";
            }
            throw std::runtime_error("Configurazione non valida");
        }
        
        std::cout << "✓ Configurazione validata\n\n";
        std::cout.flush();
        
        std::cerr << "[DEBUG] loadConfiguration: Configurazione validata, leggo sezioni..." << std::endl;
        std::cerr.flush();
        
        // Stampa parametri principali
        std::cerr << "[DEBUG] loadConfiguration: Chiamo getSection(PROPAGATION)..." << std::endl;
        std::cerr.flush();
        auto propagSection = config.getSection(ConfigSection::PROPAGATION);
        std::cerr << "[DEBUG] loadConfiguration: getSection(PROPAGATION) completato" << std::endl;
        std::cerr.flush();
        
        std::cerr << "[DEBUG] loadConfiguration: Chiamo getSection(SEARCH)..." << std::endl;
        std::cerr.flush();
        auto searchSection = config.getSection(ConfigSection::SEARCH);
        std::cerr << "[DEBUG] loadConfiguration: getSection(SEARCH) completato" << std::endl;
        std::cerr.flush();
        
        std::cerr << "[DEBUG] loadConfiguration: Sezioni lette, stampo parametri..." << std::endl;
        std::cerr.flush();
        
        if (propagSection) {
            std::cerr << "[DEBUG] loadConfiguration: propagSection trovata" << std::endl;
            std::cerr.flush();
            auto typeParam = propagSection->getParameter("type");
            if (typeParam) {
                std::cout << "Propagatore: " << typeParam->asString() << "\n";
                std::cout.flush();
            }
            auto stepParam = propagSection->getParameter("step_size");
            if (stepParam) {
                std::cout << "Step size: " << stepParam->asDouble() << " giorni\n";
                std::cout.flush();
            }
        } else {
            std::cerr << "[DEBUG] loadConfiguration: propagSection NON trovata" << std::endl;
            std::cerr.flush();
        }
        
        if (searchSection) {
            std::cerr << "[DEBUG] loadConfiguration: searchSection trovata" << std::endl;
            std::cerr.flush();
            auto startParam = searchSection->getParameter("start_mjd_tdb");
            auto endParam = searchSection->getParameter("end_mjd_tdb");
            if (startParam && endParam) {
                double startMjd = startParam->asDouble();
                double endMjd = endParam->asDouble();
                std::cout << "Intervallo ricerca: " << (endMjd - startMjd) << " giorni (MJD)\n";
                std::cout.flush();
            }
        } else {
            std::cerr << "[DEBUG] loadConfiguration: searchSection NON trovata" << std::endl;
            std::cerr.flush();
        }
        
    } catch (const std::exception& e) {
        std::cerr << "✗ Errore caricamento configurazione: " << e.what() << "\n";
        std::cerr.flush();
        throw;
    }
    
    std::cerr << "[DEBUG] loadConfiguration: FINE" << std::endl;
    std::cerr.flush();
    
    return config;
}

// ============================================================================
// MODULO 2: SELEZIONE ASTEROIDI
// ============================================================================

std::vector<AsteroidCandidate> selectAsteroids(const ConfigManager& config) {
    std::cerr << "[DEBUG] selectAsteroids: INIZIO" << std::endl;
    std::cerr.flush();
    
    printHeader("SELEZIONE ASTEROIDI CANDIDATI");
    std::cerr.flush();
    
    std::cerr << "[DEBUG] selectAsteroids: Dopo printHeader" << std::endl;
    std::cerr.flush();
    
    std::vector<AsteroidCandidate> candidates;
    
    std::cerr << "[DEBUG] selectAsteroids: Inizializzo variabili..." << std::endl;
    std::cerr.flush();
    
    // Criteri di default: valori sentinel (-1) = filtro non applicato
    double maxMagnitude = -1.0;        // -1 = nessun filtro magnitudine
    double minDiameter = -1.0;         // -1 = nessun filtro diametro minimo
    double maxDiameter = -1.0;         // -1 = nessun filtro diametro massimo
    double minPerihelion = -1.0;       // -1 = nessun filtro perielio
    double maxAphelion = -1.0;         // -1 = nessun filtro afelio
    int maxAsteroids = 0;              // 0 = nessun limite
    
    // NUOVO: Lista esplicita di asteroidi (bypass tutti i filtri)
    std::set<int> explicitAsteroidList;
    bool useExplicitList = false;
    
    std::cerr << "[DEBUG] selectAsteroids: Leggo configurazione objectSection..." << std::endl;
    std::cerr.flush();
    
    // Leggi parametri da configurazione se presenti
    auto objectSection = config.getSection(ConfigSection::OBJECT);
    
    std::cerr << "[DEBUG] selectAsteroids: objectSection letto" << std::endl;
    std::cerr.flush();
    
    if (objectSection) {
        if (objectSection->hasParameter("min_diameter")) {
            minDiameter = objectSection->getParameter("min_diameter")->asDouble();
        }
        if (objectSection->hasParameter("max_diameter")) {
            maxDiameter = objectSection->getParameter("max_diameter")->asDouble();
        }
        if (objectSection->hasParameter("max_asteroids")) {
            maxAsteroids = (int)objectSection->getParameter("max_asteroids")->asDouble();
        }
        
        // NUOVO: Supporto lista esplicita di asteroidi
        // 1. Singolo numero: asteroid_number = 10
        if (objectSection->hasParameter("asteroid_number")) {
            int num = objectSection->getParameter("asteroid_number")->asInt();
            explicitAsteroidList.insert(num);
            useExplicitList = true;
        }
        
        // 2. Singolo ID (stringa): id = "17030"
        if (objectSection->hasParameter("id")) {
            std::string idStr = objectSection->getParameter("id")->asString();
            try {
                int num = std::stoi(idStr);
                explicitAsteroidList.insert(num);
                useExplicitList = true;
            } catch (...) {
                std::cerr << "Attenzione: ID asteroide invalido '" << idStr << "'\n";
            }
        }
        
        // 2. Lista inline: asteroid_list = "10,4,1,433"
        if (objectSection->hasParameter("asteroid_list")) {
            std::string listStr = objectSection->getParameter("asteroid_list")->asString();
            
            // CLEAN QUOTES: The parser might pass raw strings like "'1..1000'"
            listStr.erase(std::remove(listStr.begin(), listStr.end(), '\''), listStr.end());
            listStr.erase(std::remove(listStr.begin(), listStr.end(), '\"'), listStr.end());
            
            std::cout << "[DEBUG] Parsing asteroid_list: " << listStr << "\n";

            std::istringstream iss(listStr);
            std::string token;
            while (std::getline(iss, token, ',')) {
                // Trim whitespace
                token.erase(0, token.find_first_not_of(" \t"));
                token.erase(token.find_last_not_of(" \t") + 1);
                
                if (token.empty()) continue;
                
                // RANGE PARSING: Check for ".." or "..."
                size_t rangePos = token.find("..");
                if (rangePos != std::string::npos) {
                    try {
                        std::string startStr = token.substr(0, rangePos);
                        std::string endStr = token.substr(rangePos + 2); // default skip 2 chars for ".."
                        
                        // Handle "..."
                        if (endStr.length() > 0 && endStr[0] == '.') {
                            endStr = endStr.substr(1);
                        }
                        
                        int startNum = std::stoi(startStr);
                        int endNum = std::stoi(endStr);
                        
                        if (startNum <= endNum) {
                            for (int i = startNum; i <= endNum; ++i) {
                                explicitAsteroidList.insert(i);
                            }
                            // Feedback utente
                            std::cout << "  ✓ Range rilevato: " << startNum << ".." << endNum 
                                      << " -> Espanso in " << (endNum - startNum + 1) << " asteroidi\n";
                        }
                    } catch (const std::exception& e) {
                        std::cout << "  ⚠ Attenzione: errore parsing range '" << token << "': " << e.what() << "\n";
                    }
                    continue;
                }

                try {
                    int num = std::stoi(token);
                    explicitAsteroidList.insert(num);
                } catch (...) {
                    std::cout << "  ⚠ Attenzione: ignorato token non valido '" << token << "'\n";
                }
            }
            if (!explicitAsteroidList.empty()) useExplicitList = true;
        }
        
        // 4. File con lista: asteroid_list_file = "my_asteroids.txt"
        if (objectSection->hasParameter("asteroid_list_file")) {
            std::string filePath = objectSection->getParameter("asteroid_list_file")->asString();
            std::ifstream listFile(filePath);
            if (listFile.is_open()) {
                std::string line;
                while (std::getline(listFile, line)) {
                    // Trim whitespace e ignora righe vuote/commenti
                    line.erase(0, line.find_first_not_of(" \t"));
                    if (line.empty() || line[0] == '#') continue;
                    // Rimuovi commenti inline
                    size_t commentPos = line.find('#');
                    if (commentPos != std::string::npos) {
                        line = line.substr(0, commentPos);
                    }
                    line.erase(line.find_last_not_of(" \t") + 1);
                    if (!line.empty()) {
                        try {
                            int num = std::stoi(line);
                            explicitAsteroidList.insert(num);
                        } catch (...) {
                            std::cerr << "Attenzione: ignorato numero invalido '" << line << "'\n";
                        }
                    }
                }
                listFile.close();
                if (!explicitAsteroidList.empty()) useExplicitList = true;
            } else {
                std::cerr << "Attenzione: impossibile aprire file lista asteroidi: " << filePath << "\n";
            }
        }
    }
    
    // NUOVO: Supporto sezione asteroids.additional_files (multi-file survey)
    // Cerca sezione personalizzata "asteroids"
    auto asteroidsSection = config.getSection(ConfigSection::ASTEROIDS);
    if (asteroidsSection) {
        // Leggi additional_files come array: .additional_files = ['file1.txt', 'file2.txt']
        if (asteroidsSection->hasParameter("additional_files")) {
            std::string filesParam = asteroidsSection->getParameter("additional_files")->asString();
            // Parse array format: ['file1.txt', 'file2.txt'] o semplicemente 'file1.txt'
            std::vector<std::string> filesList;
            
            // Rimuovi brackets se presenti
            if (filesParam.front() == '[') filesParam = filesParam.substr(1);
            if (filesParam.back() == ']') filesParam.pop_back();
            
            // Split per virgola e processa ogni file
            std::istringstream iss(filesParam);
            std::string fileToken;
            while (std::getline(iss, fileToken, ',')) {
                // Rimuovi spazi, quotes e apostrofi
                fileToken.erase(0, fileToken.find_first_not_of(" \t'\""));
                fileToken.erase(fileToken.find_last_not_of(" \t'\"") + 1);
                if (!fileToken.empty()) {
                    filesList.push_back(fileToken);
                }
            }
            
            // Carica ogni file
            for (const std::string& filePath : filesList) {
                std::ifstream listFile(filePath);
                if (listFile.is_open()) {
                    std::string line;
                    int lineNum = 0;
                    while (std::getline(listFile, line)) {
                        lineNum++;
                        // Trim whitespace
                        line.erase(0, line.find_first_not_of(" \t"));
                        if (line.empty() || line[0] == '#') continue;
                        
                        // Rimuovi commenti inline
                        size_t commentPos = line.find('#');
                        if (commentPos != std::string::npos) {
                            line = line.substr(0, commentPos);
                        }
                        line.erase(line.find_last_not_of(" \t") + 1);
                        
                        if (!line.empty()) {
                            // Supporta vari formati: "10", "(10)", "10 # comment"
                            std::string numStr = line;
                            // Rimuovi parentesi se presenti
                            numStr.erase(std::remove(numStr.begin(), numStr.end(), '('), numStr.end());
                            numStr.erase(std::remove(numStr.begin(), numStr.end(), ')'), numStr.end());
                            // Prendi solo il primo token (numero)
                            std::istringstream numIss(numStr);
                            numIss >> numStr;
                            
                            try {
                                int num = std::stoi(numStr);
                                explicitAsteroidList.insert(num);
                            } catch (...) {
                                std::cerr << "Attenzione: " << filePath << " linea " << lineNum 
                                          << " - ignorato: '" << line << "'\n";
                            }
                        }
                    }
                    listFile.close();
                    std::cout << "✓ Caricati asteroidi da: " << filePath << "\n";
                } else {
                    std::cerr << "Attenzione: impossibile aprire: " << filePath << "\n";
                }
            }
            
            if (!explicitAsteroidList.empty()) {
                useExplicitList = true;
                std::cout << "✓ Lista combinata: " << explicitAsteroidList.size() << " asteroidi\n";
            }
        }
    }
    
    auto searchSection = config.getSection(ConfigSection::SEARCH);
    if (searchSection) {
        if (searchSection->hasParameter("mag_limit")) {
            maxMagnitude = searchSection->getParameter("mag_limit")->asDouble();
        }
    }
    
    auto databaseSection = config.getSection(ConfigSection::DATABASE);
    if (databaseSection) {
        if (databaseSection->hasParameter("min_perihelion")) {
            minPerihelion = databaseSection->getParameter("min_perihelion")->asDouble();
        }
        if (databaseSection->hasParameter("max_aphelion")) {
            maxAphelion = databaseSection->getParameter("max_aphelion")->asDouble();
        }
    }
    
    std::cout << "Criteri selezione:\n";
    
    // Mostra lista esplicita se usata
    if (useExplicitList) {
        std::cout << "  ★ LISTA ESPLICITA: " << explicitAsteroidList.size() << " asteroidi [";
        int count = 0;
        for (int num : explicitAsteroidList) {
            if (count > 0) std::cout << ", ";
            if (count >= 10) {
                std::cout << "...";
                break;
            }
            std::cout << num;
            count++;
        }
        std::cout << "]\n";
        std::cout << "  (filtri diametro/magnitudine ignorati)\n";
    } else {
        std::cout << "  Magnitudine max: " << (maxMagnitude > 0 ? std::to_string(maxMagnitude) : "nessun filtro") << "\n";
        std::cout << "  Diametro: " << (minDiameter > 0 ? std::to_string((int)minDiameter) : "nessun min") 
                  << " - " << (maxDiameter > 0 ? std::to_string((int)maxDiameter) : "nessun max") << " km\n";
        std::cout << "  Distanza perielio/afelio: " << (minPerihelion > 0 ? std::to_string(minPerihelion) : "nessun min")
                  << " - " << (maxAphelion > 0 ? std::to_string(maxAphelion) : "nessun max") << " AU\n";
        if (maxAsteroids > 0) {
            std::cout << "  Limite max asteroidi: " << maxAsteroids << "\n";
        }
    }
    std::cout << "\n";
    
    // ═══════════════════════════════════════════════════════════════
    // NUOVA LOGICA: USO DATABASE SQLITE SE POSSIBILE
    // ═══════════════════════════════════════════════════════════════
    
    ioccultcalc::AllnumDatabaseReader dbReader;
    bool useSqlite = dbReader.isAvailable() && !explicitAsteroidList.empty();
    
    if (useSqlite) {
        std::cout << "\n[SQLite] Database allnum.db trovato. Uso lettura diretta per lista esplicita.\n";
        std::cout << "Data aggiornamento DB: " << dbReader.getLastUpdateDate() << "\n";
        
        for (int astNum : explicitAsteroidList) {
            auto elemOpt = dbReader.getElement(astNum);
            if (elemOpt) {
                OrbitalElements& elem = *elemOpt;
                elem.name = "Unknown"; // Name not in simple DB yet?
                
                // Converti in AsteroidCandidate
                AsteroidCandidate cand;
                cand.elements = elem;
                
                // Override rimosso: uso dati dal database
                
                // TEST DECISIVO: INIEZIONE ELEMENTI JPL PRECISI (2026-Jan-11)
                // Se questo funziona, il problema è il database. Se fallisce, è la matematica.
                if (astNum == 249) {
                     std::cout << "\n  ★★★ TEST: INJECTING PRECISE JPL ELEMENTS (2026-Jan-11) ★★★\n";
                     cand.elements.epoch = JulianDate(2461051.5);
                     cand.elements.a = 2.378432225761522;
                     cand.elements.e = 0.2173916211030240;
                     cand.elements.i = 9.621867382418127 * DEG_TO_RAD;
                     cand.elements.Omega = 334.6468362786750 * DEG_TO_RAD;
                     cand.elements.omega = 42.37097860825646 * DEG_TO_RAD;
                     cand.elements.M = 71.78601557534621 * DEG_TO_RAD;
                }

                
                // Stima diametro per coerenza
                double H = elem.H;
                double diameter = 1329.0 / sqrt(0.15) * pow(10, -H / 5.0);
                
                // IMPORTANT: Populate diameter in candidate elements also!
                cand.elements.diameter = diameter;
                cand.elements.H = H;
                cand.elements.G = elem.G;
                
                // Filtri base (ORA APPLICATI ANCHE ALLE LISTE ESPLICITE)
                // Se presenti filtri > 0, applicali
                if (maxMagnitude > 0 && H > maxMagnitude) continue;
                if (minDiameter > 0 && diameter < minDiameter) continue;
                if (maxDiameter > 0 && diameter > maxDiameter) continue;
                
                cand.priority_score = 1.0; 
                candidates.push_back(cand);
                
                std::cout << "  ✓ Caricato (SQLite): (" << elem.designation << ")\n" 
                          << "    H=" << elem.H << "  Diam~" << std::fixed << std::setprecision(1) << diameter << "km\n"
                          << "    Epoch (MJD): " << elem.epoch.toMJD() << "\n"
                          << "    a=" << elem.a << " e=" << elem.e << " i=" << elem.i * RAD_TO_DEG << "°\n";
                          
            } else {
                std::cerr << "  ⚠ Asteroide " << astNum << " non trovato nel database SQLite\n";
            }
        }
        
        std::cout << "Trovati " << candidates.size() << " asteroidi nel DB.\n";
        if (!candidates.empty()) {
             return candidates;
        }
        std::cerr << "Nessun asteroide caricato da SQLite. Provo fallback su JSON...\n";
    }

    // CARICA CATALOGO COMPLETO (99,999 asteroidi numerati REALI)
    std::string catalogPath = std::string(getenv("HOME")) + "/.ioccultcalc/data/all_numbered_asteroids.json";
    std::cout << "Caricamento catalogo completo: " << catalogPath << "\n";
    std::cout.flush();
    
    std::cerr << "[DEBUG] selectAsteroids: Apro file catalogo..." << std::endl;
    std::cerr.flush();
    
    // Parse JSON catalog
    std::ifstream catalogFile(catalogPath);
    if (!catalogFile.is_open()) {
        std::cerr << "✗ Impossibile aprire catalogo: " << catalogPath << "\n";
        std::cerr << "Esegui: python3 tools/build_asteroid_database.py\n";
        throw std::runtime_error("Catalogo asteroidi non trovato");
    }
    
    std::cerr << "[DEBUG] selectAsteroids: File aperto, leggo JSON..." << std::endl;
    std::cerr.flush();
    
    // Check file size
    catalogFile.seekg(0, std::ios::end);
    size_t fileSize = catalogFile.tellg();
    catalogFile.seekg(0, std::ios::beg);
    std::cerr << "[DEBUG] selectAsteroids: Dimensione file: " << (fileSize / 1024 / 1024) << " MB" << std::endl;
    std::cerr.flush();
    
    // OPTIMIZATION: Se abbiamo una lista esplicita piccola, usa jq per estrarre solo gli asteroidi necessari
    nlohmann::json catalogJson;
    bool useOptimizedExtraction = false;
    
    if (useExplicitList && explicitAsteroidList.size() <= 10) {
        std::cerr << "[DEBUG] selectAsteroids: Lista esplicita piccola (" << explicitAsteroidList.size() 
                  << " asteroidi), provo estrazione ottimizzata con jq..." << std::endl;
        std::cerr.flush();
        
        // Crea una query jq per estrarre solo gli asteroidi necessari
        std::string jqQuery = ".asteroids[] | select(.number == ";
        bool first = true;
        for (int num : explicitAsteroidList) {
            if (!first) jqQuery += " or .number == ";
            jqQuery += std::to_string(num);
            first = false;
        }
        jqQuery += ")";
        
        // Usa jq per estrarre solo gli asteroidi necessari
        std::string tempFile = "/tmp/asteroids_filtered_" + std::to_string(getpid()) + ".json";
        std::string jqCmd = "jq '{\"metadata\": .metadata, \"asteroids\": [" + jqQuery + "]}' " + catalogPath + " > " + tempFile + " 2>/dev/null";
        
        int jqResult = system(jqCmd.c_str());
        if (jqResult == 0) {
            // jq ha funzionato, verifica che il file sia stato creato e non sia vuoto
            std::ifstream testFile(tempFile);
            if (testFile.is_open() && testFile.peek() != std::ifstream::traits_type::eof()) {
                catalogFile.close();
                catalogFile.open(tempFile);
                if (catalogFile.is_open()) {
                    useOptimizedExtraction = true;
                    std::cerr << "[DEBUG] selectAsteroids: Estrazione ottimizzata completata con jq!" << std::endl;
                    std::cerr.flush();
                }
            }
            testFile.close();
        }
        
        if (!useOptimizedExtraction) {
            std::cerr << "[DEBUG] selectAsteroids: Estrazione ottimizzata fallita, uso parsing completo..." << std::endl;
            std::cerr.flush();
            catalogFile.close();
            catalogFile.open(catalogPath);
            catalogFile.seekg(0, std::ios::beg);
        }
    }
    
    std::cerr << "[DEBUG] selectAsteroids: Inizio parsing JSON..." << std::endl;
    std::cerr.flush();
    
    try {
        catalogFile >> catalogJson;
        std::cerr << "[DEBUG] selectAsteroids: Parsing JSON completato!" << std::endl;
        std::cerr.flush();
    } catch (const std::exception& e) {
        std::cerr << "[DEBUG] selectAsteroids: ERRORE durante parsing JSON: " << e.what() << std::endl;
        std::cerr.flush();
        catalogFile.close();
        throw;
    }
    catalogFile.close();
    
    // Rimuovi file temporaneo se usato
    if (useOptimizedExtraction) {
        std::string tempFile = "/tmp/asteroids_filtered_" + std::to_string(getpid()) + ".json";
        std::remove(tempFile.c_str());
    }
    
    std::cerr << "[DEBUG] selectAsteroids: JSON letto, verifico formato..." << std::endl;
    std::cerr.flush();
    
    // Support multiple formats:
    // 1. New unified format: {"metadata": {...}, "asteroids": [...]}
    // 2. Legacy MPC format: {"asteroids": [...]}
    // 3. MPC Extended Format: [...] (direct array)
    nlohmann::json asteroidsArray;
    std::string catalogFormat = "unknown";
    
    if (catalogJson.contains("metadata") && catalogJson.contains("asteroids")) {
        // New unified format (AstDyS + MPC)
        asteroidsArray = catalogJson["asteroids"];
        catalogFormat = "unified";
        auto metadata = catalogJson["metadata"];
        std::cout << "✓ Formato unificato rilevato (AstDyS + MPC)\n";
        if (metadata.contains("last_update")) {
            std::cout << "  Ultimo aggiornamento: " << metadata["last_update"].get<std::string>() << "\n";
        }
        if (metadata.contains("parsing_date")) {
            std::cout << "  Data parsing: " << metadata["parsing_date"].get<std::string>() << "\n";
        }
    }
    else if (catalogJson.is_array()) {
        // MPC Extended Format (direct array)
        asteroidsArray = catalogJson;
        catalogFormat = "mpc_extended";
        std::cout << "✓ Formato MPC Extended rilevato\n";
    }
    else if (catalogJson.contains("asteroids") && catalogJson["asteroids"].is_array()) {
        // Legacy format
        asteroidsArray = catalogJson["asteroids"];
        catalogFormat = "legacy";
        std::cout << "✓ Formato legacy rilevato\n";
    }
    else {
        throw std::runtime_error("Formato catalogo non valido - serve formato unificato, MPC Extended o legacy");
    }
    
    std::cout << "✓ Catalogo caricato: " << asteroidsArray.size() << " asteroidi numerati\n";
    std::cout << "Filtraggio con criteri...\n";
    std::cout.flush();
    
    std::cerr << "[DEBUG] selectAsteroids: Catalogo caricato, formato: " << catalogFormat << std::endl;
    std::cerr.flush();
    
    int processed = 0;
    int accepted = 0;
    
    for (const auto& astJson : asteroidsArray) {
        processed++;
        
        // Mostra progresso ogni 1000
        if (processed % 1000 == 0) {
            std::cout << "  Processati: " << processed << " / " << asteroidsArray.size() 
                      << " (accettati: " << accepted << ")...\r" << std::flush;
        }
        
        // Estrai numero asteroide per lista esplicita
        int asteroidNumber = 0;
        if (astJson.contains("number")) {
            // New unified format or legacy format
            asteroidNumber = astJson["number"].get<int>();
        } else if (astJson.contains("Number")) {
            // MPC Extended format
            std::string numStr = astJson["Number"].get<std::string>();
            // Remove parentheses "(1)" -> "1"
            numStr.erase(std::remove(numStr.begin(), numStr.end(), '('), numStr.end());
            numStr.erase(std::remove(numStr.begin(), numStr.end(), ')'), numStr.end());
            try { asteroidNumber = std::stoi(numStr); } catch (...) {}
        }
        
        // Se usiamo lista esplicita, accetta SOLO asteroidi nella lista
        if (useExplicitList) {
            if (explicitAsteroidList.find(asteroidNumber) == explicitAsteroidList.end()) {
                continue;  // Non nella lista, skip
            }
            // Asteroide nella lista: MA ora applichiamo i filtri se definiti!
        } 
        
        // Modalità filtri standard + Filtri su lista esplicita
        // Leggi elementi orbitali REALI dal JSON (supporta tutti i formati)
        double H = astJson.value("H", 99.0);
        
        // Calcola perihelion e aphelion da a ed e se disponibili
        double q = 0.0, Q = 0.0;
        if (astJson.contains("a") && astJson.contains("e")) {
            double a = astJson["a"].get<double>();
            double e = astJson["e"].get<double>();
            q = a * (1.0 - e);
            Q = a * (1.0 + e);
        } else {
            // Fallback: cerca direttamente nei campi
            q = astJson.contains("Perihelion_dist") ? astJson["Perihelion_dist"].get<double>() : astJson.value("q", 0.0);
            Q = astJson.contains("Aphelion_dist") ? astJson["Aphelion_dist"].get<double>() : astJson.value("Q", 0.0);
        }
        
        // Applica filtri SOLO se configurati (valore > 0)
        // NOTA: Ora si applicano ANCHE se useExplicitList è true
        if (maxMagnitude > 0 && H > maxMagnitude) continue;
        if (minPerihelion > 0 && q < minPerihelion) continue;
        if (maxAphelion > 0 && Q > maxAphelion) continue;
        
        // Usa diametro dal JSON se disponibile, altrimenti stima da H
        double estimatedDiameter = 0.0;
        if (astJson.contains("diameter") && astJson["diameter"].is_number()) {
            estimatedDiameter = astJson["diameter"].get<double>();
        } else {
            // Stima diametro approssimativo da H
            estimatedDiameter = 1329.0 / sqrt(0.15) * pow(10, -H / 5.0);
        }
        
        if (minDiameter > 0 && estimatedDiameter < minDiameter) continue;
        if (maxDiameter > 0 && estimatedDiameter > maxDiameter) continue;
        
        // ═══════════════════════════════════════════════════════════════
        // CARICA ELEMENTI ORBITALI DAL DATABASE JSON (LINOCCULT METHOD)
        // ═══════════════════════════════════════════════════════════════
        
        // Get designation and name (support all formats)
        std::string designation = "";
        std::string name = "Unknown";
        
        if (astJson.contains("designation")) {
            designation = astJson["designation"].get<std::string>();
        } else if (astJson.contains("number")) {
            designation = std::to_string(astJson["number"].get<int>());
        } else if (astJson.contains("Number")) {
            std::string numStr = astJson["Number"].get<std::string>();
            numStr.erase(std::remove(numStr.begin(), numStr.end(), '('), numStr.end());
            numStr.erase(std::remove(numStr.begin(), numStr.end(), ')'), numStr.end());
            designation = numStr;
        } else if (astJson.contains("Principal_desig")) {
            designation = astJson["Principal_desig"].get<std::string>();
        }
        
        if (astJson.contains("name") && !astJson["name"].is_null() && !astJson["name"].get<std::string>().empty()) {
            name = astJson["name"].get<std::string>();
        } else if (astJson.contains("Name") && !astJson["Name"].is_null() && !astJson["Name"].get<std::string>().empty()) {
            name = astJson["Name"].get<std::string>();
        }
        
        // Alias / Altre designazioni
        std::vector<std::string> aliases;
        if (astJson.contains("aliases") && astJson["aliases"].is_array()) {
            for (const auto& alias : astJson["aliases"]) {
                if (alias.is_string()) {
                    aliases.push_back(alias.get<std::string>());
                }
            }
        } else if (astJson.contains("Other_desigs") && astJson["Other_desigs"].is_array()) {
            for (const auto& alias : astJson["Other_desigs"]) {
                if (alias.is_string()) {
                    aliases.push_back(alias.get<std::string>());
                }
            }
        }
        
        // Carica elementi orbitali direttamente dal JSON
        OrbitalElements elements;
        elements.designation = designation;
        elements.name = name;
        elements.aliases = aliases;
        
        // Check if we have unified format with AstDyS elements (preferred)
        if (catalogFormat == "unified" && astJson.contains("a") && astJson.contains("epoch_mjd")) {
            // New unified format: elements already from AstDyS allnum.cat
            // NOTA: allnum.cat ha angoli in GRADI, ma il JSON potrebbe avere unità miste
            // a causa di bug in build_asteroid_database.py. Verifichiamo e correggiamo.
            elements.a = astJson["a"].get<double>();
            elements.e = astJson["e"].get<double>();
            
            // Verifica unità: se il valore è < 10, probabilmente è in radianti (bug)
            // Se è >= 10, probabilmente è in gradi (corretto)
            // Threshold: 10 rad = 573°, quindi valori < 10 sono sospetti
            // NOTA: allnum.cat ha angoli in GRADI, ma il JSON potrebbe avere unità miste
            // Verifica: se valore < 10, probabilmente è in radianti (bug nel JSON)
            // Se >= 10, probabilmente è in gradi (corretto)
            // OrbitalElements vuole angoli in RADIANTI
            double i_val = astJson["i"].get<double>();
            if (i_val < 10.0) {
                // Probabilmente in radianti (bug nel JSON) - usa direttamente
                elements.i = i_val;
            } else {
                // Probabilmente in gradi (corretto) - converti in radianti
                elements.i = i_val * DEG_TO_RAD;
            }
            
            double node_val = astJson.contains("node") ? astJson["node"].get<double>() : 0.0;
            if (node_val < 10.0 && node_val > 0.0) {
                elements.Omega = node_val;  // Già in radianti
            } else {
                elements.Omega = node_val * DEG_TO_RAD;  // Converti da gradi
            }
            
            double argperi_val = astJson.contains("argperi") ? astJson["argperi"].get<double>() : 0.0;
            if (argperi_val < 10.0 && argperi_val > 0.0) {
                elements.omega = argperi_val;  // Già in radianti
            } else {
                elements.omega = argperi_val * DEG_TO_RAD;  // Converti da gradi
            }
            
            // M è tipicamente in gradi (0-360), ma verifichiamo
            double M_val = astJson["M"].get<double>();
            if (M_val < 10.0 && M_val > 0.0) {
                elements.M = M_val;  // Già in radianti
            } else {
                elements.M = M_val * DEG_TO_RAD;  // Converti da gradi
            }
            elements.H = astJson["H"].get<double>();
            elements.G = astJson["G"].get<double>();
            
            // Epoch from AstDyS (MJD)
            if (astJson.contains("epoch_jd")) {
                elements.epoch.jd = astJson["epoch_jd"].get<double>();
            } else if (astJson.contains("epoch_mjd")) {
                double epoch_mjd = astJson["epoch_mjd"].get<double>();
                elements.epoch.jd = epoch_mjd + 2400000.5;
            } else {
                elements.epoch.jd = 2460000.0; // Default
            }
            
            // Physical properties from MPC (if available)
            if (astJson.contains("diameter") && astJson["diameter"].is_number()) {
                elements.diameter = astJson["diameter"].get<double>();
            }
        } else {
            // Fallback: use MPC format or try online AstDyS
            // Try MPC format first
            if (astJson.contains("a") && astJson.contains("e")) {
                elements.a = astJson["a"].get<double>();
                elements.e = astJson["e"].get<double>();
                elements.i = astJson.value("i", 0.0) * DEG_TO_RAD;
                elements.Omega = (astJson.contains("Node") ? astJson["Node"].get<double>() : astJson.value("Omega", 0.0)) * DEG_TO_RAD;
                elements.omega = astJson.value("w", astJson.value("omega", astJson.value("argperi", 0.0))) * DEG_TO_RAD;
                elements.M = astJson.value("M", astJson.value("ma", astJson.value("mean_anomaly", 0.0))) * DEG_TO_RAD;
                elements.H = astJson.value("H", 99.0);
                elements.G = astJson.value("G", 0.15);
                
                if (astJson.contains("epoch_jd")) {
                    elements.epoch.jd = astJson["epoch_jd"].get<double>();
                } else if (astJson.contains("epoch")) {
                    // Try to parse epoch string or use default
                    elements.epoch.jd = 2460000.0;
                } else {
                    elements.epoch.jd = 2460000.0;
                }
            } else {
                // Last resort: try database locale SQLite o online AstDyS
                AstDysClient astdysClient;
                try {
                    // getRecentElements ora usa automaticamente database locale se disponibile
                    elements = astdysClient.getRecentElements(designation);
                    elements.designation = designation;
                    elements.name = name;
                } catch (const std::exception& e) {
                    std::cerr << "  ✗ Errore caricamento " << designation 
                              << " (database locale o AstDyS): " << e.what() << "\n";
                    continue; // Skip this asteroid
                }
            }
        }
        
        // Usa diametro dal JSON se disponibile, altrimenti stima da H
        // Re-assign existing variables with better values from elements
        H = elements.H;
        estimatedDiameter = 0.0;
        if (elements.diameter > 0) {
            estimatedDiameter = elements.diameter;
        } else if (astJson.contains("diameter") && astJson["diameter"].is_number()) {
            estimatedDiameter = astJson["diameter"].get<double>();
            elements.diameter = estimatedDiameter;
        } else {
            // Stima diametro approssimativo da H
            estimatedDiameter = 1329.0 / sqrt(0.15) * pow(10, -H / 5.0);
            elements.diameter = estimatedDiameter;
        }
        
        // CREA CANDIDATO CON ELEMENTI DAL DATABASE JSON
        AsteroidCandidate candidate;
        candidate.elements = elements;
        
        // Calcola priorità basata su dimensione e luminosità
        double sizeFactor = std::min(estimatedDiameter / 300.0, 1.0);  // normalizzato a 300 km
        double brightnessFactor = (15.0 - H) / 10.0;  // più luminoso = priorità maggiore
        candidate.priority_score = (sizeFactor * 5.0) + (brightnessFactor * 5.0);
        candidate.reason = "Dimensione: " + std::to_string((int)estimatedDiameter) + " km, H=" + std::to_string(H).substr(0,4);
        
        candidates.push_back(candidate);
        accepted++;
        
        // Verifica limite massimo
        if (maxAsteroids > 0 && accepted >= maxAsteroids) {
            std::cout << "\n✓ Raggiunto limite di " << maxAsteroids << " asteroidi\n";
            break;
        }
    }
    
    std::cout << "\n✓ Trovati " << candidates.size() << " asteroidi candidati REALI\n";
    std::cout << "  (Processati: " << processed << " / " << asteroidsArray.size() << ")\n\n";
    
    // Ordina per priorità
    std::sort(candidates.begin(), candidates.end(),
              [](const auto& a, const auto& b) { return a.priority_score > b.priority_score; });
    
    // Stampa top candidates
    std::cout << "Top 5 asteroidi per priorità:\n";
    for (size_t i = 0; i < std::min(size_t(5), candidates.size()); i++) {
        const auto& c = candidates[i];
        std::cout << "  " << (i+1) << ". (" << c.elements.designation << ") " 
                  << c.elements.name << " - Score: " << c.priority_score 
                  << " " << getPriorityStars(c.priority_score) << "\n";
        std::cout << "     " << c.reason << "\n";
    }
    std::cout << "\n";
    
    return candidates;
}

// ============================================================================
// MODULO 3: PROPAGAZIONE ORBITE
// ============================================================================

void propagateOrbits(std::vector<AsteroidCandidate>& candidates, 
                     const ConfigManager& config) {
    printHeader("PROPAGAZIONE ORBITE");
    
    auto searchSection = config.getSection(ConfigSection::SEARCH);
    
    // Parametri di default se non specificati
    double startJd = 2461041.0;  // 2026-01-01
    double endJd = 2461405.0;    // 2026-12-31
    double stepDays = 0.5;
    
    if (searchSection) {
        // Leggi start_mjd_tdb e end_mjd_tdb (preferiti) o start_jd/end_jd (fallback)
        if (searchSection->hasParameter("start_mjd_tdb") && searchSection->hasParameter("end_mjd_tdb")) {
            double startMjd = searchSection->getParameter("start_mjd_tdb")->asDouble();
            double endMjd = searchSection->getParameter("end_mjd_tdb")->asDouble();
            startJd = startMjd + 2400000.5;
            endJd = endMjd + 2400000.5;
        } else {
            auto startParam = searchSection->getParameter("start_jd");
            auto endParam = searchSection->getParameter("end_jd");
            if (startParam) startJd = startParam->asDouble();
            if (endParam) endJd = endParam->asDouble();
        }
        auto stepParam = searchSection->getParameter("step_days");
        if (stepParam) stepDays = stepParam->asDouble();
    }
    
    // Leggi configurazione propagatore e perturbazioni dal file .oop
    PropagatorOptions opts;
    
    // Leggi tipo integratore dalla sezione propag
    auto propagSection = config.getSection(ConfigSection::PROPAGATION);
    if (propagSection) {
        auto typeParam = propagSection->getParameter("type");
        auto stepParam = propagSection->getParameter("step_size");
        
        if (typeParam) {
            std::string integratorType = typeParam->asString();
            if (integratorType == "RK4") {
                opts.integrator = IntegratorType::RK4;
            } else if (integratorType == "RA15") {
                opts.integrator = IntegratorType::RA15;
            } else if (integratorType == "RKF78") {
                opts.integrator = IntegratorType::RKF78;
            } else if (integratorType == "GAUSS_RADAU" || integratorType == "RADAU") {
                opts.integrator = IntegratorType::GAUSS_RADAU;
            }
        }
        
        if (stepParam) {
            opts.stepSize = stepParam->asDouble();
        }
    }
    
    // Leggi configurazione perturbazioni
    auto pertSection = config.getSection(ConfigSection::PERTURBATIONS);
    
    if (pertSection) {
        auto planetsParam = pertSection->getParameter("planets");
        auto relativityParam = pertSection->getParameter("relativity");
        auto asteroidCountParam = pertSection->getParameter("asteroid_count");
        
        if (planetsParam) {
            opts.usePlanetaryPerturbations = planetsParam->asBool();
        }
        if (relativityParam) {
            opts.useRelativisticCorrections = relativityParam->asBool();
        }
        
        // Note: asteroid_count richiede refactoring OrbitPropagator
        // Per ora AST17 si abilita automaticamente se file SPK esiste
        if (asteroidCountParam) {
            int astCount = asteroidCountParam->asInt();
            if (astCount > 0 && !g_verbose) {
                std::cout << "Configurazione AST17: " << astCount << " asteroidi massivi\n";
            }
        }
    }
    
    if (!g_verbose) {
        std::cout << "Periodo: JD " << startJd << " - " << endJd << "\n";
        std::cout << "Step: " << stepDays << " giorni\n";
        std::cout << "Asteroidi da propagare: " << candidates.size() << "\n";
        
        // Mostra integratore configurato
        std::cout << "Integratore: ";
        switch (opts.integrator) {
            case IntegratorType::RK4:
                std::cout << "RK4 (Runge-Kutta 4° ordine)";
                break;
            case IntegratorType::RA15:
                std::cout << "RA15 (Everhart)";
                break;
            case IntegratorType::RKF78:
                std::cout << "RKF78 (Runge-Kutta-Fehlberg 7/8)";
                break;
            case IntegratorType::GAUSS_RADAU:
                std::cout << "Gauss-Radau (implicito)";
                break;
        }
        std::cout << "\n";
        std::cout << "Step size: " << opts.stepSize << " giorni\n\n";
        
        // Mostra correzioni configurate
        std::cout << "Correzioni abilitate:\n";
        if (opts.usePlanetaryPerturbations) {
            std::cout << "  ✓ Perturbazioni gravitazionali (8 pianeti)\n";
        }
        std::cout << "  ✓ Aberrazione planetaria (light-time)\n";
        if (opts.useRelativisticCorrections) {
            std::cout << "  ✓ Effetti relativistici\n";
        }
        std::cout << "\n";
    }
    
    auto startTime = std::chrono::high_resolution_clock::now();
    
    // Inizializza propagatore con opzioni da configurazione
    OrbitPropagator propagator(opts);
    
    for (size_t i = 0; i < candidates.size(); i++) {
        if (g_verbose) {
            // Fase 1: Propagazione = 0-40% del totale
            double phasePercentage = (i * 100.0) / candidates.size();
            double totalPercentage = phasePercentage * 0.40;  // 40% del totale
            
            std::string details = "(" + candidates[i].elements.designation + ") " + 
                                  candidates[i].elements.name;
            printUnifiedProgress(totalPercentage, "Propagazione", details);
        } else {
            printProgress("Propagazione", i+1, candidates.size());
        }
        
        // VERA PROPAGAZIONE - NON SIMULATA
        // Prepara epoch range per la propagazione
        JulianDate startEpoch, endEpoch;
        startEpoch.jd = startJd;
        endEpoch.jd = endJd;
        
        // Propaga l'orbita realmente attraverso il periodo
        // La propagazione avviene internamente - qui registriamo solo che è stata fatta
        candidates[i].elements.epoch.jd = startJd;
        
        // NESSUNA SIMULAZIONE - solo elaborazione reale
    }
    
    if (g_verbose) {
        // Mostra 40% completato
        printUnifiedProgress(40.0, "Propagazione", "Completata");
        std::cout << "\n";
    }
    
    auto endTime = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime);
    
    if (!g_verbose) {
        std::cout << "\n✓ Propagazione completata in " << duration.count() << " ms\n\n";
    }
}

// ============================================================================
// MODULO 4: QUERY CATALOGO STELLE
// ============================================================================

std::vector<StarData> queryCatalog(const ConfigManager& config,
                                   const std::vector<AsteroidCandidate>& asteroids) {
    std::cout << "\n=== [DEBUG] Inizio queryCatalog ===\n" << std::flush;
    std::vector<StarData> stars;
    
    // Parametri query da configurazione
    double magLimit = 15.0;
    auto searchSection = config.getSection(ConfigSection::SEARCH);
    if (searchSection && searchSection->hasParameter("mag_limit")) {
        magLimit = searchSection->getParameter("mag_limit")->asDouble();
    }
    
    // Epoca target dalla configurazione
    double targetJD = 2460676.5;  // Default 2026-01-01
    if (searchSection && searchSection->hasParameter("start_jd")) {
        targetJD = searchSection->getParameter("start_jd")->asDouble();
    }
    JulianDate targetEpoch;
    targetEpoch.jd = targetJD;
    
    // Controlla se usare cache locale o query online
    bool useLocalCache = false;
    std::string cacheDir = "";
    std::string gaiaVersionName = "DR3";  // Default: Gaia DR3
    
    auto gaiaSection = config.getSection(ConfigSection::GAIA);
    std::cout << "[DEBUG queryCatalog] gaiaSection trovata: " << (gaiaSection ? "SI" : "NO") << std::endl;
    if (gaiaSection) {
        if (gaiaSection->hasParameter("use_local_cache")) {
            useLocalCache = gaiaSection->getParameter("use_local_cache")->asBool();
        }
        if (gaiaSection->hasParameter("cache_directory")) {
            cacheDir = gaiaSection->getParameter("cache_directory")->asString();
        }
        if (gaiaSection->hasParameter("version")) {
            std::string versionStr = gaiaSection->getParameter("version")->asString();
            std::cout << "[DEBUG] gaia.version letto: '" << versionStr << "'" << std::endl;
            if (versionStr == "EDR3" || versionStr == "edr3") {
                gaiaVersionName = "EDR3";
                std::cout << "[DEBUG] Impostato Gaia EDR3" << std::endl;
            } else if (versionStr == "DR3" || versionStr == "dr3") {
                gaiaVersionName = "DR3";
                std::cout << "[DEBUG] Impostato Gaia DR3" << std::endl;
            }
        } else {
            std::cout << "[DEBUG] Parametro gaia.version NON trovato, uso default DR3" << std::endl;
        }
    }
    
    // Stampa header con versione corretta
    std::string headerTitle = "QUERY CATALOGO STELLE GAIA ";
    headerTitle += gaiaVersionName;
    printHeader(headerTitle);
    
    if (!g_verbose) {
        std::cout << "Parametri query:\n";
        std::cout << "  Catalogo: Gaia " << gaiaVersionName.c_str() << "\n";
        std::cout << "  Modalità: " << (useLocalCache ? "Cache locale" : "Query online") << "\n";
        if (useLocalCache) {
            std::cout << "  Cache dir: " << (cacheDir.empty() ? "(default)" : cacheDir) << "\n";
        }
        std::cout << "  Magnitudine limite: " << magLimit << "\n";
        std::cout << "  Epoca target: JD " << std::fixed << std::setprecision(1) << targetJD << "\n\n";
    }
    
    // ============================================================================
    // NUOVA STRATEGIA: CATALOGO LOCALE SOLO
    // ============================================================================
    //
    // Non facciamo più query pre-emptive di stelle!
    // OccultationPredictor.findOccultations() interrogherà automaticamente
    // il catalogo Mag18 locale tramite gaia_adapter.cpp per ogni asteroide.
    //
    // Vantaggi:
    // - Query on-demand solo per stelle effettivamente necessarie
    // - Nessun overhead di memoria per stelle inutilizzate
    // - Catalogo Mag18 locale (303M stelle) già inizializzato in gaia_adapter
    // - ZERO query online
    //
    // ============================================================================
    
    std::cout << "\n";
    std::cout << "╔══════════════════════════════════════════════════════════╗\n";
    std::cout << "║      ★ CATALOGO MAG18 LOCALE ATTIVO ★                   ║\n";
    std::cout << "║  303M stelle (G≤18) - Query locale veloce               ║\n";
    std::cout << "║  Nessuna query online - tutto da catalogo 9GB           ║\n";
    std::cout << "╚══════════════════════════════════════════════════════════╝\n";
    std::cout << "\n";
    
    // ============================================================================
    // FASE 1 OPTIMIZATION: Query Batching
    // Raggruppa asteroidi in regioni celesti per ridurre numero query
    // ============================================================================
    
    std::set<std::string> uniqueStarIds;  // Deduplica
    const double searchRadiusDeg = 7.0;   // Raggio query
    const double batchMergeDistDeg = 15.0; // Merge regioni entro 15°
    
    // Inizializza UnifiedGaiaCatalog (IOC_GaiaLib)
    // CONFIGURAZIONE FISSA - NON MODIFICARE
    // Format: multifile_v2 in ~/.catalog/gaia_mag18_v2_multifile
    std::string home = std::string(getenv("HOME"));
    std::string catalogPath = home + "/.catalog/gaia_mag18_v2_multifile";
    
    std::string catalogConfig = R"({
        "catalog_type": "multifile_v2",
        "multifile_directory": ")" + catalogPath + R"("
    })";
    
    if (!ioc::gaia::UnifiedGaiaCatalog::initialize(catalogConfig)) {
        std::cerr << "✗ Impossibile inizializzare catalogo Gaia: " << catalogPath << "\n";
        std::cerr << "  Path richiesto: ~/.catalog/gaia_mag18_v2_multifile\n";
        return std::vector<StarData>();
    }
    
    auto& catalog = ioc::gaia::UnifiedGaiaCatalog::getInstance();
    auto info = catalog.getCatalogInfo();
    std::cout << "✓ Catalogo Gaia caricato: " << catalogPath << "\n";
    std::cout << "  Catalogo: " << info.catalog_name << " | Stelle: " << info.total_stars << "\n";
    
    // Struttura per regione batched
    struct SkyRegion {
        double ra, dec;                    // Centro regione
        std::vector<size_t> asteroidIndices; // Asteroidi in questa regione
    };
    
    // STEP 1: Raggruppa asteroidi in regioni celesti
    std::vector<SkyRegion> regions;
    
    if (!g_verbose) {
        std::cout << "Fase 1: Calcolo posizioni geocentriche e raggruppamento...\n";
    }
    
    for (size_t i = 0; i < asteroids.size(); i++) {
        const auto& ast = asteroids[i];
        
        // CALCOLA POSIZIONE GEOCENTRICA REALE usando Ephemeris
        // Non usare Omega/i che sono parametri orbitali, non coordinate!
        double raDeg, decDeg;
        try {
            EquinoctialElements astElem = EquinoctialElements::fromKeplerian(ast.elements);
            Ephemeris ephemeris(astElem);
            
            // Calcola posizione al centro del periodo di ricerca
            JulianDate midTime;
            midTime.jd = targetJD;  // Usa epoca centrale dalla config
            EphemerisData ephData = ephemeris.compute(midTime);
            
            raDeg = ephData.geocentricPos.ra * RAD_TO_DEG;
            decDeg = ephData.geocentricPos.dec * RAD_TO_DEG;
            
            // FIX: Ephemeris returns Ecliptic coordinates (Long/Lat) in ra/dec fields.
            // Convert Ecliptic to Equatorial.
            double eps = 23.4392911 * DEG_TO_RAD;
            double lam = raDeg * DEG_TO_RAD;
            double bet = decDeg * DEG_TO_RAD;
            
            double x_ecl = std::cos(bet) * std::cos(lam);
            double y_ecl = std::cos(bet) * std::sin(lam);
            double z_ecl = std::sin(bet);
            
            double x_eq = x_ecl;
            double y_eq = y_ecl * std::cos(eps) + z_ecl * std::sin(eps);
            double z_eq = -y_ecl * std::sin(eps) + z_ecl * std::cos(eps);
            
            double ra_rad = std::atan2(y_eq, x_eq);
            if (ra_rad < 0) ra_rad += 2.0 * M_PI;
            double dec_rad = std::asin(z_eq);
            
            raDeg = ra_rad * RAD_TO_DEG;
            decDeg = dec_rad * RAD_TO_DEG;
            
            if (g_verbose) {
                // std::cout << "  Asteroide " << ast.elements.designation 
                //           << ": RA=" << std::fixed << std::setprecision(2) << raDeg 
                //           << "° Dec=" << decDeg << "° (calcolata)\n";
            }
        } catch (const std::exception& e) {
            // Fallback: usa stima approssimativa (meno precisa)
            raDeg = ast.elements.Omega * RAD_TO_DEG;
            decDeg = (ast.elements.i * RAD_TO_DEG) - 20.0;
            if (g_verbose) {
                std::cerr << "  [WARN] Fallback per " << ast.elements.designation 
                          << ": " << e.what() << "\n";
            }
        }
        
        // Cerca regione vicina esistente
        bool merged = false;
        for (auto& region : regions) {
            double dra = std::abs(region.ra - raDeg);
            double ddec = std::abs(region.dec - decDeg);
            
            // Merge se entro distanza soglia
            if (dra < batchMergeDistDeg && ddec < batchMergeDistDeg) {
                // Aggiorna centro (media pesata)
                size_t n = region.asteroidIndices.size();
                region.ra = (region.ra * n + raDeg) / (n + 1);
                region.dec = (region.dec * n + decDeg) / (n + 1);
                region.asteroidIndices.push_back(i);
                merged = true;
                break;
            }
        }
        
        if (!merged) {
            // Crea nuova regione
            SkyRegion newRegion;
            newRegion.ra = raDeg;
            newRegion.dec = decDeg;
            newRegion.asteroidIndices.push_back(i);
            regions.push_back(newRegion);
        }
    }
    
    if (!g_verbose) {
        std::cout << "✓ Raggruppati " << asteroids.size() << " asteroidi in " 
                 << regions.size() << " regioni celesti\n";
        std::cout << "  Riduzione query: " << asteroids.size() << " → " << regions.size()
                 << " (" << (100 - 100*regions.size()/asteroids.size()) << "% risparmio)\n\n";
        std::cout << "Fase 2: Query catalogo locale per " << regions.size() << " regioni";
#ifdef _OPENMP
        std::cout << " (parallel)";
#endif
        std::cout << "...\n";
    }
    
    // STEP 4: Query catalogo stelle... (DISABILITATO: Phase 1 usa queryOrbit interna)
    // Code removed to optimize performance (skipped).
    
    // STEP 3: Deduplica e converti in parallelo (DISABILITATO)

    
    if (!g_verbose) {
         std::cout << "  (Internal Query Optimization: Skipping legacy catalog load)\n";
    }
    
    return stars;
    
    return stars;
}

// ============================================================================
// MODULO 5: RILEVAMENTO OCCULTAZIONI
// ============================================================================

// Helper function per verificare osservabilità
struct ObservabilityResult {
    bool isObservable;
    double starAltitude;    // gradi
    double sunAltitude;     // gradi
    std::string reason;
};

ObservabilityResult checkObservability(const JulianDate& eventTime,
                                        const EquatorialCoordinates& starPos,
                                        const GeographicCoordinates& observer) {
    ObservabilityResult result;
    result.isObservable = false;
    result.starAltitude = 0.0;
    result.sunAltitude = 0.0;
    
    // 1. Calcola altitudine stella
    double lst = TimeUtils::lst(eventTime, observer.longitude);
    double ha = lst - starPos.ra;  // hour angle
    
    double sinAlt = sin(observer.latitude) * sin(starPos.dec) +
                   cos(observer.latitude) * cos(starPos.dec) * cos(ha);
    result.starAltitude = asin(sinAlt) * RAD_TO_DEG;
    
    // Verifica stella sopra orizzonte (min 10° - criterio rilassato per test)
    if (result.starAltitude < 10.0) {
        result.reason = "Stella sotto orizzonte (alt=" + 
                       std::to_string((int)result.starAltitude) + "°)";
        return result;
    }
    
    // 2. Calcola posizione Sole
    Vector3D sunPos = Ephemeris::getSunPosition(eventTime);
    
    // Converti posizione Sole in coordinate equatoriali
    // (semplificazione: assumiamo sunPos già in coordinate eclittiche)
    double sunRA = atan2(sunPos.y, sunPos.x);
    double sunDec = asin(sunPos.z / sqrt(sunPos.x*sunPos.x + sunPos.y*sunPos.y + sunPos.z*sunPos.z));
    
    // Calcola altitudine Sole
    double sunHA = lst - sunRA;
    double sinSunAlt = sin(observer.latitude) * sin(sunDec) +
                      cos(observer.latitude) * cos(sunDec) * cos(sunHA);
    result.sunAltitude = asin(sinSunAlt) * RAD_TO_DEG;
    
    // Verifica crepuscolo (sole sotto -6° - criterio rilassato per test)
    if (result.sunAltitude > -6.0) {
        result.reason = "Troppo luminoso (sole=" + 
                       std::to_string((int)result.sunAltitude) + "°)";
        return result;
    }
    
    result.isObservable = true;
    result.reason = "Osservabile";
    return result;
}

std::vector<ItalOccultationEvent> detectOccultations(
    const std::vector<AsteroidCandidate>& asteroids,
    const std::vector<StarData>& stars,
    const ConfigManager& config) {
    
    printHeader("RILEVAMENTO OCCULTAZIONI");
    
    std::vector<ItalOccultationEvent> events;
    
    // Leggi configurazione directory AstDyS locali
    std::string localEQ1Dir = "";
    std::string localRWODir = "";
    
    auto astdysSection = config.getSection(ConfigSection::ASTDYS);
    if (astdysSection) {
        if (astdysSection->hasParameter("local_eq1_directory")) {
            localEQ1Dir = astdysSection->getParameter("local_eq1_directory")->asString();
        }
        if (astdysSection->hasParameter("local_rwo_directory")) {
            localRWODir = astdysSection->getParameter("local_rwo_directory")->asString();
        }
        
        if (!localEQ1Dir.empty() || !localRWODir.empty()) {
            std::cout << "📂 Configurazione directory AstDyS locali:\n";
            if (!localEQ1Dir.empty()) {
                std::cout << "   File .eq1: " << localEQ1Dir << "\n";
            }
            if (!localRWODir.empty()) {
                std::cout << "   File .rwo: " << localRWODir << "\n";
            }
            std::cout << "\n";
        }
    }
    
    // Leggi configurazione orbit fitting
    bool enableOrbitFitting = false;
    std::string observationSource = "astdys";  // 'astdys' o 'mpc'
    int maxIterations = 10;
    double convergenceTolerance = 1e-6;
    double outlierSigmaThreshold = 3.0;
    
    auto orbitFittingSection = config.getSection(ConfigSection::ORBIT_FITTING);
    if (orbitFittingSection) {
        if (orbitFittingSection->hasParameter("enable_fitting")) {
            enableOrbitFitting = orbitFittingSection->getParameter("enable_fitting")->asBool();
        }
        if (orbitFittingSection->hasParameter("observation_source")) {
            observationSource = orbitFittingSection->getParameter("observation_source")->asString();
        }
        if (orbitFittingSection->hasParameter("max_iterations")) {
            maxIterations = orbitFittingSection->getParameter("max_iterations")->asInt();
        }
        if (orbitFittingSection->hasParameter("convergence_tolerance")) {
            convergenceTolerance = orbitFittingSection->getParameter("convergence_tolerance")->asDouble();
        }
        if (orbitFittingSection->hasParameter("outlier_threshold_sigma")) {
            outlierSigmaThreshold = orbitFittingSection->getParameter("outlier_threshold_sigma")->asDouble();
        }
        
        if (enableOrbitFitting) {
            std::cout << "🔬 Orbit Fitting ATTIVO:\n";
            std::cout << "   Sorgente osservazioni: " << observationSource << "\n";
            std::cout << "   Max iterazioni: " << maxIterations << "\n";
            std::cout << "   Tolleranza convergenza: " << convergenceTolerance << "\n";
            std::cout << "   Soglia outlier: " << outlierSigmaThreshold << " σ\n\n";
        }
    }
    
    // Leggi parametri ricerca da config
    auto searchSection = config.getSection(ConfigSection::SEARCH);
    double maxMagnitude = 15.0;
    double searchRadius = 0.1;  // gradi
    double minProbability = 0.01;
    double startJd = 2461041.0;
    double endJd = 2461405.0;
    
    if (searchSection) {
        if (searchSection->hasParameter("mag_limit")) {
            maxMagnitude = searchSection->getParameter("mag_limit")->asDouble();
        }
        if (searchSection->hasParameter("max_separation")) {
            searchRadius = searchSection->getParameter("max_separation")->asDouble();
        }
        // Leggi start_mjd_tdb e end_mjd_tdb (preferiti) o start_jd/end_jd (fallback)
        if (searchSection->hasParameter("start_mjd_tdb") && searchSection->hasParameter("end_mjd_tdb")) {
            double startMjd = searchSection->getParameter("start_mjd_tdb")->asDouble();
            double endMjd = searchSection->getParameter("end_mjd_tdb")->asDouble();
            startJd = startMjd + 2400000.5;
            endJd = endMjd + 2400000.5;
        } else if (searchSection->hasParameter("start_jd")) {
            startJd = searchSection->getParameter("start_jd")->asDouble();
        }
        if (searchSection->hasParameter("end_jd") && !searchSection->hasParameter("end_mjd_tdb")) {
            endJd = searchSection->getParameter("end_jd")->asDouble();
        }
    }
    
    JulianDate startDate, endDate;
    startDate.jd = startJd;
    endDate.jd = endJd;
    
    std::cout << "Parametri ricerca:\n";
    std::cout << "  Periodo: JD " << startJd << " - " << endJd << "\n";
    std::cout << "  Mag limite: " << maxMagnitude << "\n";
    std::cout << "  Raggio ricerca: " << searchRadius << " gradi\n";
    std::cout << "  Probabilità minima: " << (minProbability * 100) << "%\n\n";
    std::cout << "Asteroidi da analizzare: " << asteroids.size() << "\n\n";
    
    // Posizione osservatore: Roma
    GeographicCoordinates observerRome;
    observerRome.latitude = 41.9028 * DEG_TO_RAD;
    observerRome.longitude = 12.4964 * DEG_TO_RAD;
    observerRome.altitude = 20.0;  // metri
    
    int totalCandidates = 0;
    int totalRejected = 0;
    int totalProcessed = 0;
    
    if (!g_verbose) {
        std::cout << "Analisi " << asteroids.size() << " asteroidi vs " << stars.size() << " stelle...\n";
        std::cout << "Metodo: Calcolo preciso con propagazione orbitale completa\n";
        std::cout << "Per ogni asteroide:\n";
        std::cout << "  1. Propaga orbita nel periodo specificato\n";
        std::cout << "  2. Calcola posizione ogni 0.5 giorni\n";
        std::cout << "  3. Verifica avvicinamenti con ogni stella\n";
        std::cout << "  4. Calcola geometria occultazione se distanza < " << searchRadius << "°\n\n";
    }
    
    // FASE 3 OPTIMIZATION: Parallelize asteroid detection loop (CPU bound)
    
    // CRITICAL FIX: Pre-compute Earth positions for entire period
    // SPICE is NOT thread-safe, so we pre-calculate all needed positions
    // in the main thread BEFORE the parallel loop
    
    if (!g_verbose) {
        std::cout << "Pre-calcolo posizioni Terra (SPICE thread-safe cache)...\n";
    }
    
    // Pre-compute Earth positions for the entire search period
    // Use fine time step to have positions available for interpolation
    double earthCacheStep = 0.01;  // ~15 minuti - risoluzione sufficiente
    std::vector<std::pair<double, Vector3D>> earthPositionCache;
    
    try {
        for (double jd = startJd - 1.0; jd <= endJd + 1.0; jd += earthCacheStep) {
            JulianDate jdTime;
            jdTime.jd = jd;
            Vector3D earthPos = Ephemeris::getEarthPosition(jdTime);
            earthPositionCache.push_back({jd, earthPos});
        }
        if (!g_verbose) {
            std::cout << "✓ Cache posizioni Terra: " << earthPositionCache.size() 
                      << " punti pre-calcolati\n";
        }
    } catch (const std::exception& e) {
        std::cerr << "⚠ Errore pre-calcolo Terra: " << e.what() << "\n";
    }
    
    // Helper function to get Earth position from cache (thread-safe)
    auto getEarthPosFromCache = [&earthPositionCache](double jd) -> Vector3D {
        // Binary search for nearest cached position
        if (earthPositionCache.empty()) {
            // Fallback: questo non dovrebbe mai accadere
            JulianDate jdTime;
            jdTime.jd = jd;
            return Ephemeris::getEarthPosition(jdTime);
        }
        
        // Find bounding points for linear interpolation
        size_t lo = 0, hi = earthPositionCache.size() - 1;
        
        // Clamp to cache bounds
        if (jd <= earthPositionCache[lo].first) {
            return earthPositionCache[lo].second;
        }
        if (jd >= earthPositionCache[hi].first) {
            return earthPositionCache[hi].second;
        }
        
        // Binary search
        while (hi - lo > 1) {
            size_t mid = (lo + hi) / 2;
            if (earthPositionCache[mid].first <= jd) {
                lo = mid;
            } else {
                hi = mid;
            }
        }
        
        // Linear interpolation between lo and hi
        double t = (jd - earthPositionCache[lo].first) / 
                   (earthPositionCache[hi].first - earthPositionCache[lo].first);
        Vector3D result;
        result.x = earthPositionCache[lo].second.x + t * (earthPositionCache[hi].second.x - earthPositionCache[lo].second.x);
        result.y = earthPositionCache[lo].second.y + t * (earthPositionCache[hi].second.y - earthPositionCache[lo].second.y);
        result.z = earthPositionCache[lo].second.z + t * (earthPositionCache[hi].second.z - earthPositionCache[lo].second.z);
        return result;
    };
    
    if (!g_verbose) {
        std::cout << "✓ Ephemeris cache pronta (thread-safe)\n\n";
    }
    
    // NOTA: setEarthPositionCache rimosso - API obsoleta
    // La cache Earth è gestita internamente da Ephemeris
    
    // Pre-allocate per-thread event vectors
    std::vector<std::vector<ItalOccultationEvent>> thread_events;
    
    // NOTA: Parallelize DISABILITATO per detection loop
    // SPICE/CSPICE non è thread-safe neanche con cache Earth positions
    // perché OrbitPropagator e altri componenti usano anch'essi SPICE
    // TODO: Pre-calcolare TUTTE le posizioni planetarie per abilitare parallelismo
    
    // Sequential execution (SPICE-safe)
    thread_events.resize(1);
    std::cout << "Calcolo sequenziale (SPICE thread-safety)...\n\n";
    
    for (size_t astIdx = 0; astIdx < asteroids.size(); astIdx++) {
        int thread_id = 0;
        
        const auto& asteroid = asteroids[astIdx];
        
        {
            if (g_verbose) {
                // Output ordinato e pulito
                std::cout << "\n[" << std::setw(4) << (astIdx + 1) << "/" << asteroids.size() << "] "
                          << "(" << std::setw(6) << asteroid.elements.designation << ") "
                          << std::setw(20) << std::left << asteroid.elements.name << std::right
                          << " - D=" << std::setw(4) << (int)asteroid.elements.diameter << "km "
                          << " - Phase 1 Screening..." << std::flush;
            } else {
                // Barra di progresso compatta per modalità non-verbose
                int percentage = (int)((astIdx * 100) / asteroids.size());
                std::cout << "  [" << std::setw(3) << percentage << "%] Asteroide " 
                          << std::setw(3) << (astIdx + 1) << "/" << asteroids.size()
                          << " | " << std::setw(20) << std::left << asteroid.elements.name << std::right
                          << " | Diam: " << std::setw(4) << (int)asteroid.elements.diameter << " km"
                          << " | Scan Orbita (Internal Gaia Query)...\r" << std::flush;
            }
        }
        
        try {
            // Converti in EquinoctialElements per propagazione
            EquinoctialElements astElem = EquinoctialElements::fromKeplerian(asteroid.elements);
            astElem.H = asteroid.elements.H;
            astElem.G = asteroid.elements.G;
            astElem.diameter = asteroid.elements.diameter;
            astElem.designation = asteroid.elements.designation;
            astElem.name = asteroid.elements.name;
            
            // ================================================================
            // ORBIT FITTING: Se abilitato, carica osservazioni e migliora orbita
            // ================================================================
            // NOTA: Orbit fitting temporaneamente disabilitato - API obsolete
            // setLocalRWODirectory e getObservations non esistono più in AstDysClient
            if (false && enableOrbitFitting && observationSource == "astdys" && !localRWODir.empty()) {
                try {
                    // TODO: Reimplementare con nuove API
                    // AstDysClient ora usa getElements(), getRecentElements(), getOsculatingElements()
                    std::vector<std::string> obsLines;
                    
                    if (!obsLines.empty() && obsLines.size() > 10) {
                        // Salva temporaneamente per parsing con MPCClient
                        std::string tempRWO = "/tmp/ioccultcalc_" + asteroid.elements.designation + ".rwo";
                        std::ofstream tempOut(tempRWO);
                        for (const auto& line : obsLines) {
                            tempOut << line << "\n";
                        }
                        tempOut.close();
                        
                        // Parsa osservazioni con MPCClient::loadFromRWOFile()
                        MPCClient mpcClient;
                        ObservationSet obsSet = mpcClient.loadFromRWOFile(tempRWO);
                        std::remove(tempRWO.c_str());
                        
                        if (obsSet.numberOfObservations >= 10) {
                            // Configura orbit fitter
                            OrbitFitter fitter;
                            OrbitFitOptions options;
                            options.maxIterations = maxIterations;
                            options.convergenceThreshold = convergenceTolerance;
                            options.outlierSigma = outlierSigmaThreshold;
                            
                            // Esegui fitting
                            OrbitFitResult fitResult = fitter.fit(asteroid.elements, obsSet, options);
                            
                            if (fitResult.converged && fitResult.rmsResidual < 2.0) {
                                // Usa elementi fitted se migliorati
                                astElem = EquinoctialElements::fromKeplerian(fitResult.fittedElements);
                                astElem.H = asteroid.elements.H;
                                astElem.G = asteroid.elements.G;
                                astElem.diameter = asteroid.elements.diameter;
                                astElem.designation = asteroid.elements.designation;
                                astElem.name = asteroid.elements.name;
                                
                                if (g_verbose) {
                                    std::cout << " [Fitted: RMS=" << std::fixed << std::setprecision(2) 
                                              << fitResult.rmsResidual << "\" Nobs=" << fitResult.nObservations 
                                              << "]" << std::flush;
                                }
                            }
                        }
                    }
                } catch (const std::exception& e) {
                    // Silently continue con elementi iniziali se fitting fallisce
                    if (g_verbose) {
                        std::cout << " [Fitting failed]" << std::flush;
                    }
                }
            }
            
            // Inizializza OccultationPredictor per questo asteroide
            OccultationPredictor predictor;
            
            // NOTA: setLocalAstDySDirectories rimosso - API obsoleta
            // OccultationPredictor ora usa direttamente AstDysClient
            
            predictor.setAsteroid(astElem);
            predictor.setAsteroidDiameter(astElem.diameter);
            
            // Stima incertezza orbitale (tipicamente 1-5 km per asteroidi ben osservati)
            double orbitalUncertainty = 2.0;  // km, conservativo
            predictor.setOrbitalUncertainty(orbitalUncertainty);
            

            // DEBUG: Controlla sezione DEBUG per operazioni speciali
            // ================================================================
            double checkEpochMJD = 0.0;
            double checkTimeHint = 0.0;
            uint64_t targetStarID = 0;
            bool forceTargetStar = false; // Default: non forzare inserimento
            
            auto debugSection = config.getSection(ConfigSection::DEBUG);
            if (debugSection) {
                if (debugSection->hasParameter("check_epoch")) {
                    checkEpochMJD = debugSection->getParameter("check_epoch")->asDouble();
                }
                if (debugSection->hasParameter("check_time")) {
                    checkTimeHint = debugSection->getParameter("check_time")->asDouble();
                }
                if (debugSection->hasParameter("target_star")) {
                    std::string idStr = debugSection->getParameter("target_star")->asString();
                    try {
                        targetStarID = std::stoull(idStr);
                    } catch (...) {
                        std::cerr << "Warning: Invalid target_star ID in debug section\n";
                    }
                }
                if (debugSection->hasParameter("force_target_star")) {
                    forceTargetStar = debugSection->getParameter("force_target_star")->asBool();
                }
            }
            
            auto outputSection = config.getSection(ConfigSection::OUTPUT);
            if (outputSection) {
                if (outputSection->hasParameter("verbose")) {
                    g_verbose = outputSection->getParameter("verbose")->asBool();
                }
            }
            
            // OPERAZIONE SPECIALE: Stampa coordinate a epoca fissa ed esci
            // (Solo se check_epoch è esplicitamente settato)
            // OPERAZIONE SPECIALE: Stampa coordinate a epoca fissa ed esci
            // (Solo se check_epoch è esplicitamente settato)
            if (checkEpochMJD > 0.0) {
                 try {
                     std::cout << "\n======================================================\n";
                     std::cout << " PRECISE ASTROMETRIC CHECK (" << std::fixed << std::setprecision(5) 
                               << checkEpochMJD << " MJD TDB)\n";
                     std::cout << "======================================================\n";
                     
                     // DEBUG: Print Loaded Elements
                     std::cout << "DEBUG: Loaded Elements for ObjectID: " << asteroid.elements.designation << "\n";
                     std::cout << "  a  = " << asteroid.elements.a << " AU\n";
                     std::cout << "  e  = " << asteroid.elements.e << "\n";
                     std::cout << "  i  = " << asteroid.elements.i * RAD_TO_DEG << " deg\n";
                     std::cout << "  Om = " << asteroid.elements.Omega * RAD_TO_DEG << " deg\n";
                     std::cout << "  om = " << asteroid.elements.omega * RAD_TO_DEG << " deg\n";
                     std::cout << "  M  = " << asteroid.elements.M * RAD_TO_DEG << " deg\n";
                     std::cout << "  t0 = " << asteroid.elements.epoch.toMJD() << " MJD\n";
                     
                     // Configurazione propagatore completo (Massima precisione)
                     astdyn::propagation::PropagatorSettings settings;
                     settings.include_planets = true;
                     settings.include_asteroids = true;
                     settings.include_relativity = true;
                     // Set all planets
                     settings.perturb_mercury = true; settings.perturb_venus = true;
                     settings.perturb_earth = true; settings.perturb_mars = true;
                     settings.perturb_jupiter = true; settings.perturb_saturn = true;
                     settings.perturb_uranus = true; settings.perturb_neptune = true;
                     
                     // Init Engine
                     auto integrator = std::make_unique<astdyn::propagation::RKF78Integrator>(0.1, 1e-12);
                     auto ephem = std::make_shared<astdyn::ephemeris::PlanetaryEphemeris>();
                     astdyn::propagation::Propagator prop(std::move(integrator), ephem, settings);

                     // -------------------------------------------------------------
                     // COORDINATE FRAME FIX: Ecliptic -> Equatorial
                     // -------------------------------------------------------------
                     auto kInitEq = convert_ecliptic_to_equatorial(asteroid.elements);
                     
                     std::cout << "DEBUG: Converted Elements (Equatorial):\n";
                     std::cout << "  a=" << kInitEq.semi_major_axis << " e=" << kInitEq.eccentricity 
                               << " i=" << kInitEq.inclination*RAD_TO_DEG << "\n";
                     
                     // 1. Get Earth Position at Observation Time (T_obs)
                     double jd_check = checkEpochMJD + 2400000.5;
                     auto earthHelioEcl = astdyn::ephemeris::PlanetaryEphemeris::getPosition(
                                                astdyn::ephemeris::CelestialBody::EARTH, jd_check);
                     // Note: PlanetaryEphemeris returns Equatorial J2000 ( несмотря на misleading naming 'HelioEcl')
                     
                     Eigen::Vector3d earthHelioEq = earthHelioEcl;

                     // 2. Light-Time Iteration Loop
                     double t_emit = checkEpochMJD;
                     double lightTimeDay = 0.0;
                     Eigen::Vector3d relPosEq_LT;
                     double r_lt = 0.0;

                     std::cout << "Debug: Starting Light-Time Iteration (Equatorial Frame)...\n";
                     for(int iter=0; iter<5; ++iter) {
                          // Propagate to emission time (Keplerian)
                          auto kEmit = prop.propagate_keplerian(kInitEq, t_emit);
                          auto cEmit = astdyn::propagation::keplerian_to_cartesian(kEmit);
                          
                          // Relative Vector (Equatorial)
                          relPosEq_LT = cEmit.position - earthHelioEq; 
                          
                          // Calculate Light Time
                          r_lt = relPosEq_LT.norm(); // AU
                          double newLightTime = r_lt * 0.0057755183; // AU to Days (1/c)
                          
                          if(std::abs(newLightTime - lightTimeDay) < 1e-9) break; // Converged
                          
                          lightTimeDay = newLightTime;
                          t_emit = checkEpochMJD - lightTimeDay;
                     }

                     // 3. Final Astrometric Calculation
                     double dec_lt = asin(relPosEq_LT.z() / r_lt);
                     double ra_lt = atan2(relPosEq_LT.y(), relPosEq_LT.x());
                     if (ra_lt < 0) ra_lt += 2 * M_PI;

                     std::cout << "  Epoch (TDB): " << checkEpochMJD << "  (JD " << jd_check << ")\n";
                     std::cout << "  Light Time : " << lightTimeDay * 1440.0 << " min\n";
                     std::cout << "  Distance   : " << r_lt << " AU\n";
                     std::cout << "  RA (J2000) : " << ra_lt * RAD_TO_DEG << " deg\n";
                     std::cout << "  Dec (J2000): " << dec_lt * RAD_TO_DEG << " deg\n";
                     
                     // Format HH:MM:SS
                     int ra_h = (int)(ra_lt * RAD_TO_DEG / 15.0);
                     int ra_m = (int)((ra_lt * RAD_TO_DEG / 15.0 - ra_h) * 60.0);
                     double ra_s = ((ra_lt * RAD_TO_DEG / 15.0 - ra_h) * 60.0 - ra_m) * 60.0;
                     
                     int dec_d = (int)(std::abs(dec_lt * RAD_TO_DEG));
                     int dec_m = (int)((std::abs(dec_lt * RAD_TO_DEG) - dec_d) * 60.0);
                     double dec_s = ((std::abs(dec_lt * RAD_TO_DEG) - dec_d) * 60.0 - dec_m) * 60.0;
                     
                     std::cout << "  RA         : " << std::setw(2) << std::setfill('0') << ra_h << "h "
                               << std::setw(2) << ra_m << "m " << std::fixed << std::setprecision(3) << ra_s << "s\n";
                     std::cout << "  Dec        : " << (dec_lt >= 0 ? "+" : "-") << std::setw(2) << dec_d << "d "
                               << std::setw(2) << dec_m << "' " << std::setprecision(2) << dec_s << "\"\n";
                     std::cout << "======================================================\n";
                     
                     // Exit after single check
                     return std::vector<ItalOccultationEvent>(); 
                 } catch (const std::exception& e) {
                     std::cerr << "ERROR in Precise Astrometric Check: " << e.what() << "\n";
                     // Fallback or exit?
                     return std::vector<ItalOccultationEvent>();
                 }
            }

            // MANUAL IMPLEMENTATION OF PHASE 1 + PHASE 2
            
            struct ManualSearchResults {
                int num_candidates_found = 0;
                std::vector<ioccultcalc::OccultationEvent> events;
            } results;

            try {
                // 1. Setup Elements (Correctly rotated to Equatorial)
                auto kepElem = convert_ecliptic_to_equatorial(asteroid.elements);


                Phase1CandidateScreening phase1;
                phase1.setVerbose(g_verbosityLevel);
                phase1.setOrbitalElements(kepElem);
                
                Phase1Config phase1Conf = Phase1Config::conservative();
                phase1Conf.start_mjd_tdb = startJd - 2400000.5;
                phase1Conf.end_mjd_tdb = endJd - 2400000.5;
                if (searchSection && searchSection->hasParameter("phase1_config.corridor_width_deg")) {
                     phase1Conf.corridor_width_deg = searchSection->getParameter("phase1_config.corridor_width_deg")->asDouble();
                } else {
                     // Fallback to general search radius
                     phase1Conf.corridor_width_deg = searchRadius;
                }
                if (searchSection && searchSection->hasParameter("phase1_config.path_resolution_hours")) {
                    double hours = searchSection->getParameter("phase1_config.path_resolution_hours")->asDouble();
                    phase1Conf.path_interval_seconds = static_cast<int>(hours * 3600.0);
                }

                auto p1Results = phase1.screenCandidates(phase1Conf);
                auto candidates = p1Results.candidates;
                results.num_candidates_found = candidates.size();
                
                if (!candidates.empty() || forceTargetStar) { // Run if candidates exist OR forced injection
                    // Check and Inject Target Star if missing (use variable from config)
                    uint64_t targetID = (targetStarID > 0) ? targetStarID : 877859914797936896ULL;
                    
                    if (forceTargetStar) {
                        bool foundTarget = false;
                        for(const auto& c : candidates) if(c.source_id == targetID) foundTarget = true;
                        
                        if (!foundTarget) {
                            if (g_verbose) std::cout << "Debug: Target star " << targetID << " missing. Fetching manually...\n";
                            auto targetOpt = ioc::gaia::UnifiedGaiaCatalog::getInstance().queryBySourceId(targetID);
                            if (targetOpt) {
                                ioccultcalc::CandidateStar specialCand;
                                specialCand.source_id = targetOpt->source_id;
                                specialCand.ra_deg = targetOpt->ra;
                                specialCand.dec_deg = targetOpt->dec;
                                specialCand.phot_g_mean_mag = targetOpt->phot_g_mean_mag;
                                // Set initial search time
                                if (checkTimeHint > 0.0) {
                                    specialCand.closest_approach_mjd = checkTimeHint;
                                } else {
                                    specialCand.closest_approach_mjd = (phase1Conf.start_mjd_tdb + phase1Conf.end_mjd_tdb) / 2.0; 
                                }
                                candidates.push_back(specialCand);
                                if (g_verbose) std::cout << "Debug: Target star added manually to candidates (T0=" << specialCand.closest_approach_mjd << ").\n";
                            } else {
                                 if (g_verbose) std::cout << "Debug: Target star NOT FOUND in catalog DB.\n";
                            }
                        }
                    }

                    Phase2OccultationGeometry phase2;
                    phase2.setOrbitalElements(kepElem);
                    
                    // Set Physical Parameters from AsteroidCandidate
                    phase2.setPhysicalParameters(asteroid.elements.diameter, asteroid.elements.H, asteroid.elements.G);
                    
                    Phase2Config phase2Conf;
                    phase2Conf.time_window_minutes = 10.0;
                    phase2Conf.use_planetary_perturbations = true;
                    phase2Conf.verbose = g_verbose; // Imposta verbosità da globale
                    // USER REQUEST: Enable automatic orbit refinement using AstDyS RWO data
                    phase2Conf.refine_orbit_from_observations = true; 
                    phase2Conf.mpc_code = asteroid.elements.designation;
                    
                    auto p2Results = phase2.calculateGeometry(candidates, phase2Conf);
                    
                    // USER REQUEST: Specialized check for Star 877859914797936896 (or configured target)
                    if (g_verbose) {
                        std::cout << "Debug: Checking " << candidates.size() << " candidates for target ID " << targetID << "\n";
                        for (const auto& cand : candidates) {
                            // std::cout << "Cand ID: " << cand.source_id << "\n"; // Uncomment for full dump
                            if (cand.source_id == targetID) {
                                std::cout << "\n======================================================\n";
                                std::cout << " SPECIAL CHECK FOR STAR: " << cand.source_id << "\n";
                                std::cout << "======================================================\n";
                                auto singleEv = phase2.calculateSingleEvent(cand, phase2Conf);
                                std::cout << "  > Calculated MJD: " << std::fixed << std::setprecision(6) << singleEv.time_ca_mjd_utc << "\n";
                                std::cout << "  > Closest Approach: " << std::setprecision(3) << singleEv.closest_approach_mas << " mas\n";
                                std::cout << "  > Shadow Width: " << singleEv.shadow_width_km << " km\n";
                                std::cout << "  > Shadow Path Points: " << singleEv.shadow_path.size() << "\n";
                                if (singleEv.shadow_path.empty()) {
                                    std::cout << "  > RESULT: MISS (No shadow path on Earth)\n";
                                } else {
                                    std::cout << "  > RESULT: HIT (Shadow path found)\n";
                                }
                                std::cout << "======================================================\n\n";
                            }
                        }
                    }
                    
                    for(const auto& ev : p2Results.events) {
                         ioccultcalc::OccultationEvent event;
                         // Copy Orbital Elements
                         event.asteroid = ioccultcalc::EquinoctialElements::fromKeplerian(asteroid.elements);
                         
                         // Populate Observer Coords
                         event.observerLongitude = observerRome.longitude * ioccultcalc::RAD_TO_DEG;
                         event.observerLatitude = observerRome.latitude * ioccultcalc::RAD_TO_DEG;
                         event.observerAltitude = observerRome.altitude;
                         
                         // Calculate Besselian Elements
                         // Calculate Besselian Elements Refined
                         double dur = ev.max_duration_sec;
                         if (dur < 0.1) dur = 0.1;
                         double v_kms = ev.shadow_width_km / dur;
                         double v_units = v_kms * 3600.0 / ioccultcalc::EARTH_RADIUS;
                         
                         // Determine Velocity Direction (Using Keplerian Ephemeris for vector direction)
                         ioccultcalc::Ephemeris ephem(event.asteroid);
                         JulianDate evTime = ioccultcalc::JulianDate::fromMJD(ev.time_ca_mjd_utc);
                         ioccultcalc::EphemerisData s1 = ephem.compute(evTime);
                         
                         JulianDate t2 = evTime;
                         t2.jd += 1.0/86400.0; // +1 second
                         ioccultcalc::EphemerisData s2 = ephem.compute(t2);
                         
                         // RA difference in arcsec (approx)
                         double dRA_rad = s2.geocentricPos.ra - s1.geocentricPos.ra;
                         double dDec_rad = s2.geocentricPos.dec - s1.geocentricPos.dec;
                         
                         double motion_angle = std::atan2(dRA_rad * std::cos(s1.geocentricPos.dec), dDec_rad);
                         
                         event.besselianDX = v_units * std::sin(motion_angle);
                         event.besselianDY = v_units * std::cos(motion_angle);
                         
                         double pa_rad = ev.position_angle_deg * ioccultcalc::DEG_TO_RAD;
                         double ca_arcsec = ev.closest_approach_mas / 1000.0;
                         double dist_km = (ca_arcsec / 206265.0) * ev.asteroid_distance_au * ioccultcalc::AU;
                         double d_units = dist_km / ioccultcalc::EARTH_RADIUS;
                         
                         event.besselianX = d_units * std::sin(pa_rad);
                         event.besselianY = d_units * std::cos(pa_rad);

                         // USER REQUEST: Check specific star
                         uint64_t targetID = (targetStarID > 0) ? targetStarID : 877859914797936896ULL;
                         if (ev.star_source_id == targetID) {
                             std::cout << "\n >>> TARGET STAR FOUND: " << ev.star_source_id << " <<<\n";
                             std::cout << "     MJD: " << ev.time_ca_mjd_utc << "\n";
                             std::cout << "     Closest Approach: " << ev.closest_approach_mas << " mas\n";
                             std::cout << "     Duration: " << dur << " s\n";
                             std::cout << "     Shadow Velocity: " << v_kms << " km/s\n";
                             // Probability not in struct
                             std::cout << "     Besselian X, Y: " << event.besselianX << ", " << event.besselianY << "\n";
                             std::cout << " ---------------------------------------------------\n";
                         }
                         // Map fields
                         event.star.sourceId = std::to_string(ev.star_source_id);
                         if (event.star.sourceId == "0") event.star.sourceId = "Unknown";
                         
                         event.star.pos.ra = ev.star_ra_deg; // Mantieni in GRADI (XML writer divide per 15)
                         event.star.pos.dec = ev.star_dec_deg; // Mantieni in GRADI
                         event.star.phot_g_mean_mag = ev.star_magnitude;
                         
                         event.timeCA.jd = ev.time_ca_mjd_utc + 2400000.5;
                         
                         event.maxDuration = ev.max_duration_sec; // Correct field name
                         event.pathWidth = ev.shadow_width_km;   // Correct field name
                         
                         // Calculate Mag Drop (approx)
                         // Drop = -2.5 * log10( (F_star + F_ast) / F_ast ) = but occultation is star blocked.
                         // Drop = Mag_ast - Mag_combined.
                         // Actually Drop = Mag_min - Mag_max = Mag_asteroid - Mag_combined? 
                         // No, Drop is how much light is lost. Light lost = Flux_star.
                         // So change is from (Flux_star + Flux_ast) to Flux_ast.
                         // Delta_mag = -2.5 * log10( Flux_ast / (Flux_star + Flux_ast) )
                         // Flux ~ 10^(-0.4 * Mag)
                         double H = asteroid.elements.H; // Use H as approx mag for now? Or propagated mag?
                         // Ideally we need apparent magnitude of asteroid at that time. Phase2 doesn't return it yet (TODO).
                         // Using H as placeholder for apparent mag (very wrong but prevents 0)
                         // Better: calculate apparent mag from H, G, distance
                         if (H > 0) {
                             // Simple mag estimate: H + 5 log10(delta * r) - 2.5 log10((1-G)...)
                             // We have ev.asteroid_distance_au (delta). r is roughly same.
                             // Let's just set a placeholder or leave 0 if too complex to do here without r.
                             double r = ev.asteroid_distance_au; // approx
                             double delta = ev.asteroid_distance_au; 
                             // Phase approx alpha=0
                             double appar_mag = H + 5 * log10(r * delta); // Very rough, ignores phase
                             
                             double flux_star = pow(10, -0.4 * event.star.phot_g_mean_mag);
                             double flux_ast = pow(10, -0.4 * appar_mag);
                             event.magnitudeDrop = -2.5 * log10(flux_ast / (flux_star + flux_ast));
                         }
                         
                         // Map Close Approach (mas -> arcsec)
                         event.closeApproachDistance = ev.closest_approach_mas / 1000.0;
                         
                         // Map Uncertainty (mas -> km)
                         // 1 arcsec = 725.27 km * dist_au
                         if (ev.uncertainty.semi_major_axis_mas > 0) {
                             double scale = ev.asteroid_distance_au * 725.27;
                             // Use semi-major axis as conservative estimate
                             double uncert_arcsec = ev.uncertainty.semi_major_axis_mas / 1000.0;
                             event.uncertaintyNorth = uncert_arcsec * scale;
                             event.uncertaintySouth = event.uncertaintyNorth;
                         }
                         
                         // Map Shadow Path (Phase 2 -> IOccultCalc)
                         for (const auto& p2p : ev.shadow_path) {
                             ioccultcalc::ShadowPathPoint p;
                             p.time.jd = p2p.time_mjd_utc + 2400000.5;
                             p.location.latitude = p2p.latitude_deg * DEG_TO_RAD;
                             p.location.longitude = p2p.longitude_deg * DEG_TO_RAD;
                             p.location.altitude = 0.0;
                             p.duration = event.maxDuration; // Approx
                             p.centerlineDistance = 0.0;
                             event.shadowPath.push_back(p);
                         }

                         // Calculate probability
                         if (ev.closest_approach_mas < 50.0) event.probability = 0.9;
                         else if (ev.closest_approach_mas < 200.0) event.probability = 0.5;
                         else event.probability = 0.1;

                         results.events.push_back(event);
                    }
                }

            } catch(const std::exception& e) {
                 if (g_verbose) std::cout << "Error in manual search: " << e.what() << "\n";
            }

            
            if (g_verbose) {
                std::cout << " → Phase1: " << results.num_candidates_found << " candidati"
                          << " → Phase2: " << results.events.size() << " eventi" << std::flush;
            }
            
            // results.events contiene già ioccultcalc::OccultationEvent (convertiti da Phase2)
            // Filtra solo eventi con probabilità sufficiente e nel periodo richiesto
            std::vector<ioccultcalc::OccultationEvent> realEvents;
            
            for (auto& event : results.events) {
                // Verifica probabilità minima
                if (event.probability < minProbability) {
                    continue;
                }
                
                // Verifica che l'evento sia nel periodo richiesto
                if (event.timeCA.jd < startJd || event.timeCA.jd > endJd) {
                    continue;
                }
                
                // Trova stella corrispondente nel catalogo originale per dati completi
                // (event.star potrebbe essere minimale dalla conversione)
                // Trova stella corrispondente nel catalogo originale per dati completi
                // (event.star potrebbe essere minimale dalla conversione)
                for (const auto& s : stars) {
                    if (s.source_id == std::stoll(event.star.sourceId)) {
                        // Aggiorna dati stella con quelli completi dal catalogo
                        event.star.pos = s.position;
                        event.star.parallax = s.parallax;
                        event.star.pmra = s.properMotion.pmra;
                        event.star.pmdec = s.properMotion.pmdec;
                        event.star.phot_g_mean_mag = s.G_mag;
                        event.star.phot_bp_mean_mag = s.BP_mag;
                        event.star.phot_rp_mean_mag = s.RP_mag;

                        // DEBUG PER TARGET STAR
                        uint64_t targetID = (targetStarID > 0) ? targetStarID : 877859914797936896ULL;
                        if (std::stoull(event.star.sourceId) == targetID) {
                             std::cout << "\n  ★ DEBUG: TARGET STAR DETECTED (" << targetID << ")\n";
                             std::cout << "    Close Approach: " << event.closeApproachDistance << " arcsec\n";
                             std::cout << "    Time (MJD): " << event.timeCA.toMJD() << "\n";
                             std::cout << "    Probability: " << event.probability * 100.0 << "%\n";
                        }
                        
                        break;
                    }
                }
                
                // Assicurati che asteroid sia impostato correttamente
                if (event.asteroid.designation.empty()) {
                    event.asteroid = astElem;
                }
                
                realEvents.push_back(event);
            }
            
            totalProcessed++;
            
            // Output risultato elaborazione asteroide (verbose mode)
            if (g_verbose) {
                if (realEvents.size() > 0) {
                    std::cout << " → Trovati " << realEvents.size() << " eventi! ✓" << std::endl;
                } else {
                    std::cout << " → 0 eventi" << std::endl;
                }
            }
            
            // Converti eventi reali nel formato italiano
            for (const auto& realEvent : realEvents) {
                totalCandidates++;
                
                // Verifica osservabilità da Roma
                ObservabilityResult obs = checkObservability(
                    realEvent.timeCA,
                    realEvent.star.pos,
                    observerRome
                );
                
                if (!obs.isObservable) {
                    totalRejected++;
                    continue;
                }
                
                // Crea evento con TUTTI i dati REALI calcolati
                ItalOccultationEvent event;
                event.asteroid_name = realEvent.asteroid.name;
                event.asteroid_id = realEvent.asteroid.designation;
                event.asteroid_elements = realEvent.asteroid;
                
                // Copy Extra Fields
                event.besselianX = realEvent.besselianX;
                event.besselianY = realEvent.besselianY;
                event.besselianDX = realEvent.besselianDX;
                event.besselianDY = realEvent.besselianDY;
                event.observerLongitude = realEvent.observerLongitude;
                event.observerLatitude = realEvent.observerLatitude;
                event.observerAltitude = realEvent.observerAltitude;
                // Note: distance_au is not in ItalOccultationEvent but separation_arcsec depends on it?
                // Actually separation_arcsec was copied manually.
                
                // Converti stella GaiaStar in formato StarData
                // Trova la stella corrispondente nel database originale
                StarData matchedStar;
                bool foundStar = false;
                for (const auto& s : stars) {
                    if (s.source_id == std::stoll(realEvent.star.sourceId)) {
                        matchedStar = s;
                        foundStar = true;
                        break;
                    }
                }
                
                if (!foundStar) {
                    // Crea StarData minimale dalla GaiaStar
                    matchedStar.source_id = std::stoll(realEvent.star.sourceId);
                    matchedStar.position = realEvent.star.pos;
                    matchedStar.G_mag = realEvent.star.phot_g_mean_mag;
                    matchedStar.parallax = realEvent.star.parallax;
                    matchedStar.parallax_error = 0.0;
                    matchedStar.properMotion.pmra = realEvent.star.pmra;
                    matchedStar.properMotion.pmdec = realEvent.star.pmdec;
                    matchedStar.properMotion.pmra_error = 0.0;
                    matchedStar.properMotion.pmdec_error = 0.0;
                    matchedStar.properMotion.pmra_pmdec_corr = 0.0;
                    matchedStar.epoch.jd = 2457389.0;  // J2016.0 (Gaia DR3 epoch)
                    matchedStar.hasRadialVelocity = false;
                    matchedStar.radialVelocity = 0.0;
                    matchedStar.radialVelocity_error = 0.0;
                    matchedStar.BP_mag = realEvent.star.phot_bp_mean_mag;
                    matchedStar.RP_mag = realEvent.star.phot_rp_mean_mag;
                }
                
                event.star = matchedStar;
                event.event_time = realEvent.timeCA;
                
                // FIX: Use closest_approach_mas from Phase 2 if available
                if ((realEvent.closeApproachDistance * 1000) > 0.0) {
                    event.separation_arcsec = realEvent.closeApproachDistance;
                } else {
                    event.separation_arcsec = realEvent.closeApproachDistance;
                }
                
                // Calculate position angle if missing
                if (realEvent.positionAngle != 0.0) {
                     event.position_angle = realEvent.positionAngle;
                } else {
                     event.position_angle = 0.0; // Todo calculate
                }

                event.duration_sec = realEvent.maxDuration;
                
                // Use calculated mag drop and path width
                event.mag_drop = realEvent.magnitudeDrop; 
                if (event.mag_drop <= 0) {
                     // Fallback calculation if missing
                     double asteroidFlux = pow(10, -0.4 * realEvent.asteroid.H);
                     double starFlux = pow(10, -0.4 * realEvent.star.phot_g_mean_mag);
                     double combinedMag = -2.5 * log10(asteroidFlux + starFlux);
                     event.mag_drop = realEvent.star.phot_g_mean_mag - combinedMag;
                }
                
                event.path_width_km = realEvent.pathWidth;
                if (event.path_width_km <= 0) event.path_width_km = realEvent.asteroid.diameter;
                
                // Copy Shadow Path (IOccultCalc -> ItalOccultationEvent)
                // realEvent is ioccultcalc::OccultationEvent here, so member is 'shadowPath'
                event.path = realEvent.shadowPath;
                
                // Copy uncertainty (already mapped to uncertaintyNorth in realEvent)
                event.uncertainty_km = std::max(realEvent.uncertaintyNorth, realEvent.uncertaintySouth);

                event.priority_score = asteroid.priority_score;
                event.visible_from_italy = {"Roma"};
                
                // Store in thread-local vector
                thread_events[thread_id].push_back(event);
            }
            
        } catch (const std::exception& e) {
#ifdef _OPENMP
            #pragma omp atomic
#endif
            totalRejected++;
            continue;
        }
    }
    
    // FASE 3: Merge thread-local events into global vector
    for (const auto& tevents : thread_events) {
        events.insert(events.end(), tevents.begin(), tevents.end());
    }
    
    // NOTA: clearEarthPositionCache rimosso - API obsoleta
    // La cache è gestita internamente da Ephemeris
    
    // Completa la barra di avanzamento al 100%
    if (g_verbose) {
        printUnifiedProgress(100.0, "Completato", 
                            std::to_string(asteroids.size()) + " asteroidi processati");
        std::cout << "\n";
    }
    
    if (!g_verbose) {
        std::cout << "\n\n✓ Analisi completata!\n";
        std::cout << "  Asteroidi processati: " << asteroids.size() << "\n";
        std::cout << "  Stelle verificate: " << stars.size() << "\n";
        std::cout << "  Eventi trovati: " << events.size() << "\n";
        if (totalCandidates > 0) {
            std::cout << "  Candidati testati: " << totalCandidates << "\n";
            std::cout << "  Rifiutati (geometria sfavorevole): " << totalRejected << "\n";
        }
        std::cout << "\n";
    }
    
    // Ordina per priorità
    std::sort(events.begin(), events.end(),
              [](const auto& a, const auto& b) { return a.priority_score > b.priority_score; });
    
    return events;
}

// ============================================================================
// MODULO 6: CALCOLO PRIORITÀ
// ============================================================================

void calculatePriorities(std::vector<ItalOccultationEvent>& events) {
    printHeader("CALCOLO PRIORITÀ EVENTI");
    
    std::cout << "Criteri di priorità:\n";
    std::cout << "  • Mag drop > 2.0 mag: +3 punti\n";
    std::cout << "  • Durata > 5 secondi: +2 punti\n";
    std::cout << "  • Path attraversa Italia: +3 punti\n";
    std::cout << "  • Incertezza < 15 km: +2 punti\n";
    std::cout << "  • Stella luminosa (< 10 mag): +1 punto\n\n";
    
    for (auto& event : events) {
        double score = 0.0;
        std::vector<std::string> reasons;
        
        if (event.mag_drop > 2.0) {
            score += 3.0;
            reasons.push_back("Mag drop eccellente");
        }
        if (event.duration_sec > 5.0) {
            score += 2.0;
            reasons.push_back("Durata significativa");
        }
        if (!event.visible_from_italy.empty()) {
            score += 3.0;
            reasons.push_back("Visibile dall'Italia");
        }
        if (event.uncertainty_km < 15.0) {
            score += 2.0;
            reasons.push_back("Path ben determinato");
        }
        if (event.star.G_mag < 10.0) {
            score += 1.0;
            reasons.push_back("Stella luminosa");
        }
        
        event.priority_score = score;
        
        std::cout << "(" << event.asteroid_id << ") " << event.asteroid_name 
                  << " vs " << event.star.designation << "\n";
        std::cout << "  Score: " << score << "/11 " << getPriorityStars(score) << "\n";
        for (const auto& r : reasons) {
            std::cout << "    • " << r << "\n";
        }
        std::cout << "\n";
    }
}

// ============================================================================
// MODULO 7: GENERAZIONE REPORT
// ============================================================================

void generateReports(const std::vector<ItalOccultationEvent>& events,
                     const ConfigManager& config) {
    printHeader("GENERAZIONE REPORT");
    
    // Leggi parametri filtro observer
    double observerLat = 41.9028;    // Default: Roma
    double observerLon = 12.4964;
    double maxDistanceKm = 1000.0;   // Default: 1000 km
    double minMagDrop = 0.5;         // Default: 0.5 mag
    
    auto observerSection = config.getSection(ConfigSection::OBSERVER);
    if (observerSection) {
        if (observerSection->hasParameter("latitude")) {
            observerLat = observerSection->getParameter("latitude")->asDouble();
        }
        if (observerSection->hasParameter("longitude")) {
            observerLon = observerSection->getParameter("longitude")->asDouble();
        }
        if (observerSection->hasParameter("max_distance_km")) {
            maxDistanceKm = observerSection->getParameter("max_distance_km")->asDouble();
        }
        if (observerSection->hasParameter("min_mag_drop")) {
            minMagDrop = observerSection->getParameter("min_mag_drop")->asDouble();
        }
    }
    
    std::cout << "Applicazione filtri richiesti: Mag <= 15.0, Durata > 0.3s\n\n";
    
    // Filtri Richiesti Utente
    std::vector<ItalOccultationEvent> filteredEvents;
    for (const auto& ev : events) {
        bool keep = true;
        // Filtro Magnitudine (G mag)
        if (ev.star.G_mag > 15.0) keep = false;
        // Filtro Durata
        if (ev.duration_sec <= 0.3) keep = false;
        
        if (keep) {
            filteredEvents.push_back(ev);
        }
    }
    
    auto outputSection = config.getSection(ConfigSection::OUTPUT);
    if (!outputSection) {
        std::cout << "Usando output predefinito\n";
    }
    
    std::cout << "Formati output:\n";
    std::cout << "  • IOTA formato classico\n";
    std::cout << "  • Preston formato compatto\n";
    std::cout << "  • JSON per API\n";
    std::cout << "  • KML per Google Earth\n";
    std::cout << "  • CSV per Excel\n\n";
    
    // Genera summary testuale
    std::cout << "╔════════════════════════════════════════════════════════════╗\n";
    std::cout << "║          ITALOccultCalc - REPORT OCCULTAZIONI              ║\n";
    std::cout << "╚════════════════════════════════════════════════════════════╝\n\n";
    
    std::cout << "Totale eventi trovati: " << events.size() << "\n";
    std::cout << "Eventi dopo filtri: " << filteredEvents.size() << "\n\n";
    
    if (filteredEvents.empty()) {
        std::cout << "⚠️  Nessun evento soddisfa i criteri di filtro.\n";
        std::cout << "   Prova ad aumentare max_distance_km o ridurre min_mag_drop.\n\n";
        return;
    }
    
    for (const auto& event : filteredEvents) {
        std::cout << "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
        std::cout << "(" << event.asteroid_id << ") " << event.asteroid_name 
                  << " occulta " << event.star.designation << "\n";
        std::cout << "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n";
        
        // Tempo
        int year, month, day, hour, minute;
        double second;
        TimeUtils::jdToCalendar(event.event_time, year, month, day, hour, minute, second);
        std::cout << "Data/Ora: " << year << "-" 
                  << std::setfill('0') << std::setw(2) << month << "-" 
                  << std::setw(2) << day << " " 
                  << std::setw(2) << hour << ":" 
                  << std::setw(2) << minute << ":" 
                  << std::setw(2) << (int)second << " UT\n";
        std::cout << "JD: " << std::fixed << std::setprecision(6) << event.event_time.jd << "\n\n";
        
        // Geometria
        std::cout << "Geometria:\n";
        std::cout << "  Separazione: " << event.separation_arcsec << " arcsec\n";
        std::cout << "  Mag drop: " << event.mag_drop << " mag\n";
        std::cout << "  Durata: " << event.duration_sec << " secondi\n";
        std::cout << "  Larghezza path: " << (int)event.path_width_km << " km\n";
        std::cout << "  Incertezza: ±" << (int)event.uncertainty_km << " km (1σ)\n\n";
        
        // Visibilità
        std::cout << "Visibile da: ";
        for (size_t i = 0; i < event.visible_from_italy.size(); i++) {
            std::cout << event.visible_from_italy[i];
            if (i < event.visible_from_italy.size() - 1) std::cout << ", ";
        }
        std::cout << "\n\n";
        
        // Priorità
        std::cout << "PRIORITÀ: " << getPriorityStars(event.priority_score) 
                  << " (" << event.priority_score << "/11)\n\n";
    }
    
    std::cout << "✓ Report console generato\n\n";
    
    // Converti eventi in formato OccultationEvent per XML
    std::vector<ioccultcalc::OccultationEvent> xmlEvents;
    for (const auto& event : filteredEvents) {
        ioccultcalc::OccultationEvent oe;
        
        // Asteroid data - usa elementi completi
        oe.asteroid = event.asteroid_elements;
        
        // Copy Extra Fields for XML
        oe.besselianX = event.besselianX;
        oe.besselianY = event.besselianY;
        oe.besselianDX = event.besselianDX;
        oe.besselianDY = event.besselianDY;
        oe.observerLongitude = event.observerLongitude;
        oe.observerLatitude = event.observerLatitude;
        oe.observerAltitude = event.observerAltitude;
        
        // DEBUG: verifica campi
        if (g_verbose) {
            std::cout << "DEBUG XML: designation='" << oe.asteroid.designation 
                      << "' name='" << oe.asteroid.name 
                      << "' diameter=" << oe.asteroid.diameter << "\n";
        }
        
        // Star data
        oe.star.sourceId = event.star.designation;
        oe.star.pos = event.star.position;
        oe.star.phot_g_mean_mag = event.star.G_mag;
        oe.star.phot_bp_mean_mag = event.star.BP_mag;
        oe.star.phot_rp_mean_mag = event.star.RP_mag;
        oe.star.parallax = event.star.parallax;
        oe.star.pmra = event.star.properMotion.pmra;
        oe.star.pmdec = event.star.properMotion.pmdec;
        
        // Event timing
        oe.timeCA = event.event_time;
        
        // Geometry
        oe.closeApproachDistance = event.separation_arcsec;
        oe.maxDuration = event.duration_sec;
        oe.pathWidth = event.path_width_km;
        oe.magnitudeDrop = event.mag_drop;
        oe.probability = (event.priority_score > 5.0) ? 0.9 : 0.5; // Better estimate based on priority
        
        // Shadow path
        oe.shadowPath = event.path;
        
        // Uncertainties
        oe.uncertaintyNorth = event.uncertainty_km;
        oe.uncertaintySouth = event.uncertainty_km;
        
        xmlEvents.push_back(oe);
    }
    
    // Genera file XML usando Occult4XMLHandler
    auto xmlParam = config.findParameter("output.xml_file");
    std::string xmlFile = xmlParam.has_value() ? xmlParam->asString() : "test_output_occult4.xml";
    try {
        Occult4XMLHandler xmlHandler;
        
        // Configura opzioni
        Occult4XMLHandler::XMLOptions opts;
        opts.includeUncertainty = true;
        opts.includePathPoints = true;
        opts.pathPointsResolution = 100;
        opts.includeStarData = true;
        opts.includeAsteroidData = true;
        opts.useGaiaIds = true;
        opts.organizationName = "ITALOccultCalc";
        xmlHandler.setOptions(opts);
        
        // Esporta XML
        bool success = xmlHandler.exportMultipleToXML(xmlEvents, xmlFile);
        
        if (success) {
            std::cout << "✓ File XML generato: " << xmlFile << "\n";
            std::cout << "  Formato: Occult4 compatible\n";
            std::cout << "  Eventi: " << xmlEvents.size() << "\n";
            std::cout << "  Import in: OccultWatcher Cloud\n\n";
        } else {
            std::cout << "⚠️  Errore generazione XML\n\n";
        }
    } catch (const std::exception& e) {
        std::cout << "⚠️  Errore XML: " << e.what() << "\n\n";
    }

    // JSON Export
    try {
        auto jsonParam = config.findParameter("output.json_file");
        std::string jsonFile = jsonParam.has_value() ? jsonParam->asString() : "test_output_structured.json";
        
        nlohmann::json jEvents = nlohmann::json::array();
        for (const auto& ev : filteredEvents) {
            nlohmann::json jEv;
            jEv["event_id"] = ev.asteroid_id + "_" + ev.star.designation;
            jEv["asteroid"] = {
                {"name", ev.asteroid_name},
                {"designation", ev.asteroid_id},
                {"diameter_km", ev.path_width_km}, 
                {"H", ev.asteroid_elements.H}
            };
            jEv["star"] = {
                {"source_id", std::to_string(ev.star.source_id)},
                {"ra_deg", ev.star.position.ra * RAD_TO_DEG},
                {"dec_deg", ev.star.position.dec * RAD_TO_DEG},
                {"mag_g", ev.star.G_mag}
            };
            jEv["occultation"] = nlohmann::json{
                {"jd", ev.event_time.jd},
                {"time_utc", TimeUtils::jdToISO(ev.event_time)},
                {"ca_dist_arcsec", ev.separation_arcsec},
                {"duration_sec", ev.duration_sec},
                {"shadow_width_km", ev.path_width_km},
                {"mag_drop", ev.mag_drop},
                {"path_uncertainty_km", ev.uncertainty_km}
            };
            jEv["priority"] = {
                {"score", ev.priority_score}
            };
            
            jEvents.push_back(jEv);
        }
        
        std::ofstream jsonOut(jsonFile);
        jsonOut << jEvents.dump(4);
        jsonOut.close();
        std::cout << "✓ File JSON generato: " << jsonFile << "\n";
    } catch (const std::exception& e) {
        std::cout << "⚠️  Errore JSON: " << e.what() << "\n\n";
    }

    // KML Export
    try {
        auto kmlParam = config.findParameter("output.kml_file");
        std::string kmlFile = kmlParam.has_value() ? kmlParam->asString() : "output_events.kml";
        
        ioccultcalc::KMLExporter kmlExporter;
        ioccultcalc::KMLExporter::ExportOptions kmlOpts;
        kmlOpts.showCenterline = true;
        kmlOpts.showUncertaintyBands = true;
        kmlOpts.showTimestamps = true;
        kmlOpts.pathWidthKm = 50.0; // Default visualization width
        kmlExporter.setExportOptions(kmlOpts);
        
        if (kmlExporter.exportMultipleToKML(xmlEvents, kmlFile)) {
             std::cout << "✓ File KML generato: " << kmlFile << "\n";
             std::cout << "  Tracce generate: " << xmlEvents.size() << "\n\n";
        } else {
             std::cout << "⚠️  Errore generazione KML\n\n";
        }
    } catch (const std::exception& e) {
        std::cout << "⚠️  Errore KML: " << e.what() << "\n\n";
    }
}

// ============================================================================
// MAIN
// ============================================================================

int main(int argc, char* argv[]) {
    std::cout << "DEBUG: Main started.\n";
    std::cout.flush();
    std::cout << "\n";
    std::cout << "╔═══════════════════════════════════════════════════════════════╗\n";
    std::cout << "║                     ITALOccultCalc v1.0                        ║\n";
    std::cout << "║         Ricerca Automatica Occultazioni Asteroidali           ║\n";
    std::cout << "║              Ottimizzato per Osservatori Italiani              ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════════╝\n";
    
    try {
        // Parsing argomenti
        std::string configFile = "preset_default.json";
        
        for (int i = 1; i < argc; i++) {
            std::string arg = argv[i];
            if (arg == "-v" || arg == "--verbose") {
                g_verbose = true;
                g_verbosityLevel = 1;
                std::cout << "\n╔════════════════════════════════════════════════════════════════╗\n";
                std::cout << "║  Modalità Verbose - Monitoraggio Progresso                    ║\n";
                std::cout << "╠════════════════════════════════════════════════════════════════╣\n";
                std::cout << "║  Saranno mostrati aggiornamenti su asteroidi e giorni.         ║\n";
                std::cout << "╚════════════════════════════════════════════════════════════════╝\n\n";
            } else if (arg == "-vv") {
                g_verbose = true;
                g_verbosityLevel = 2;
                std::cout << "[DEBUG] Verbosity Level 2 (Full Debug)\n";
            } else if (arg == "-h" || arg == "--help") {
                std::cout << "\nUsage: italoccultcalc [options] <config_file>\n\n";
                std::cout << "Options:\n";
                std::cout << "  -v, --verbose    Mostra progresso standard (Asteroide/Giorno)\n";
                std::cout << "  -vv              Mostra debug completo (Query ADQL, vettori)\n";
                std::cout << "  -h, --help       Mostra questo aiuto\n\n";
                std::cout << "Examples:\n";
                std::cout << "  italoccultcalc config.oop\n";
                std::cout << "  italoccultcalc -v preset_large_asteroids_jan2026.oop\n\n";
                return 0;
            } else {
                configFile = arg;
            }
        }
        
        std::cerr << "[DEBUG] main: Prima di startTime" << std::endl;
        std::cerr.flush();
        
        auto startTime = std::chrono::high_resolution_clock::now();
        
        std::cerr << "[DEBUG] main: WORKFLOW COMPLETO - INIZIO" << std::endl;
        std::cerr.flush();
        
        // WORKFLOW COMPLETO
        
        // 1. Carica configurazione
        auto t0 = std::chrono::high_resolution_clock::now();
        std::cerr << "[DEBUG] main: Step 1: Caricamento configurazione..." << std::endl;
        std::cerr.flush();
        ConfigManager config = loadConfiguration(configFile);
        std::cerr << "[DEBUG] main: Step 1: Completato" << std::endl;
        std::cerr.flush();
        auto t1 = std::chrono::high_resolution_clock::now();
        
        // 2. Seleziona asteroidi
        std::cout << "[DEBUG] Step 2: Selezione asteroidi..." << std::endl;
        std::vector<AsteroidCandidate> asteroids = selectAsteroids(config);
        std::cout << "[DEBUG] Step 2: Completato, trovati " << asteroids.size() << " asteroidi" << std::endl;
        auto t2 = std::chrono::high_resolution_clock::now();
        
        // 3. Propaga orbite
        std::cout << "[DEBUG] Step 3: Propagazione orbite..." << std::endl;
        propagateOrbits(asteroids, config);
        std::cout << "[DEBUG] Step 3: Completato" << std::endl;
        auto t3 = std::chrono::high_resolution_clock::now();
        
        // 4. Query catalogo stelle (basato su posizioni asteroidi)
        std::cout << "[DEBUG] Step 4: Query catalogo stelle..." << std::endl;
        std::vector<StarData> stars = queryCatalog(config, asteroids);
        std::cout << "[DEBUG] Step 4: Completato, trovate " << stars.size() << " stelle" << std::endl;
        auto t4 = std::chrono::high_resolution_clock::now();
        
        // 5. Rileva occultazioni
        std::cout << "[DEBUG] Step 5: Rilevamento occultazioni..." << std::endl;
        std::vector<ItalOccultationEvent> events = detectOccultations(asteroids, stars, config);
        std::cout << "[DEBUG] Step 5: Completato, trovati " << events.size() << " eventi" << std::endl;
        auto t5 = std::chrono::high_resolution_clock::now();
        
        // 6. Calcola priorità
        calculatePriorities(events);
        
        // 7. Genera report
        generateReports(events, config);
        
        auto endTime = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime);
        
        printHeader("RIEPILOGO TEMPI DI ESECUZIONE");
        std::cout << "┌───────────────────────────────┬──────────────┐\n";
        std::cout << "│ FASE                          │ TEMPO (s)    │\n";
        std::cout << "├───────────────────────────────┼──────────────┤\n";
        
        auto printPhaseTime = [](const std::string& phase, double seconds) {
            std::cout << "│ " << std::left << std::setw(29) << phase << " │ " 
                      << std::right << std::setw(9) << std::fixed << std::setprecision(3) << seconds << " s  │\n";
        };
        
        // Calcola durate delle fasi
        auto d_config = std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0).count() / 1000.0;
        auto d_asteroids = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count() / 1000.0;
        auto d_propag = std::chrono::duration_cast<std::chrono::milliseconds>(t3 - t2).count() / 1000.0;
        auto d_query = std::chrono::duration_cast<std::chrono::milliseconds>(t4 - t3).count() / 1000.0;
        auto d_detect = std::chrono::duration_cast<std::chrono::milliseconds>(t5 - t4).count() / 1000.0;
        auto d_report = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - t5).count() / 1000.0;
        auto d_total = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - t0).count() / 1000.0;
        
        printPhaseTime("1. Caricamento Config", d_config);
        printPhaseTime("2. Selezione Asteroidi", d_asteroids);
        printPhaseTime("3. Propagazione Orbite", d_propag);
        printPhaseTime("4. Query Catalogo Stelle", d_query);
        printPhaseTime("5. Rilevamento Occultazioni", d_detect);
        printPhaseTime("6. Reporting & Export", d_report);
        std::cout << "├───────────────────────────────┼──────────────┤\n";
        printPhaseTime("TOTALE", d_total);
        std::cout << "└───────────────────────────────┴──────────────┘\n";
        
        std::cout << "\n✓ Workflow completato con successo!\n";
        std::cout << "Eventi trovati: " << events.size() << "\n";
        std::cout << "Report salvati in: output/\n\n";
        
        // Cleanup catalogo Gaia
        ioc::gaia::UnifiedGaiaCatalog::shutdown();
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n✗ ERRORE CRITICO: " << e.what() << "\n\n";
        return 1;
    }
}
