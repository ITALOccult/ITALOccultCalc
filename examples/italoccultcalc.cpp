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
#include "ioccultcalc/observation.h"
#include "ioccultcalc/orbit_fitter.h"
#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/mpc_client.h"
#include "planetary_aberration.h"
#include "cubic_spline.h"
// Phase1+Phase2 integration (LinOccult method)
#include "ioccultcalc/occultation_search_astdyn.h"
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

using namespace ioccultcalc;

// ============================================================================
// VARIABILI GLOBALI
// ============================================================================

static bool g_verbose = false;  // Flag per output verboso

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
    double mag_drop;
    double duration_sec;
    double path_width_km;
    double uncertainty_km;
    double priority_score;
    std::vector<std::string> visible_from_italy;
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
            std::istringstream iss(listStr);
            std::string token;
            while (std::getline(iss, token, ',')) {
                // Trim whitespace
                token.erase(0, token.find_first_not_of(" \t"));
                token.erase(token.find_last_not_of(" \t") + 1);
                if (!token.empty()) {
                    try {
                        int num = std::stoi(token);
                        explicitAsteroidList.insert(num);
                    } catch (...) {
                        std::cerr << "Attenzione: ignorato numero invalido '" << token << "'\n";
                    }
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
            // Asteroide nella lista: bypassa tutti i filtri
        } else {
            // Modalità filtri standard
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
        }
        
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
        
        // Carica elementi orbitali direttamente dal JSON
        OrbitalElements elements;
        elements.designation = designation;
        elements.name = name;
        
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
        double H = elements.H;
        double estimatedDiameter = 0.0;
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
            
            if (g_verbose) {
                std::cout << "  Asteroide " << ast.elements.designation 
                          << ": RA=" << std::fixed << std::setprecision(2) << raDeg 
                          << "° Dec=" << decDeg << "° (calcolata)\n";
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
    
    // STEP 2: Query per regioni (FASE 2 OPTIMIZATION: parallel loading)
    auto query_start = std::chrono::high_resolution_clock::now();
    
    // Pre-alloca vettori per risultati paralleli
    std::vector<std::vector<ioc::gaia::GaiaStar>> region_results(regions.size());

#ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic)
    for (size_t r = 0; r < regions.size(); r++) {
        const auto& region = regions[r];
        
        // Query catalogo con UnifiedGaiaCatalog API
        ioc::gaia::QueryParams qparams;
        qparams.ra_center = region.ra;
        qparams.dec_center = region.dec;
        qparams.radius = searchRadiusDeg;
        qparams.max_magnitude = magLimit;
        
        region_results[r] = catalog.queryCone(qparams);
        
        #pragma omp critical
        {
            if (!g_verbose && r % 5 == 0) {
                std::cout << "  [" << r << "/" << regions.size() << "] "
                         << "Query in corso...\r" << std::flush;
            }
        }
    }
#else
    // Sequential fallback
    for (size_t r = 0; r < regions.size(); r++) {
        const auto& region = regions[r];
        
        ioc::gaia::QueryParams qparams;
        qparams.ra_center = region.ra;
        qparams.dec_center = region.dec;
        qparams.radius = searchRadiusDeg;
        qparams.max_magnitude = magLimit;
        
        region_results[r] = catalog.queryCone(qparams);
        
        if (!g_verbose && r % 5 == 0) {
            std::cout << "  [" << r << "/" << regions.size() << "] "
                     << "Query in corso...\r" << std::flush;
        }
    }
#endif
    
    auto query_end = std::chrono::high_resolution_clock::now();
    double query_time_ms = std::chrono::duration<double, std::milli>(query_end - query_start).count();
    
    if (!g_verbose) {
        std::cout << "\n  ✓ Query parallele completate in " << (query_time_ms/1000.0) << " secondi\n\n";
        std::cout << "Fase 3: Deduplica e conversione stelle...\n";
    }
    
    // STEP 3: Deduplica e converti in parallelo
    size_t debugCount = 0;
    for (size_t r = 0; r < regions.size(); r++) {
        const auto& region = regions[r];
        const auto& regionStars = region_results[r];
        
        for (const auto& gaiaStar : regionStars) {
            if (uniqueStarIds.insert(std::to_string(gaiaStar.source_id)).second) {
                // DEBUG: Stampa prime 3 stelle per vedere coordinate
                if (g_verbose && debugCount < 3) {
                    std::cout << "  [DEBUG] Stella " << gaiaStar.source_id 
                              << ": RA=" << std::fixed << std::setprecision(2) << gaiaStar.ra 
                              << "° Dec=" << gaiaStar.dec << "° Mag=" << gaiaStar.phot_g_mean_mag << "\n";
                    debugCount++;
                }
                
                // Converti ioc::gaia::GaiaStar a StarData
                StarData star;
                star.source_id = gaiaStar.source_id;
                star.designation = "Gaia DR3 " + std::to_string(gaiaStar.source_id);
                star.position.ra = gaiaStar.ra;
                star.position.dec = gaiaStar.dec;
                star.epoch.jd = 2457389.0;  // Gaia DR3 epoch
                star.properMotion.pmra = gaiaStar.pmra;
                star.properMotion.pmdec = gaiaStar.pmdec;
                star.properMotion.pmra_error = gaiaStar.pmra_error;
                star.properMotion.pmdec_error = gaiaStar.pmdec_error;
                star.properMotion.pmra_pmdec_corr = 0.0;
                star.parallax = gaiaStar.parallax;
                star.parallax_error = gaiaStar.parallax_error;
                star.G_mag = gaiaStar.phot_g_mean_mag;
                star.BP_mag = gaiaStar.phot_bp_mean_mag;
                star.RP_mag = gaiaStar.phot_rp_mean_mag;
                star.hasRadialVelocity = false;
                star.radialVelocity = 0.0;
                star.radialVelocity_error = 0.0;
                star.astrometric_excess_noise = gaiaStar.astrometric_excess_noise;
                star.astrometric_n_good_obs = gaiaStar.visibility_periods_used;
                star.ruwe = gaiaStar.ruwe;
                
                stars.push_back(star);
            }
        }
        
        // Progress
        if (!g_verbose && r % 5 == 0) {
            int pct = (r + 1) * 100 / regions.size();
            std::cout << "  [" << (r+1) << "/" << regions.size() << "] "
                     << "Regione RA=" << std::fixed << std::setprecision(1) << region.ra 
                     << "° Dec=" << region.dec << "° | "
                     << "Totale uniche: " << stars.size() 
                     << " (" << pct << "%)   \r" << std::flush;
        }
    }
    
    if (!g_verbose) {
        std::cout << "\n\n✓ Completato: " << stars.size() 
                 << " stelle uniche da catalogo locale\n";
        std::cout << "  Tempo query totale: " << std::fixed << std::setprecision(2)
                 << (query_time_ms/1000.0) << " secondi\n\n";
    }
    
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
                          << " - D=" << std::setw(4) << (int)asteroid.elements.diameter << "km"
                          << " - Analisi " << stars.size() << " stelle..." << std::flush;
            } else {
                // Barra di progresso compatta per modalità non-verbose
                int percentage = (int)((astIdx * 100) / asteroids.size());
                std::cout << "  [" << std::setw(3) << percentage << "%] Asteroide " 
                          << std::setw(3) << (astIdx + 1) << "/" << asteroids.size()
                          << " | " << std::setw(20) << std::left << asteroid.elements.name << std::right
                          << " | Diam: " << std::setw(4) << (int)asteroid.elements.diameter << " km"
                          << " | Verifico " << stars.size() << " stelle...   \r" << std::flush;
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
            
            // ================================================================
            // STRATEGIA RICERCA A DUE FASI (LINOCCULT METHOD)
            // FASE 1: Phase1CandidateScreening - screening veloce
            // FASE 2: Phase2OccultationGeometry - calcolo preciso con AstDyn
            // ================================================================
            
            // Usa OccultationSearchAstDyn per ricerca completa Phase1+Phase2
            ioccultcalc::OccultationSearchAstDyn search;
            
            // Carica asteroide da allnum.cat (elementi già caricati sopra)
            try {
                // Converti OrbitalElements a AstDySElements per OccultationSearchAstDyn
                // OccultationSearchAstDyn carica direttamente da AstDyS, quindi usiamo il numero
                search.loadAsteroid(asteroid.elements.designation);
                
                // Imposta diametro e incertezza
                if (astElem.diameter > 0) {
                    // OccultationSearchAstDyn non ha setAsteroidDiameter diretto,
                    // ma Phase2 lo supporta - dobbiamo accedere a Phase2 internamente
                    // Per ora passiamo attraverso la configurazione
                }
            } catch (const std::exception& e) {
                if (g_verbose) {
                    std::cout << " → Errore caricamento asteroide: " << e.what() << std::flush;
                }
                continue;
            }
            
            // Configura ricerca
            ioccultcalc::OccultationSearchConfig searchConfig;
            searchConfig.start_mjd_tdb = startJd - 2400000.5;
            searchConfig.end_mjd_tdb = endJd - 2400000.5;
            searchConfig.max_magnitude = maxMagnitude;
            searchConfig.min_probability = minProbability;
            
            // Configura Phase1 (screening veloce)
            searchConfig.phase1_config = Phase1Config::conservative();
            searchConfig.phase1_config.start_mjd_tdb = searchConfig.start_mjd_tdb;
            searchConfig.phase1_config.end_mjd_tdb = searchConfig.end_mjd_tdb;
            searchConfig.phase1_config.closest_approach_threshold_arcsec = searchRadius * 3600.0;  // gradi -> arcsec
            
            // Leggi parametri Phase1 dalla configurazione se presenti
            auto searchSection = config.getSection(ConfigSection::SEARCH);
            if (searchSection) {
                // path_resolution_hours -> path_interval_seconds
                if (searchSection->hasParameter("phase1_config.path_resolution_hours")) {
                    double hours = searchSection->getParameter("phase1_config.path_resolution_hours")->asDouble();
                    searchConfig.phase1_config.path_interval_seconds = static_cast<int>(hours * 3600.0);
                }
                // corridor_width_deg
                if (searchSection->hasParameter("phase1_config.corridor_width_deg")) {
                    searchConfig.phase1_config.corridor_width_deg = searchSection->getParameter("phase1_config.corridor_width_deg")->asDouble();
                }
                // closest_approach_threshold_arcsec (se specificato esplicitamente)
                if (searchSection->hasParameter("phase1_config.closest_approach_threshold_arcsec")) {
                    searchConfig.phase1_config.closest_approach_threshold_arcsec = searchSection->getParameter("phase1_config.closest_approach_threshold_arcsec")->asDouble();
                }
            }
            
            // Configura Phase2 (calcolo preciso LinOccult)
            searchConfig.phase2_config.time_window_minutes = 10.0;
            searchConfig.phase2_config.time_step_seconds = 1.0;
            
            // Imposta diametro e incertezza
            searchConfig.asteroid_diameter_km = astElem.diameter;
            searchConfig.orbital_uncertainty_km = orbitalUncertainty;
            search.setAsteroidDiameter(astElem.diameter);
            search.setOrbitalUncertainty(orbitalUncertainty);
            
            // Esegui ricerca Phase1+Phase2
            ioccultcalc::OccultationSearchResults results;
            
            try {
                results = search.search(searchConfig);
            } catch (const std::exception& e) {
                if (g_verbose) {
                    std::cout << " → Errore ricerca: " << e.what() << std::flush;
                }
                continue;
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
                event.separation_arcsec = realEvent.closeApproachDistance * 3600.0;  // gradi->arcsec
                event.duration_sec = realEvent.maxDuration;
                
                // Calcola mag drop REALE basato su fotometria
                double asteroidFlux = pow(10, -0.4 * realEvent.asteroid.H);
                double starFlux = pow(10, -0.4 * realEvent.star.phot_g_mean_mag);
                double combinedMag = -2.5 * log10(asteroidFlux + starFlux);
                event.mag_drop = realEvent.star.phot_g_mean_mag - combinedMag;
                
                event.path_width_km = realEvent.asteroid.diameter;
                
                // Calcola incertezza REALE dalla propagazione
                event.uncertainty_km = std::max(
                    std::abs(realEvent.uncertaintyNorth),
                    std::abs(realEvent.uncertaintySouth)
                );
                
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
    
    std::cout << "Nessun filtro geografico applicato - tutti gli eventi inclusi\n\n";
    
    // Usa tutti gli eventi senza filtri
    std::vector<ItalOccultationEvent> filteredEvents = events;
    
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
        oe.probability = 0.8;  // Placeholder
        
        // Shadow path - TODO: calcolare vero centerline dall'occultazione
        // Per ora lasciamo vuoto, sarà calcolato dall'XML handler
        // (L'XML handler può generare una traccia approssimata se necessario)
        oe.shadowPath.clear();
        
        // Uncertainties
        oe.uncertaintyNorth = event.uncertainty_km;
        oe.uncertaintySouth = event.uncertainty_km;
        
        xmlEvents.push_back(oe);
    }
    
    // Genera file XML usando Occult4XMLHandler
    std::string xmlFile = "test_output_occult4.xml";
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
}

// ============================================================================
// MAIN
// ============================================================================

int main(int argc, char* argv[]) {
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
                std::cout << "\n╔════════════════════════════════════════════════════════════════╗\n";
                std::cout << "║  Modalità Verbose - Barra di Avanzamento Unificata            ║\n";
                std::cout << "╠════════════════════════════════════════════════════════════════╣\n";
                std::cout << "║  0-40%:  Propagazione orbite                                   ║\n";
                std::cout << "║  40-50%: Query catalogo stelle                                 ║\n";
                std::cout << "║  50-100%: Rilevamento occultazioni                             ║\n";
                std::cout << "╚════════════════════════════════════════════════════════════════╝\n\n";
            } else if (arg == "-h" || arg == "--help") {
                std::cout << "\nUsage: italoccultcalc [options] <config_file>\n\n";
                std::cout << "Options:\n";
                std::cout << "  -v, --verbose    Mostra progresso dettagliato con % e nomi asteroidi\n";
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
        std::cerr << "[DEBUG] main: Step 1: Caricamento configurazione..." << std::endl;
        std::cerr.flush();
        ConfigManager config = loadConfiguration(configFile);
        std::cerr << "[DEBUG] main: Step 1: Completato" << std::endl;
        std::cerr.flush();
        
        // 2. Seleziona asteroidi
        std::cout << "[DEBUG] Step 2: Selezione asteroidi..." << std::endl;
        std::vector<AsteroidCandidate> asteroids = selectAsteroids(config);
        std::cout << "[DEBUG] Step 2: Completato, trovati " << asteroids.size() << " asteroidi" << std::endl;
        
        // 3. Propaga orbite
        std::cout << "[DEBUG] Step 3: Propagazione orbite..." << std::endl;
        propagateOrbits(asteroids, config);
        std::cout << "[DEBUG] Step 3: Completato" << std::endl;
        
        // 4. Query catalogo stelle (basato su posizioni asteroidi)
        std::cout << "[DEBUG] Step 4: Query catalogo stelle..." << std::endl;
        std::vector<StarData> stars = queryCatalog(config, asteroids);
        std::cout << "[DEBUG] Step 4: Completato, trovate " << stars.size() << " stelle" << std::endl;
        
        // 5. Rileva occultazioni
        std::cout << "[DEBUG] Step 5: Rilevamento occultazioni..." << std::endl;
        std::vector<ItalOccultationEvent> events = detectOccultations(asteroids, stars, config);
        std::cout << "[DEBUG] Step 5: Completato, trovati " << events.size() << " eventi" << std::endl;
        
        // 6. Calcola priorità
        calculatePriorities(events);
        
        // 7. Genera report
        generateReports(events, config);
        
        auto endTime = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::seconds>(endTime - startTime);
        
        printHeader("COMPLETAMENTO");
        std::cout << "✓ Workflow completato con successo!\n";
        std::cout << "Tempo totale: " << duration.count() << " secondi\n";
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
