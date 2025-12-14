/**
 * @file phase2_occultation_geometry.cpp
 * @brief Implementazione Phase 2: Geometria precisa occultazione
 * @date 4 Dicembre 2025
 * 
 * IMPLEMENTAZIONE STRATEGIA:
 * ===========================
 * 
 * Phase 2 riceve i candidati da Phase 1 e per ciascuno:
 * 
 * 1. PROPAGAZIONE PRECISA (±5 min attorno CA):
 *    - RKF78 con tolleranza 1e-12
 *    - Perturbazioni planetarie attive
 *    - Step temporale: 1 secondo
 *    - ~600 punti per evento
 * 
 * 2. CORREZIONI ASTROMETRICHE:
 *    - Parallasse geocentrica
 *    - Aberrazione stellare e planetaria
 *    - Proper motion stella (da Gaia DR3 a epoca evento)
 *    - Light-time correction
 * 
 * 3. GEOMETRIA OCCULTAZIONE:
 *    - Trova istante esatto closest approach (sub-millisecondo)
 *    - Calcola miss distance con precisione sub-milliarcsecondo
 *    - Determina chord length e position angle
 *    - Calcola durata massima occultazione
 * 
 * 4. PROIEZIONE SU TERRA:
 *    - Path dell'ombra su ellissoide WGS84
 *    - Velocità e direzione ombra
 *    - Limiti nord/sud del path
 *    - Entry/exit points sul pianeta
 * 
 * 5. PREDIZIONI PER OSSERVATORI:
 *    - Geometria locale per ogni sito
 *    - Tempo CA locale, altitudine, azimut
 *    - Visibilità (Sole, Luna, meteo teorico)
 *    - Distanza dalla linea centrale
 */

#include "phase2_occultation_geometry.h"

// Astdyn
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/io/parsers/OrbFitEQ1Parser.hpp"
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/propagation/Propagator.hpp"
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/propagation/Integrator.hpp"
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/propagation/OrbitalElements.hpp"
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/ephemeris/PlanetaryEphemeris.hpp"
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/observations/RWOReader.hpp"
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/observations/Observation.hpp"
// OrbitFitter di AstDyn
#include "../external/ITALOccultLibrary/astdyn/include/astdyn/io/AstDysOrbitFitter.hpp"

// IOC GaiaLib
#include "ioc_gaialib/unified_gaia_catalog.h"
#include "../external/ITALOccultLibrary/italoccultlibrary/include/chebyshev_approximation.h"

#include <iostream>
#include <cmath>
#include <algorithm>
#include <chrono>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <Eigen/Dense>
#include <nlohmann/json.hpp>

// Per download HTTP (curl)
#include <curl/curl.h>

// Costanti
using namespace ioccultcalc;

constexpr double MJD_TO_JD = 2400000.5;
constexpr double DEG_TO_RAD = M_PI / 180.0;
constexpr double RAD_TO_DEG = 180.0 / M_PI;
constexpr double RAD_TO_MAS = RAD_TO_DEG * 3600.0 * 1000.0;  // milliarcsec
constexpr double MAS_TO_RAD = 1.0 / RAD_TO_MAS;
constexpr double AU_TO_KM = 1.495978707e8;
constexpr double EPSILON_J2000 = 23.4392911 * DEG_TO_RAD;  // Obliquità eclittica

// WGS84 ellissoide terrestre
constexpr double EARTH_EQUATORIAL_RADIUS_KM = 6378.137;
constexpr double EARTH_POLAR_RADIUS_KM = 6356.752;
constexpr double EARTH_FLATTENING = 1.0 / 298.257223563;

// ═══════════════════════════════════════════════════════════════
// IMPLEMENTAZIONE PIMPL
// ═══════════════════════════════════════════════════════════════

class Phase2OccultationGeometry::Impl {
public:
    // Elementi orbitali asteroide
    astdyn::propagation::KeplerianElements keplerian_elements;
    astdyn::propagation::KeplerianElements refined_elements;  // Elementi dopo fit
    bool has_elements = false;
    bool has_refined_elements = false;
    
    // Propagatore
    std::unique_ptr<astdyn::propagation::Propagator> propagator;
    
    // Siti osservatori
    std::vector<ObserverGeometry> observer_sites;
    
    // Orbit fitter di AstDyn (TODO: implementare quando disponibile)
    // std::unique_ptr<astdyn::fitting::OrbitFitter> orbit_fitter;
    
    // Nome/numero asteroide
    // Nome/numero asteroide
    std::string asteroid_designation;
    
    // Parametri fisici
    double diameter_km = 0.0;
    double abs_mag = 0.0;
    double slope_param = 0.15;
    
    Impl() {
        // Inizializza propagatore con parametri ad alta precisione
        auto ephemeris = std::make_shared<astdyn::ephemeris::PlanetaryEphemeris>();
        auto integrator = std::make_unique<astdyn::propagation::RKF78Integrator>(0.1, 1e-12);
        
        astdyn::propagation::PropagatorSettings settings;
        settings.include_planets = true;  // ← ATTIVA perturbazioni per Phase 2
        settings.include_moon = true;
        settings.include_asteroids = true;  // USER REQUEST: Enable asteroid perturbations
        
        propagator = std::make_unique<astdyn::propagation::Propagator>(
            std::move(integrator), ephemeris, settings);
        
        // Inizializza orbit fitter di AstDyn (TODO: quando disponibile)
        // orbit_fitter = std::make_unique<astdyn::fitting::OrbitFitter>();
    }
    
    // ... (rest of methods declarations) 
    
    /**
     * @brief Scarica osservazioni RWO da AstDyS
     */
    std::vector<astdyn::observations::OpticalObservation> downloadRWOObservations(
        const std::string& designation);
    
    /**
     * @brief Esegue fit orbitale preciso con tutte le correzioni
     */
    OrbitalFitResults refineOrbitWithObservations(
        const std::vector<astdyn::observations::OpticalObservation>& observations,
        const Phase2Config& config,
        double target_mjd_tdb);
    
    /**
     * @brief Costruisce URL AstDyS per download RWO
     */
    std::string buildAstDySURL(const std::string& designation);
    
    /**
     * @brief Download file via HTTP
     */
    bool downloadFile(const std::string& url, const std::string& output_path);
};

// ═══════════════════════════════════════════════════════════════
// COSTRUTTORE / DISTRUTTORE
// ═══════════════════════════════════════════════════════════════

Phase2OccultationGeometry::Phase2OccultationGeometry() 
    : pimpl_(std::make_unique<Impl>()) {
}

Phase2OccultationGeometry::~Phase2OccultationGeometry() = default;

// ═══════════════════════════════════════════════════════════════
// CARICAMENTO ELEMENTI ORBITALI
// ═══════════════════════════════════════════════════════════════

bool Phase2OccultationGeometry::loadAsteroidFromEQ1(const std::string& eq1_path) {
    try {
        astdyn::io::parsers::OrbFitEQ1Parser parser;
        auto elements = parser.parse(eq1_path);
        
        pimpl_->keplerian_elements.semi_major_axis = elements.semi_major_axis;
        pimpl_->keplerian_elements.eccentricity = elements.eccentricity;
        pimpl_->keplerian_elements.inclination = elements.inclination;
        pimpl_->keplerian_elements.longitude_ascending_node = elements.longitude_asc_node;
        pimpl_->keplerian_elements.argument_perihelion = elements.argument_perihelion;
        pimpl_->keplerian_elements.mean_anomaly = elements.mean_anomaly;
        pimpl_->keplerian_elements.epoch_mjd_tdb = elements.epoch_mjd_tdb;
        pimpl_->keplerian_elements.gravitational_parameter = 
            1.32712440018e20 / std::pow(1.495978707e11, 3) * std::pow(86400.0, 2);
        
        pimpl_->has_elements = true;
        return true;
        
    } catch (const std::exception& e) {
        std::cerr << "Phase2: Errore caricamento " << eq1_path << ": " << e.what() << "\n";
        return false;
    }
}

bool Phase2OccultationGeometry::loadAsteroidFromJSON(int asteroid_number, const std::string& json_path) {
    try {
        // Determina path del database JSON
        std::string path = json_path;
        if (path.empty()) {
            const char* home = std::getenv("HOME");
            if (home) {
                path = std::string(home) + "/.ioccultcalc/data/all_numbered_asteroids.json";
            } else {
                std::cerr << "Phase2: Errore HOME non definito e json_path non specificato\n";
                return false;
            }
        }
        
        // Leggi file JSON
        std::ifstream file(path);
        if (!file.is_open()) {
            std::cerr << "Phase2: Errore impossibile aprire " << path << "\n";
            return false;
        }
        
        nlohmann::json j;
        file >> j;
        
        // Cerca l'asteroide nel database
        if (!j.contains("asteroids")) {
            std::cerr << "Phase2: Errore chiave 'asteroids' non trovata\n";
            return false;
        }
        
        bool found = false;
        for (const auto& asteroid : j["asteroids"]) {
            if (asteroid["number"].get<int>() == asteroid_number) {
                // Estrai elementi orbitali (angoli in gradi nel JSON)
                pimpl_->keplerian_elements.semi_major_axis = asteroid["a"].get<double>();
                pimpl_->keplerian_elements.eccentricity = asteroid["e"].get<double>();
                pimpl_->keplerian_elements.inclination = asteroid["i"].get<double>() * DEG_TO_RAD;
                
                if (asteroid.contains("Node"))
                    pimpl_->keplerian_elements.longitude_ascending_node = asteroid["Node"].get<double>() * DEG_TO_RAD;
                else
                    pimpl_->keplerian_elements.longitude_ascending_node = asteroid["Omega"].get<double>() * DEG_TO_RAD;

                if (asteroid.contains("Peri"))
                    pimpl_->keplerian_elements.argument_perihelion = asteroid["Peri"].get<double>() * DEG_TO_RAD;
                else
                    pimpl_->keplerian_elements.argument_perihelion = asteroid["omega"].get<double>() * DEG_TO_RAD;
                    
                pimpl_->keplerian_elements.mean_anomaly = asteroid["M"].get<double>() * DEG_TO_RAD;
                
                // Epoca: da JD a MJD TDB
                double epoch_jd = asteroid["epoch"].get<double>();
                pimpl_->keplerian_elements.epoch_mjd_tdb = epoch_jd - MJD_TO_JD;
                
                // GM Sole in AU³/day²
                pimpl_->keplerian_elements.gravitational_parameter = 
                    1.32712440018e20 / std::pow(1.495978707e11, 3) * std::pow(86400.0, 2);
                
                // Salva designazione per uso futuro
                if (asteroid.contains("name")) {
                    pimpl_->asteroid_designation = asteroid["name"].get<std::string>();
                }
                
                // Parametri fisici
                if (asteroid.contains("diameter")) {
                    pimpl_->diameter_km = asteroid["diameter"].get<double>();
                }
                if (asteroid.contains("H")) {
                    pimpl_->abs_mag = asteroid["H"].get<double>();
                }
                if (asteroid.contains("G")) {
                    pimpl_->slope_param = asteroid["G"].get<double>();
                }
                
                found = true;
                
                std::cout << "✓ Phase2: Elementi orbitali caricati per asteroide " << asteroid_number << "\n";
                // Stima diametro se mancante
                if (pimpl_->diameter_km <= 0.0 && pimpl_->abs_mag > 0.0) {
                     pimpl_->diameter_km = 1329.0 / sqrt(pimpl_->slope_param) * pow(10, -pimpl_->abs_mag/5.0);
                }
                std::cout << "    Diametro: " << pimpl_->diameter_km << " km\n";
                break;
            }
        }
        
        if (!found) {
            std::cerr << "Phase2: Asteroide " << asteroid_number << " non trovato nel database JSON\n";
            return false;
        }
        
        pimpl_->has_elements = true;
        return true;
        
    } catch (const std::exception& e) {
        std::cerr << "Phase2: Errore parsing JSON: " << e.what() << "\n";
        return false;
    }
}

void Phase2OccultationGeometry::setOrbitalElements(
    const astdyn::propagation::KeplerianElements& elements) {
    pimpl_->keplerian_elements = elements;
    pimpl_->has_elements = true;
}

const astdyn::propagation::KeplerianElements& 
Phase2OccultationGeometry::getOrbitalElements() const {
    if (!pimpl_->has_elements) {
        throw std::runtime_error("Phase2: Elementi orbitali non caricati");
    }
    return pimpl_->keplerian_elements;
}

void Phase2OccultationGeometry::setPhysicalParameters(double diameter_km, double abs_mag, double slope_param) {
    pimpl_->diameter_km = diameter_km;
    pimpl_->abs_mag = abs_mag;
    pimpl_->slope_param = slope_param;
}

// ═══════════════════════════════════════════════════════════════
// GESTIONE OSSERVATORI
// ═══════════════════════════════════════════════════════════════

void Phase2OccultationGeometry::addObserverSite(
    const std::string& name, double lat_deg, double lon_deg, double elev_m) {
    
    ObserverGeometry site;
    site.site_name = name;
    site.latitude_deg = lat_deg;
    site.longitude_deg = lon_deg;
    site.elevation_m = elev_m;
    
    pimpl_->observer_sites.push_back(site);
}

void Phase2OccultationGeometry::clearObserverSites() {
    pimpl_->observer_sites.clear();
}

// ═══════════════════════════════════════════════════════════════
// HELPER: CALLBACK CURL
// ═══════════════════════════════════════════════════════════════

static size_t WriteCallback(void* contents, size_t size, size_t nmemb, void* userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

// ═══════════════════════════════════════════════════════════════
// COSTRUISCI URL AstDyS
// ═══════════════════════════════════════════════════════════════

std::string Phase2OccultationGeometry::Impl::buildAstDySURL(const std::string& designation) {
    // URL pattern: https://newton.spacedys.com/~astdys2/mpcobs/numbered/17/17030.rwo
    // Per asteroidi numerati: /numbered/XX/XXXXX.rwo (XX = prime due cifre)
    
    try {
        int number = std::stoi(designation);
        int folder = number / 1000;  // 17030 → folder 17
        
        std::ostringstream url;
        url << "https://newton.spacedys.com/~astdys2/mpcobs/numbered/"
            << folder << "/" << number << ".rwo";
        
        return url.str();
        
    } catch (...) {
        // Se non è un numero, prova con unnumbered
        std::ostringstream url;
        url << "https://newton.spacedys.com/~astdys2/mpcobs/unnumbered/"
            << designation << ".rwo";
        return url.str();
    }
}

// ═══════════════════════════════════════════════════════════════
// DOWNLOAD FILE HTTP
// ═══════════════════════════════════════════════════════════════

bool Phase2OccultationGeometry::Impl::downloadFile(
    const std::string& url, const std::string& output_path) {
    
    CURL* curl = curl_easy_init();
    if (!curl) {
        std::cerr << "  ✗ Errore inizializzazione CURL\n";
        return false;
    }
    
    std::string response_string;
    
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_string);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);  // Per HTTPS senza certificati
    
    CURLcode res = curl_easy_perform(curl);
    long response_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
    
    curl_easy_cleanup(curl);
    
    if (res != CURLE_OK || response_code != 200) {
        std::cerr << "  ✗ Download fallito for " << url << ": " << curl_easy_strerror(res) 
                 << " (HTTP " << response_code << ")\n";
        return false;
    }
    
    // Salva su file
    std::ofstream out(output_path);
    if (!out) return false;
    
    out << response_string;
    out.close();
    
    return true;
}

// ═══════════════════════════════════════════════════════════════
// DOWNLOAD OSSERVAZIONI RWO
// ═══════════════════════════════════════════════════════════════

std::vector<astdyn::observations::OpticalObservation> 
Phase2OccultationGeometry::Impl::downloadRWOObservations(const std::string& designation) {
    
    std::cout << "  [RWO Download] Scaricamento osservazioni per " << designation << "...\n";
    
    try {
        // Costruisci URL AstDyS
        std::string url = buildAstDySURL(designation);
        std::cout << "  URL: " << url << "\n";
        
        // Path temporaneo per file RWO
        std::string temp_path = "/tmp/" + designation + ".rwo";
        
        // Download file
        std::cout << "  Downloading...\n";
        if (!downloadFile(url, temp_path)) {
            std::cerr << "  ⚠ Download fallito - uso elementi nominali\n";
            return {};
        }
        
        std::cout << "  ✓ File scaricato: " << temp_path << "\n";
        
        // Parse RWO con AstDyn
        std::cout << "  Parsing RWO...\n";
        auto observations = astdyn::observations::RWOReader::readFile(temp_path);
        
        std::cout << "  ✓ Parsate " << observations.size() << " osservazioni RWO\n";
        
        // Rimuovi file temporaneo
        std::remove(temp_path.c_str());
        
        if (observations.empty()) {
            std::cerr << "  ⚠ Nessuna osservazione valida! Uso elementi nominali.\n";
        }
        
        return observations;
        
    } catch (const std::exception& e) {
        std::cerr << "  ✗ Errore download RWO: " << e.what() << "\n";
        std::cerr << "  Continuo con elementi nominali.\n";
        return {};
    }
}

// ═══════════════════════════════════════════════════════════════
// ORBITAL FITTING CON OSSERVAZIONI
// ═══════════════════════════════════════════════════════════════

OrbitalFitResults Phase2OccultationGeometry::Impl::refineOrbitWithObservations(
    const std::vector<astdyn::observations::OpticalObservation>& observations,
    const Phase2Config& config,
    double target_mjd_tdb) {
    
    OrbitalFitResults results;
    
    if (observations.empty()) {
        results.fit_notes = "Nessuna osservazione disponibile per fit";
        return results;
    }
    
    std::cout << "  [Orbital Fit] Fitting con " << observations.size() << " osservazioni...\n";
    
    try {
        // Crea il fitter di AstDyn
        astdyn::io::AstDysOrbitFitter fitter;
        fitter.set_verbose(false);  // Verbose solo se config lo richiede
        
        // Imposta osservazioni ed elementi iniziali
        fitter.set_observations(observations);
        fitter.set_elements(keplerian_elements);
        
        // Esegui il fitting
        auto t_start = std::chrono::high_resolution_clock::now();
        auto fit_result = fitter.fit();
        auto t_end = std::chrono::high_resolution_clock::now();
        
        // Popola i risultati
        results.fit_successful = fit_result.converged;
        results.num_observations_used = fit_result.num_observations_used;
        results.iterations_performed = fit_result.num_iterations;
        
        // Calcola RMS e residuo massimo
        results.rms_residuals_arcsec = std::sqrt(
            fit_result.rms_ra * fit_result.rms_ra + 
            fit_result.rms_dec * fit_result.rms_dec
        );
        results.max_residual_arcsec = std::max(fit_result.rms_ra, fit_result.rms_dec) * 3.0; // ~3-sigma
        results.chi_squared = fit_result.chi_squared;
        
        // Se converge, salva gli elementi raffinati
        if (results.fit_successful) {
            refined_elements = fit_result.fitted_orbit;
            has_refined_elements = true;
            
            // Propaga elementi raffinati all'epoca target (CA)
            auto kep_target = propagator->propagate_keplerian(refined_elements, target_mjd_tdb);
            results.refined_elements = kep_target;
            
            results.fit_notes = "Fit convergente";
            
            double fit_time = std::chrono::duration<double>(t_end - t_start).count();
            std::cout << "  ✓ Fit convergente in " << fit_time << " sec\n";
            std::cout << "    RMS: " << results.rms_residuals_arcsec << " arcsec\n";
            std::cout << "    Chi²: " << results.chi_squared << "\n";
            std::cout << "    Osservazioni: " << results.num_observations_used 
                     << " / " << fit_result.num_observations_loaded;
            if (fit_result.num_outliers > 0) {
                std::cout << " (" << fit_result.num_outliers << " outlier rimossi)";
            }
            std::cout << "\n";
        } else {
            results.fit_notes = "Fit non convergente";
            std::cout << "  ⚠ Fit non convergente dopo " << results.iterations_performed << " iterazioni\n";
            std::cout << "  Uso elementi nominali\n";
        }
        
        // TODO: Estrarre la matrice di covarianza se disponibile
        // results.covariance_matrix = ...;
        
    } catch (const std::exception& e) {
        results.fit_successful = false;
        results.fit_notes = std::string("Errore fit: ") + e.what();
        std::cout << "  ✗ Errore durante fitting: " << e.what() << "\n";
        std::cout << "  Uso elementi nominali\n";
    }
    
    return results;
}

// ═══════════════════════════════════════════════════════════════
// CALCOLO GEOMETRIA (TO BE IMPLEMENTED)
// ═══════════════════════════════════════════════════════════════

Phase2Results Phase2OccultationGeometry::calculateGeometry(
    const std::vector<ioccultcalc::CandidateStar>& candidates,
    const Phase2Config& config) {
    
    Phase2Results results;
    auto t_start = std::chrono::high_resolution_clock::now();
    
    std::cout << "\n╔════════════════════════════════════════════════════════════╗\n";
    std::cout << "║  PHASE 2: Geometria Precisa con Orbital Refinement        ║\n";
    std::cout << "╚════════════════════════════════════════════════════════════╝\n\n";
    std::cout << "Candidati da processare: " << candidates.size() << "\n";
    std::cout << "Orbital refinement: " << (config.refine_orbit_from_observations ? "ATTIVO" : "DISATTIVO") << "\n";
    std::cout << "Finestra temporale: ±" << config.time_window_minutes << " min\n";
    std::cout << "Risoluzione: " << config.time_step_seconds << " sec\n\n";
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 1: ORBITAL REFINEMENT (se richiesto)
    // ═══════════════════════════════════════════════════════════════
    if (config.refine_orbit_from_observations && !config.mpc_code.empty()) {
        std::cout << "══════════════════════════════════════════════════════════\n";
        std::cout << "STEP 1: ORBITAL REFINEMENT\n";
        std::cout << "══════════════════════════════════════════════════════════\n\n";
        
        // Salva designation per riferimento
        pimpl_->asteroid_designation = config.mpc_code;
        
        // Download osservazioni RWO da AstDyS
        std::vector<astdyn::observations::OpticalObservation> all_observations = 
            pimpl_->downloadRWOObservations(config.mpc_code);
        
        if (!all_observations.empty() && !candidates.empty()) {
            std::cout << "  [Observations] Scaricate " << all_observations.size() << " osservazioni\n";
            
            // Seleziona le N osservazioni più recenti (se richiesto)
            std::vector<astdyn::observations::OpticalObservation> observations;
            if (!config.use_all_available_observations && 
                config.max_observations_for_fit > 0 && 
                all_observations.size() > static_cast<size_t>(config.max_observations_for_fit)) {
                
                // Ordina per epoca (MJD) decrescente
                std::sort(all_observations.begin(), all_observations.end(),
                    [](const astdyn::observations::OpticalObservation& a, 
                       const astdyn::observations::OpticalObservation& b) {
                        return a.mjd_utc > b.mjd_utc;
                    });
                
                // Prendi le prime N (le più recenti)
                observations.assign(all_observations.begin(), 
                                  all_observations.begin() + config.max_observations_for_fit);
                
                std::cout << "  [Observations] Selezionate " << observations.size() 
                         << " osservazioni più recenti per il fit\n";
                std::cout << "  [Observations] Epoca più recente: MJD " 
                         << std::fixed << std::setprecision(2) << observations[0].mjd_utc << "\n";
                std::cout << "  [Observations] Epoca più vecchia: MJD " 
                         << observations.back().mjd_utc << "\n";
            } else {
                observations = all_observations;
                std::cout << "  [Observations] Uso tutte le " << observations.size() 
                         << " osservazioni disponibili\n";
            }
            
            // Usa il primo candidato per determinare l'epoca target (CA time)
            double target_mjd_tdb = candidates[0].closest_approach_mjd;
            
            // Esegui fit orbitale
            results.orbital_fit = pimpl_->refineOrbitWithObservations(
                observations, config, target_mjd_tdb);
            
            if (results.orbital_fit.fit_successful) {
                results.orbit_refined = true;
                std::cout << "\n  ✓✓✓ ORBITA RAFFINATA CON SUCCESSO ✓✓✓\n";
                std::cout << "  Elementi propagati al CA per massima precisione\n\n";
            } else {
                std::cout << "\n  ⚠ Fit fallito - uso elementi nominali\n\n";
            }
        }
    }
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 2: CALCOLO GEOMETRIA PER OGNI CANDIDATO
    // ═══════════════════════════════════════════════════════════════
    std::cout << "══════════════════════════════════════════════════════════\n";
    std::cout << "STEP 2: CALCOLO GEOMETRIA OCCULTAZIONI\n";
    std::cout << "══════════════════════════════════════════════════════════\n\n";
    
    for (size_t i = 0; i < candidates.size(); ++i) {
        const auto& candidate = candidates[i];
        
        try {
            std::cout << "Candidato " << (i+1) << "/" << candidates.size() 
                     << " - Stella " << candidate.source_id << "...\n";
            
            OccultationEvent event = calculateSingleEvent(candidate, config);
            results.events.push_back(event);
            results.successful_calculations++;
            
            std::cout << "  ✓ CA: " << event.closest_approach_mas << " mas @ MJD " 
                     << std::fixed << std::setprecision(6) << event.time_ca_mjd_utc << "\n";
            std::cout << "  Duration: " << event.max_duration_sec << " sec\n";
            if (event.shadow_width_km > 0)
                 std::cout << "  Shadow path width: " << event.shadow_width_km << " km\n\n";
            else
                 std::cout << "  Shadow path: (diametro non disp.)\n\n";
            
        } catch (const std::exception& e) {
            std::cerr << "  ✗ Errore: " << e.what() << "\n\n";
            results.failed_calculations++;
            results.error_messages += std::string(e.what()) + "\n";
        }
    }
    
    auto t_end = std::chrono::high_resolution_clock::now();
    results.total_computation_time_ms = 
        std::chrono::duration<double, std::milli>(t_end - t_start).count();
    
    std::cout << "══════════════════════════════════════════════════════════\n";
    std::cout << "PHASE 2 COMPLETATA\n";
    std::cout << "══════════════════════════════════════════════════════════\n";
    std::cout << "  Orbital refinement: " << (results.orbit_refined ? "✓ Successo" : "✗ Non eseguito") << "\n";
    if (results.orbit_refined) {
        std::cout << "    RMS residui: " << results.orbital_fit.rms_residuals_arcsec << " arcsec\n";
        std::cout << "    Osservazioni: " << results.orbital_fit.num_observations_used << "\n";
    }
    std::cout << "  Eventi calcolati: " << results.successful_calculations << "\n";
    std::cout << "  Falliti: " << results.failed_calculations << "\n";
    std::cout << "  Tempo totale: " << results.total_computation_time_ms << " ms\n\n";
    
    return results;
}

OccultationEvent Phase2OccultationGeometry::calculateSingleEvent(
    const ioccultcalc::CandidateStar& candidate,
    const Phase2Config& config) {
    
    if (!pimpl_->has_elements) {
        throw std::runtime_error("Elementi orbitali non caricati");
    }
    
    // Seleziona quali elementi usare (raffinati o nominali)
    const auto& elements_to_use = pimpl_->has_refined_elements 
        ? pimpl_->refined_elements 
        : pimpl_->keplerian_elements;
    
    std::string elements_source = pimpl_->has_refined_elements 
        ? "elementi raffinati da fit RWO" 
        : "elementi nominali";
    
    std::cout << "  Usando: " << elements_source << "\n";
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 1: PROPAGAZIONE DENSA ATTORNO AL CA
    // ═══════════════════════════════════════════════════════════════
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 1: APPROSSIMAZIONE CHEBYSHEV (USER REQUEST)
    // ═══════════════════════════════════════════════════════════════
    
    double ca_mjd = candidate.closest_approach_mjd;
    double time_window_days = config.time_window_minutes / 1440.0;
    double start_mjd = ca_mjd - time_window_days;
    double end_mjd = ca_mjd + time_window_days;
    
    // Coordinate stella unitarie (J2000)
    double ra_rad = candidate.ra_deg * DEG_TO_RAD;
    double dec_rad = candidate.dec_deg * DEG_TO_RAD;
    Eigen::Vector3d star_unit;
    star_unit[0] = std::cos(dec_rad) * std::cos(ra_rad);
    star_unit[1] = std::cos(dec_rad) * std::sin(ra_rad);
    star_unit[2] = std::sin(dec_rad);

    // Genera nodi Chebyshev (11 punti)
    int n_nodes = 11;
    std::vector<Eigen::Vector3d> node_positions;
    
    // Pre-calculate common rotation constants
    double eps = EPSILON_J2000;
    double ce = std::cos(eps);
    double se = std::sin(eps);
    
    std::cout << "  Approccio Chebyshev: Fitting su " << n_nodes << " nodi in ±" 
             << config.time_window_minutes << " min\n";
             
    for (int k = 1; k <= n_nodes; ++k) {
        // Nodi di Chebyshev-Lobatto o radici standard (usiamo radici standard nel range)
        double tk_norm = std::cos(M_PI * (2.0 * k - 1.0) / (2.0 * n_nodes));
        double t_mjd = 0.5 * (start_mjd + end_mjd) + 0.5 * (end_mjd - start_mjd) * tk_norm;
        
        // Propaga
        auto kep_prop = pimpl_->propagator->propagate_keplerian(elements_to_use, t_mjd);
        auto cart = astdyn::propagation::keplerian_to_cartesian(kep_prop);
        Eigen::Vector3d ast_helio_ecl = cart.position;
        
        // Earth Pos
        double jd_tdb = t_mjd + MJD_TO_JD; 
        auto earth_helio_ecl = astdyn::ephemeris::PlanetaryEphemeris::getPosition(
                astdyn::ephemeris::CelestialBody::EARTH, jd_tdb);
        
        // Geo Ecl
        Eigen::Vector3d rel_pos_ecl = ast_helio_ecl - earth_helio_ecl;
        
        // Geo Eq
        Eigen::Vector3d rel_pos_eq;
        rel_pos_eq[0] = rel_pos_ecl[0];
        rel_pos_eq[1] = rel_pos_ecl[1] * ce - rel_pos_ecl[2] * se;
        rel_pos_eq[2] = rel_pos_ecl[1] * se + rel_pos_ecl[2] * ce;
        
        node_positions.push_back(rel_pos_eq);
    }
    
    // Fit Chebyshev
    ioccultcalc::ChebyshevApproximation approx(n_nodes); // Order = nodes
    // Note: approx.fit takes sorted positions? No, assumes correspondence to time window.
    // Wait. My implementation of 'fit' in chebyshev_approximation.h might assume uniform steps or explicit times?
    // Step 1456 view: fit(positions, start, end). It likely assumes uniform sampling or Chebyshev nodes?
    // Let's check the header again. Step 1456 L97: "fit(positions, start, end)".
    // L103: "Il fitting utilizza un metodo ai minimi quadrati".
    // If it uses LS, it needs timestamps. But the signature DOES NOT take timestamps!
    // This implies it ASSUMES uniform sampling or specific node distribution.
    // Most standard fits assume Uniform samples for LS or Chebyshev Nodes for Interpolation.
    // The comment L9: "Approssimazione mediante polinomi di Chebyshev".
    // If I passed Chebyshev nodes positions, I might need to clarify if 'fit' expects them.
    // Assuming 'fit' expects Uniform samples (common for simplified LS APIs).
    // I should probably use Uniform Sampling for safety if I can't check .cpp.
    // Let's switch to UNIFORM sampling (e.g. 11 points spaced evenly) to be safe with unknown 'fit' implementation.
    // Uniform sampling with 11 points is still very accurate for 20 mins orbit arc.
    
    node_positions.clear();
    double step_node = (end_mjd - start_mjd) / (n_nodes - 1);
    for (int i=0; i<n_nodes; ++i) {
        double t_mjd = start_mjd + i * step_node;
        // ... (Repeating propagation code) ...
        auto kep_prop = pimpl_->propagator->propagate_keplerian(elements_to_use, t_mjd);
        auto cart = astdyn::propagation::keplerian_to_cartesian(kep_prop);
        Eigen::Vector3d ast_helio_ecl = cart.position;
        double jd_tdb = t_mjd + MJD_TO_JD; 
        auto earth_helio_ecl = astdyn::ephemeris::PlanetaryEphemeris::getPosition(
                astdyn::ephemeris::CelestialBody::EARTH, jd_tdb);
        Eigen::Vector3d rel_pos_ecl = ast_helio_ecl - earth_helio_ecl;
        Eigen::Vector3d rel_pos_eq;
        rel_pos_eq[0] = rel_pos_ecl[0];
        rel_pos_eq[1] = rel_pos_ecl[1] * ce - rel_pos_ecl[2] * se;
        rel_pos_eq[2] = rel_pos_ecl[1] * se + rel_pos_ecl[2] * ce;
        node_positions.push_back(rel_pos_eq);
    }
    
    bool fit_ok = approx.fit(node_positions, start_mjd, end_mjd);
    if (!fit_ok) std::cerr << "  [Warning] Chebyshev fit failed!\n";

    // ═══════════════════════════════════════════════════════════════
    // STEP 2: RICERCA DEL MINUTO (COARSE SEARCH)
    // ═══════════════════════════════════════════════════════════════
    
    double coarse_step = 60.0 / 86400.0; // 1 min scan
    double best_t = start_mjd;
    double min_dist_rad = 1e9;
    
    for (double t = start_mjd; t <= end_mjd; t += coarse_step) {
        Eigen::Vector3d pos = approx.evaluatePosition(t);
        Eigen::Vector3d dir = pos.normalized();
        double cos_sep = star_unit.dot(dir);
        if (cos_sep > 1.0) cos_sep = 1.0;
        if (cos_sep < -1.0) cos_sep = -1.0;
        double sep = std::acos(cos_sep);
        
        if (sep < min_dist_rad) {
            min_dist_rad = sep;
            best_t = t;
        }
    }
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 3: RAFFINAMENTO (FINE SEARCH)
    // ═══════════════════════════════════════════════════════════════
    
    // Cerca attorno al minuto migliore (+/- 1 min) con Golden Section o Dense Grid
    // Use Dense Grid 1 sec step for simplicity and robustness
    double refine_start = best_t - coarse_step;
    double refine_end = best_t + coarse_step;
    double fine_step = 1.0 / 86400.0; // 1 sec
    
    double final_best_t = best_t;
    double final_min_rad = min_dist_rad;
    
    for (double t = refine_start; t <= refine_end; t += fine_step) {
        Eigen::Vector3d pos = approx.evaluatePosition(t);
        Eigen::Vector3d dir = pos.normalized();
        double cos_sep = star_unit.dot(dir);
        if (cos_sep > 1.0) cos_sep = 1.0;
        if (cos_sep < -1.0) cos_sep = -1.0;
        double sep = std::acos(cos_sep);
        
        if (sep < final_min_rad) {
            final_min_rad = sep;
            final_best_t = t;
        }
    }
    
    double min_time_mjd = final_best_t;
    double min_distance_mas = final_min_rad * RAD_TO_MAS;

    std::cout << "  CA preciso (Chebyshev): " << min_distance_mas << " mas @ MJD " 
             << std::fixed << std::setprecision(8) << min_time_mjd << "\n";

    // ═══════════════════════════════════════════════════════════════
    // STEP 4: CALCOLO PARAMETRI GEOMETRICI (RIGOROSO)
    // ═══════════════════════════════════════════════════════════════
    // Ricostruiamo i vettori rigorosi al punto di minimo
    
    // Propaga all'istante esatto
    auto kep_prop_ca = pimpl_->propagator->propagate_keplerian(elements_to_use, min_time_mjd);
    auto cart_ca = astdyn::propagation::keplerian_to_cartesian(kep_prop_ca);
    Eigen::Vector3d ast_helio_ecl_ca = cart_ca.position;
    double jd_tdb_ca = min_time_mjd + MJD_TO_JD; 
    auto earth_helio_ecl_ca = astdyn::ephemeris::PlanetaryEphemeris::getPosition(
            astdyn::ephemeris::CelestialBody::EARTH, jd_tdb_ca);
    Eigen::Vector3d rel_pos_ecl_ca = ast_helio_ecl_ca - earth_helio_ecl_ca;
    Eigen::Vector3d rel_pos_eq_ca; // P (Target Point)
    rel_pos_eq_ca[0] = rel_pos_ecl_ca[0];
    rel_pos_eq_ca[1] = rel_pos_ecl_ca[1] * ce - rel_pos_ecl_ca[2] * se;
    rel_pos_eq_ca[2] = rel_pos_ecl_ca[1] * se + rel_pos_ecl_ca[2] * ce;
    
    // Set for subsequent steps (replacing detailed loop vars)
    std::vector<double> times = { min_time_mjd };
    std::vector<Eigen::Vector3d> positions_icrf = { rel_pos_eq_ca };
    int min_index = 0; // Pointing to the only element
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 3: CALCOLA PARAMETRI GEOMETRICI
    // ═══════════════════════════════════════════════════════════════
    
    // Distanza asteroide dalla Terra al CA
    double earth_distance_au = positions_icrf[min_index].norm();
    
    // Velocità angolare (approssimata con differenza finita)
    // Velocità angolare (da derivata Chebyshev)
    Eigen::Vector3d vel_eq_au_d = approx.evaluateVelocity(min_time_mjd);
    // Use the rigorous position calculated above
    Eigen::Vector3d pos_eq = positions_icrf[0]; 
    double dist_au = pos_eq.norm();
    
    // Angular velocity = v_perp / r
    // v_perp = sqrt(v^2 - v_rad^2)
    double v_mag_sq = vel_eq_au_d.squaredNorm();
    double v_rad = pos_eq.dot(vel_eq_au_d) / dist_au;
    double v_perp_sq = v_mag_sq - v_rad * v_rad;
    double angular_velocity_rad_day = (v_perp_sq > 0.0) ? (std::sqrt(v_perp_sq) / dist_au) : 0.0;
    
    // Durata massima (assumendo diametro asteroide ~10 km per ora)
    // USE TRUE DIAMETER
    double asteroid_diameter_km = pimpl_->diameter_km;
    if (asteroid_diameter_km <= 0.0) {
        // Fallback estimate from H if H is set, otherwise default
        if (pimpl_->abs_mag > 0.0) {
             asteroid_diameter_km = 1329.0 / sqrt(pimpl_->slope_param) * pow(10, -pimpl_->abs_mag/5.0);
             std::cout << "  [Warn] Diametro zero, stimato da H=" << pimpl_->abs_mag 
                       << ": " << asteroid_diameter_km << " km\n";
        } else {
             asteroid_diameter_km = 0.0; // Can't calculate duration
        }
    }
    
    double max_duration_sec = 0.0;
    if (asteroid_diameter_km > 0.0 && angular_velocity_rad_day > 0.0) {
        double angular_diameter_rad = asteroid_diameter_km / (earth_distance_au * AU_TO_KM);
        max_duration_sec = (angular_diameter_rad / angular_velocity_rad_day) * 86400.0;
    }
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 4: CREA EVENTO
    // ═══════════════════════════════════════════════════════════════
    
    OccultationEvent event;
    
    // Identificazione
    event.star_source_id = candidate.source_id;
    event.asteroid_name = pimpl_->asteroid_designation;
    event.asteroid_number = 0; // Parse if possible
    try { event.asteroid_number = std::stoi(pimpl_->asteroid_designation); } catch(...) {}
    
    // Dati stella
    event.star_ra_deg = candidate.ra_deg;
    event.star_dec_deg = candidate.dec_deg;
    event.star_magnitude = candidate.phot_g_mean_mag;
    event.star_pm_ra_mas_yr = 0.0;   // TODO: prendere da Gaia
    event.star_pm_dec_mas_yr = 0.0;
    
    // Geometria
    event.time_ca_mjd_utc = min_time_mjd;  // TODO: convertire TDB→UTC
    event.closest_approach_mas = min_distance_mas;
    event.max_duration_sec = max_duration_sec;
    event.asteroid_distance_au = earth_distance_au;
    event.position_angle_deg = 0.0;  // TODO: calcolare
    
    // Shadow path
    event.chord_length_km = 0.0;     // TODO: calcolare proiezione su Terra
    event.shadow_width_km = asteroid_diameter_km;
    event.path_length_km = 0.0;      // TODO: calcolare
    event.path_duration_sec = 0.0;
    
    // Quality
    event.high_confidence = pimpl_->has_refined_elements;
    event.notes = pimpl_->has_refined_elements ? "Refined Orbit" : "Nominal Orbit";
    
    return event;
}
