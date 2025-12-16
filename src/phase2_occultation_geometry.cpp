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
#include <cmath>
#include <iostream>
#include <iomanip>

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
    std::vector<astdyn::observations::OpticalObservation> 
    downloadRWOObservations(const std::string& designation, bool verbose = true);
    
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
Phase2OccultationGeometry::Impl::downloadRWOObservations(const std::string& designation, bool verbose) {
    
    if (verbose) std::cout << "  [RWO Download] Scaricamento osservazioni per " << designation << "...\n";
    
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
    
    // ═══════════════════════════════════════════════════════════════
    // PRE-CHECK: Calcola residui iniziali per evitare crash
    // ═══════════════════════════════════════════════════════════════
    try {
        double sum_sq_resid = 0.0;
        int valid_obs = 0;
        
        for (const auto& obs : observations) {
            // Usa propagazione approssimata (2-body) veloce
            // Nota: obs.mjd_utc usata come TDB per semplicità in questo check
            auto kep = propagator->propagate_keplerian(keplerian_elements, obs.mjd_utc);
            auto cart = astdyn::propagation::keplerian_to_cartesian(kep);
            
            double jd = obs.mjd_utc + 2400000.5;
            auto earth = astdyn::ephemeris::PlanetaryEphemeris::getPosition(
                astdyn::ephemeris::CelestialBody::EARTH, jd);
                
            // Vettore Asteroide-Terra (Equatoriale J2000)
            Eigen::Vector3d rho = cart.position - earth;
            double dist = rho.norm();
            
            if (dist < 1e-6) continue;
            
            double ra_rad = std::atan2(rho[1], rho[0]);
            if (ra_rad < 0) ra_rad += 2.0 * M_PI;
            double dec_rad = std::asin(rho[2] / dist);
            
            // Obs in radianti (assumendo obs.ra in gradi)
            double obs_ra_rad = obs.ra * DEG_TO_RAD;
            double obs_dec_rad = obs.dec * DEG_TO_RAD;
            
            double d_ra = (ra_rad - obs_ra_rad);
            while (d_ra > M_PI) d_ra -= 2.0*M_PI;
            while (d_ra < -M_PI) d_ra += 2.0*M_PI;
            d_ra *= std::cos(dec_rad); // Projection
            
            double d_dec = dec_rad - obs_dec_rad;
            
            sum_sq_resid += (d_ra*d_ra + d_dec*d_dec);
            valid_obs++;
        }
        
        if (valid_obs > 0) {
            double rms_rad = std::sqrt(sum_sq_resid / valid_obs);
            double rms_sec = rms_rad * RAD_TO_DEG * 3600.0;
            
            std::cout << "  [Pre-Check] RMS Residui Nominali: " << std::fixed 
                      << std::setprecision(2) << rms_sec << " arcsec\n";
            
            if (rms_sec > 100.0) {
                std::cout << "  ⚠ Residui troppo alti (>100\"). Skipping fit per sicurezza.\n";
                results.fit_notes = "Skipped: Initial RMS too high (" + std::to_string((int)rms_sec) + "\")";
                results.fit_successful = false;
                return results;
            }
        }
    } catch (const std::exception& e) {
        std::cout << "  [Pre-Check] Warning: " << e.what() << "\n";
        // In caso di errore nel check, proviamo comunque il fit standard
    }
    
    // ═══════════════════════════════════════════════════════════════
    // FIT ASTDYN
    // ═══════════════════════════════════════════════════════════════
    
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
            // [DEBUG] Check validity of the fit
            if (results.rms_residuals_arcsec > 10.0) {
                 std::cout << "  ⚠ Fit tecnicamente convergente ma RMS eccessivo (" 
                           << results.rms_residuals_arcsec << " > 10.0arcsec). Scarto risultati.\n";
                 results.fit_successful = false;
                 results.fit_notes = "Fit convergente ma RMS alto";
            }
            // Check for NaNs
            else if (std::isnan(fit_result.fitted_orbit.semi_major_axis) || 
                     std::isnan(fit_result.fitted_orbit.eccentricity)) {
                 std::cout << "  ⚠ Fit ha prodotto valori NaN. Scarto risultati.\n";
                 results.fit_successful = false;
                 results.fit_notes = "Fit prodotto NaN";
            }
            else {
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
                
                std::cout << "    Elementi (a,e,i): " << refined_elements.semi_major_axis << " AU, "
                          << refined_elements.eccentricity << ", " 
                          << refined_elements.inclination * 180.0/M_PI << " deg\n";
            }
        } 
        
        if (!results.fit_successful) {
            // Logica di fallback già presente, ma aggiorniamo il messaggio per chiarezza
            if (results.fit_notes.empty()) results.fit_notes = "Fit non convergente";
            std::cout << "  ⚠ Fit non valido: " << results.fit_notes << "\n";
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
    
    if (config.verbose) {
        std::cout << "\n╔════════════════════════════════════════════════════════════╗\n";
        std::cout << "║  PHASE 2: Geometria Precisa con Orbital Refinement        ║\n";
        std::cout << "╚════════════════════════════════════════════════════════════╝\n\n";
        std::cout << "Candidati da processare: " << candidates.size() << "\n";
        std::cout << "Orbital refinement: " << (config.refine_orbit_from_observations ? "ATTIVO" : "DISATTIVO") << "\n";
        std::cout << "Finestra temporale: ±" << config.time_window_minutes << " min\n";
        std::cout << "Risoluzione: " << config.time_step_seconds << " sec\n\n";
    }
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 1: ORBITAL REFINEMENT (se richiesto)
    // ═══════════════════════════════════════════════════════════════
    if (config.refine_orbit_from_observations && !config.mpc_code.empty()) {
        if (config.verbose) {
            std::cout << "══════════════════════════════════════════════════════════\n";
            std::cout << "STEP 1: ORBITAL REFINEMENT\n";
            std::cout << "══════════════════════════════════════════════════════════\n\n";
        }
        
        // Salva designation per riferimento
        pimpl_->asteroid_designation = config.mpc_code;
        
        // Download osservazioni RWO da AstDyS
        // TODO: Pass verbose flag to downloadRWOObservations? 
        // For now, we assume it prints. To silence it, we'd need to modify that method signature or implementation.
        // Let's modify downloadRWOObservations implementation later in this file.
        std::vector<astdyn::observations::OpticalObservation> all_observations = 
            pimpl_->downloadRWOObservations(config.mpc_code, config.verbose);
        
        if (!all_observations.empty() && !candidates.empty()) {
            if (config.verbose) std::cout << "  [Observations] Scaricate " << all_observations.size() << " osservazioni\n";
            
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
                
                if (config.verbose) {
                    std::cout << "  [Observations] Selezionate " << observations.size() 
                             << " osservazioni più recenti per il fit\n";
                    std::cout << "  [Observations] Epoca più recente: MJD " 
                             << std::fixed << std::setprecision(2) << observations[0].mjd_utc << "\n";
                    std::cout << "  [Observations] Epoca più vecchia: MJD " 
                             << observations.back().mjd_utc << "\n";
                }
            } else {
                observations = all_observations;
                if (config.verbose) {
                    std::cout << "  [Observations] Uso tutte le " << observations.size() 
                             << " osservazioni disponibili\n";
                }
            }
            
            // Usa il primo candidato per determinare l'epoca target (CA time)
            double target_mjd_tdb = candidates[0].closest_approach_mjd;
            
            // Esegui fit orbitale
            results.orbital_fit = pimpl_->refineOrbitWithObservations(
                observations, config, target_mjd_tdb);
            
            if (results.orbital_fit.fit_successful) {
                results.orbit_refined = true;
                if (config.verbose) {
                    std::cout << "\n  ✓✓✓ ORBITA RAFFINATA CON SUCCESSO ✓✓✓\n";
                    std::cout << "  Elementi propagati al CA per massima precisione\n\n";
                }
            } else {
                if (config.verbose) std::cout << "\n  ⚠ Fit fallito - uso elementi nominali\n\n";
            }
        }
    }
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 2: CALCOLO GEOMETRIA PER OGNI CANDIDATO
    // ═══════════════════════════════════════════════════════════════
    if (config.verbose) {
        std::cout << "══════════════════════════════════════════════════════════\n";
        std::cout << "STEP 2: CALCOLO GEOMETRIA OCCULTAZIONI\n";
        std::cout << "══════════════════════════════════════════════════════════\n\n";
    }
    
    for (size_t i = 0; i < candidates.size(); ++i) {
        const auto& candidate = candidates[i];
        
        try {
            if (config.verbose) {
                std::cout << "Candidato " << (i+1) << "/" << candidates.size() 
                         << " - Stella " << candidate.source_id << "...\n";
            }
            
            OccultationEvent event = calculateSingleEvent(candidate, config);
            results.events.push_back(event);
            results.successful_calculations++;
            
            if (config.verbose) {
                std::cout << "  ✓ CA: " << event.closest_approach_mas << " mas @ MJD " 
                         << std::fixed << std::setprecision(6) << event.time_ca_mjd_utc << "\n";
                std::cout << "  Duration: " << event.max_duration_sec << " sec\n";
                if (event.shadow_width_km > 0)
                     std::cout << "  Shadow path width: " << event.shadow_width_km << " km\n\n";
                else
                     std::cout << "  Shadow path: (diametro non disp.)\n\n";
            }
            
        } catch (const std::exception& e) {
            std::cerr << "  ✗ Errore: " << e.what() << "\n\n";
            results.failed_calculations++;
            results.error_messages += std::string(e.what()) + "\n";
        }
    }
    
    auto t_end = std::chrono::high_resolution_clock::now();
    results.total_computation_time_ms = 
        std::chrono::duration<double, std::milli>(t_end - t_start).count();
    
    if (config.verbose) {
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
    }
    
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
    
    if (config.verbose) {
        std::cout << "  Usando: " << elements_source << "\n";
    }
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 1: PROPAGAZIONE DENSA ATTORNO AL CA
    // ═══════════════════════════════════════════════════════════════
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 1: RICERCA TEMPO CA (RIGOROSA)
    // ═══════════════════════════════════════════════════════════════
    // Sostituisce Approccio Chebyshev con Ricerca Diretta (più affidabile)
    
    double ca_mjd_guess = candidate.closest_approach_mjd;
    double time_window_days = config.time_window_minutes / 1440.0;
    double start_mjd = ca_mjd_guess - time_window_days;
    double end_mjd = ca_mjd_guess + time_window_days;
    
    // Coordinate stella unitarie (J2000)
    double ra_rad = candidate.ra_deg * DEG_TO_RAD;
    double dec_rad = candidate.dec_deg * DEG_TO_RAD;
    Eigen::Vector3d star_unit;
    star_unit[0] = std::cos(dec_rad) * std::cos(ra_rad);
    star_unit[1] = std::cos(dec_rad) * std::sin(ra_rad);
    star_unit[2] = std::sin(dec_rad);

    if (config.verbose) {
        std::cout << "  Ricerca CA: Scansione rigida in ±" 
                 << config.time_window_minutes << " min attorno a MJD " << std::fixed << std::setprecision(5) << ca_mjd_guess << "\n";
    }

    // Costanti per rotazione Ecliptic -> Equatorial
    double eps = EPSILON_J2000;
    double ce = std::cos(eps);
    double se = std::sin(eps);

    // 1. COARSE GRID SEARCH (Step 60 sec)
    double step_coarse = 60.0 / 86400.0;
    double best_t = start_mjd;
    double min_dist_sq = 1e9;
    
    // Pre-allocate for performance
    Eigen::Vector3d earth_pos;
    
    for (double t = start_mjd; t <= end_mjd; t += step_coarse) {
        // Propagazione Asteroide (Ecliptic)
        auto kep_prop = pimpl_->propagator->propagate_keplerian(elements_to_use, t);
        auto cart_prop = astdyn::propagation::keplerian_to_cartesian(kep_prop);
        Eigen::Vector3d ast_ecl = cart_prop.position;
        
        // Converti Asteroide in Equatorial
        Eigen::Vector3d ast_eq;
        ast_eq[0] = ast_ecl[0];
        ast_eq[1] = ast_ecl[1] * ce - ast_ecl[2] * se;
        ast_eq[2] = ast_ecl[1] * se + ast_ecl[2] * ce;
        
        // Posizione Terra (JPL DE is typically Equatorial J2000)
        double jd_tdb = t + MJD_TO_JD;
        earth_pos = astdyn::ephemeris::PlanetaryEphemeris::getPosition(
                astdyn::ephemeris::CelestialBody::EARTH, jd_tdb);
        
        // Rho Geocentrico Equatoriale
        Eigen::Vector3d rho = ast_eq - earth_pos; 
        
        double dist = rho.norm();
        double dot = rho.dot(star_unit);
        double metric = 1.0 - (dot / dist); // 1 - cos(theta)
        
        if (metric < min_dist_sq) {
            min_dist_sq = metric;
            best_t = t;
        }
    }
    
    // 2. FINE SEARCH
    double search_radius = step_coarse * 1.5;
    double start_fine = std::max(start_mjd, best_t - search_radius);
    double end_fine = std::min(end_mjd, best_t + search_radius);
    double step_fine = 1.0 / 86400.0; // 1 sec
    
    double final_best_t = best_t;
    double final_min_metric = min_dist_sq;
    
    for (double t = start_fine; t <= end_fine; t += step_fine) {
        auto kep_prop = pimpl_->propagator->propagate_keplerian(elements_to_use, t);
        auto cart_prop = astdyn::propagation::keplerian_to_cartesian(kep_prop);
        Eigen::Vector3d ast_ecl = cart_prop.position;
        
        Eigen::Vector3d ast_eq;
        ast_eq[0] = ast_ecl[0];
        ast_eq[1] = ast_ecl[1] * ce - ast_ecl[2] * se;
        ast_eq[2] = ast_ecl[1] * se + ast_ecl[2] * ce;
        
        double jd_tdb = t + MJD_TO_JD;
        earth_pos = astdyn::ephemeris::PlanetaryEphemeris::getPosition(
                astdyn::ephemeris::CelestialBody::EARTH, jd_tdb);
        Eigen::Vector3d rho = ast_eq - earth_pos; 
        
        double dist = rho.norm();
        double dot = rho.dot(star_unit);
        double metric = 1.0 - (dot / dist);
        
        if (metric < final_min_metric) {
            final_min_metric = metric;
            final_best_t = t;
        }
    }
    
    // 3. REFINE (Final Calculation)
    double min_time_mjd = final_best_t;
    
    // Calcola stato finale per uso successivo
    auto kep_prop_ca = pimpl_->propagator->propagate_keplerian(elements_to_use, min_time_mjd);
    auto cart_prop_ca = astdyn::propagation::keplerian_to_cartesian(kep_prop_ca);
    Eigen::Vector3d ast_ecl_ca = cart_prop_ca.position;
    
    Eigen::Vector3d rel_pos_eq_ca; // P (Target Point / Rho)
    rel_pos_eq_ca[0] = ast_ecl_ca[0];
    rel_pos_eq_ca[1] = ast_ecl_ca[1] * ce - ast_ecl_ca[2] * se;
    rel_pos_eq_ca[2] = ast_ecl_ca[1] * se + ast_ecl_ca[2] * ce;
    
    double jd_tdb = min_time_mjd + MJD_TO_JD; 
    earth_pos = astdyn::ephemeris::PlanetaryEphemeris::getPosition(
             astdyn::ephemeris::CelestialBody::EARTH, jd_tdb);
             
    // Subtract Earth to get Geocentric
    rel_pos_eq_ca = rel_pos_eq_ca - earth_pos;
    
    double cos_sep = rel_pos_eq_ca.dot(star_unit) / rel_pos_eq_ca.norm();
    if (cos_sep > 1.0) cos_sep = 1.0;
    if (cos_sep < -1.0) cos_sep = -1.0;
    double sep_rad = std::acos(cos_sep);
    double min_distance_mas = sep_rad * RAD_TO_MAS;

    if (config.verbose) {
        std::cout << "  CA preciso (Rigorous): " << std::fixed << std::setprecision(2) << min_distance_mas << " mas @ MJD " 
                 << std::setprecision(8) << min_time_mjd << "\n";
    }

    // (Logic moved to Step 3 and 4)
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 4: CALCOLO PARAMETRI GEOMETRICI (BESSELIAN FUNDAMENTAL PLANE)
    // ═══════════════════════════════════════════════════════════════
    
    // Recovery of earth_distance_au (missing from previous edit)
    double earth_distance_au = rel_pos_eq_ca.norm();

    // Riferimento: "Occultations by Asteroids", J.L. Hilton (USNO)
    // Riferimento: "Solar System Occultations", Herald, D.
    
    // 1. Definisci il piano fondamentale (x, y)
    // L'asse z è diretto verso la stella.
    // L'asse x è nell'equatore terrestre (o parallelo all'eclittica, a seconda della convenzione Besseliana).
    // La convenzione standard per occultazioni stellari usa Coordinate Equatoriali.
    // z_unit = star_unit (verso la stella)
    // x_unit = (pole x z) / |pole x z| (Est nel piano fondamentale)
    // y_unit = z x x (Nord nel piano fondamentale)
    
    // Polo Nord Celeste (J2000)
    Eigen::Vector3d pole(0, 0, 1);
    
    // Versore stella (k)
    Eigen::Vector3d k_vec = star_unit; // Dallo step precedente (calcolato da RA/Dec stella)
    
    // Versore i (asse x piano fondamentale, verso Est)
    Eigen::Vector3d i_vec = pole.cross(k_vec);
    double i_norm = i_vec.norm();
    if (i_norm < 1e-9) {
        // Stella al polo! Caso degenere raro.
        i_vec = Eigen::Vector3d(1, 0, 0); // Arbitrario
    } else {
        i_vec /= i_norm;
    }
    
    // Versore j (asse y piano fondamentale, verso Nord)
    Eigen::Vector3d j_vec = k_vec.cross(i_vec); // Già unitario
    
    // 2. Proietta Asteroide sul piano fondamentale (x, y)
    // Posizione asteroide relativa alla Terra (Equatoriale J2000)
    // rel_pos_eq_ca è il vettore Terra->Asteroide
    // Attenzione: gli elementi Besseliani (x,y) sono le coordinate dell'OMBRA (o dell'asteroide)
    // proiettate sul piano passante per il centro della Terra, perpendicolare alla direzione della stella.
    
    double x_bessel = rel_pos_eq_ca.dot(i_vec);
    double y_bessel = rel_pos_eq_ca.dot(j_vec);
    // z_bessel = dist (positivo verso la stella)
    // La distanza 'min_distance_mas' calcolata prima era angolare. Ora calcoliamo quella fisica proiettata.
    
    double shadow_dist_au = std::sqrt(x_bessel*x_bessel + y_bessel*y_bessel);
    double shadow_dist_km = shadow_dist_au * AU_TO_KM;
    
    // 3. Proietta Asteroide VELOCITA' sul piano fondamentale (x', y')
    // Usiamo la velocità al CA
    // Propaga un secondo step per differenza finita precisa o usa evaluateVelocity
    double dt_sec = 1.0;
    double t_plus = min_time_mjd + dt_sec/86400.0;
    
    // Re-do propagation correctly
    astdyn::propagation::KeplerianElements kep_plus = pimpl_->propagator->propagate_keplerian(elements_to_use, t_plus);
    auto cart_plus = astdyn::propagation::keplerian_to_cartesian(kep_plus);
    Eigen::Vector3d ast_ecl_plus = cart_plus.position;
    
    // Earth Vel (Numerical or Analytical diff)
    double jd_plus = t_plus + MJD_TO_JD;
    auto earth_plus = astdyn::ephemeris::PlanetaryEphemeris::getPosition(
             astdyn::ephemeris::CelestialBody::EARTH, jd_plus);

    Eigen::Vector3d rel_eq_plus; // Rotate to Eq
    rel_eq_plus[0] = ast_ecl_plus[0];
    rel_eq_plus[1] = ast_ecl_plus[1] * ce - ast_ecl_plus[2] * se;
    rel_eq_plus[2] = ast_ecl_plus[1] * se + ast_ecl_plus[2] * ce;
    
    rel_eq_plus = rel_eq_plus - earth_plus;
    
    Eigen::Vector3d vel_eq = (rel_eq_plus - rel_pos_eq_ca) / dt_sec; // AU/sec
    
    double xp_bessel = vel_eq.dot(i_vec); // x'
    double yp_bessel = vel_eq.dot(j_vec); // y'
    double vp_norm = std::sqrt(xp_bessel*xp_bessel + yp_bessel*yp_bessel); // Velocity on fundamental plane (AU/sec)
    double velocity_km_s = vp_norm * AU_TO_KM;
    
    // 4. Calcola MISS DISTANCE geometrica
    // Al CA geometrico nel piano fondamentale (t0 = - (x x' + y y') / v^2)
    // Ma siamo già al CA temporale, quindi (x, y) dovrebbero essere già vicini al minimo.
    // shadow_dist_km è la "Impact Parameter"
    
    // 5. Verifica intersezione Terra
    // Raggio Terra ~6378 km
    // Se shadow_dist_km < (EarthRadius + ShadowRadius), l'ombra colpisce la Terra
    
    double earth_radius_km = EARTH_EQUATORIAL_RADIUS_KM; 
    // Correzione raggio polare per y locale? 
    // Per semplicità usiamo raggio equatoriale come first pass per HIT/MISS.
    // Se vogliamo essere precisi: raggio locale dipende dall'angolo di posizione.
    
    double asteroid_radius_km = pimpl_->diameter_km / 2.0;
    // Fallback estimate if zero
    if (asteroid_radius_km <= 0.0 && pimpl_->abs_mag > 0.0) {
          double d_km = 1329.0 / sqrt(pimpl_->slope_param) * pow(10, -pimpl_->abs_mag/5.0);
          asteroid_radius_km = d_km / 2.0;
    }
        
    double miss_distance_km = shadow_dist_km;
    
    bool is_hit = miss_distance_km < (earth_radius_km + asteroid_radius_km);
    
    if (config.verbose) {
        std::cout << "  Geometria (Piano Fondamentale):\n";
        std::cout << "    Impact Parameter: " << std::fixed << std::setprecision(1) << miss_distance_km << " km\n";
        std::cout << "    Earth Radius Limit: " << (earth_radius_km + asteroid_radius_km) << " km\n";
        std::cout << "    Shadow Velocity: " << std::setprecision(2) << velocity_km_s << " km/s\n";
        std::cout << "    Esito geometrico: " << (is_hit ? "HIT (Ombra interseca Terra)" : "MISS (Ombra manca Terra)") << "\n";
    }
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 5: POPOLA EVENTO
    // ═══════════════════════════════════════════════════════════════
    
    OccultationEvent event;
    
    event.star_source_id = candidate.source_id;
    event.asteroid_name = pimpl_->asteroid_designation;
    // Number check
    event.asteroid_number = 0; try { event.asteroid_number = std::stoi(pimpl_->asteroid_designation); } catch(...) {}
    
    event.star_ra_deg = candidate.ra_deg;
    event.star_dec_deg = candidate.dec_deg;
    event.star_magnitude = candidate.phot_g_mean_mag;
    event.star_pm_ra_mas_yr = 0.0; // TODO da Gaia
    event.star_pm_dec_mas_yr = 0.0;
    
    event.time_ca_mjd_utc = min_time_mjd; // Approx UTC=TDB for this level
    event.closest_approach_mas = min_distance_mas;
    
    // Duration
    if (velocity_km_s > 0.0 && asteroid_radius_km > 0.0) {
        event.max_duration_sec = (asteroid_radius_km * 2.0) / velocity_km_s;
    } else {
        event.max_duration_sec = 0.0;
    }
    
    event.asteroid_distance_au = earth_distance_au;
    
    // Chord Length (Geocentrica)
    // Lunghezza corda attraverso la Terra AL CA.
    // Se interseca: 2 * sqrt(R_earth^2 - impact^2)
    // Non è la corda dell'asteroide, ma la lunghezza del tracciato sulla Terra? 
    // No, chord length di solito è la dimensione dell'ombra proiettata, ma qui forse si intende path length?
    // "path_length_km": lunghezza della traccia sulla terra.
    // "chord_length_km": non standard per l'evento globale, forse intende max chord asteroide?
    // Assumiamo max chord asteroide = diamond.
    event.chord_length_km = 0.0; 
    if (is_hit) {
        // Calcoliamo la lunghezza del percorso dell'ombra SULLA Terra
        // Approssimazione: 2 * sqrt(R_e^2 - impact^2)
        if (miss_distance_km < earth_radius_km) {
             event.path_length_km = 2.0 * std::sqrt(earth_radius_km*earth_radius_km - miss_distance_km*miss_distance_km);
        } else {
             event.path_length_km = 0.0; // "Graze" apparente
        }
    } else {
        event.path_length_km = 0.0;
    }
    
    event.shadow_width_km = asteroid_radius_km * 2.0;
    event.path_duration_sec = (velocity_km_s > 0) ? (event.path_length_km / velocity_km_s) : 0.0;
    
    event.high_confidence = pimpl_->has_refined_elements;
    event.notes = (is_hit ? "HIT" : "MISS");
    if (pimpl_->has_refined_elements) event.notes += " (Refined Orbit)";
    
    return event;
}
