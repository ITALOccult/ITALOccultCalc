/**
 * @file phase2_occultation_geometry.cpp
 * @brief Restyling professionale basato su euristiche CfYH
 */

#include "phase2_occultation_geometry.h"
#include <Eigen/Dense>
#include <iostream>
#include <memory>
#include <cmath>
#include <algorithm>
#include <fstream>
#include <sstream>
#include <curl/curl.h>

#include "ioccultcalc/converters.h"
#include "ioccultcalc/orbital_conversions.h"
#include "astdyn_wrapper.h"
#include "ioccultcalc/occultation_predictor.h" // Ensure this is included for OccultationPredictor and OccultationEvent

// Estraiamo la logica di trasformazione per renderla testabile (Funzione Pura)
namespace {
    /**
     * @brief Ruota uno stato da Eclittico J2000 a Equatoriale J2000
     */
    void rotateEclipticToEquatorial(Eigen::Vector3d& pos, Eigen::Vector3d& vel) {
        const double eps = 23.4392911 * M_PI / 180.0;
        Eigen::Matrix3d R;
        R << 1, 0, 0,
             0, std::cos(eps), -std::sin(eps),
             0, std::sin(eps),  std::cos(eps);
        pos = R * pos;
        vel = R * vel;
    }
}

namespace ioccultcalc {

class Phase2OccultationGeometry::Impl {
public:
    std::shared_ptr<ISPReader> spk_reader;
    std::unique_ptr<OccultationPredictor> predictor;
    std::unique_ptr<AstDynWrapper> astdyn_wrapper;
    
    // Stato dell'asteroide
    struct AsteroidState {
        std::string name;
        std::string designation;
        double diameter = 0.0;
        double H = 0.0;
        double G = 0.15;
        astdyn::propagation::KeplerianElements elements;
        astdyn::propagation::KeplerianElements refined_elements;
        bool refined = false;
        bool has_elements = false;
        Eigen::MatrixXd covariance;
        bool has_covariance = false;
    } asteroid;

    std::vector<ObserverGeometry> observer_sites;

    explicit Impl(std::shared_ptr<ISPReader> reader) 
        : spk_reader(std::move(reader)) {
        astdyn_wrapper = std::make_unique<AstDynWrapper>(PropagationSettings::highAccuracy());
        
        // Ensure SPICE reader is valid or create a fallback/default one could go here
        // But we assume the passed reader is what we want.
        // If spk_reader is null, we might want to try to initialize a default one safely
        if (!spk_reader) {
             const char* home = std::getenv("HOME");
             if (home) {
                 // Try to locate default kernels
                 // This logic was previously inline, cleaner here or in a factory
             }
        }
        
        predictor = std::make_unique<OccultationPredictor>(spk_reader);
    }
    
    // Helper to download files (implementation kept for compatibility/completeness)
    bool downloadFile(const std::string& url, const std::string& output_path);
    std::string buildAstDySURL(const std::string& designation, const std::string& extension);
    bool downloadAstDysFiles(const std::string& designation, bool verbose);


    /**
     * @brief Business Logic: Calcola la geometria per una stella specifica
     */
    Phase2OccultationEvent processSingleCandidate(const ioccultcalc::CandidateStar& star_data, const Phase2Config& config);
    
    // Helper for uncertainty
    UncertaintyEllipse calculateProjectedUncertainty(const OccultationEvent& raw_event);
    
    Phase2Results calculateGeometry(const std::vector<ioccultcalc::CandidateStar>& candidates,
                                  const Phase2Config& config);

    // Stub for iterative refinement
    OrbitalFitResults refineOrbitWithObservations(
        const std::vector<ioccultcalc::Observation>& observations,
        const Phase2Config& config,
        double target_mjd_tdb);
        
    // Download RWO stub
    std::vector<ioccultcalc::Observation> downloadRWOObservations(const std::string& designation, bool verbose);

    // Download EQ1 stub
    bool downloadEQ1Elements(const std::string& designation, bool verbose);

    // Helpers CfYH
    Eigen::Vector3d getEarthPositionEquatorial(double jd);
    Eigen::Vector3d getApparentTopocentricVector(AstDynWrapper* wrapper, const Eigen::Vector3d& earthPosEqu, double jd);
    
    // GSS Refinement Implementation
    double computeAngularDistanceAt(double t_mjd, const ioccultcalc::GaiaStar& star);
    double findPreciseCATime(double t_nominal_mjd, const ioccultcalc::GaiaStar& star);
    
    // Epoch Verification (CfYH)
    void verifyElementEpoch(double eventMJD, const Phase2Config& config);
};


// ═══════════════════════════════════════════════════════════════
// COSTRUTTORE / DISTRUTTORE
// ═══════════════════════════════════════════════════════════════

Phase2OccultationGeometry::Phase2OccultationGeometry() 
    : pimpl_(std::make_unique<Impl>(nullptr)) {
}

Phase2OccultationGeometry::Phase2OccultationGeometry(std::shared_ptr<ISPReader> reader)
    : pimpl_(std::make_unique<Impl>(std::move(reader))) {
}

Phase2OccultationGeometry::~Phase2OccultationGeometry() = default;

// ═══════════════════════════════════════════════════════════════
// CARICAMENTO ELEMENTI ORBITALI
// ═══════════════════════════════════════════════════════════════

bool Phase2OccultationGeometry::loadAsteroidFromEQ1(const std::string& eq1_path) {
    if (pimpl_->astdyn_wrapper->loadFromEQ1File(eq1_path)) {
        pimpl_->asteroid.has_elements = true;
        pimpl_->asteroid.elements = pimpl_->astdyn_wrapper->getKeplerianElements();
        pimpl_->asteroid.designation = pimpl_->astdyn_wrapper->getObjectName();
        pimpl_->asteroid.name = pimpl_->astdyn_wrapper->getObjectName();
        
        auto cov = pimpl_->astdyn_wrapper->getCovariance();
        if (cov) {
             pimpl_->asteroid.covariance = *cov;
             pimpl_->asteroid.has_covariance = true;
        }
        return true;
    }
    return false;
}

bool Phase2OccultationGeometry::loadAsteroidFromJSON(int asteroid_number, const std::string& json_path) {
    // Keep existing JSON loading logic but simplify
    try {
        std::string path = json_path;
        if (path.empty()) {
            const char* home = std::getenv("HOME");
            if (home) {
                path = std::string(home) + "/.ioccultcalc/data/all_numbered_asteroids.json";
            } else {
                return false;
            }
        }
        
        std::ifstream file(path);
        if (!file.is_open()) return false;
        
        nlohmann::json j;
        file >> j;
        
        if (!j.contains("asteroids")) return false;
        
        for (const auto& asteroid : j["asteroids"]) {
            if (asteroid["number"].get<int>() == asteroid_number) {
                pimpl_->asteroid.elements.semi_major_axis = asteroid["a"].get<double>();
                pimpl_->asteroid.elements.eccentricity = asteroid["e"].get<double>();
                pimpl_->asteroid.elements.inclination = asteroid["i"].get<double>() * (M_PI/180.0); // Convert to radians
                pimpl_->asteroid.elements.longitude_ascending_node = (asteroid.contains("Node") ? asteroid["Node"].get<double>() : asteroid["Omega"].get<double>()) * (M_PI/180.0);
                pimpl_->asteroid.elements.argument_perihelion = (asteroid.contains("Peri") ? asteroid["Peri"].get<double>() : asteroid["omega"].get<double>()) * (M_PI/180.0);
                pimpl_->asteroid.elements.mean_anomaly = asteroid["M"].get<double>() * (M_PI/180.0);
                pimpl_->asteroid.elements.epoch_mjd_tdb = asteroid["epoch"].get<double>() - 2400000.5;
                
                pimpl_->asteroid.designation = std::to_string(asteroid_number);
                pimpl_->asteroid.name = asteroid.contains("name") ? asteroid["name"].get<std::string>() : pimpl_->asteroid.designation;
                
                pimpl_->asteroid.H = asteroid.value("H", 99.0);
                pimpl_->asteroid.diameter = asteroid.value("diameter", 0.0);
                if (pimpl_->asteroid.diameter <= 0.0 && pimpl_->asteroid.H < 30.0) {
                     pimpl_->asteroid.diameter = 1329.0 / std::sqrt(pimpl_->asteroid.G) * std::pow(10, -pimpl_->asteroid.H/5.0);
                }
                
                pimpl_->asteroid.has_elements = true;
                return true;
            }
        }
    } catch (...) {
        return false;
    }
    return false;
}

void Phase2OccultationGeometry::setOrbitalElements(
    const astdyn::propagation::KeplerianElements& elements,
    const std::string& name,
    FrameType frame,
    ElementType type) {
    
    pimpl_->asteroid.name = name;
    pimpl_->asteroid.designation = name; // Simplified
    pimpl_->asteroid.elements = elements;
    pimpl_->asteroid.has_elements = true;
    
    // Pass to wrapper immediately
    pimpl_->astdyn_wrapper->setKeplerianElements(
        elements.semi_major_axis,
        elements.eccentricity,
        elements.inclination,
        elements.longitude_ascending_node,
        elements.argument_perihelion,
        elements.mean_anomaly,
        elements.epoch_mjd_tdb,
        name,
        frame,
        type
    );
    
    if (elements.covariance) {
        pimpl_->asteroid.covariance = *elements.covariance;
        pimpl_->asteroid.has_covariance = true;
    }
}

const astdyn::propagation::KeplerianElements& Phase2OccultationGeometry::getOrbitalElements() const {
    if (!pimpl_->asteroid.has_elements) throw std::runtime_error("No elements loaded");
    return pimpl_->asteroid.elements;
}

void Phase2OccultationGeometry::setPhysicalParameters(double diameter_km, double abs_mag, double slope_param) {
    pimpl_->asteroid.diameter = diameter_km;
    pimpl_->asteroid.H = abs_mag;
    pimpl_->asteroid.G = slope_param;
    
    if (pimpl_->predictor) {
        pimpl_->predictor->setAsteroidDiameter(diameter_km);
    }
}

void Phase2OccultationGeometry::addObserverSite(const std::string& name, double lat_deg, double lon_deg, double elev_m) {
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
// CORE BUSINESS LOGIC
// ═══════════════════════════════════════════════════════════════

Phase2Results Phase2OccultationGeometry::calculateGeometry(
    const std::vector<ioccultcalc::CandidateStar>& candidates,
    const Phase2Config& config) {
    
    return pimpl_->calculateGeometry(candidates, config);
}

Phase2Results Phase2OccultationGeometry::Impl::calculateGeometry(
    const std::vector<ioccultcalc::CandidateStar>& candidates,
    const Phase2Config& config) {

    Phase2Results results;
    
    // Auto-download logic for completeness (simplified)
    if (config.auto_download_astdys && !asteroid.has_elements && !asteroid.designation.empty()) {
         downloadAstDysFiles(asteroid.designation, config.verbose);
    }

    if (!asteroid.has_elements) {
        std::cerr << "Phase2: Critical Error - No orbital elements available.\n";
        return results;
    }

    std::cout << "\n╔════════════════════════════════════════════════════════════╗\n";
    std::cout << "║  PHASE 2: Geometria Precisa (Clean Architecture)          ║\n";
    std::cout << "╚════════════════════════════════════════════════════════════╝\n";
    std::cout << "Processing " << candidates.size() << " candidates for asteroid " << asteroid.name << "\n";
    std::cout << "Diameter: " << asteroid.diameter << " km\n";

    // Setup Predictor with current asteroid state
    // CRITICAL: Ensure AstDyn elements (Ecliptic) are passed correctly
    // But here we rely on AstDynWrapper for propagation if we do manual steps,
    // OR we pass Equinoctial elements to Predictor.
    
    // Update predictor with implicit transformation if needed
    // The key fix from user is: 
    // "Trasformiamo i Kepleriani di AstDyn in Equinoziali Equatoriali per il Predictor"
    
    auto current_elements = asteroid.refined ? asteroid.refined_elements : asteroid.elements;
    
    // Convert to Equinoctial for the predictor
    // NOTE: Predictor expects `AstDynEquinoctialElements` struct from `types.h`
    // We need to construct it carefully.
    
    AstDynEquinoctialElements eq_elements = AstDynEquinoctialElements::fromKeplerian(
        current_elements.semi_major_axis,
        current_elements.eccentricity,
        current_elements.inclination,
        current_elements.argument_perihelion,
        current_elements.longitude_ascending_node,
        current_elements.mean_anomaly,
        JulianDate(current_elements.epoch_mjd_tdb + 2400000.5)
    );
    
    // Set explicit frame - CRITICAL
    // AstDyn elements are typically Ecliptic.
    eq_elements.frame = FrameType::ECLIPTIC_J2000; 
    
    // Pass covariance if available
    if (asteroid.has_covariance) {
        eq_elements.hasCovariance = true;
        eq_elements.covariance.resize(6, std::vector<double>(6));
        for(int r=0; r<6; r++)
            for(int c=0; c<6; c++)
                eq_elements.covariance[r][c] = asteroid.covariance(r,c);
    }
    
    predictor->setAsteroid(eq_elements);
    predictor->setAsteroidDiameter(asteroid.diameter);

    int idx = 0;
    for (const auto& candidate : candidates) {
        idx++;
        std::cout << "Candidato " << idx << "/" << candidates.size() << " - Stella " << candidate.source_id << "...";
        
        try {
            Phase2OccultationEvent event = processSingleCandidate(candidate, config);
            
            if (event.max_duration_sec > 0.0) {
                 results.events.push_back(event);
                 std::cout << "\n  ✓ CA: " << event.closest_approach_mas << " mas @ MJD " << event.time_ca_mjd_utc << "\n";
                 std::cout << "  Duration: " << event.max_duration_sec << " sec\n";
            } else {
                 std::cout << " (Skipped: Duration 0)\n";
            }
        } catch (const std::exception& e) {
            std::cout << " Error: " << e.what() << "\n";
        }
    }
    
    return results;
}

Phase2OccultationEvent Phase2OccultationGeometry::calculateSingleEvent(
    const ioccultcalc::CandidateStar& candidate, const Phase2Config& config) {
    if (pimpl_->asteroid.diameter <= 0) {
        // Fallback estimation or throw
        if (pimpl_->asteroid.H < 30) {
             pimpl_->asteroid.diameter = 1329.0 / std::sqrt(0.15) * std::pow(10, -pimpl_->asteroid.H/5.0);
             std::cout << "Warning: Estimated diameter to " << pimpl_->asteroid.diameter << " km\n";
        } else {
             throw std::runtime_error("Physical parameters missing for asteroid " + pimpl_->asteroid.name);
        }
    }
    return pimpl_->processSingleCandidate(candidate, config);
}


    /**
     * @brief Conversione Cartesiano -> RA/Dec (Equatoriale)
     */
    static void cartesianToRaDec(const Eigen::Vector3d& pos, double& ra_rad, double& dec_rad) {
        double r = pos.norm();
        ra_rad = std::atan2(pos.y(), pos.x());
        if (ra_rad < 0) ra_rad += 2.0 * M_PI;
        dec_rad = std::asin(pos.z() / r);
    }

    /**
     * @brief Distanza angolare precisa (Haversine/Vincenty per piccoli angoli)
     */
    static double angularDistance(double ra1, double dec1, double ra2, double dec2) {
        // Formula del coseno sferico standard per ora, sufficiente per il check
        // Per piccoli angoli meglio Haversine, ma qui stiamo checkando > 1 grado
         double sin_dDec_2 = std::sin((dec2 - dec1) / 2.0);
         double sin_dRa_2 = std::sin((ra2 - ra1) / 2.0);
         double a = sin_dDec_2 * sin_dDec_2 +
                    std::cos(dec1) * std::cos(dec2) * sin_dRa_2 * sin_dRa_2;
         return 2.0 * std::atan2(std::sqrt(a), std::sqrt(1.0 - a));
    }

    /**
     * @brief Recupera posizione Terra da SPICE in J2000 (ICRF)
     */
    // --- DEBUG HELPER ---
    static void debugVector(const std::string& label, const Eigen::Vector3d& vec, const std::string& frame) {
        double ra = std::atan2(vec.y(), vec.x()) * 180.0 / M_PI;
        if (ra < 0) ra += 360.0;
        double dec = std::asin(vec.z() / vec.norm()) * 180.0 / M_PI;
        std::printf("[DEBUG %-10s] %-15s | RA: %8.4f deg Dec: %8.4f deg | Norm: %14.9f\n", 
                    frame.c_str(), label.c_str(), ra, dec, vec.norm());
    }
    // --------------------

    Eigen::Vector3d Phase2OccultationGeometry::Impl::getEarthPositionEquatorial(double jd) {
        // Tenta prima con AstDyn Ephemeris (statico, include fallback analitico VSOP87)
        try {
            // Nota: AstDyn PlanetaryEphemeris usa JD TDB e restituisce Eclittico J2000
            using namespace astdyn::ephemeris;
            Eigen::Vector3d posEcl = PlanetaryEphemeris::getPosition(CelestialBody::EARTH, jd);
            
            // Ruota in Equatoriale
            Eigen::Vector3d posEqu = posEcl;
            Eigen::Vector3d velDummy = Eigen::Vector3d::Zero();
            // Funzione helper namespace anonimo
            rotateEclipticToEquatorial(posEqu, velDummy);
            
            // 3. Ispezione Terra (da AstDyn/SPICE)
            debugVector("Earth_JPL", posEqu, "EQU_J2000");

            return posEqu;
        } catch (...) {
            // Se fallisce, prova spk_reader diretto
        }

        if (!spk_reader) {
            // Fallback pericoloso: non abbiamo la Terra precisa. Assumiamo (0,0,0) elio (NON ACCETTABILE fase 2)
            // Lanciamo eccezione
            throw std::runtime_error("SPICE required for Earth position");
        }
        // NAIF ID: 399 (Earth), Center: 10 (Sun)
        auto state = spk_reader->getState(399, jd, 10); 
        Eigen::Vector3d posEqu(state.first.x, state.first.y, state.first.z);
        debugVector("Earth_JPL", posEqu, "EQU_J2000");
        return posEqu;
    }

    /**
     * @brief Calcola vettore topocentrico apparente asteroid-terra
     */
    Eigen::Vector3d Phase2OccultationGeometry::Impl::getApparentTopocentricVector(AstDynWrapper* wrapper, const Eigen::Vector3d& earthPosEqu, double jd) {
        // 1. Propagate asteroid (Returns ICRF/Equatorial from AstDynWrapper)
        auto state_equ = wrapper->propagateToEpoch(jd - 2400000.5); 
        
        Eigen::Vector3d astPosEqu(state_equ.position.x(), state_equ.position.y(), state_equ.position.z());
        
        // 2. Convergenza Light Time check
        double dist = (astPosEqu - earthPosEqu).norm();
        double c_au_day = 173.1446327; // High precision c
        double lt = 0.0;
        
        std::printf("  [LT START] Initial Dist: %.6f AU\n", dist);
        for(int i=0; i<3; ++i) {
             double old_lt = lt;
             lt = dist / c_au_day;
             std::printf("  [LT Iter %d] Delta_Tau: %10.4e sec | LT: %.6f sec\n", i, (lt - old_lt) * 86400.0, lt * 86400.0);
             
             // Repropagate at t - lt
             auto s = wrapper->propagateToEpoch((jd - lt) - 2400000.5);
             astPosEqu = Eigen::Vector3d(s.position.x(), s.position.y(), s.position.z());
             dist = (astPosEqu - earthPosEqu).norm();
        }
        
        Eigen::Vector3d rho_vec = astPosEqu - earthPosEqu;
        // 4. Ispezione Vettore Relativo
        debugVector("Rel_Vector", rho_vec, "EQU_J2000");
        
        return rho_vec;
    }

    /**
     * @brief Verifica la consistenza degli elementi prima di processare
     */
    void Phase2OccultationGeometry::Impl::verifyElementEpoch(double eventMJD, const Phase2Config& config) {
        double epochDiff = std::abs(asteroid.elements.epoch_mjd_tdb - eventMJD);
        std::cout << "[DEBUG_EPOCH] Epoch: " << asteroid.elements.epoch_mjd_tdb 
                  << " Event: " << eventMJD << " Diff: " << epochDiff << " days\n";
        
        if (epochDiff > 100.0) { // Se gli elementi sono più vecchi di 100 giorni
            std::cout << "[WARNING] Element epoch is " << epochDiff 
                      << " days away from event (Epoch=" << asteroid.elements.epoch_mjd_tdb << ", Event=" << eventMJD << "). Precision will be LOW.\n";
            
            // Force High Accuracy N-Body Propagation
            std::cout << "  -> Forcing High Accuracy N-Body Propagation Settings.\n";
            PropagationSettings s = PropagationSettings::highAccuracy();
            // Ensure all perturbations are ON
            s.include_planets = true;
            s.include_asteroids = true; 
            s.include_relativity = true;
            astdyn_wrapper->setSettings(s);
            
            // Se possibile, forza il download di elementi aggiornati (.eq1)
            if (config.auto_download_astdys) {
                downloadEQ1Elements(asteroid.designation, true);
            }
        }
    }

    /**
     * @brief Funzione obiettivo per la minimizzazione
     */
    double Phase2OccultationGeometry::Impl::computeAngularDistanceAt(double t_mjd, const ioccultcalc::GaiaStar& star) {
        Eigen::Vector3d earthPosEqu = getEarthPositionEquatorial(t_mjd + 2400000.5);
        Eigen::Vector3d rho_vec = getApparentTopocentricVector(astdyn_wrapper.get(), earthPosEqu, t_mjd + 2400000.5);
        
        double ra_ast, dec_ast;
        cartesianToRaDec(rho_vec, ra_ast, dec_ast);
        
        return angularDistance(ra_ast, dec_ast, star.pos.ra * M_PI/180.0, star.pos.dec * M_PI/180.0);
    }

    /**
     * @brief Golden Section Search per il Closest Approach
     */
    double Phase2OccultationGeometry::Impl::findPreciseCATime(double t_nominal_mjd, const ioccultcalc::GaiaStar& star) {
        const double phi = (1.0 + std::sqrt(5.0)) / 2.0;
        const double resphi = 2.0 - phi;
        
        // Intervallo di ricerca: +/- 10 minuti
        double a = t_nominal_mjd - (10.0 / 1440.0);
        double b = t_nominal_mjd + (10.0 / 1440.0);
        
        double x1 = a + resphi * (b - a);
        double x2 = b - resphi * (b - a);
        
        double f1 = computeAngularDistanceAt(x1, star);
        double f2 = computeAngularDistanceAt(x2, star);
        
        // Tolleranza: 1 millisecondo (1e-8 giorni circa)
        int max_iter = 50;
        int iter = 0;
        while (std::abs(b - a) > 1e-8 && iter++ < max_iter) {
            if (f1 < f2) {
                b = x2; x2 = x1; f2 = f1;
                x1 = a + resphi * (b - a);
                f1 = computeAngularDistanceAt(x1, star);
            } else {
                a = x1; x1 = x2; f1 = f2;
                x2 = b - resphi * (b - a);
                f2 = computeAngularDistanceAt(x2, star);
            }
        }
        return (a + b) / 2.0;
    }

    Phase2OccultationEvent Phase2OccultationGeometry::Impl::processSingleCandidate(const ioccultcalc::CandidateStar& star_data, const Phase2Config& config) {
        // 1. Data Setup
        ioccultcalc::GaiaStar star = ioccultcalc::converters::toGaiaStar(star_data);
        
        // 0. Verify Epoch & Setup Propagator (CfYH)
        verifyElementEpoch(star_data.closest_approach_mjd, config);

        // Load elements first!
        auto current_elements = asteroid.refined ? asteroid.refined_elements : asteroid.elements;
        astdyn_wrapper->setKeplerianElements(
            current_elements.semi_major_axis, current_elements.eccentricity, current_elements.inclination,
            current_elements.longitude_ascending_node, current_elements.argument_perihelion,
            current_elements.mean_anomaly, current_elements.epoch_mjd_tdb,
            asteroid.name, FrameType::ECLIPTIC_J2000
        );

        // 1.1 GSS Refinement (CfYH)
        double t_mjd = findPreciseCATime(star_data.closest_approach_mjd, star);
        auto julian_date = JulianDate(t_mjd + 2400000.5);



        // 2. Load Elements (Already done above for GSS)
        // ... skipped setKeplerianElements call as it is persistent in wrapper ...

        // 3. Validation Logic (The "fail-fast" check)
        Eigen::Vector3d earthPosEqu = getEarthPositionEquatorial(julian_date.jd);
        Eigen::Vector3d rho_vec = getApparentTopocentricVector(astdyn_wrapper.get(), earthPosEqu, julian_date.jd);
        
        double ra_ast, dec_ast;
        cartesianToRaDec(rho_vec, ra_ast, dec_ast);
        
        double dist_rad = angularDistance(ra_ast, dec_ast, star.pos.ra * M_PI/180.0, star.pos.dec * M_PI/180.0);
        double dist_mas = dist_rad * (180.0/M_PI) * 3600.0 * 1000.0;
        
        std::cout << "--- FINAL GEOMETRY CHECK ---\n";
        std::cout << "Target Star RA:  " << star.pos.ra << " deg\n";
        double ra_ast_deg = ra_ast * 180.0/M_PI;
        std::cout << "Asteroid RA:     " << ra_ast_deg << " deg\n";
        std::cout << "Diff RA (mas):   " << (ra_ast_deg - star.pos.ra) * 3600000.0 << " mas\n";
        
        if (dist_mas > 3600000.0 * 1.0) { // > 1 degree tolerance
             // Fail fast but gracefully by returning empty event? Or Throw?
             // User requested: throw std::runtime_error
             // But to keep the loop running for other candidates, maybe meaningful skip?
             // The prompt implies we want to confirm the discrepancy. Throwing stops execution.
             // We'll throw to signal "Divergenza orbitale" as requested.
             throw std::runtime_error("Divergenza orbitale: l'asteroide e' troppo lontano dalla stella (" + std::to_string(dist_mas/1000.0/3600.0) + " deg).");
        }

        // 4. Safe Procedure: Run High Precision Predictor
        // Prepare Elements for Predictor (Rotated to Equatorial)
        // Note: The Wrapper is Ecliptic. The Predictor needs Equatorial.
        // Or we pass Ecliptic and let Predictor rotate?
        // Earlier we saw Predictor + Ecliptic was ~94deg. 
        // The check above calculated Ast position ourselves.
        // Let's pass Explicitly Rotated elements to Predictor to be consistent with our Check!
        
        // Use our manual rotation that we know resulted in the [CHECK] coordinates
        // Actually, converting Orbital Elements is cleaner.
        
        // Re-calculate KepEqu from our AstPosEqu/VelEqu?
        // Or simpler: Trust Predictor if we pass FrameType::ECLIPTIC_J2000 and it handles it?
        // If Predictor handles SPICE Earth, it should match our check.
        // Let's stick to standard flow now that Check passed.
        
        AstDynEquinoctialElements eq_elements = AstDynEquinoctialElements::fromKeplerian(
             current_elements.semi_major_axis, current_elements.eccentricity, current_elements.inclination,
             current_elements.argument_perihelion, current_elements.longitude_ascending_node, 
             current_elements.mean_anomaly, JulianDate(current_elements.epoch_mjd_tdb + 2400000.5)
        );
        eq_elements.frame = FrameType::ECLIPTIC_J2000;
        if (asteroid.has_covariance) {
            eq_elements.hasCovariance = true;
            eq_elements.covariance.resize(6, std::vector<double>(6));
            for(int r=0; r<6; r++) for(int c=0; c<6; c++) eq_elements.covariance[r][c] = asteroid.covariance(r,c);
        }
        
        // Predictor internally manages Earth pos via same SPICE reader, should result in similar values.
        predictor->setAsteroid(eq_elements);
        predictor->setAsteroidDiameter(asteroid.diameter);

        auto raw_event = predictor->predictOccultation(star, julian_date);
        
        // 5. Mapping
        Phase2OccultationEvent event;
        event.star_source_id = std::stoll(star.sourceId);
        event.star_ra_deg = star.pos.ra; 
        event.star_dec_deg = star.pos.dec;
        event.star_magnitude = star.phot_g_mean_mag;
        event.asteroid_name = asteroid.name;
        // ... (rest of mapping)
        event.asteroid_number = 0; try { event.asteroid_number = std::stoi(asteroid.designation); } catch(...) {}
        
        event.time_ca_mjd_utc = raw_event.timeCA.jd - 2400000.5;
        event.closest_approach_mas = raw_event.closeApproachDistance * 1000.0; // raw_event in arcsec? Predictor says event.closeApproachDistance is arcsec.
        // Warning: raw_event.closeApproachDistance in Predictor.cpp says: = std::acos(...) * RAD_TO_DEG * 3600.0; -> So it IS Arcseconds.
        // So * 1000 is MAS. Correct.
        
        event.max_duration_sec = raw_event.maxDuration;
        event.shadow_width_km = asteroid.diameter;
        event.shadow_velocity_kms = raw_event.besselianDX; 
        event.utc_string = raw_event.utc_string;
        event.mag_drop = raw_event.magnitudeDrop;
        event.h_mag = asteroid.H;
        event.asteroid_distance_au = raw_event.asteroidDistanceAu;
        event.uncertainty = calculateProjectedUncertainty(raw_event);

        for (const auto& pt : raw_event.shadowPath) {
            Phase2ShadowPathPoint ppt;
            ppt.time_mjd_utc = pt.time.jd - 2400000.5;
            ppt.latitude_deg = pt.location.latitude;
            ppt.longitude_deg = pt.location.longitude;
            event.shadow_path.push_back(ppt);
        }

        return event;
    }

UncertaintyEllipse Phase2OccultationGeometry::Impl::calculateProjectedUncertainty(const OccultationEvent& raw_event) {
    UncertaintyEllipse ue;
    ue.semi_major_axis_mas = raw_event.uncertaintyNorth * 1000.0; // Simplification, need full covariance mapping
    ue.semi_minor_axis_mas = raw_event.uncertaintySouth * 1000.0;
    ue.position_angle_deg = 0.0;
    
    // If we had the B-Plane covariance in raw_event, we would diagonalize it here.
    return ue;
}

// ═══════════════════════════════════════════════════════════════
// DOWNLOAD HELPERS (Keep existing logic simplified)
// ═══════════════════════════════════════════════════════════════

static size_t WriteCallback(void* contents, size_t size, size_t nmemb, void* userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

std::string Phase2OccultationGeometry::Impl::buildAstDySURL(const std::string& designation, const std::string& extension) {
    std::string base_path = (extension == "rwo") ? "https://newton.spacedys.com/~astdys2/mpcobs/" : "https://newton.spacedys.com/~astdys2/epoch/";
    try {
        int number = std::stoi(designation);
        int folder = number / 1000;
        std::ostringstream url;
        url << base_path << "numbered/" << folder << "/" << number << "." << extension;
        return url.str();
    } catch (...) {
        return "";
    }
}

bool Phase2OccultationGeometry::Impl::downloadFile(const std::string& url, const std::string& output_path) {
    CURL* curl = curl_easy_init();
    if (!curl) return false;
    std::string response;
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
    CURLcode res = curl_easy_perform(curl);
    long code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &code);
    curl_easy_cleanup(curl);
    if (res != CURLE_OK || code != 200) return false;
    std::ofstream out(output_path);
    if (out) { out << response; out.close(); return true; }
    return false;
}

bool Phase2OccultationGeometry::Impl::downloadAstDysFiles(const std::string& designation, bool verbose) {
    // Basic implementation: attempt EQ1 download
    std::string url = buildAstDySURL(designation, "eq1");
    std::string path = "/tmp/" + designation + ".eq1";
    if (downloadFile(url, path)) {
        if (astdyn_wrapper->loadFromEQ1File(path)) {
             asteroid.has_elements = true;
             asteroid.elements = astdyn_wrapper->getKeplerianElements();
             auto cov = astdyn_wrapper->getCovariance();
             if (cov) { asteroid.covariance = *cov; asteroid.has_covariance = true; }
             return true;
        }
    }
    return false;
}

// Stub for iterative refinement
OrbitalFitResults Phase2OccultationGeometry::Impl::refineOrbitWithObservations(
    const std::vector<ioccultcalc::Observation>& observations,
    const Phase2Config& config,
    double target_mjd_tdb) {
    OrbitalFitResults r;
    r.fit_successful=false; 
    return r;
}

// Download RWO stub
std::vector<ioccultcalc::Observation> Phase2OccultationGeometry::Impl::downloadRWOObservations(const std::string& designation, bool verbose) {
    return {};
}

// Download EQ1 stub
bool Phase2OccultationGeometry::Impl::downloadEQ1Elements(const std::string& designation, bool verbose) {
    return false;
}

} // namespace ioccultcalc
