#include "ioccultcalc/occultation_engine.h"
#include "ioccultcalc/eq1_parser.h"
#include "ioccultcalc/asteroid_sqlite_db.h"
#include "ioccultcalc/mpc_client.h"
#include "ioccultcalc/astdyn_interface.h"
#include <iostream>
#include "ioccultcalc/types.h"

namespace {
    template<typename T>
    T safe_get(const nlohmann::json& j, const std::string& key, T defaultValue) {
        if (j.contains(key) && !j[key].is_null()) {
            try {
                return j[key].get<T>();
            } catch (...) {
                return defaultValue;
            }
        }
        return defaultValue;
    }
}

namespace ioccultcalc {


OccultationEngine::OccultationEngine() = default;
OccultationEngine::~OccultationEngine() = default;

bool OccultationEngine::setAsteroidElements(const AstDynEquinoctialElements& elements) {
    current_elements_ = elements;
    elements_loaded_ = true;
    syncElements();
    return true;
}

bool OccultationEngine::loadAsteroidFromEQ1(int number, const std::string& eq1_path) {
    if (loadAsteroidFromEQ1(eq1_path)) {
        current_elements_.number = number;
        return true;
    }
    return false;
}

bool OccultationEngine::loadAsteroidFromEQ1(const std::string& eq1_path) {
    try {
        auto elements = EQ1Parser::parseFile(eq1_path);
        
        current_elements_.a = elements.a;
        current_elements_.h = elements.h;
        current_elements_.k = elements.k;
        current_elements_.p = elements.p;
        current_elements_.q = elements.q;
        current_elements_.lambda = elements.lambda * DEG_TO_RAD; // Convert to radians
        current_elements_.epoch = JulianDate::fromMJD(elements.epoch_mjd);
        current_elements_.name = elements.name;
        current_elements_.designation = elements.name; 
        current_elements_.H = elements.H;
        current_elements_.G = elements.G;
        
        // Metadata
        current_elements_.frame = FrameType::ECLIPTIC_J2000;
        current_elements_.type = ElementType::MEAN_ASTDYS;
        
        elements_loaded_ = true;
        syncElements();
        return true;
    } catch (const std::exception& e) {
        std::cerr << "[OccultationEngine] Error loading .eq1: " << e.what() << std::endl;
        return false;
    }
}

bool OccultationEngine::loadAsteroidFromJSON(int number, const std::string& path) {
    // Search path logic
    std::string searchPath = path;
    if (searchPath.empty()) {
        const char* home = getenv("HOME");
        if (home) {
            searchPath = std::string(home) + "/.ioccultcalc/data/all_numbered_asteroids.json";
        }
    }

    try {
        std::ifstream f(searchPath);
        if (f.is_open()) {
            std::cout << "[OccultationEngine] Opened JSON file: " << searchPath << std::endl;
            nlohmann::json j;
            f >> j;
            
            nlohmann::json data;
            bool found = false;
            nlohmann::json asteroidList;

            if (j.is_array()) {
                asteroidList = j;
            } else if (j.is_object() && j.contains("asteroids")) {
                asteroidList = j["asteroids"];
                std::cout << "[OccultationEngine] Found 'asteroids' array in JSON." << std::endl;
            }

            if (asteroidList.is_array()) {
                std::cout << "[OccultationEngine] Searching in asteroid array (size: " << asteroidList.size() << ")..." << std::endl;
                for (const auto& item : asteroidList) {
                    if (item.value("asteroid", 0) == number || item.value("number", 0) == number) {
                        data = item;
                        found = true;
                        break;
                    }
                }
            } 
            
            if (!found && j.is_object()) {
                std::string s_num = std::to_string(number);
                if (j.contains(s_num)) {
                    data = j[s_num];
                    found = true;
                    std::cout << "[OccultationEngine] Found asteroid " << number << " as direct map key." << std::endl;
                }
            }

            if (found) {
                std::cout << "[OccultationEngine] Successfully found asteroid " << number << " in JSON." << std::endl;
                // Construct AstDynEquinoctialElements
                current_elements_.number = number;
                current_elements_.name = safe_get<std::string>(data, "name", "Unknown");
                current_elements_.designation = std::to_string(number);
                
                current_elements_.H = safe_get<double>(data, "H", 15.0);
                current_elements_.G = safe_get<double>(data, "G", 0.15);
                current_elements_.diameter = safe_get<double>(data, "diameter", 0.0);
                
                // Load Orbital Elements
                current_elements_.a = safe_get<double>(data, "a", 0.0);
                current_elements_.h = safe_get<double>(data, "h", 0.0);
                current_elements_.k = safe_get<double>(data, "k", 0.0);
                current_elements_.p = safe_get<double>(data, "p", 0.0);
                current_elements_.q = safe_get<double>(data, "q", 0.0);
                
                // CRITICAL: lambda is Mean Longitude, JSON 'ma' is Mean Anomaly.
                // Do NOT assign ma to lambda directly if we are in Equinoctial mode.
                // If the JSON has h,k,p,q, it should also have 'lambda'.
                if (data.contains("lambda")) {
                    current_elements_.lambda = safe_get<double>(data, "lambda", 0.0) * (data.contains("lambda_deg") ? DEG_TO_RAD : 1.0);
                } else if (!data.contains("e")) {
                     // Fallback if no lambda but ma is present - this is risky if h,k,p,q are non-zero
                     current_elements_.lambda = safe_get<double>(data, "ma", 0.0) * DEG_TO_RAD; 
                }
                
                // Handle Keplerian if needed (if JSON has a,e,i,om,w,ma)
                if (data.contains("e") && !data["e"].is_null() && current_elements_.h == 0) {
                    double ecc = safe_get<double>(data, "e", 0.0);
                    double inc = safe_get<double>(data, "i", 0.0) * DEG_TO_RAD;
                    double om = safe_get<double>(data, "om", 0.0) * DEG_TO_RAD;
                    double w = safe_get<double>(data, "w", 0.0) * DEG_TO_RAD;
                    double ma = safe_get<double>(data, "ma", 0.0) * DEG_TO_RAD;
                    double a = safe_get<double>(data, "a", 0.0);
                    double epoch = safe_get<double>(data, "epoch", 0.0);
                    if (epoch > 2400000.5) epoch -= 2400000.5;
                    
                    auto kep = AstDynEquinoctialElements::fromKeplerian(a, ecc, inc, w, om, ma, JulianDate::fromMJD(epoch));
                    current_elements_.a = kep.a;
                    current_elements_.h = kep.h;
                    current_elements_.k = kep.k;
                    current_elements_.p = kep.p;
                    current_elements_.q = kep.q;
                    current_elements_.lambda = kep.lambda;
                    current_elements_.epoch = kep.epoch;
                }
                
                // Fallback diameter from H
                if (current_elements_.diameter <= 0 && current_elements_.H > 0) {
                    current_elements_.diameter = (1329.0 / std::sqrt(0.15)) * std::pow(10.0, -0.2 * current_elements_.H);
                }
                
                // Update components
                phase1_.loadAsteroidFromJSON(number, searchPath);
                phase2_.loadAsteroidFromJSON(number, searchPath);
                
                elements_loaded_ = true;
                syncElements();
                return true;
            } else {
                std::cout << "[OccultationEngine] Asteroid " << number << " NOT found in JSON." << std::endl;
            }
        }
    } catch (const std::exception& e) {
        std::cerr << "[OccultationEngine] JSON Load Error: " << e.what() << std::endl;
    }

    return false;
}

bool OccultationEngine::loadAsteroidFromDB(int number) {
    try {
        AsteroidSqliteDatabase db;
        auto orbital = db.getOrbitalElements(number);
        if (orbital) {
            current_elements_ = orbital->toEquinoctial();
            
            // Fallback diameter if missing in DB
            if (current_elements_.diameter <= 0 && current_elements_.H > 0) {
                current_elements_.diameter = (1329.0 / std::sqrt(0.15)) * std::pow(10.0, -0.2 * current_elements_.H);
            }

            elements_loaded_ = true;
            syncElements();
            return true;
        }
    } catch (const std::exception& e) {
        std::cerr << "[OccultationEngine] SQLite Error: " << e.what() << std::endl;
    }
    return false;
}

Phase1Results OccultationEngine::runPhase1(const Phase1Config& config) {
    if (!elements_loaded_) {
        std::cerr << "[OccultationEngine] Warning: No asteroid elements loaded. Attempting to proceed..." << std::endl;
    }
    return phase1_.screenCandidates(config);
}

std::vector<Phase2OccultationEvent> OccultationEngine::runPhase2(const Phase2Config& config, const std::vector<CandidateStar>& candidates) {
    if (!elements_loaded_) {
        std::cerr << "[OccultationEngine] Warning: No asteroid elements loaded. Attempting to proceed..." << std::endl;
    }

    if (candidates.empty()) {
        // No events, skip refinement and phase 2
        return {};
    }

    // Refinement Step (Conditional)
    // Only refine if requested AND if we have candidates (events)
    if (config.refine_orbit) {
        int n_obs = (config.last_n_obs > 0) ? config.last_n_obs : 100;
        refineOrbit(n_obs);
    }

    // Create a modified config for Phase 2 to prevent double refinement
    Phase2Config p2Config = config;
    p2Config.refine_orbit = false; // Already handled by Engine

    return phase2_.calculatePreciseGeometry(candidates, p2Config);
}

bool OccultationEngine::refineOrbit(int n_observations) {
    // Use name or number (Prioritize NUMBER for AstDyS RWO downloads)
    std::string designation;
    if (current_elements_.number > 0) {
        designation = std::to_string(current_elements_.number);
    } else {
        designation = current_elements_.name;
        if (designation.empty()) designation = current_elements_.designation;
    }

    std::cout << "\n[OccultationEngine] 🔄 Refining orbit for " << designation 
              << " with last " << n_observations << " observations..." << std::endl;

    try {
        MPCClient mpc;
        ObservationSet obsSet = mpc.getRecentObservations(designation, n_observations);
        
        if (obsSet.observations.empty()) {
            std::cerr << "[OccultationEngine] ⚠️ No observations found. Skipping refinement." << std::endl;
            return false;
        }

        // Convert to RWO format for fitting
        std::vector<RWOObservation> rwoList;
        for (const auto& obs : obsSet.observations) {
            RWOObservation rwo;
            rwo.designation = designation;
            rwo.mjd_utc = obs.epoch.jd - 2400000.5;
            rwo.ra_deg = obs.obs.ra * RAD_TO_DEG;
            rwo.dec_deg = obs.obs.dec * RAD_TO_DEG;
            rwo.ra_sigma_arcsec = 0.5; // Default sigma
            rwo.dec_sigma_arcsec = 0.5;
            rwo.obs_code = obs.observatoryCode;
            rwoList.push_back(rwo);
        }

        // Setup Fitter
        AstDynOrbitFitter fitter;
        fitter.setConvergenceTolerance(1e-9);
        
        // Convert current elements to AstDyS format for fitter
        auto orb = OrbitalElements::fromEquinoctial(current_elements_);
        auto initial = astdyn_utils::toAstDySElements(orb);

        // Run Fit
        auto fitRes = fitter.fit(initial, rwoList);
        
        std::cout << "[OccultationEngine] Fit completed. Observations used: " << fitRes.n_used 
                  << "/" << fitRes.n_observations << ". RMS=" << fitRes.rms_total_arcsec << " arcsec." << std::endl;

        if (fitRes.n_used > 5 && fitRes.rms_total_arcsec < 2.0) {
             // Accept new orbit
             auto newOrb = fitRes.fitted_elements.toOrbitalElements();
             
             // PRESERVE METADATA
             int saved_number = current_elements_.number;
             std::string saved_name = current_elements_.name;
             std::string saved_desig = current_elements_.designation;
             double saved_H = current_elements_.H;
             double saved_G = current_elements_.G;
             double saved_D = current_elements_.diameter;

             current_elements_ = newOrb.toEquinoctial();
             
             current_elements_.number = saved_number;
             current_elements_.name = saved_name;
             current_elements_.designation = saved_desig;
             current_elements_.H = saved_H;
             current_elements_.G = saved_G;
             current_elements_.diameter = saved_D;
             
             std::cout << "[OccultationEngine] ✅ Orbit updated successfully." << std::endl;
             syncElements(); // Propagate to Phase 1 & 2 logic
             return true;
        } else {
             std::cout << "[OccultationEngine] ❌ Fit rejected (poor RMS or few obs). Keeping original orbit." << std::endl;
        }

    } catch (const std::exception& e) {
        std::cerr << "[OccultationEngine] 💥 Refinement error: " << e.what() << std::endl;
    }
    return false;
}

void OccultationEngine::syncElements() {
    if (!elements_loaded_) return;
    
    // Push elements to both phases
    phase1_.setAsteroidElements(current_elements_);
    phase2_.setAsteroidElements(current_elements_);
}

} // namespace ioccultcalc
