#include "ioccultcalc/occultation_engine.h"
#include "ioccultcalc/eq1_parser.h"
#include "ioccultcalc/asteroid_sqlite_db.h"
#include <iostream>

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
    // Rely on Phase1/Phase2 loaders which handle the DB lookup logic
    if (phase1_.loadAsteroidFromJSON(number, path)) {
        // Since Phase 1 loaded it, we need to ensure Phase 2 also has it.
        // In the interest of full sync, it would be better to extract elements,
        // but for now we call the JSON loader on both components.
        phase2_.loadAsteroidFromJSON(number, path);
        elements_loaded_ = true;
        // Note: we don't have the elements in current_elements_ here, 
        // which is a limitation of JSON loading bypass. 
        // Ideally we'd have a common DB loader that returns AstDynEquinoctialElements.
        return true;
    }
    return false;
}

bool OccultationEngine::loadAsteroidFromDB(int number) {
    try {
        AsteroidSqliteDatabase db;
        auto orbital = db.getOrbitalElements(number);
        if (orbital) {
            current_elements_ = orbital->toEquinoctial();
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
    return phase2_.calculatePreciseGeometry(candidates, config);
}

void OccultationEngine::syncElements() {
    if (!elements_loaded_) return;
    
    // Push elements to both phases
    phase1_.setAsteroidElements(current_elements_);
    phase2_.setAsteroidElements(current_elements_);
}

} // namespace ioccultcalc
