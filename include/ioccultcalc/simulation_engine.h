#ifndef IOCCULTCALC_SIMULATION_ENGINE_H
#define IOCCULTCALC_SIMULATION_ENGINE_H

#include "ioccultcalc/config_manager.h"
#include "ioccultcalc/simulation_types.h"
#include "phase2_occultation_geometry.h"
#include <vector>
#include <memory>

namespace ioccultcalc {

/**
 * @brief Orchestrates the occultation simulation workflow.
 * 
 * This class is the core engine that:
 * 1. Selects candidate asteroids based on configuration.
 * 2. Performs Phase 1 screening for each asteroid.
 * 3. Performs Phase 2 precise geometric calculations for candidates.
 * 4. Generates output reports.
 */
class SimulationEngine {
public:
    explicit SimulationEngine(const ConfigManager& config);
    
    /**
     * @brief Run the simulation workflow.
     * @return Summary of the simulation execution.
     */
    SimulationSummary run();

private:
    const ConfigManager& config_;

    // Core steps
    std::vector<AsteroidCandidate> selectAsteroids();
    void processAsteroid(const AsteroidCandidate& asteroid, std::vector<Phase2OccultationEvent>& allEvents);

    // Helpers (Internal selection logic moved from main)
    AsteroidTargetList parseExplicitTargets();
    SelectionFilters parseSelectionFilters();
    std::vector<AsteroidCandidate> fetchFromSqlite(const AsteroidTargetList& targets, const SelectionFilters& filters);
    std::vector<AsteroidCandidate> fetchFromJsonCatalog(const AsteroidTargetList& targets, const SelectionFilters& filters);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_SIMULATION_ENGINE_H
