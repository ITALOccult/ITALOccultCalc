#ifndef IOCCULTCALC_SIMULATION_TYPES_H
#define IOCCULTCALC_SIMULATION_TYPES_H

#include "ioccultcalc/orbital_elements.h"
#include <vector>
#include <string>
#include <set>

namespace ioccultcalc {

/**
 * @brief Simple asteroid candidate structure
 */
struct AsteroidCandidate {
    OrbitalElements elements;
    double priority_score = 0.0;
    std::string reason;
};

/**
 * @brief Selection filters for asteroid screening
 */
struct SelectionFilters {
    double max_magnitude = -1.0;
    double min_diameter = -1.0;
    double max_diameter = -1.0;
    double min_perihelion = -1.0;
    double max_aphelion = -1.0;
    int max_asteroids = 0;

    bool is_valid_magnitude(double H) const {
        return max_magnitude <= 0 || H <= max_magnitude;
    }

    bool is_valid_diameter(double diameter) const {
        if (min_diameter > 0 && diameter < min_diameter) return false;
        if (max_diameter > 0 && diameter > max_diameter) return false;
        return true;
    }
};

struct AsteroidTargetList {
    std::set<int> ids;
    bool active = false;
};

struct SimulationSummary {
    int total_asteroids = 0;
    int asteroids_with_candidates = 0;
    int total_candidates_found = 0;
    int total_precise_events = 0;
    double execution_time_sec = 0.0;
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_SIMULATION_TYPES_H
