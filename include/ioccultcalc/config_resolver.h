#ifndef IOCCULTCALC_CONFIG_RESOLVER_H
#define IOCCULTCALC_CONFIG_RESOLVER_H

#include "ioccultcalc/config_manager.h"
#include "ioccultcalc/simulation_types.h"
#include "phase1_candidate_screening.h"
#include "phase2_occultation_geometry.h"

namespace ioccultcalc {

/**
 * @brief Utility to resolve domain-specific configurations from the generic ConfigManager.
 * 
 * Adheres to Mark Seemann's principles by centralizing the "mapping" logic between
 * persistence/config formats and domain objects.
 */
class ConfigResolver {
public:
    static Phase1Config resolvePhase1(const ConfigManager& config);
    static Phase2Config resolvePhase2(const ConfigManager& config);
    static SelectionFilters resolveFilters(const ConfigManager& config);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_CONFIG_RESOLVER_H
