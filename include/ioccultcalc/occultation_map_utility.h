/**
 * @file occultation_map_utility.h
 * @brief Utility class to generate ground maps from JSON/XML occultation data.
 */

#ifndef IOCCULTCALC_OCCULTATION_MAP_UTILITY_H
#define IOCCULTCALC_OCCULTATION_MAP_UTILITY_H

#include "ioccultcalc/output_manager.h"
#include <string>
#include <vector>

namespace ioccultcalc {

/**
 * @brief Utility to load occultation data and render Earth Maps.
 */
class OccultationMapUtility {
public:
    OccultationMapUtility();
    ~OccultationMapUtility();

    /**
     * @brief Load event data from a JSON file.
     * @param path Path to the JSON file
     * @return true if loaded successfully
     */
    bool loadFromJson(const std::string& path);

    /**
     * @brief Load event data from an XML (Occult4) file.
     * @param path Path to the XML file
     * @return true if loaded successfully
     */
    bool loadFromXml(const std::string& path);

    /**
     * @brief Generate an Earth Map PNG from the loaded data.
     * @param outputPath Path where the PNG will be saved
     * @param dataDir Path to the IOC_Earth data directory (shapefiles)
     * @return true if generated successfully
     */
    bool generateMap(const std::string& outputPath, const std::string& dataDir = "external/IOC_Earth/data/");

    /**
     * @brief Get the loaded event data.
     */
    const OutputEvent& getEvent() const { return event_; }

private:
    OutputEvent event_;
    bool isDataLoaded_;
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_OCCULTATION_MAP_UTILITY_H
