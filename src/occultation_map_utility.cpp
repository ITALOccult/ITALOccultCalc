#include "ioccultcalc/occultation_map_utility.h"
#include "ioccultcalc/occult4_xml.h"
#include "../external/IOC_Earth/include/ioc_earth/OccultationRenderer.h"
#include <iostream>

namespace ioccultcalc {

OccultationMapUtility::OccultationMapUtility() : isDataLoaded_(false) {}

OccultationMapUtility::~OccultationMapUtility() = default;

bool OccultationMapUtility::loadFromXml(const std::string& path) {
    std::cout << "DEBUG: Entering loadFromXml with path: " << path << " (size: " << path.length() << ")" << std::endl;
    std::cout.flush();
    try {
        Occult4XMLHandler xmlHandler;
        auto events = xmlHandler.loadFromXML(path);
        if (events.empty()) {
            std::cerr << "[OccultationMapUtility] No events found in XML: " << path << std::endl;
            return false;
        }

        // Carichiamo solo il primo evento per semplicità della utility
        const auto& e = events[0];
        
        event_.asteroid_number = std::atoi(e.asteroidNumber.c_str());
        event_.asteroid_name = e.asteroidName;
        event_.star_id = (e.starCatalog.empty() ? "" : e.starCatalog + " ") + e.starId;
        event_.star_ra_deg = e.starRA;
        event_.star_dec_deg = e.starDec;
        event_.star_mag = e.starMag;
        event_.jd_event = e.jdEvent;
        event_.utc_string = e.dateTimeUTC;
        event_.closest_approach_arcsec = e.closeApproachDist;
        event_.shadow_width_km = e.pathWidth;
        event_.duration_seconds = e.maxDuration;
        event_.path_uncertainty_km = e.uncertainty;
        event_.mag_drop = e.dropMag;

        for (const auto& p : e.centerLine) {
            ::ioccultcalc::OutputEvent::PathPoint pt{};
            pt.latitude = p.latitude;
            pt.longitude = p.longitude;
            event_.central_path.push_back(pt);
        }
        std::cout << "[OccultationMapUtility] Loaded " << event_.central_path.size() << " center line points." << std::endl;

        for (const auto& p : e.northLimit) {
            ::ioccultcalc::OutputEvent::PathPoint pt{};
            pt.latitude = p.latitude;
            pt.longitude = p.longitude;
            event_.north_limit.push_back(pt);
        }
        std::cout << "[OccultationMapUtility] Loaded " << event_.north_limit.size() << " north limit points." << std::endl;

        for (const auto& p : e.southLimit) {
            ::ioccultcalc::OutputEvent::PathPoint pt{};
            pt.latitude = p.latitude;
            pt.longitude = p.longitude;
            event_.south_limit.push_back(pt);
        }
        std::cout << "[OccultationMapUtility] Loaded " << event_.south_limit.size() << " south limit points." << std::endl;

        // Carichiamo eventuali altri eventi come percorsi comparativi
        for (size_t i = 1; i < events.size(); ++i) {
            const auto& e_comp = events[i];
            OutputEvent::ComparativePath cp;
            cp.name = e_comp.asteroidName + " " + e_comp.eventId;
            cp.color = (i % 2 == 1) ? "#FF0000" : "#00FF00"; // Alterna rosso e verde
            
            for (const auto& p : e_comp.centerLine) {
                cp.points.emplace_back(p.latitude, p.longitude);
            }
            
            event_.comparative_paths.push_back(cp);
            std::cout << "[OccultationMapUtility] Loaded comparative path: " << cp.name 
                      << " with " << cp.points.size() << " points." << std::endl;
        }

        isDataLoaded_ = true;
        return true;
    } catch (const std::exception& ex) {
        std::cerr << "[OccultationMapUtility] XML Error: " << ex.what() << std::endl;
        return false;
    }
}

bool OccultationMapUtility::generateMap(const std::string& outputPath, const std::string& dataDir) {
    if (!isDataLoaded_) {
        std::cerr << "[OccultationMapUtility] Error: No data loaded." << std::endl;
        return false;
    }

    try {
        ioc_earth::OccultationRenderer mapRenderer(1200, 800);
        mapRenderer.setDataDirectory(dataDir);

        ioc_earth::OccultationData data;
        data.event_id = event_.asteroid_name + "_" + std::to_string(event_.asteroid_number);
        data.asteroid_name = "(" + std::to_string(event_.asteroid_number) + ") " + event_.asteroid_name;
        data.star_name = event_.star_id;
        data.date_time_utc = event_.utc_string;
        data.duration_seconds = event_.duration_seconds;
        data.magnitude_drop = event_.mag_drop;

        for (const auto& p : event_.central_path) {
            data.central_line.emplace_back(p.longitude, p.latitude);
        }

        for (const auto& p : event_.north_limit) {
            data.northern_limit.emplace_back(p.longitude, p.latitude);
        }

        for (const auto& p : event_.south_limit) {
            data.southern_limit.emplace_back(p.longitude, p.latitude);
        }

        for (const auto& cp_src : event_.comparative_paths) {
            ioc_earth::OccultationData::ComparativePath cp_dst;
            cp_dst.name = cp_src.name;
            cp_dst.color = cp_src.color;
            for (const auto& p : cp_src.points) {
                cp_dst.points.emplace_back(p.longitude, p.latitude);
            }
            data.comparative_paths.push_back(cp_dst);
        }

        mapRenderer.setOccultationData(data);
        mapRenderer.autoCalculateExtent(20.0); // Margine 20%
        
        std::cout << "[OccultationMapUtility] Rendering map to: " << outputPath << std::endl;
        return mapRenderer.renderOccultationMap(outputPath, true);
    } catch (const std::exception& ex) {
        std::cerr << "[OccultationMapUtility] Rendering Error: " << ex.what() << std::endl;
        return false;
    }
}

} // namespace ioccultcalc
