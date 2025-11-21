/**
 * @file spk_reader.cpp
 * @brief Implementazione lettore file SPK usando jpl_eph di Bill Gray
 * 
 * Usa la libreria jpl_eph (https://github.com/Bill-Gray/jpl_eph)
 * GNU GPL License, compatible con il nostro progetto
 */

#include "ioccultcalc/spk_reader.h"
#include "ioccultcalc/jpl_de_downloader.h"
#include "ioccultcalc/jpleph.h"
#include <stdexcept>
#include <iostream>
#include <map>
#include <cstring>

namespace ioccultcalc {

class SPKReader::Impl {
public:
    void *ephem = nullptr;
    std::string filePath;
    bool loaded = false;
    std::string version;
    double coverageStart = 0.0;  // JD
    double coverageEnd = 0.0;    // JD
    
    ~Impl() {
        if (ephem) {
            jpl_close_ephemeris(ephem);
            ephem = nullptr;
        }
    }
    
    bool loadFile(const std::string& path) {
        if (ephem) {
            jpl_close_ephemeris(ephem);
            ephem = nullptr;
        }
        
        filePath = path;
        
        // Inizializza ephemeris con jpl_eph
        ephem = jpl_init_ephemeris(path.c_str(), nullptr, nullptr);
        
        if (!ephem) {
            int err = jpl_init_error_code();
            std::cerr << "SPKReader: jpl_init_ephemeris failed with error " << err << std::endl;
            return false;
        }
        
        // Leggi informazioni dall'ephemeris
        coverageStart = jpl_get_double(ephem, JPL_EPHEM_START_JD);
        coverageEnd = jpl_get_double(ephem, JPL_EPHEM_END_JD);
        
        int ephemVersion = (int)jpl_get_long(ephem, JPL_EPHEM_EPHEMERIS_VERSION);
        
        // Determina versione dal nome file o dal numero
        if (path.find("de430") != std::string::npos || ephemVersion == 430) {
            version = "DE430";
        } else if (path.find("de441") != std::string::npos || ephemVersion == 441) {
            version = "DE441";
        } else if (path.find("de440") != std::string::npos || ephemVersion == 440) {
            version = "DE440";
        } else if (path.find("de421") != std::string::npos || ephemVersion == 421) {
            version = "DE421";
        } else {
            version = "DE" + std::to_string(ephemVersion);
        }
        
        loaded = true;
        
        std::cerr << "SPKReader: Successfully loaded " << version 
                 << " covering JD " << coverageStart << " to " << coverageEnd << std::endl;
        
        return true;
    }
    
    Vector3D getPosition(int bodyId, double jd, int centerId) {
        auto [pos, vel] = getState(bodyId, jd, centerId);
        return pos;
    }
    
    std::pair<Vector3D, Vector3D> getState(int bodyId, double jd, int centerId) {
        if (!loaded || !ephem) {
            throw std::runtime_error("SPK file not loaded");
        }
        
        // jpl_pleph usa questa convenzione:
        // 1=Mercury, 2=Venus, 3=Earth, 4=Mars, 5=Jupiter, 6=Saturn, 
        // 7=Uranus, 8=Neptune, 9=Pluto, 10=Moon, 11=Sun, 12=SSB, 13=EMB
        
        double state[6];  // x, y, z, vx, vy, vz
        
        int err = jpl_pleph(ephem, jd, bodyId, centerId, state, 1);
        
        if (err != 0) {
            throw std::runtime_error("jpl_pleph error " + std::to_string(err) + 
                                   " for body " + std::to_string(bodyId) + 
                                   " at JD " + std::to_string(jd));
        }
        
        // jpl_pleph restituisce coordinate in AU e AU/day
        Vector3D pos(state[0], state[1], state[2]);
        Vector3D vel(state[3], state[4], state[5]);
        
        return {pos, vel};
    }
};

SPKReader::SPKReader() : pImpl(new Impl()) {}

SPKReader::~SPKReader() = default;

bool SPKReader::loadFile(const std::string& filePath) {
    return pImpl->loadFile(filePath);
}

bool SPKReader::ensureFileLoaded(const std::string& version) {
    // Try standard JPL filename first (linux_p1550p2650.430)
    std::string cacheDir = getenv("HOME") + std::string("/.ioccultcalc/ephemerides/");
    
    // Map version to filename
    std::map<std::string, std::string> versionToFile = {
        {"DE430", "linux_p1550p2650.430"},
        {"DE441", "linux_m13000p17000.441"},  // DE441: 343 asteroids + planets
        {"DE440", "linux_p1550p2650.440"}
    };
    
    auto it = versionToFile.find(version);
    if (it != versionToFile.end()) {
        std::string filePath = cacheDir + it->second;
        if (loadFile(filePath)) {
            return true;
        }
    }
    
    // Fallback: try with downloader (old format)
    JPLDEDownloader downloader;
    try {
        std::string filePath = downloader.ensureFileAvailable(version);
        return loadFile(filePath);
    } catch (const std::exception& e) {
        std::cerr << "Error loading SPK file: " << e.what() << std::endl;
        return false;
    }
}

Vector3D SPKReader::getPosition(int bodyId, double jd, int centerId) {
    return pImpl->getPosition(bodyId, jd, centerId);
}

std::pair<Vector3D, Vector3D> SPKReader::getState(int bodyId, double jd, int centerId) {
    return pImpl->getState(bodyId, jd, centerId);
}

bool SPKReader::isLoaded() const {
    return pImpl->loaded;
}

std::pair<double, double> SPKReader::getCoverage() const {
    return {pImpl->coverageStart, pImpl->coverageEnd};
}

std::string SPKReader::getVersion() const {
    return pImpl->version;
}

// NAIF ID utilities
std::string naifIdToName(int id) {
    static std::map<int, std::string> names = {
        {1, "Mercury"}, {2, "Venus"}, {3, "Earth"}, {4, "Mars"},
        {5, "Jupiter"}, {6, "Saturn"}, {7, "Uranus"}, {8, "Neptune"}, {9, "Pluto"},
        {10, "Moon"}, {11, "Sun"}, {399, "Earth"}, {0, "Solar System Barycenter"}
    };
    
    auto it = names.find(id);
    return it != names.end() ? it->second : "Unknown";
}

int nameToNaifId(const std::string& name) {
    static std::map<std::string, int> ids = {
        {"Mercury", 1}, {"Venus", 2}, {"Earth", 3}, {"Mars", 4},
        {"Jupiter", 5}, {"Saturn", 6}, {"Uranus", 7}, {"Neptune", 8}, {"Pluto", 9},
        {"Moon", 10}, {"Sun", 11}, {"SSB", 0}
    };
    
    auto it = ids.find(name);
    return it != ids.end() ? it->second : -1;
}

} // namespace ioccultcalc
