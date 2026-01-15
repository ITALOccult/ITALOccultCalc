/**
 * @file spice_spk_reader.cpp
 * @brief Implementazione lettore SPK con CSPICE
 */

#include "ioccultcalc/spice_spk_reader.h"
#include <string>
#include <vector>
#include <memory>
#include <iomanip>
#include <iostream>
#include "ioccultcalc/coordinates.h"
#include <cstring>
#include <cstdlib>

// Include CSPICE
extern "C" {
    #include "SpiceUsr.h"
}

namespace ioccultcalc {

// Struttura per cache interpolazione Chebyshev
struct CacheEntry {
    int bodyId;
    int centerId;
    double jdStart;
    double jdEnd;
    std::vector<Vector3D> positions;
    std::vector<Vector3D> velocities;
    std::vector<double> times;
};

class SPICESPKReader::Impl {
public:
    int handle;
    bool loaded;
    std::string filepath;
    
    // Cache per interpolazione Chebyshev
    static constexpr int CACHE_SIZE = 10;
    static constexpr int INTERP_POINTS = 7;  // Punti per interpolazione
    static constexpr double CACHE_SPAN_DAYS = 1.0;  // Span temporale cache
    std::vector<CacheEntry> cache;
    
    Impl() : handle(-1), loaded(false) {
        // Inizializza CSPICE error handling
        erract_c("SET", 0, const_cast<char*>("RETURN"));
        
        // Sopprime output errori SPICE su stderr (evita spam da asteroidi mancanti)
        // Gli errori vengono comunque gestiti via failed_c() e getmsg_c()
        errprt_c("SET", 0, const_cast<char*>("NONE"));
        
        cache.reserve(CACHE_SIZE);
    }
    
    ~Impl() {
        close();
    }
    
    bool loadFile(const std::string& path) {
        if (loaded) {
            close();
        }
        
        // Carica frames kernel per ECLIPJ2000_DE441 (IAU 2006 obliquity, DE441-compatible)
        const char* home = getenv("HOME");
        if (home) {
            std::string framesPath = std::string(home) + "/.ioccultcalc/ephemerides/eclipj2000_de441.tf";
            FILE* f = fopen(framesPath.c_str(), "r");
            if (f) {
                fclose(f);
                furnsh_c(framesPath.c_str());
                if (failed_c()) {
                    char msg[1841];
                    getmsg_c("SHORT", 1840, msg);
                    std::cerr << "Warning: Could not load frames kernel: " << msg << std::endl;
                    reset_c();
                }
            }
        }
        
        // Carica SPK kernel
        furnsh_c(path.c_str());
        
        if (failed_c()) {
            char msg[1841];
            getmsg_c("SHORT", 1840, msg);
            std::cerr << "SPICE error loading " << path << ": " << msg << std::endl;
            reset_c();
            return false;
        }
        
        filepath = path;
        loaded = true;
        
        std::cerr << "SPICESPKReader: Loaded " << path << std::endl;
        return true;
    }
    
    // Query diretta SPK senza cache
    std::pair<Vector3D, Vector3D> getStateDirect(int bodyId, double jd, int centerId) {
        if (!loaded) {
            throw std::runtime_error("SPK file not loaded");
        }
        
        // Converti JD in ET (Ephemeris Time)
        double et = (jd - 2451545.0) * 86400.0;  // secondi da J2000
        
        // Buffer per stato (pos + vel, 6 elementi)
        double state[6];
        double lt;  // light time (non usato)
        
        // SPKEZR: calcola stato di bodyId rispetto a centerId
        char targetStr[64];
        char observerStr[64];
        snprintf(targetStr, sizeof(targetStr), "%d", bodyId);
        snprintf(observerStr, sizeof(observerStr), "%d", centerId);

        static int spk_log_count = 0;
        if (spk_log_count < 5) {
             std::cout << "[DEBUG_SPK] Requesting Body=" << bodyId << " Center=" << centerId 
                       << " JD=" << std::fixed << std::setprecision(5) << jd 
                       << " ET=" << std::setprecision(2) << et << std::endl;
             spk_log_count++;
        }
        
        const char* frame = "J2000";
        spkezr_c(targetStr, et, frame, "NONE", observerStr, state, &lt);
        
        // DEBUG temporaneo
        constexpr double KM_TO_AU_DBG = 1.0 / 149597870.7;
        static bool debugPrinted = false;
        if (!debugPrinted && bodyId == 399 && centerId == 10) {
            std::cerr << "DEBUG SPK frame=" << frame << " body=399 center=10:\n";
            std::cerr << "  pos=[" << state[0]*KM_TO_AU_DBG << ", " << state[1]*KM_TO_AU_DBG << ", " << state[2]*KM_TO_AU_DBG << "] AU\n";
            debugPrinted = true;
        }
        
        if (failed_c()) {
            char msg[1841];
            getmsg_c("SHORT", 1840, msg);
            reset_c();
            throw std::runtime_error(std::string("SPICE error for body ") + 
                                   std::to_string(bodyId) + ": " + msg);
        }
        
        // SPICE restituisce in km e km/s, converti in AU e AU/day
        constexpr double KM_TO_AU = 1.0 / 149597870.7;
        constexpr double KMS_TO_AUD = 86400.0 / 149597870.7;
        
        Vector3D pos(state[0] * KM_TO_AU, state[1] * KM_TO_AU, state[2] * KM_TO_AU);
        Vector3D vel(state[3] * KMS_TO_AUD, state[4] * KMS_TO_AUD, state[5] * KMS_TO_AUD);
        
        return {pos, vel};
    }
    
    // Interpolazione Lagrange (più stabile per pochi punti)
    Vector3D lagrangeInterpolate(const std::vector<Vector3D>& points,
                                 const std::vector<double>& times,
                                 double targetTime) {
        int n = points.size();
        if (n == 0) throw std::runtime_error("No points for interpolation");
        if (n == 1) return points[0];
        
        // Interpolazione polinomiale di Lagrange
        Vector3D result(0, 0, 0);
        
        for (int i = 0; i < n; ++i) {
            // Calcola L_i(targetTime)
            double L_i = 1.0;
            for (int j = 0; j < n; ++j) {
                if (i != j) {
                    L_i *= (targetTime - times[j]) / (times[i] - times[j]);
                }
            }
            
            // Aggiungi contributo
            result.x += points[i].x * L_i;
            result.y += points[i].y * L_i;
            result.z += points[i].z * L_i;
        }
        
        return result;
    }
    
    // Cerca o crea cache entry
    CacheEntry* getCacheEntry(int bodyId, int centerId, double jd) {
        // Cerca cache esistente
        for (auto& entry : cache) {
            if (entry.bodyId == bodyId && entry.centerId == centerId &&
                jd >= entry.jdStart && jd <= entry.jdEnd) {
                return &entry;
            }
        }
        
        // Crea nuova entry
        CacheEntry newEntry;
        newEntry.bodyId = bodyId;
        newEntry.centerId = centerId;
        newEntry.jdStart = jd - CACHE_SPAN_DAYS / 2.0;
        newEntry.jdEnd = jd + CACHE_SPAN_DAYS / 2.0;
        
        // Campiona punti per interpolazione
        double dt = CACHE_SPAN_DAYS / (INTERP_POINTS - 1);
        for (int i = 0; i < INTERP_POINTS; ++i) {
            double t = newEntry.jdStart + i * dt;
            auto [pos, vel] = getStateDirect(bodyId, t, centerId);
            newEntry.positions.push_back(pos);
            newEntry.velocities.push_back(vel);
            newEntry.times.push_back(t);
        }
        
        // Aggiungi a cache (sostituisci più vecchio se piena)
        if (cache.size() >= CACHE_SIZE) {
            cache.erase(cache.begin());
        }
        cache.push_back(newEntry);
        
        return &cache.back();
    }
    
    std::pair<Vector3D, Vector3D> getState(int bodyId, double jd, int centerId) {
        // Usa cache con interpolazione per Terra (query frequenti)
        if (bodyId == 399 && centerId == 10) {
            CacheEntry* entry = getCacheEntry(bodyId, centerId, jd);
            
            Vector3D pos = lagrangeInterpolate(entry->positions, entry->times, jd);
            Vector3D vel = lagrangeInterpolate(entry->velocities, entry->times, jd);
            
            return {pos, vel};
        }
        
        // Query diretta per altri corpi
        return getStateDirect(bodyId, jd, centerId);
    }
    
    void close() {
        if (loaded) {
            // CSPICE richiede unload dei kernel
            unload_c(filepath.c_str());
            loaded = false;
            filepath.clear();
        }
    }
};

SPICESPKReader::SPICESPKReader() : pImpl(std::make_unique<Impl>()) {}

SPICESPKReader::~SPICESPKReader() = default;

bool SPICESPKReader::loadFile(const std::string& filepath) {
    return pImpl->loadFile(filepath);
}

bool SPICESPKReader::ensureFileLoaded(const std::string& name) {
    // Cache directory
    const char* home = getenv("HOME");
    if (!home) {
        std::cerr << "HOME not set\n";
        return false;
    }
    
    std::string cacheDir = std::string(home) + "/.ioccultcalc/ephemerides/";
    std::string filepath = cacheDir + name;
    
    // Verifica se esiste
    FILE* f = fopen(filepath.c_str(), "rb");
    if (!f) {
        std::cerr << "SPK file not found: " << filepath << std::endl;
        std::cerr << "Download it from: https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/asteroids/" << std::endl;
        return false;
    }
    fclose(f);
    
    return loadFile(filepath);
}

Vector3D SPICESPKReader::getPosition(int bodyId, double jd, int centerId) {
    auto [pos, vel] = pImpl->getState(bodyId, jd, centerId);
    return pos;
}

std::pair<Vector3D, Vector3D> SPICESPKReader::getState(int bodyId, double jd, int centerId) {
    return pImpl->getState(bodyId, jd, centerId);
}

bool SPICESPKReader::isLoaded() const {
    return pImpl->loaded;
}

std::vector<int> SPICESPKReader::getAvailableBodies() const {
    // TODO: implementare enumerazione corpi
    return {};
}

void SPICESPKReader::close() {
    pImpl->close();
}

    SPKState SPICESPKReader::getEarthState(double jd) {
        try {
            auto s = getState(399, jd, 10);
            return { s.first, s.second, jd };
        } catch (const std::exception& e) {
            std::cerr << "[SPK_WARN] Failed to load Earth (399). Trying EMB (3). Error: " << e.what() << std::endl;
            auto s = getState(3, jd, 10);
            std::cerr << "[SPK_WARN] FALLBACK TO EMB (3) SUCCESSFUL! Kernel is missing 399 center." << std::endl;
            return { s.first, s.second, jd };
        }
    }

} // namespace ioccultcalc
