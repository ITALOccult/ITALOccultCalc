/**
 * Test diagnostico completo per frame SPK
 * 
 * Prova tutte le combinazioni di:
 * - Frame: J2000, ECLIPJ2000, ICRF, IAU_EARTH
 * - Center: 0 (SSB), 10 (Sun)
 * 
 * Confronta con Horizons per trovare combinazione corretta
 */

#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/spice_spk_reader.h"
#include "ioccultcalc/types.h"
#include <iostream>
#include <iomanip>
#include <vector>
#include <string>
#include <cmath>

// CSPICE forward declarations
extern "C" {
    void furnsh_c(const char* file);
    void spkezr_c(const char* targ, double et, const char* ref,
                  const char* abcorr, const char* obs,
                  double starg[6], double* lt);
    int failed_c();
    void getmsg_c(const char* option, int lenout, char* msg);
    void reset_c();
}

using namespace ioccultcalc;

struct TestConfig {
    std::string frameName;
    int centerId;
    std::string centerName;
};

Vector3D testSPKFrame(const std::string& frame, int centerId, 
                      double jd, const std::string& spkFile) {
    // Converti JD in ET
    double et = (jd - 2451545.0) * 86400.0;
    
    double state[6];
    double lt;
    
    char targetStr[64];
    char observerStr[64];
    snprintf(targetStr, sizeof(targetStr), "399");  // Earth
    snprintf(observerStr, sizeof(observerStr), "%d", centerId);
    
    spkezr_c(targetStr, et, frame.c_str(), "NONE", observerStr, state, &lt);
    
    if (failed_c()) {
        char msg[1841];
        getmsg_c("SHORT", 1840, msg);
        reset_c();
        return Vector3D(NAN, NAN, NAN);
    }
    
    // Converti km -> AU
    constexpr double KM_TO_AU = 1.0 / 149597870.7;
    return Vector3D(
        state[0] * KM_TO_AU,
        state[1] * KM_TO_AU,
        state[2] * KM_TO_AU
    );
}

int main() {
    std::cout << std::fixed << std::setprecision(6);
    
    // Data test: 2024-12-10 02:30 UTC
    JulianDate testDate(2460653.604167);  // MJD 60653.104167
    
    std::cout << "=== TEST FRAME SPK PER POSIZIONE TERRA ===\n";
    std::cout << "Data: 2024-12-10 02:30 UTC (JD " << testDate.jd << ")\n\n";
    
    // 1. Scarica posizione di riferimento da Horizons
    std::cout << "1. Download posizione Terra da Horizons (@sun, ICRF)...\n";
    JPLHorizonsClient horizons;
    Vector3D horizonsPos, horizonsVel;
    
    try {
        auto [pos, vel] = horizons.getStateVectors("399", testDate, "@sun");
        horizonsPos = pos;
        horizonsVel = vel;
        
        std::cout << "   Horizons: [" << horizonsPos.x << ", " 
                  << horizonsPos.y << ", " << horizonsPos.z << "] AU\n";
        std::cout << "   Magnitude: " << horizonsPos.magnitude() << " AU\n\n";
    } catch (const std::exception& e) {
        std::cerr << "   ERRORE: " << e.what() << "\n";
        return 1;
    }
    
    // 2. Carica SPK file
    std::cout << "2. Caricamento SPK DE440s...\n";
    
    std::string spkPath = std::string(getenv("HOME")) + 
                          "/.ioccultcalc/ephemerides/de440s.bsp";
    
    try {
        furnsh_c(spkPath.c_str());
        if (failed_c()) {
            char msg[1841];
            getmsg_c("SHORT", 1840, msg);
            reset_c();
            std::cerr << "   ERRORE caricamento SPK: " << msg << "\n";
            return 1;
        }
        std::cout << "   SPK caricato: " << spkPath << "\n\n";
    } catch (...) {
        std::cerr << "   ERRORE: File non trovato\n";
        return 1;
    }
    
    // 3. Test tutte le combinazioni
    std::cout << "3. Test frame e center:\n\n";
    
    std::vector<TestConfig> configs = {
        {"J2000", 10, "Sun"},
        {"J2000", 0, "SSB"},
        {"ECLIPJ2000", 10, "Sun"},
        {"ECLIPJ2000", 0, "SSB"},
        {"ICRF", 10, "Sun"},
        {"ICRF", 0, "SSB"}
    };
    
    struct Result {
        std::string config;
        Vector3D pos;
        double error;
        bool valid;
    };
    
    std::vector<Result> results;
    
    for (const auto& cfg : configs) {
        std::cout << "   Testing: frame=" << cfg.frameName 
                  << ", center=" << cfg.centerName << " (" << cfg.centerId << ")\n";
        
        Vector3D spkPos = testSPKFrame(cfg.frameName, cfg.centerId, 
                                       testDate.jd, spkPath);
        
        Result r;
        r.config = cfg.frameName + " @ " + cfg.centerName;
        r.pos = spkPos;
        r.valid = !std::isnan(spkPos.x);
        
        if (r.valid) {
            std::cout << "      Pos: [" << spkPos.x << ", " 
                      << spkPos.y << ", " << spkPos.z << "] AU\n";
            
            Vector3D diff = spkPos - horizonsPos;
            r.error = diff.magnitude();
            
            std::cout << "      Errore vs Horizons: " << r.error << " AU";
            
            if (r.error < 0.001) {
                std::cout << " ✓ ECCELLENTE (<1000 km)\n";
            } else if (r.error < 0.01) {
                std::cout << " ✓ BUONO (<1.5M km)\n";
            } else if (r.error < 0.1) {
                std::cout << " ⚠ ACCETTABILE (<15M km)\n";
            } else {
                std::cout << " ✗ TROPPO GRANDE (>" << int(r.error * 149.6) << "M km)\n";
            }
            
            // Analisi componenti
            std::cout << "      Delta: [" << diff.x << ", " 
                      << diff.y << ", " << diff.z << "] AU\n";
            
            // Quale componente domina?
            double maxComp = std::max({std::abs(diff.x), 
                                       std::abs(diff.y), 
                                       std::abs(diff.z)});
            if (std::abs(diff.x) == maxComp) {
                std::cout << "      Errore dominante: X\n";
            } else if (std::abs(diff.y) == maxComp) {
                std::cout << "      Errore dominante: Y\n";
            } else {
                std::cout << "      Errore dominante: Z\n";
            }
            
        } else {
            std::cout << "      ✗ FALLITO (frame non supportato?)\n";
            r.error = 1e10;
        }
        
        results.push_back(r);
        std::cout << "\n";
    }
    
    // 4. Test barycentric -> heliocentric manuale
    std::cout << "4. Test conversione manuale barycentric->heliocentric:\n\n";
    
    Vector3D earthSSB = testSPKFrame("J2000", 0, testDate.jd, spkPath);
    Vector3D sunSSB = testSPKFrame("J2000", 0, testDate.jd, spkPath);
    
    // Per il Sole serve body ID 10
    double et = (testDate.jd - 2451545.0) * 86400.0;
    double sunState[6];
    double lt;
    spkezr_c("10", et, "J2000", "NONE", "0", sunState, &lt);
    
    if (!failed_c()) {
        constexpr double KM_TO_AU = 1.0 / 149597870.7;
        Vector3D sunFromSSB(
            sunState[0] * KM_TO_AU,
            sunState[1] * KM_TO_AU,
            sunState[2] * KM_TO_AU
        );
        
        Vector3D earthHelio = earthSSB - sunFromSSB;
        
        std::cout << "   Terra @ SSB: [" << earthSSB.x << ", " 
                  << earthSSB.y << ", " << earthSSB.z << "] AU\n";
        std::cout << "   Sole @ SSB:  [" << sunFromSSB.x << ", " 
                  << sunFromSSB.y << ", " << sunFromSSB.z << "] AU\n";
        std::cout << "   Terra @ Sun: [" << earthHelio.x << ", " 
                  << earthHelio.y << ", " << earthHelio.z << "] AU\n";
        
        Vector3D diff = earthHelio - horizonsPos;
        double error = diff.magnitude();
        
        std::cout << "   Errore vs Horizons: " << error << " AU\n";
        
        if (error < 0.001) {
            std::cout << "   ✓ SOLUZIONE TROVATA! Usare SSB-Sun manuale\n";
        }
    } else {
        reset_c();
        std::cout << "   ✗ Fallito\n";
    }
    std::cout << "\n";
    
    // 5. Sommario finale
    std::cout << "=== SOMMARIO RISULTATI ===\n\n";
    std::cout << "Riferimento Horizons: [" << horizonsPos.x << ", " 
              << horizonsPos.y << ", " << horizonsPos.z << "] AU\n\n";
    
    std::cout << "Frame/Center                 | Errore (AU) | Errore (km)      | Status\n";
    std::cout << "-----------------------------+-------------+------------------+----------\n";
    
    for (const auto& r : results) {
        if (!r.valid) continue;
        
        std::cout << std::left << std::setw(28) << r.config << " | ";
        std::cout << std::right << std::setw(11) << r.error << " | ";
        std::cout << std::setw(16) << int(r.error * 149597870.7) << " | ";
        
        if (r.error < 0.001) {
            std::cout << "✓✓✓ OTTIMO";
        } else if (r.error < 0.01) {
            std::cout << "✓✓ BUONO";
        } else if (r.error < 0.1) {
            std::cout << "✓ OK";
        } else {
            std::cout << "✗ MALE";
        }
        std::cout << "\n";
    }
    
    std::cout << "\n=== RACCOMANDAZIONI ===\n\n";
    
    // Trova migliore
    double minError = 1e10;
    std::string bestConfig;
    
    for (const auto& r : results) {
        if (r.valid && r.error < minError) {
            minError = r.error;
            bestConfig = r.config;
        }
    }
    
    if (minError < 0.001) {
        std::cout << "✓ Configurazione ottimale trovata: " << bestConfig << "\n";
        std::cout << "  Errore: " << int(minError * 149597870.7) << " km\n";
        std::cout << "  Usare questa configurazione in spice_spk_reader.cpp\n";
    } else if (minError < 0.01) {
        std::cout << "⚠ Configurazione accettabile: " << bestConfig << "\n";
        std::cout << "  Errore: " << int(minError * 149597870.7 / 1000.0) << " Mm\n";
        std::cout << "  Potrebbe richiedere calibrazione aggiuntiva\n";
    } else {
        std::cout << "✗ NESSUNA configurazione soddisfacente trovata\n";
        std::cout << "  Migliore: " << bestConfig << " (errore: " << minError << " AU)\n";
        std::cout << "  Problema potrebbe essere:\n";
        std::cout << "  - Versione SPK sbagliata (provare DE441 full invece di DE440s)\n";
        std::cout << "  - Bug in conversione unità o frame\n";
        std::cout << "  - Necessaria trasformazione aggiuntiva non documentata\n";
    }
    
    return 0;
}
