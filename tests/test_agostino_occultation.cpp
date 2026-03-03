#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/spice_spk_reader.h"
#include "ioccultcalc/types.h"
#include <iostream>

using namespace ioccultcalc;

int main() {
    std::cerr << "[TEST] Agostino 12848 Occultation 2026-03-10" << std::endl;
    
    // Carica SPICE
    auto reader = std::make_shared<SPICESPKReader>();
    if (!reader->ensureFileLoaded("de440.bsp")) {
        if (!reader->ensureFileLoaded("de441_part-2.bsp")) {
            std::cerr << "[ERROR] Nessun SPK trovato" << std::endl;
            return 1;
        }
    }
    std::cerr << "[INFO] SPK caricato" << std::endl;
    
    // Dati stella dall'XML
    double ra_deg = 18.26476288 * 15.0;  // ore -> gradi
    double dec_deg = -8.9274210;
    double mag = 14.20;
    
    std::cerr << "[INFO] Star RA: " << ra_deg << "°, Dec: " << dec_deg << "°" << std::endl;
    
    // Data evento: 2026-03-10 (JD approssimativo)
    JulianDate jd_event(2461045.0);
    
    // Test posizione Terra
    Ephemeris eph(reader);
    Vector3D earth_pos = eph.getEarthPosition(jd_event);
    double dist_au = earth_pos.magnitude();
    
    std::cerr << "[RESULT] Earth distance on 2026-03-10: " << dist_au << " AU" << std::endl;
    std::cerr << "[RESULT] Expected: ~0.99-1.01 AU" << std::endl;
    
    // Test passa se la distanza è ragionevole
    if (dist_au > 0.98 && dist_au < 1.02) {
        std::cerr << "[PASS] Test base OK" << std::endl;
        return 0;
    } else {
        std::cerr << "[FAIL] Posizione Terra anomala" << std::endl;
        return 1;
    }
}
