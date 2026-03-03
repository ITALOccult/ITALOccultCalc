#include <iostream>
#include <cmath>
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/coordinates.h"
#include "ioccultcalc/spice_spk_reader.h"

using namespace ioccultcalc;

int main() {
    std::cerr << "[TEST] Frame Consistency Start" << std::endl;

    // Carica SPICE
    auto reader = std::make_shared<SPICESPKReader>();
    if (!reader->ensureFileLoaded("de440.bsp")) {
        std::cerr << "[WARN] SPK non trovato, uso dati analitici" << std::endl;
    }

    // Inizializza provider
    initializeSpiceProvider(reader);

    // Test 1: Posizione Terra
    JulianDate jd(2459000.5);
    Vector3D pos = Ephemeris::getEarthPosition(jd);
    double dist = pos.magnitude() * 149597870.7; // km
    std::cerr << "[TEST] Earth distance: " << dist << " km (atteso ~150M)" << std::endl;

    // Test 2: Velocità Terra
    Vector3D vel = Ephemeris::getEarthVelocity(jd);
    double v_kms = vel.magnitude() * 149597870.7 / 86400.0; // km/s
    std::cerr << "[TEST] Earth velocity: " << v_kms << " km/s (atteso ~30)" << std::endl;

    // Test 3: Round-trip equatoriale-eclittico
    Vector3D eq(0, 0, 1);
    Vector3D ecl = Coordinates::equatorialToEcliptic(eq);
    Vector3D back = Coordinates::eclipticToEquatorial(ecl);
    double err = acos(std::max(-1.0, std::min(1.0, eq.dot(back)))) * 206265; // arcsec
    std::cerr << "[TEST] Round-trip error: " << err << " arcsec" << std::endl;

    return (err < 1.0 && v_kms > 25 && v_kms < 35) ? 0 : 1;
}
