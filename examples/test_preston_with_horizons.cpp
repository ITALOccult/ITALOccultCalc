/**
 * @file test_preston_with_horizons.cpp
 * @brief Test occultazione Preston usando ELEMENTI OSCULANTI da Horizons
 * 
 * Test della predizione (704) Interamnia occulting TYC 5857-01303-1 
 * on 2024-12-10 usando elementi osculanti corretti da JPL Horizons
 * invece degli elementi medi di AstDyS.
 * 
 * Questo dovrebbe dare una predizione ACCURATA vicina a quella di Preston.
 */

#include <iostream>
#include <iomanip>
#include <cmath>
#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/coordinates.h"
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/time_utils.h"
#include "ioccultcalc/jpl_horizons_client.h"

using namespace ioccultcalc;

struct SearchResult {
    JulianDate closestTime;
    double minSeparation;  // arcsec
    double asteroidRA;     // degrees
    double asteroidDec;    // degrees
};

SearchResult searchClosestApproach(
    const AstDynEquinoctialElements& elements,
    double starRA, double starDec,
    const JulianDate& startDate,
    const JulianDate& endDate,
    OrbitPropagator& propagator,
    JPLHorizonsClient& horizons)
{
    double timeSpan = endDate.jd - startDate.jd;
    double stepDays = 1.0 / 24.0;  // 1 ora
    int nSteps = static_cast<int>(timeSpan / stepDays);
    
    SearchResult result;
    result.minSeparation = 1e10;
    
    std::cout << "  Cercando in " << nSteps << " step di 1 ora...\n";
    
    for (int i = 0; i <= nSteps; i++) {
        JulianDate currentTime;
        currentTime.jd = startDate.jd + i * stepDays;
        
        auto state = propagator.propagate(elements, currentTime);
        
        // CRUCIALE: Converti heliocentrico → geocentrico
        // Usa SPK/DE440s (ora corretto con frame ECLIPJ2000!)
        Vector3D earthPos = Ephemeris::getEarthPosition(currentTime);
        Vector3D geocentricPos = state.position - earthPos;
        
        // Conversione cartesiano → sferica (GEOCENTRICO)
        double r_mag = geocentricPos.magnitude();
        double ra_rad = atan2(geocentricPos.y, geocentricPos.x);
        double dec_rad = asin(geocentricPos.z / r_mag);
        
        double raHours = ra_rad * 12.0 / M_PI;
        if (raHours < 0) raHours += 24.0;
        double decDeg = dec_rad * 180.0 / M_PI;
        double raDeg = raHours * 15.0;
        
        // Separazione angolare
        double dRA = (raDeg - starRA);
        double dDec = (decDeg - starDec);
        
        // Correzione per crossing RA=0
        if (dRA > 180.0) dRA -= 360.0;
        if (dRA < -180.0) dRA += 360.0;
        
        double separation = sqrt(dRA * dRA * cos(starDec * M_PI / 180.0) * cos(starDec * M_PI / 180.0) + dDec * dDec) * 3600.0;
        
        if (separation < result.minSeparation) {
            result.minSeparation = separation;
            result.closestTime = currentTime;
            result.asteroidRA = raDeg;
            result.asteroidDec = decDeg;
        }
    }
    
    return result;
}

int main() {
    std::cout << "\n═══════════════════════════════════════════════════════\n";
    std::cout << "  Preston Occultation Test - WITH HORIZONS ELEMENTS\n";
    std::cout << "═══════════════════════════════════════════════════════\n\n";
    
    try {
        // Dati evento Preston
        std::string asteroidNumber = "704";
        double starRA = 51.077083;   // degrees (03h 24m 18.5s)
        double starDec = 23.278333;  // degrees (+23° 16' 42")
        std::string eventDateStr = "2024-12-10T02:30:00";
        JulianDate eventDate = TimeUtils::isoToJD(eventDateStr);
        double asteroidDiameter = 306.0;  // km
        
        std::cout << "Evento: (704) Interamnia occults TYC 5857-01303-1\n";
        std::cout << "Data predetta: 2024-12-10 02:30 UTC\n";
        std::cout << "Stella: RA=" << starRA << "°, Dec=" << starDec << "°\n";
        std::cout << "Diametro: " << asteroidDiameter << " km\n\n";
        
        // STEP 1: Ottieni elementi osculanti da Horizons
        std::cout << "STEP 1: Scarica elementi OSCULANTI da Horizons...\n";
        std::cout << "  Epoca: 2024-12-10 (epoca evento esatta)\n";
        
        JulianDate elemEpoch;
        elemEpoch.jd = eventDate.jd;  // Epoca esatta dell'evento
        
        AstDysClient client;
        std::cout << "  Downloading...\n";
        OrbitState state = client.getStateFromHorizons(asteroidNumber, elemEpoch);
        
        std::cout << "  ✓ Vettori ricevuti:\n";
        std::cout << "    r = " << state.position.magnitude() << " AU\n";
        
        // STEP 2: Converti in elementi
        std::cout << "\nSTEP 2: Converte in elementi osculanti...\n";
        PropagatorOptions opts;
        opts.usePlanetaryPerturbations = true;
        opts.stepSize = 0.1;
        
        OrbitPropagator propagator(opts);
        AstDynEquinoctialElements elements = propagator.stateToElements(state);
        elements.designation = "(704) Interamnia";
        
        std::cout << "  ✓ Elementi calcolati:\n";
        std::cout << "    a = " << std::fixed << std::setprecision(6) << elements.a << " AU\n";
        double ecc = sqrt(elements.h*elements.h + elements.k*elements.k);
        std::cout << "    e = " << ecc << "\n\n";
        
        // STEP 3: Elementi già all'epoca evento, no propagazione necessaria
        std::cout << "STEP 3: Elementi già all'epoca: " << TimeUtils::jdToISO(elemEpoch) << "\n";
        std::cout << "  (no propagazione necessaria per test iniziale)\n\n";
        
        // STEP 4: Cerca closest approach
        std::cout << "STEP 4: Cerca momento di massimo avvicinamento...\n";
        
        JulianDate searchStart, searchEnd;
        searchStart.jd = eventDate.jd - 0.25;  // ±6 ore
        searchEnd.jd = eventDate.jd + 0.25;
        
        JPLHorizonsClient horizonsClient;
        auto result = searchClosestApproach(elements, starRA, starDec, 
                                           searchStart, searchEnd, propagator, horizonsClient);
        
        std::cout << "\n";
        std::cout << "═══════════════════════════════════════════════════════\n";
        std::cout << "                    RISULTATI\n";
        std::cout << "═══════════════════════════════════════════════════════\n\n";
        
        std::cout << "Closest Approach:\n";
        std::cout << "  Tempo: " << TimeUtils::jdToISO(result.closestTime) << " UTC\n";
        std::cout << "  Separazione: " << std::fixed << std::setprecision(2) 
                  << result.minSeparation << " arcsec\n\n";
        
        std::cout << "Posizione asteroide:\n";
        std::cout << "  RA:  " << std::fixed << std::setprecision(6) 
                  << result.asteroidRA << "°\n";
        std::cout << "  Dec: " << result.asteroidDec << "°\n\n";
        
        std::cout << "Posizione stella:\n";
        std::cout << "  RA:  " << starRA << "°\n";
        std::cout << "  Dec: " << starDec << "°\n\n";
        
        // Valutazione
        double maxShadowArcsec = (asteroidDiameter / result.minSeparation) * 206265.0;
        
        std::cout << "Valutazione:\n";
        std::cout << "  Ombra massima: ~" << std::setprecision(1) 
                  << maxShadowArcsec << " arcsec\n";
        
        if (result.minSeparation < maxShadowArcsec / 2.0) {
            std::cout << "  ✓ OCCULTAZIONE POSSIBILE! (separazione < ombra)\n";
        } else if (result.minSeparation < maxShadowArcsec) {
            std::cout << "  ⚠ Occultazione marginale (vicino al bordo)\n";
        } else {
            std::cout << "  ✗ Miss (separazione > ombra asteroide)\n";
        }
        
        std::cout << "\n";
        std::cout << "Confronto con Preston:\n";
        std::cout << "  Tempo predetto Preston: 2024-12-10 02:30 UTC\n";
        std::cout << "  Tempo calcolato:        " << TimeUtils::jdToISO(result.closestTime) << "\n";
        
        double timeDiff = (result.closestTime.jd - eventDate.jd) * 24.0 * 60.0;  // minuti
        std::cout << "  Differenza: " << std::setprecision(1) << timeDiff << " minuti\n";
        
        if (fabs(timeDiff) < 30.0 && result.minSeparation < 2.0) {
            std::cout << "\n  ✓✓ ECCELLENTE! Predizione accurata!\n";
        } else if (fabs(timeDiff) < 60.0 && result.minSeparation < 5.0) {
            std::cout << "\n  ✓ BUONA predizione\n";
        } else {
            std::cout << "\n  ⚠ Predizione necessita calibrazione\n";
        }
        
        std::cout << "\n═══════════════════════════════════════════════════════\n\n";
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
