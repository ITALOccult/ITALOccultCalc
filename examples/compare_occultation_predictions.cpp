/**
 * @file compare_occultation_predictions.cpp
 * @brief Confronta predizioni occultazione con e senza fix SPK
 * 
 * Mostra impatto del fix ECLIPJ2000 sulle predizioni
 */

#include <iostream>
#include <iomanip>
#include <cmath>
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/time_utils.h"
#include "ioccultcalc/coordinates.h"

using namespace ioccultcalc;

struct PredictionResult {
    JulianDate closestTime;
    double separation; // arcsec
    double asteroidRA;  // deg
    double asteroidDec; // deg
};

PredictionResult predictOccultation(
    const EquinoctialElements& elements,
    double starRA, double starDec,
    const JulianDate& startDate,
    const JulianDate& endDate,
    bool useHorizonsEarth) 
{
    PropagatorOptions opts;
    opts.integrator = IntegratorType::RK4;
    opts.stepSize = 0.1;
    opts.usePlanetaryPerturbations = true;
    OrbitPropagator propagator(opts);
    
    JPLHorizonsClient horizons;
    
    double minSeparation = 1e10;
    JulianDate closestTime = startDate;
    double closestRA = 0.0, closestDec = 0.0;
    
    // Cerca con step di 1 ora
    double stepDays = 1.0 / 24.0;
    int nSteps = int((endDate.jd - startDate.jd) / stepDays);
    
    for (int i = 0; i <= nSteps; i++) {
        JulianDate currentTime;
        currentTime.jd = startDate.jd + i * stepDays;
        
        auto state = propagator.propagate(elements, currentTime);
        
        // Posizione Terra
        Vector3D earthPos;
        if (useHorizonsEarth) {
            auto [pos, vel] = horizons.getStateVectors("399", currentTime, "@sun");
            earthPos = pos;
        } else {
            earthPos = Ephemeris::getEarthPosition(currentTime);
        }
        
        Vector3D geocentricPos = state.position - earthPos;
        
        // Conversione a RA/Dec
        double r_mag = geocentricPos.magnitude();
        double ra_rad = atan2(geocentricPos.y, geocentricPos.x);
        double dec_rad = asin(geocentricPos.z / r_mag);
        
        double raHours = ra_rad * 12.0 / M_PI;
        if (raHours < 0) raHours += 24.0;
        double decDeg = dec_rad * 180.0 / M_PI;
        double raDeg = raHours * 15.0;
        
        // Separazione angolare
        double dRA = (raDeg - starRA) * cos(starDec * M_PI / 180.0);
        double dDec = (decDeg - starDec);
        double separation = sqrt(dRA * dRA + dDec * dDec) * 3600.0; // arcsec
        
        if (separation < minSeparation) {
            minSeparation = separation;
            closestTime = currentTime;
            closestRA = raDeg;
            closestDec = decDeg;
        }
    }
    
    return {closestTime, minSeparation, closestRA, closestDec};
}

int main() {
    std::cout << std::fixed << std::setprecision(6);
    
    std::cout << "\n╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║   CONFRONTO PREDIZIONI OCCULTAZIONE                       ║\n";
    std::cout << "║   Prima e Dopo Fix ECLIPJ2000                             ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n\n";
    
    // Evento test: (704) Interamnia × TYC 5857-01303-1
    std::cout << "Evento: (704) Interamnia × TYC 5857-01303-1\n";
    std::cout << "Stella: RA=51.077083° Dec=+23.278333°\n";
    std::cout << "Data predizione Preston: 2024-12-10 02:30 UTC\n\n";
    
    double starRA = 51.077083;
    double starDec = 23.278333;
    
    // Download elementi da Horizons
    std::cout << "Download elementi osculanti da Horizons...\n";
    
    AstDysClient client;
    JPLHorizonsClient horizons;
    
    JulianDate epoch = TimeUtils::isoToJD("2024-12-10T00:00:00");
    
    std::cout << "  Epoca: 2024-12-10 00:00 UTC\n";
    
    auto state = client.getStateFromHorizons("704", epoch);
    
    // Converti stato in elementi
    PropagatorOptions opts;
    opts.integrator = IntegratorType::RK4;
    OrbitPropagator propagator(opts);
    auto elements = propagator.stateToElements(state);
    
    std::cout << "  ✓ a=" << elements.a << " AU\n";
    std::cout << "  ✓ e=" << sqrt(elements.h*elements.h + elements.k*elements.k) << "\n\n";
    
    // Finestra di ricerca
    JulianDate searchStart = TimeUtils::isoToJD("2024-12-09T12:00:00");
    JulianDate searchEnd = TimeUtils::isoToJD("2024-12-10T12:00:00");
    
    std::cout << "╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║   1. PREDIZIONE CON HORIZONS (riferimento perfetto)      ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n\n";
    
    std::cout << "Calcolo posizione Terra da Horizons API...\n";
    auto resultHorizons = predictOccultation(elements, starRA, starDec, 
                                             searchStart, searchEnd, true);
    
    std::cout << "\nRisultato:\n";
    std::cout << "  Tempo closest approach: " << TimeUtils::jdToISO(resultHorizons.closestTime) << "\n";
    std::cout << "  Separazione minima:     " << int(resultHorizons.separation) << " arcsec (";
    std::cout << std::setprecision(2) << (resultHorizons.separation / 3600.0) << "°)\n";
    std::cout << "  Posizione asteroide:    RA=" << std::setprecision(6) << resultHorizons.asteroidRA;
    std::cout << "° Dec=" << resultHorizons.asteroidDec << "°\n";
    
    std::cout << "\n╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║   2. PREDIZIONE CON SPK/DE440s ECLIPJ2000 (locale)       ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n\n";
    
    std::cout << "Calcolo posizione Terra da SPK locale (frame ECLIPJ2000)...\n";
    auto resultSPK = predictOccultation(elements, starRA, starDec,
                                        searchStart, searchEnd, false);
    
    std::cout << "\nRisultato:\n";
    std::cout << "  Tempo closest approach: " << TimeUtils::jdToISO(resultSPK.closestTime) << "\n";
    std::cout << "  Separazione minima:     " << int(resultSPK.separation) << " arcsec (";
    std::cout << std::setprecision(2) << (resultSPK.separation / 3600.0) << "°)\n";
    std::cout << "  Posizione asteroide:    RA=" << std::setprecision(6) << resultSPK.asteroidRA;
    std::cout << "° Dec=" << resultSPK.asteroidDec << "°\n";
    
    // Confronto
    std::cout << "\n╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║   CONFRONTO TRA LE DUE PREDIZIONI                         ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n\n";
    
    double timeDiff = (resultSPK.closestTime.jd - resultHorizons.closestTime.jd) * 24.0 * 60.0; // minuti
    double sepDiff = resultSPK.separation - resultHorizons.separation;
    double raDiff = (resultSPK.asteroidRA - resultHorizons.asteroidRA) * 3600.0; // arcsec
    double decDiff = (resultSPK.asteroidDec - resultHorizons.asteroidDec) * 3600.0; // arcsec
    
    std::cout << "Differenze (SPK - Horizons):\n\n";
    
    std::cout << "  Tempo:        " << std::setw(10) << std::setprecision(1) << timeDiff << " minuti\n";
    std::cout << "  Separazione:  " << std::setw(10) << std::setprecision(1) << sepDiff << " arcsec\n";
    std::cout << "  RA:           " << std::setw(10) << std::setprecision(1) << raDiff << " arcsec\n";
    std::cout << "  Dec:          " << std::setw(10) << std::setprecision(1) << decDiff << " arcsec\n\n";
    
    // Posizione stella per riferimento
    std::cout << "Posizione stella target:\n";
    std::cout << "  RA:  " << starRA << "°\n";
    std::cout << "  Dec: " << starDec << "°\n\n";
    
    // Valutazione
    std::cout << "╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║   VALUTAZIONE DIFFERENZE                                  ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n\n";
    
    std::cout << "Differenze attese con errore Terra ~260k km:\n\n";
    
    if (std::abs(timeDiff) < 60) {
        std::cout << "✓ Tempo: <60 min differenza\n";
    } else {
        std::cout << "⚠ Tempo: >" << int(std::abs(timeDiff)) << " min differenza\n";
    }
    
    if (std::abs(sepDiff) < 200) {
        std::cout << "✓ Separazione: <200\" differenza\n";
    } else {
        std::cout << "⚠ Separazione: >" << int(std::abs(sepDiff)) << "\" differenza\n";
    }
    
    double posError = sqrt(raDiff*raDiff + decDiff*decDiff);
    if (posError < 200) {
        std::cout << "✓ Posizione: <200\" errore totale\n";
    } else {
        std::cout << "⚠ Posizione: " << int(posError) << "\" errore totale\n";
    }
    
    std::cout << "\n═══════════════════════════════════════════════════════════\n";
    std::cout << "                    CONCLUSIONE\n";
    std::cout << "═══════════════════════════════════════════════════════════\n\n";
    
    std::cout << "Impatto errore Terra (260k km) sulla predizione:\n\n";
    std::cout << "  • Errore temporale: ~" << int(std::abs(timeDiff)) << " minuti\n";
    std::cout << "  • Errore posizionale: ~" << int(posError) << " arcsec (~" ;
    std::cout << std::setprecision(2) << (posError/60.0) << " arcmin)\n\n";
    
    if (posError < 60) {
        std::cout << "✓✓ Predizione SPK locale è accurata per occultazioni\n";
        std::cout << "   (errore <1 arcmin accettabile per la maggior parte degli eventi)\n";
    } else if (posError < 300) {
        std::cout << "✓ Predizione SPK locale è utilizzabile\n";
        std::cout << "   (errore <5 arcmin, sufficiente per eventi grandi)\n";
    } else {
        std::cout << "⚠ Predizione SPK potrebbe richiedere calibrazione\n";
        std::cout << "   (errore >" << int(posError/60) << " arcmin significativo)\n";
    }
    
    std::cout << "\n";
    
    return 0;
}
