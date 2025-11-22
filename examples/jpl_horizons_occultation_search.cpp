/**
 * @file jpl_horizons_occultation_search.cpp
 * @brief Ricerca occultazioni usando elementi orbitali da AstDyS/JPL Horizons
 * 
 * Procedura completa:
 * 1. Scarica elementi orbitali da AstDyS per qualsiasi asteroide
 * 2. Propaga gli elementi al momento desiderato usando IOccultCalc
 * 3. Calcola geometria occultazione (distanza stella-asteroide)
 * 4. Determina visibilità e probabilità
 * 
 * Uso:
 *   ./jpl_horizons_occultation_search <asteroid_id> <star_ra> <star_dec> <start_date> <end_date> [diameter]
 * 
 * Esempio:
 *   ./jpl_horizons_occultation_search 433 10.684 41.269 2026-01-01 2026-12-31
 *   ./jpl_horizons_occultation_search 4 137.302 25.788 2026-01-01 2026-03-31 525
 *   (Cerca occultazioni di stelle da parte di 433 Eros o 4 Vesta)
 */

#include <iostream>
#include <iomanip>
#include <cmath>
#include <vector>
#include <algorithm>
#include <ioccultcalc/astdys_client.h>
#include <ioccultcalc/orbit_propagator.h>
#include <ioccultcalc/orbital_elements.h>
#include <ioccultcalc/coordinates.h>
#include <ioccultcalc/time_utils.h>
#include <ioccultcalc/jpl_ephemeris.h>
#include <ioccultcalc/types.h>

using namespace ioccultcalc;

/**
 * @brief Risultato di un'occultazione calcolata
 */
struct OccultationResult {
    JulianDate epoch;
    Vector3D asteroidPos;      // AU (heliocentrico)
    Vector3D earthPos;         // AU (heliocentrico)
    double raAsteroid;         // radianti (geocentrico)
    double decAsteroid;        // radianti (geocentrico)
    double raStar;             // radianti
    double decStar;            // radianti
    double separation;         // arcsecondi (separazione angolare)
    double shadowWidth;        // km (larghezza dell'ombra)
    double distance;           // AU (distanza Terra-asteroide)
    double magnitude;          // mag apparente asteroide
    bool isOccultation;        // true se separazione < raggio asteroide proiettato
    double probability;        // 0-1 (probabilità occultazione)
    
    OccultationResult() 
        : raAsteroid(0), decAsteroid(0), raStar(0), decStar(0),
          separation(0), shadowWidth(0), distance(0), magnitude(0),
          isOccultation(false), probability(0) {}
};

/**
 * @brief Converte RA/Dec (gradi) in radianti
 */
void parseCoordinates(const std::string& raStr, const std::string& decStr,
                     double& ra, double& dec) {
    ra = std::stod(raStr) * M_PI / 180.0;
    dec = std::stod(decStr) * M_PI / 180.0;
}

/**
 * @brief Calcola separazione angolare tra due punti sulla sfera celeste
 * Formula dell'haversine per evitare problemi numerici
 */
double angularSeparation(double ra1, double dec1, double ra2, double dec2) {
    double sdlat = sin((dec2 - dec1) / 2.0);
    double sdlon = sin((ra2 - ra1) / 2.0);
    double a = sdlat * sdlat + cos(dec1) * cos(dec2) * sdlon * sdlon;
    double c = 2.0 * asin(sqrt(a));
    return c * 180.0 / M_PI * 3600.0; // radianti -> arcsecondi
}

/**
 * @brief Scarica elementi orbitali da AstDyS per qualsiasi asteroide
 */
EquinoctialElements getOrbitalElements(const std::string& targetId, JulianDate epoch) {
    std::cout << "📡 Downloading orbital elements for asteroid: " << targetId << "\n";
    std::cout << "   Source: AstDyS/Lowell Observatory system\n";
    
    try {
        // Usa AstDyS client per scaricare elementi aggiornati
        ioccultcalc::AstDysClient client;
        
        // AstDyS accetta direttamente il numero come stringa
        auto elements = client.getElements(targetId);
        
        std::cout << "   ✓ Elements downloaded successfully\n";
        std::cout << "   Epoch: " << TimeUtils::jdToISO(elements.epoch) << "\n";
        std::cout << "   Name: " << elements.name << "\n\n";
        
        // Converti a elementi Kepleriani per visualizzazione
        OrbitalElements kepler = elements.toKeplerian();
        
        std::cout << "   Orbital elements:\n";
        std::cout << "     a = " << kepler.a << " AU\n";
        std::cout << "     e = " << kepler.e << "\n";
        std::cout << "     i = " << kepler.i * 180.0 / M_PI << "°\n";
        std::cout << "     Ω = " << kepler.Omega * 180.0 / M_PI << "°\n";
        std::cout << "     ω = " << kepler.omega * 180.0 / M_PI << "°\n";
        std::cout << "     M = " << kepler.M * 180.0 / M_PI << "°\n";
        if (kepler.H > 0) {
            std::cout << "     H = " << kepler.H << " mag\n";
        }
        if (kepler.diameter > 0) {
            std::cout << "     Diameter = " << kepler.diameter << " km\n";
        }
        std::cout << "\n";
        
        return elements;
        
    } catch (const std::exception& e) {
        std::cerr << "❌ Error downloading elements: " << e.what() << "\n";
        std::cerr << "   Please check:\n";
        std::cerr << "   - Asteroid number/designation is valid\n";
        std::cerr << "   - Network connection is working\n";
        std::cerr << "   - AstDyS service is available\n\n";
        throw;
    }
}

/**
 * @brief Propaga elementi orbitali a una nuova epoca usando propagatore condiviso
 */
OrbitState propagateToEpoch(OrbitPropagator& propagator, const EquinoctialElements& elements, JulianDate targetEpoch) {
    // Converti elementi in stato vettoriale all'epoca iniziale
    OrbitState initialState = propagator.elementsToState(elements);
    
    // Propaga
    OrbitState finalState = propagator.propagate(initialState, targetEpoch);
    
    return finalState;
}

/**
 * @brief Calcola geometria occultazione per una singola epoca
 */
OccultationResult calculateOccultationGeometry(
    const OrbitState& asteroidState,
    double starRA,
    double starDec,
    double asteroidDiameter) 
{
    OccultationResult result;
    result.epoch = asteroidState.epoch;
    result.asteroidPos = asteroidState.position;
    result.raStar = starRA;
    result.decStar = starDec;
    
    // Calcola posizione Terra usando il ForceModel (VSOP87)
    ForceModel forces;
    Vector3D earthPosHelio = forces.getBodyPosition(PerturbingBody::EARTH, asteroidState.epoch.jd);
    result.earthPos = earthPosHelio;
    
    // Posizione asteroide rispetto alla Terra (geocentrico)
    Vector3D asteroidGeo;
    asteroidGeo.x = asteroidState.position.x - earthPosHelio.x;
    asteroidGeo.y = asteroidState.position.y - earthPosHelio.y;
    asteroidGeo.z = asteroidState.position.z - earthPosHelio.z;
    
    // Distanza Terra-asteroide
    result.distance = sqrt(asteroidGeo.x * asteroidGeo.x + 
                          asteroidGeo.y * asteroidGeo.y + 
                          asteroidGeo.z * asteroidGeo.z);
    
    // Converti posizione geocentrica in RA/Dec
    EquatorialCoordinates eqCoords = Coordinates::cartesianToEquatorial(asteroidGeo);
    result.raAsteroid = eqCoords.ra;
    result.decAsteroid = eqCoords.dec;
    
    // Calcola separazione angolare stella-asteroide
    result.separation = angularSeparation(
        result.raAsteroid, result.decAsteroid,
        starRA, starDec
    );
    
    // Calcola raggio angolare asteroide visto dalla Terra
    double radiusKm = asteroidDiameter / 2.0; // km
    double distanceKm = result.distance * 149597870.7; // AU -> km
    double angularRadiusRad = atan(radiusKm / distanceKm); // radianti
    double angularRadiusArcsec = angularRadiusRad * 180.0 / M_PI * 3600.0;
    
    // Larghezza dell'ombra (proiezione su superficie Terra)
    result.shadowWidth = asteroidDiameter * (1.0 - result.distance / 1.0); // semplificato
    
    // Determina se c'è occultazione
    result.isOccultation = (result.separation < angularRadiusArcsec);
    
    // Probabilità: Gaussiana basata su separazione vs raggio
    double sigma = angularRadiusArcsec * 2.0; // 2 sigma = raggio
    double chi2 = pow(result.separation / sigma, 2.0);
    result.probability = exp(-chi2 / 2.0);
    
    // Stima magnitudine apparente (formula semplificata)
    double phase = 0.0; // angolo di fase (semplificato = 0)
    result.magnitude = result.asteroidPos.x + 5.0 * log10(result.distance * result.distance);
    
    return result;
}

/**
 * @brief Main: ricerca occultazioni in un intervallo temporale
 */
int main(int argc, char* argv[]) {
    std::cout << "\n";
    std::cout << "╔════════════════════════════════════════════════════════════╗\n";
    std::cout << "║  IOccultCalc - Occultation Search (Any Asteroid)          ║\n";
    std::cout << "║  Propagazione con AST17 (17 asteroidi massivi)            ║\n";
    std::cout << "╚════════════════════════════════════════════════════════════╝\n\n";
    
    // Parametri default
    std::string asteroidId = "433";         // 433 Eros
    double starRA = 88.79293899 * M_PI / 180.0;   // Betelgeuse (gradi -> rad)
    double starDec = 7.40703634 * M_PI / 180.0;   // Betelgeuse
    std::string startDateStr = "2026-01-01";
    std::string endDateStr = "2026-12-31";
    double asteroidDiameter = 0.0; // Sarà scaricato da AstDyS
    
    // Parse argomenti
    if (argc > 1) asteroidId = argv[1];
    if (argc > 2) starRA = std::stod(argv[2]) * M_PI / 180.0;
    if (argc > 3) starDec = std::stod(argv[3]) * M_PI / 180.0;
    if (argc > 4) startDateStr = argv[4];
    if (argc > 5) endDateStr = argv[5];
    if (argc > 6) asteroidDiameter = std::stod(argv[6]);
    
    std::cout << "📋 Parameters:\n";
    std::cout << "   Asteroid ID: " << asteroidId << "\n";
    std::cout << "   Star RA:  " << starRA * 180.0 / M_PI << "° (J2000)\n";
    std::cout << "   Star Dec: " << starDec * 180.0 / M_PI << "° (J2000)\n";
    std::cout << "   Start: " << startDateStr << "\n";
    std::cout << "   End:   " << endDateStr << "\n";
    if (asteroidDiameter > 0) {
        std::cout << "   Asteroid diameter (user): " << asteroidDiameter << " km\n";
    }
    std::cout << "\n";
    
    try {
        // 1. Carica elementi orbitali da AstDyS
        JulianDate startDate = TimeUtils::isoToJD(startDateStr);
        JulianDate endDate = TimeUtils::isoToJD(endDateStr);
        
        EquinoctialElements elements = getOrbitalElements(asteroidId, startDate);
        
        // Converti a Kepleriani per leggere H e diameter
        OrbitalElements kepler = elements.toKeplerian();
        
        // Se il diametro non è specificato dall'utente, usa quello da AstDyS
        if (asteroidDiameter <= 0 && kepler.diameter > 0) {
            asteroidDiameter = kepler.diameter;
            std::cout << "   Using diameter from AstDyS: " << asteroidDiameter << " km\n\n";
        } else if (asteroidDiameter <= 0 && kepler.H > 0) {
            // Default: stima dal H (magnitudine assoluta)
            // Formula: D (km) ≈ 1329 * 10^(-H/5) / sqrt(albedo)
            // Assumiamo albedo = 0.15 (tipico per asteroidi di tipo S)
            asteroidDiameter = 1329.0 * pow(10.0, -kepler.H / 5.0) / sqrt(0.15);
            std::cout << "   ⚠️  Diameter estimated from H=" << kepler.H 
                     << ": ~" << asteroidDiameter << " km (albedo=0.15)\n\n";
        } else if (asteroidDiameter <= 0) {
            // Fallback: 20 km (tipico NEA)
            asteroidDiameter = 20.0;
            std::cout << "   ⚠️  Diameter not available, using default: 20 km\n\n";
        }
        
        // 2. Setup propagatore (una volta sola per efficienza)
        std::cout << "🔧 Initializing propagator with AST17...\n";
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RK4;
        opts.stepSize = 0.05;  // 0.05 giorni = ottimo trade-off
        opts.usePlanetaryPerturbations = true;
        opts.useRelativisticCorrections = false;
        opts.useSolarRadiationPressure = false;
        opts.useNonGravitational = false;
        opts.maxSteps = 1000000;
        
        OrbitPropagator propagator(opts);
        std::cout << "   ✓ Propagator ready\n\n";
        
        // 3. Cerca occultazioni nell'intervallo
        std::cout << "🔍 Searching occultations...\n";
        double timeSpan = endDate.jd - startDate.jd;
        std::cout << "   Time span: " << timeSpan << " days\n";
        
        double stepDays = 0.5; // Campiona ogni 12 ore
        int nSteps = static_cast<int>(timeSpan / stepDays);
        
        std::cout << "   Steps: " << nSteps << " (every " << stepDays << " days)\n\n";
        
        std::vector<OccultationResult> candidates;
        int progressCounter = 0;
        
        for (int i = 0; i <= nSteps; i++) {
            JulianDate currentEpoch(startDate.jd + i * stepDays);
            
            // Progress indicator
            if (i % 100 == 0 || i == nSteps) {
                std::cout << "\r   Progress: " << std::fixed << std::setprecision(1) 
                         << (100.0 * i / nSteps) << "% [" << i << "/" << nSteps << "]" 
                         << std::flush;
            }
            
            // Propaga a questa epoca (usa propagatore condiviso)
            OrbitState state = propagateToEpoch(propagator, elements, currentEpoch);
            
            // Calcola geometria
            OccultationResult result = calculateOccultationGeometry(
                state, starRA, starDec, asteroidDiameter
            );
            
            // Se candidato promettente (separazione < 60 arcsec o probabilità > 1%)
            if (result.separation < 60.0 || result.probability > 0.01) {
                candidates.push_back(result);
            }
        }
        
        std::cout << "\n\n";
        
        // 4. Mostra risultati
        std::cout << "═══════════════════════════════════════════════════════════\n";
        std::cout << "RESULTS: " << candidates.size() << " candidate occultations found\n";
        std::cout << "═══════════════════════════════════════════════════════════\n\n";
        
        if (candidates.empty()) {
            std::cout << "No close approaches found in this time span.\n";
            std::cout << "Try:\n";
            std::cout << "  - Longer time span\n";
            std::cout << "  - Different star coordinates\n";
            std::cout << "  - Different asteroid\n\n";
        } else {
            // Ordina per separazione (migliori prima)
            std::sort(candidates.begin(), candidates.end(),
                     [](const OccultationResult& a, const OccultationResult& b) {
                         return a.separation < b.separation;
                     });
            
            std::cout << std::fixed << std::setprecision(2);
            std::cout << "┌────────────────────┬──────────┬──────────┬──────────┬──────────┐\n";
            std::cout << "│ Date (UTC)         │ Sep (\")  │ Prob (%) │ Dist(AU) │ Status   │\n";
            std::cout << "├────────────────────┼──────────┼──────────┼──────────┼──────────┤\n";
            
            for (size_t i = 0; i < std::min(candidates.size(), size_t(20)); i++) {
                const auto& c = candidates[i];
                std::string dateStr = TimeUtils::jdToISO(c.epoch);
                std::string status = c.isOccultation ? "✓ OCCULT" : "  Close";
                
                std::cout << "│ " << std::setw(18) << dateStr << " │ "
                         << std::setw(8) << c.separation << " │ "
                         << std::setw(8) << (c.probability * 100.0) << " │ "
                         << std::setw(8) << c.distance << " │ "
                         << std::setw(8) << status << " │\n";
            }
            
            std::cout << "└────────────────────┴──────────┴──────────┴──────────┴──────────┘\n\n";
            
            // Dettagli del migliore candidato
            if (!candidates.empty()) {
                const auto& best = candidates[0];
                std::cout << "🎯 Best candidate:\n";
                std::cout << "   Date: " << TimeUtils::jdToISO(best.epoch) << "\n";
                std::cout << "   Separation: " << best.separation << " arcsec\n";
                std::cout << "   Probability: " << (best.probability * 100.0) << " %\n";
                std::cout << "   Distance: " << best.distance << " AU\n";
                std::cout << "   Shadow width: ~" << best.shadowWidth << " km\n";
                std::cout << "   Asteroid RA/Dec: " << (best.raAsteroid * 180.0 / M_PI) 
                         << "° / " << (best.decAsteroid * 180.0 / M_PI) << "°\n";
                std::cout << "   Star RA/Dec: " << (best.raStar * 180.0 / M_PI) 
                         << "° / " << (best.decStar * 180.0 / M_PI) << "°\n\n";
            }
        }
        
        std::cout << "✓ Search complete!\n\n";
        
    } catch (const std::exception& e) {
        std::cerr << "❌ Error: " << e.what() << "\n\n";
        return 1;
    }
    
    return 0;
}
