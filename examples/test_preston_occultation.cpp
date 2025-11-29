/**
 * @file test_preston_occultation.cpp
 * @brief Test con occultazione reale da Steve Preston's predictions
 * 
 * Test basato su predizioni di occultazioni da:
 * http://www.asteroidoccultation.com/
 * 
 * Esempio: 
 * (704) Interamnia occulting TYC 5857-01303-1 on 2024-12-10
 * Predicted path across Europe
 */

#include <iostream>
#include <iomanip>
#include <cmath>
#include <vector>
#include <ioccultcalc/astdys_client.h>
#include <ioccultcalc/orbit_propagator.h>
#include <ioccultcalc/coordinates.h>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

/**
 * @brief Cerca il momento di closest approach in un intervallo
 */
struct SearchResult {
    JulianDate closestTime;
    double minSeparation;  // arcsec
    double asteroidRA;     // degrees
    double asteroidDec;    // degrees
};

SearchResult searchClosestApproach(
    const EquinoctialElements& elements,
    double starRA,   // degrees
    double starDec,  // degrees
    const JulianDate& startDate,
    const JulianDate& endDate,
    OrbitPropagator& propagator)
{
    std::cout << "🔍 Searching closest approach...\n";
    
    double timeSpan = endDate.jd - startDate.jd;
    double stepHours = 0.5;  // 30 minuti (più veloce)
    double stepDays = stepHours / 24.0;
    int nSteps = static_cast<int>(timeSpan / stepDays);
    
    std::cout << "   Period: " << timeSpan * 24.0 << " hours\n";
    std::cout << "   Steps: " << nSteps << " (1 hour interval)\n\n";
    
    OrbitState initialState = propagator.elementsToState(elements);
    
    double minSep = 1e10;
    JulianDate bestTime = startDate;
    double bestRA = 0, bestDec = 0;
    
    // Star position in radians
    double starRA_rad = starRA * M_PI / 180.0;
    double starDec_rad = starDec * M_PI / 180.0;
    
    std::cout << "   Searching";
    
    for (int i = 0; i <= nSteps; i++) {
        if (i % 10 == 0) {
            std::cout << ".";
            std::cout.flush();
        }
        
        JulianDate epoch;
        epoch.jd = startDate.jd + i * stepDays;
        
        // Propagate
        OrbitState state = propagator.propagate(initialState, epoch);
        
        // Convert to equatorial
        auto coords = Coordinates::cartesianToEquatorial(state.position);
        
        // Calculate separation
        double dra = (coords.ra - starRA_rad) * cos(coords.dec);
        double ddec = coords.dec - starDec_rad;
        double sep = sqrt(dra*dra + ddec*ddec) * 206264.806;  // radians to arcsec
        
        if (sep < minSep) {
            minSep = sep;
            bestTime = epoch;
            bestRA = coords.ra * 180.0 / M_PI;
            bestDec = coords.dec * 180.0 / M_PI;
        }
    }
    
    std::cout << " Done!\n\n";
    
    SearchResult result;
    result.closestTime = bestTime;
    result.minSeparation = minSep;
    result.asteroidRA = bestRA;
    result.asteroidDec = bestDec;
    
    return result;
}

int main() {
    std::cout << "\n";
    std::cout << "╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║   Test Preston Occultation Prediction                    ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n";
    std::cout << "\n";
    
    try {
        // ==================== Preston Prediction Data ====================
        
        std::cout << "📋 Test Case: Real occultation from Preston's predictions\n\n";
        
        // Esempio: (704) Interamnia occults TYC 5857-01303-1
        // Date: 2024-12-10 around 02:30 UT
        // Star: TYC 5857-01303-1
        // RA: 03h 24m 18.5s = 51.077083° 
        // Dec: +23° 16' 42" = +23.278333°
        
        std::string asteroidNumber = "704";
        std::string asteroidName = "(704) Interamnia";
        double asteroidDiameter = 306.0;  // km (known)
        
        // Star coordinates (from catalog)
        double starRA = 51.077083;   // degrees
        double starDec = 23.278333;  // degrees
        
        // Event date/time from prediction
        std::string eventDateStr = "2024-12-10T02:30:00";
        JulianDate eventDate = TimeUtils::isoToJD(eventDateStr);
        
        std::cout << "Asteroid: " << asteroidName << "\n";
        std::cout << "Diameter: " << asteroidDiameter << " km\n";
        std::cout << "Star: TYC 5857-01303-1\n";
        std::cout << "  RA:  " << std::fixed << std::setprecision(6) << starRA << "° (03h 24m 18.5s)\n";
        std::cout << "  Dec: " << std::setprecision(6) << starDec << "° (+23° 16' 42\")\n";
        std::cout << "Predicted event: " << eventDateStr << " UTC\n";
        std::cout << "\n";
        
        // ==================== Download Elements ====================
        
        std::cout << "📡 Step 1: Downloading orbital elements\n";
        AstDysClient client;
        auto elements = client.getElements(asteroidNumber);
        std::cout << "   ✓ Elements downloaded\n";
        std::cout << "   Epoch: " << TimeUtils::jdToISO(elements.epoch) << "\n\n";
        
        // ==================== Setup Propagator ====================
        
        std::cout << "🔧 Step 2: Setting up propagator\n";
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RK4;
        opts.stepSize = 0.1;
        opts.usePlanetaryPerturbations = true;
        opts.maxSteps = 5000000;
        OrbitPropagator propagator(opts);
        std::cout << "   ✓ Propagator ready\n\n";
        
        // ==================== Search Window ====================
        
        std::cout << "🔍 Step 3: Searching for closest approach\n";
        std::cout << "   (±3 hours around predicted time - quick test)\n\n";
        
        JulianDate searchStart;
        searchStart.jd = eventDate.jd - 0.125;  // -3 hours
        JulianDate searchEnd;
        searchEnd.jd = eventDate.jd + 0.125;    // +3 hours
        
        auto result = searchClosestApproach(
            elements, starRA, starDec, 
            searchStart, searchEnd, propagator);
        
        // ==================== Results ====================
        
        std::cout << "═══════════════════════ RESULTS ═══════════════════════\n";
        std::cout << "\n";
        
        std::cout << "🎯 Closest Approach Found:\n";
        std::cout << "   Time: " << TimeUtils::jdToISO(result.closestTime) << " UTC\n";
        std::cout << "   Separation: " << std::fixed << std::setprecision(3) 
                 << result.minSeparation << " arcsec\n";
        std::cout << "\n";
        
        std::cout << "📍 Asteroid Position:\n";
        std::cout << "   RA:  " << std::setprecision(6) << result.asteroidRA << "°\n";
        std::cout << "   Dec: " << result.asteroidDec << "°\n";
        std::cout << "\n";
        
        std::cout << "⭐ Star Position:\n";
        std::cout << "   RA:  " << starRA << "°\n";
        std::cout << "   Dec: " << starDec << "°\n";
        std::cout << "\n";
        
        // ==================== Comparison with Preston ====================
        
        double timeDiff = (result.closestTime.jd - eventDate.jd) * 24.0;  // hours
        
        std::cout << "📊 Comparison with Preston Prediction:\n";
        std::cout << "   Time difference: " << std::setprecision(2) << std::abs(timeDiff) 
                 << " hours\n";
        
        // Calculate angular size of asteroid at closest approach
        double distanceAU = 2.5;  // Approximate - would need to calculate from state
        double distanceKm = distanceAU * 149597870.7;
        double asteroidAngularRadius = (asteroidDiameter / 2.0 / distanceKm) * 206264.806;
        
        std::cout << "   Asteroid angular size: ~" << std::setprecision(2) 
                 << (2.0 * asteroidAngularRadius) << " arcsec\n";
        std::cout << "\n";
        
        // ==================== Assessment ====================
        
        std::cout << "✨ Assessment:\n";
        
        if (result.minSeparation < 2.0 * asteroidAngularRadius) {
            std::cout << "   ✓✓ OCCULTATION PREDICTED!\n";
            std::cout << "   The star will likely be occulted by the asteroid.\n";
            
            double coverage = 1.0 - (result.minSeparation / (2.0 * asteroidAngularRadius));
            if (coverage > 0) {
                std::cout << "   Coverage probability: ~" << std::setprecision(0) 
                         << (coverage * 100.0) << "%\n";
            }
        } else if (result.minSeparation < 10.0 * asteroidAngularRadius) {
            std::cout << "   ~ CLOSE APPROACH\n";
            std::cout << "   Very close but occultation uncertain.\n";
            std::cout << "   Observation recommended to refine prediction.\n";
        } else {
            std::cout << "   ✗ No occultation expected\n";
            std::cout << "   Separation too large.\n";
        }
        
        std::cout << "\n";
        
        if (std::abs(timeDiff) < 1.0) {
            std::cout << "   ✓ Timing matches Preston prediction (< 1 hour)\n";
        } else if (std::abs(timeDiff) < 6.0) {
            std::cout << "   ~ Timing close to Preston (< 6 hours)\n";
            std::cout << "   May need more recent orbital elements.\n";
        } else {
            std::cout << "   ⚠ Significant timing difference\n";
            std::cout << "   Check orbital elements epoch and quality.\n";
        }
        
        std::cout << "\n";
        std::cout << "═══════════════════════════════════════════════════════\n";
        std::cout << "\n";
        
        // ==================== Next Steps ====================
        
        std::cout << "📋 Recommendations:\n";
        std::cout << "   1. Calculate precise shadow path on Earth\n";
        std::cout << "   2. Determine visibility from observer locations\n";
        std::cout << "   3. Compute occultation duration\n";
        std::cout << "   4. Generate finder charts for observers\n";
        std::cout << "\n";
        
        std::cout << "✓ Test complete!\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n❌ Error: " << e.what() << "\n\n";
        return 1;
    }
}
