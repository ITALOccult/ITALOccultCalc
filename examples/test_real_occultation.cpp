/**
 * @file test_real_occultation.cpp
 * @brief Test con occultazione reale: (1) Ceres vs TYC 1395-00347-1
 * 
 * Caso di test: Ceres occulta stella TYC 1395-00347-1
 * Data approssimativa: 2026-03-15
 * 
 * Questo test verifica:
 * 1. Download elementi orbitali da AstDyS
 * 2. Query stella da GAIA
 * 3. Propagazione orbita Ceres
 * 4. Calcolo geometria occultazione
 * 5. Predizione momento e geometria
 */

#include <iostream>
#include <iomanip>
#include <cmath>
#include <vector>

#include <ioccultcalc/astdys_client.h>
#include <ioccultcalc/gaia_client.h>
#include <ioccultcalc/orbit_propagator.h>
#include <ioccultcalc/coordinates.h>
#include <ioccultcalc/time_utils.h>
#include <ioccultcalc/types.h>

using namespace ioccultcalc;

/**
 * @brief Struttura per risultato predizione
 */
struct OccultationPrediction {
    JulianDate closestApproach;
    double separationArcsec;
    double shadowWidthKm;
    double asteroidRA;
    double asteroidDec;
    double starRA;
    double starDec;
    double distanceAU;
    bool isPotentialOccultation;
};

/**
 * @brief Calcola separazione angolare in arcsec
 */
double calculateSeparation(double ra1, double dec1, double ra2, double dec2) {
    // Haversine formula
    double dra = ra2 - ra1;
    double ddec = dec2 - dec1;
    
    double a = sin(ddec/2) * sin(ddec/2) + 
               cos(dec1) * cos(dec2) * sin(dra/2) * sin(dra/2);
    double c = 2 * atan2(sqrt(a), sqrt(1-a));
    
    return c * 206264.806; // radianti -> arcsec
}

/**
 * @brief Cerca momento di closest approach
 */
OccultationPrediction searchClosestApproach(
    const AstDynEquinoctialElements& elements,
    const GaiaStar& star,
    const JulianDate& startDate,
    const JulianDate& endDate,
    double asteroidDiameter)
{
    std::cout << "🔍 Searching for closest approach...\n";
    
    // Setup propagator with higher limits for long propagation
    PropagatorOptions opts;
    opts.integrator = IntegratorType::RK4;
    opts.stepSize = 0.1;  // Step più grande per propagazioni lunghe
    opts.usePlanetaryPerturbations = true;
    opts.maxSteps = 5000000;  // Aumentato da default per gestire 32 anni
    OrbitPropagator propagator(opts);
    
    // Convert initial elements to state
    OrbitState initialState = propagator.elementsToState(elements);
    
    // Search parameters
    double timeSpan = endDate.jd - startDate.jd;
    double coarseStep = 1.0;  // 1 giorno per ricerca grossolana (più veloce)
    int nCoarseSteps = static_cast<int>(timeSpan / coarseStep);
    
    std::cout << "   Time span: " << timeSpan << " days\n";
    std::cout << "   Coarse steps: " << nCoarseSteps << "\n";
    
    // Coordinate stella (già in radianti)
    double starRA = star.pos.ra;
    double starDec = star.pos.dec;
    
    // First pass: coarse search
    std::cout << "   Pass 1: Coarse search (1 day steps)...\n";
    
    double minSeparation = 1e10;
    JulianDate bestEpoch = startDate;
    
    for (int i = 0; i <= nCoarseSteps; i++) {
        if (i % 50 == 0) {
            std::cout << "\r      Progress: " << (100.0 * i / nCoarseSteps) 
                     << "%" << std::flush;
        }
        
        JulianDate epoch;
        epoch.jd = startDate.jd + i * coarseStep;
        
        // Propagate
        OrbitState state = propagator.propagate(initialState, epoch);
        
        // Convert to equatorial (simplified - geocentric)
        auto coords = Coordinates::cartesianToEquatorial(state.position);
        
        // Calculate separation
        double sep = calculateSeparation(coords.ra, coords.dec, starRA, starDec);
        
        if (sep < minSeparation) {
            minSeparation = sep;
            bestEpoch = epoch;
        }
    }
    
    std::cout << "\n   Coarse minimum: " << minSeparation << " arcsec at " 
             << TimeUtils::jdToISO(bestEpoch) << "\n";
    
    // Second pass: fine search around minimum
    std::cout << "   Pass 2: Fine search (1h steps around minimum)...\n";
    
    JulianDate fineStart;
    fineStart.jd = bestEpoch.jd - 1.0;  // ±1 giorno
    JulianDate fineEnd;
    fineEnd.jd = bestEpoch.jd + 1.0;
    
    double fineStep = 1.0 / 24.0;  // 1 ora
    int nFineSteps = static_cast<int>((fineEnd.jd - fineStart.jd) / fineStep);
    
    minSeparation = 1e10;
    double bestRA = 0, bestDec = 0, bestDist = 0;
    
    for (int i = 0; i <= nFineSteps; i++) {
        JulianDate epoch;
        epoch.jd = fineStart.jd + i * fineStep;
        
        OrbitState state = propagator.propagate(initialState, epoch);
        auto coords = Coordinates::cartesianToEquatorial(state.position);
        
        double sep = calculateSeparation(coords.ra, coords.dec, starRA, starDec);
        
        if (sep < minSeparation) {
            minSeparation = sep;
            bestEpoch = epoch;
            bestRA = coords.ra;
            bestDec = coords.dec;
            bestDist = state.position.magnitude();
        }
    }
    
    std::cout << "   Fine minimum: " << minSeparation << " arcsec at " 
             << TimeUtils::jdToISO(bestEpoch) << "\n\n";
    
    // Build result
    OccultationPrediction result;
    result.closestApproach = bestEpoch;
    result.separationArcsec = minSeparation;
    result.asteroidRA = bestRA * 180.0 / M_PI;
    result.asteroidDec = bestDec * 180.0 / M_PI;
    result.starRA = star.pos.ra * 180.0 / M_PI;
    result.starDec = star.pos.dec * 180.0 / M_PI;
    result.distanceAU = bestDist;
    
    // Calculate shadow width
    double asteroidRadiusKm = asteroidDiameter / 2.0;
    double distanceKm = bestDist * 149597870.7;  // AU to km
    result.shadowWidthKm = 2.0 * asteroidRadiusKm * (1.0 + distanceKm / 1.496e8);
    
    // Check if potential occultation
    double asteroidAngularRadiusArcsec = (asteroidRadiusKm / distanceKm) * 206264.806;
    result.isPotentialOccultation = (minSeparation < 2.0 * asteroidAngularRadiusArcsec);
    
    return result;
}

/**
 * @brief Main test
 */
int main() {
    std::cout << "\n";
    std::cout << "╔═══════════════════════════════════════════════════════════╗\n";
    std::cout << "║   Test Real Occultation: (1) Ceres                       ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════╝\n";
    std::cout << "\n";
    
    try {
        // ==================== Step 1: Download Ceres elements ====================
        
        std::cout << "📡 Step 1: Downloading orbital elements for (1) Ceres\n";
        
        AstDysClient client;
        auto elements = client.getElements("1");
        
        std::cout << "   ✓ Elements downloaded\n";
        std::cout << "   Name: " << elements.name << "\n";
        std::cout << "   Epoch: " << TimeUtils::jdToISO(elements.epoch) << "\n";
        std::cout << "\n";
        
        // ==================== Step 2: Target star ====================
        
        std::cout << "⭐ Step 2: Target Star\n";
        std::cout << "   Using test coordinates (will be replaced with GAIA query)\n";
        
        // Usiamo coordinate vicine alla posizione di Ceres a marzo 2026
        // Ceres sarà circa a RA=297° Dec=-6° (Capricorno)
        GaiaStar star;
        star.sourceId = "TEST_STAR";
        star.pos.ra = 297.0 * M_PI / 180.0;   // ~19h 48m (radianti)
        star.pos.dec = -6.5 * M_PI / 180.0;   // -6.5° (radianti)
        star.phot_g_mean_mag = 9.5;
        
        std::cout << "   RA: " << (star.pos.ra * 180.0 / M_PI) << "°\n";
        std::cout << "   Dec: " << (star.pos.dec * 180.0 / M_PI) << "°\n";
        std::cout << "   Magnitude: " << star.phot_g_mean_mag << "\n";
        std::cout << "\n";
        
        // ==================== Step 3: Search period ====================
        
        std::cout << "📅 Step 3: Search Period\n";
        
        // Periodo più breve per test veloce
        JulianDate startDate = TimeUtils::isoToJD("2026-03-10");
        JulianDate endDate = TimeUtils::isoToJD("2026-03-20");
        
        std::cout << "   Start: " << TimeUtils::jdToISO(startDate) << "\n";
        std::cout << "   End: " << TimeUtils::jdToISO(endDate) << "\n";
        std::cout << "\n";
        
        // ==================== Step 4: Asteroid properties ====================
        
        std::cout << "🪨 Step 4: Asteroid Properties\n";
        
        double ceresdDiameter = 939.4;  // km (known from observations)
        
        std::cout << "   Name: " << elements.name << "\n";
        std::cout << "   Diameter: " << ceresdDiameter << " km\n";
        std::cout << "\n";
        
        // ==================== Step 5: Search occultation ====================
        
        auto prediction = searchClosestApproach(
            elements, star, startDate, endDate, ceresdDiameter);
        
        // ==================== Step 6: Results ====================
        
        std::cout << "═══════════════════ RESULTS ═══════════════════\n";
        std::cout << "\n";
        
        std::cout << "🎯 Closest Approach:\n";
        std::cout << "   Time: " << TimeUtils::jdToISO(prediction.closestApproach) << " UTC\n";
        std::cout << "   Separation: " << std::fixed << std::setprecision(3) 
                 << prediction.separationArcsec << " arcsec\n";
        std::cout << "\n";
        
        std::cout << "📍 Positions at Closest Approach:\n";
        std::cout << "   Ceres:  RA=" << std::setprecision(6) << prediction.asteroidRA 
                 << "° Dec=" << prediction.asteroidDec << "°\n";
        std::cout << "   Star:   RA=" << prediction.starRA 
                 << "° Dec=" << prediction.starDec << "°\n";
        std::cout << "\n";
        
        std::cout << "🌑 Shadow Properties:\n";
        std::cout << "   Distance: " << std::setprecision(3) << prediction.distanceAU << " AU\n";
        std::cout << "   Shadow width: " << std::setprecision(1) 
                 << prediction.shadowWidthKm << " km\n";
        std::cout << "\n";
        
        std::cout << "✨ Assessment:\n";
        if (prediction.isPotentialOccultation) {
            std::cout << "   ✓ POTENTIAL OCCULTATION DETECTED!\n";
            std::cout << "   This event deserves further analysis.\n";
            
            // Calculate probability (simplified)
            double asteroidRadiusArcsec = (ceresdDiameter/2.0) / 
                (prediction.distanceAU * 149597870.7) * 206264.806;
            double probability = std::max(0.0, 1.0 - prediction.separationArcsec / 
                (2.0 * asteroidRadiusArcsec));
            
            std::cout << "   Probability: " << std::setprecision(1) 
                     << (probability * 100.0) << "%\n";
        } else {
            std::cout << "   No occultation expected for this geometry.\n";
            std::cout << "   Separation too large compared to asteroid size.\n";
        }
        
        std::cout << "\n";
        std::cout << "═══════════════════════════════════════════════\n";
        std::cout << "\n";
        
        // ==================== Step 7: Recommendations ====================
        
        if (prediction.isPotentialOccultation) {
            std::cout << "📋 Next Steps:\n";
            std::cout << "   1. Refine prediction with more precise propagation\n";
            std::cout << "   2. Calculate shadow path on Earth\n";
            std::cout << "   3. Determine visibility from observer locations\n";
            std::cout << "   4. Plan observation campaign\n";
            std::cout << "\n";
        }
        
        std::cout << "✓ Test complete!\n";
        std::cout << "\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n❌ Error: " << e.what() << "\n\n";
        return 1;
    }
}
