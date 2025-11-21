/**
 * @file test_propagator_simple.cpp
 * @brief Test semplificato: usa stato cartesiano diretto da JPL
 */

#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/jpl_horizons_client.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

void printSeparator() {
    std::cout << std::string(70, '=') << "\n";
}

int main(int argc, char** argv) {
    std::string asteroid = "433";
    double testDays = 40.0;
    
    if (argc > 1) asteroid = argv[1];
    if (argc > 2) testDays = std::atof(argv[2]);
    
    printSeparator();
    std::cout << "   TEST PROPAGATORE (STATO CARTESIANO DIRETTO)\n";
    printSeparator();
    std::cout << "\n";
    
    try {
        JPLHorizonsClient horizons;
        horizons.setTimeout(60);
        
        // Epoca iniziale
        JulianDate epoch0(2460000.0);
        std::cout << "Epoca iniziale: JD " << std::fixed << std::setprecision(2) 
                  << epoch0.jd << "\n\n";
        
        // Scarica stato iniziale da JPL (heliocentric, Sun-centered)
        std::cout << "Stato iniziale da JPL Horizons:\n";
        auto [pos0, vel0] = horizons.getStateVectors(asteroid, epoch0, "@sun");
        
        std::cout << "  r = (" << std::setprecision(12) << pos0.x << ", "
                  << pos0.y << ", " << pos0.z << ") AU\n";
        std::cout << "  v = (" << vel0.x << ", "
                  << vel0.y << ", " << vel0.z << ") AU/day\n";
        std::cout << "  |r| = " << std::setprecision(10) << pos0.magnitude() << " AU\n";
        std::cout << "  |v| = " << vel0.magnitude() << " AU/day\n\n";
        
        // Setup propagatore con RA15 (OrbFit algorithm - CORRETTO!)
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RA15;  // RA15 di Everhart
        opts.stepSize = 0.1;                     // Step iniziale (adattivo)
        opts.tolerance = 1e-12;                  // Massima precisione
        opts.usePlanetaryPerturbations = true;
        opts.useRelativisticCorrections = true;
        opts.useSolarRadiationPressure = true;   // ATTIVA per NEAs come Eros
        opts.areaToMassRatio = 0.001;  // ~1 m²/ton (tipico per asteroidi)
        
        OrbitPropagator propagator(opts);
        
        std::cout << "Integratore: RA15 (Radau 15° ordine - OrbFit)\n";
        std::cout << "Step: " << opts.stepSize << " giorni, Tolleranza: " << opts.tolerance << "\n\n";
        
        // Crea stato iniziale
        OrbitState initialState(epoch0, pos0, vel0);
        
        // Test a diversi intervalli
        std::cout << "TEST PROPAGAZIONE:\n";
        std::cout << std::string(70, '-') << "\n";
        std::cout << std::setw(12) << "Δt (giorni)"
                  << std::setw(18) << "Err Pos (km)"
                  << std::setw(18) << "Err Pos (m)"
                  << std::setw(18) << "Err Vel (mm/s)\n";
        std::cout << std::string(70, '-') << "\n";
        
        std::vector<double> intervals = {1, 5, 10, 20, testDays};
        
        for (double dt : intervals) {
            JulianDate targetEpoch(epoch0.jd + dt);
            
            // Propaga localmente
            auto localState = propagator.propagate(initialState, targetEpoch);
            
            // Scarica da JPL (heliocentric)
            auto [posJPL, velJPL] = horizons.getStateVectors(asteroid, targetEpoch, "@sun");
            
            // Errori
            auto [posErrorKm, velErrorMms] = compareWithHorizons(
                localState.position, localState.velocity,
                posJPL, velJPL
            );
            
            std::cout << std::setw(12) << std::fixed << std::setprecision(1) << dt
                      << std::setw(18) << std::setprecision(6) << posErrorKm
                      << std::setw(18) << std::setprecision(2) << posErrorKm * 1000.0
                      << std::setw(18) << std::setprecision(3) << velErrorMms << "\n";
        }
        
        std::cout << "\n";
        
        // Dettaglio test finale
        printSeparator();
        std::cout << "   DETTAGLIO " << static_cast<int>(testDays) << " GIORNI\n";
        printSeparator();
        std::cout << "\n";
        
        JulianDate testEpoch(epoch0.jd + testDays);
        auto localState = propagator.propagate(initialState, testEpoch);
        auto [posJPL, velJPL] = horizons.getStateVectors(asteroid, testEpoch, "@sun");
        
        std::cout << "Locale:\n";
        std::cout << "  r = (" << std::setprecision(12) << localState.position.x << ", "
                  << localState.position.y << ", " << localState.position.z << ") AU\n";
        std::cout << "  v = (" << localState.velocity.x << ", "
                  << localState.velocity.y << ", " << localState.velocity.z << ") AU/day\n\n";
        
        std::cout << "JPL:\n";
        std::cout << "  r = (" << posJPL.x << ", "
                  << posJPL.y << ", " << posJPL.z << ") AU\n";
        std::cout << "  v = (" << velJPL.x << ", "
                  << velJPL.y << ", " << velJPL.z << ") AU/day\n\n";
        
        Vector3D dr = localState.position - posJPL;
        Vector3D dv = localState.velocity - velJPL;
        
        std::cout << "Differenze:\n";
        std::cout << "  Δr = (" << std::scientific << std::setprecision(6) 
                  << dr.x << ", " << dr.y << ", " << dr.z << ") AU\n";
        std::cout << "  |Δr| = " << std::fixed << std::setprecision(6) 
                  << dr.magnitude() * 149597870.7 << " km\n";
        std::cout << "       = " << std::setprecision(2) 
                  << dr.magnitude() * 149597870.7 * 1000.0 << " m\n\n";
        
        std::cout << "  Δv = (" << std::scientific << std::setprecision(6)
                  << dv.x << ", " << dv.y << ", " << dv.z << ") AU/day\n";
        std::cout << "  |Δv| = " << std::fixed << std::setprecision(4)
                  << dv.magnitude() * 149597870.7 / 86400.0 * 1000.0 << " mm/s\n\n";
        
        double posErrorKm = dr.magnitude() * 149597870.7;
        
        if (posErrorKm < 0.001) {
            std::cout << "✓ ECCEZIONALE: < 1 m\n";
        } else if (posErrorKm < 1.0) {
            std::cout << "✓ ECCELLENTE: < 1 km\n";
        } else if (posErrorKm < 100) {
            std::cout << "✓ OTTIMO: < 100 km\n";
        } else if (posErrorKm < 10000) {
            std::cout << "✓ BUONO: < 10,000 km\n";
        } else {
            std::cout << "✗ DA MIGLIORARE: > 10,000 km\n";
        }
        
        printSeparator();
        
    } catch (const std::exception& e) {
        std::cerr << "\nERRORE: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
