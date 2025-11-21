/**
 * @file validate_with_jpl.cpp
 * @brief Validazione propagatore con dati JPL Horizons
 * 
 * Confronta la propagazione locale con le effemeridi ufficiali JPL
 */

#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/astdys_client.h"
#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>

using namespace ioccultcalc;

void printSeparator() {
    std::cout << std::string(70, '=') << "\n";
}

void printSubSeparator() {
    std::cout << std::string(70, '-') << "\n";
}

int main(int argc, char** argv) {
    std::string asteroid = "433";  // Eros di default
    std::vector<double> testIntervals = {1, 10, 30, 100, 365};  // giorni
    
    if (argc > 1) asteroid = argv[1];
    
    printSeparator();
    std::cout << "   VALIDAZIONE PROPAGATORE CON JPL HORIZONS\n";
    printSeparator();
    std::cout << "\n";
    
    try {
        // 1. Carica elementi da AstDyS
        std::cout << "1. Caricamento elementi orbitali da AstDyS...\n";
        AstDysClient astdys;
        auto elements = astdys.getElements(asteroid);
        
        auto kep = elements.toKeplerian();
        std::cout << "   Asteroide: (" << asteroid << ")\n";
        std::cout << "   Epoca: JD " << std::fixed << std::setprecision(2) 
                  << elements.epoch.jd << "\n";
        std::cout << "   a = " << std::setprecision(6) << kep.a << " AU\n";
        std::cout << "   e = " << kep.e << "\n";
        std::cout << "   i = " << std::setprecision(2) << kep.i * 180/M_PI << "°\n\n";
        
        // 2. Setup propagatore
        std::cout << "2. Setup propagatore numerico...\n";
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RK4;
        opts.stepSize = 1.0;
        opts.usePlanetaryPerturbations = true;
        opts.useRelativisticCorrections = false;
        
        OrbitPropagator propagator(opts);
        std::cout << "   Integratore: RK4\n";
        std::cout << "   Step size: " << opts.stepSize << " giorni\n";
        std::cout << "   Perturbazioni planetarie: ON\n\n";
        
        // 3. Setup JPL Horizons
        std::cout << "3. Connessione a JPL Horizons...\n";
        JPLHorizonsClient horizons;
        horizons.setTimeout(60);  // Timeout lungo per JPL
        
        // Test connessione
        std::cout << "   Verifica disponibilità oggetto...";
        if (!horizons.isTargetAvailable(asteroid)) {
            std::cout << " ERRORE\n";
            std::cout << "   Oggetto " << asteroid << " non trovato in JPL Horizons\n";
            return 1;
        }
        std::cout << " OK\n\n";
        
        // 4. Test a diversi intervalli temporali
        printSeparator();
        std::cout << "   TEST DI ACCURATEZZA\n";
        printSeparator();
        std::cout << "\n";
        
        std::cout << std::setw(12) << "Δt (giorni)"
                  << std::setw(15) << "Err Pos (km)"
                  << std::setw(15) << "Err Pos (%)"
                  << std::setw(15) << "Err Vel (mm/s)"
                  << std::setw(12) << "Tempo (s)\n";
        printSubSeparator();
        
        for (double dt : testIntervals) {
            // Epoca target
            JulianDate targetEpoch(elements.epoch.jd + dt);
            
            // Propagazione locale
            auto startTime = std::chrono::high_resolution_clock::now();
            auto localState = propagator.propagate(elements, targetEpoch);
            auto endTime = std::chrono::high_resolution_clock::now();
            double computeTime = std::chrono::duration<double>(endTime - startTime).count();
            
            // Effemeridi JPL
            std::cout << "   Scaricamento da JPL (Δt=" << std::setw(3) 
                      << static_cast<int>(dt) << " giorni)..." << std::flush;
            
            try {
                auto [jplPos, jplVel] = horizons.getStateVectors(asteroid, targetEpoch, "@sun");
                
                std::cout << " OK\n";
                
                // Calcola errori
                auto [posErrorKm, velErrorMms] = compareWithHorizons(
                    localState.position, localState.velocity,
                    jplPos, jplVel
                );
                
                // Errore percentuale
                double distanceAU = localState.position.magnitude();
                double posErrorPercent = (posErrorKm / (distanceAU * 149597870.7)) * 100.0;
                
                // Output
                std::cout << std::setw(12) << std::fixed << std::setprecision(0) << dt
                          << std::setw(15) << std::setprecision(2) << posErrorKm
                          << std::setw(15) << std::scientific << std::setprecision(2) << posErrorPercent
                          << std::setw(15) << std::fixed << std::setprecision(1) << velErrorMms
                          << std::setw(12) << std::setprecision(3) << computeTime << "\n";
                
            } catch (const std::exception& e) {
                std::cout << " ERRORE\n";
                std::cout << "   " << e.what() << "\n";
            }
        }
        
        std::cout << "\n";
        
        // 5. Test dettagliato a 100 giorni
        printSeparator();
        std::cout << "   DETTAGLIO TEST 100 GIORNI\n";
        printSeparator();
        std::cout << "\n";
        
        double testDt = 100.0;
        JulianDate testEpoch(elements.epoch.jd + testDt);
        
        std::cout << "Propagazione locale...\n";
        auto localState = propagator.propagate(elements, testEpoch);
        auto stats = propagator.getLastStats();
        
        std::cout << "  Posizione (AU): ("
                  << std::setprecision(10) << localState.position.x << ", "
                  << localState.position.y << ", "
                  << localState.position.z << ")\n";
        std::cout << "  Velocità (AU/day): ("
                  << localState.velocity.x << ", "
                  << localState.velocity.y << ", "
                  << localState.velocity.z << ")\n";
        std::cout << "  r = " << localState.position.magnitude() << " AU\n";
        std::cout << "  v = " << localState.velocity.magnitude() << " AU/day\n";
        std::cout << "  Passi integrazione: " << stats.nSteps << "\n";
        std::cout << "  Tempo calcolo: " << std::setprecision(4) 
                  << stats.computeTime << " s\n\n";
        
        std::cout << "Effemeridi JPL Horizons...\n";
        auto [jplPos, jplVel] = horizons.getStateVectors(asteroid, testEpoch, "@sun");
        
        std::cout << "  Posizione (AU): ("
                  << std::setprecision(10) << jplPos.x << ", "
                  << jplPos.y << ", "
                  << jplPos.z << ")\n";
        std::cout << "  Velocità (AU/day): ("
                  << jplVel.x << ", "
                  << jplVel.y << ", "
                  << jplVel.z << ")\n";
        std::cout << "  r = " << jplPos.magnitude() << " AU\n";
        std::cout << "  v = " << jplVel.magnitude() << " AU/day\n\n";
        
        // Differenze
        Vector3D deltaPos = localState.position - jplPos;
        Vector3D deltaVel = localState.velocity - jplVel;
        
        std::cout << "Differenze:\n";
        std::cout << "  ΔX = " << std::scientific << std::setprecision(4) 
                  << deltaPos.x << " AU = " << std::fixed << std::setprecision(2)
                  << deltaPos.x * 149597870.7 << " km\n";
        std::cout << "  ΔY = " << std::scientific << deltaPos.y 
                  << " AU = " << std::fixed << deltaPos.y * 149597870.7 << " km\n";
        std::cout << "  ΔZ = " << std::scientific << deltaPos.z 
                  << " AU = " << std::fixed << deltaPos.z * 149597870.7 << " km\n";
        std::cout << "  |Δr| = " << std::scientific << deltaPos.magnitude()
                  << " AU = " << std::fixed << deltaPos.magnitude() * 149597870.7 << " km\n\n";
        
        std::cout << "  ΔVX = " << std::scientific << deltaVel.x << " AU/day\n";
        std::cout << "  ΔVY = " << deltaVel.y << " AU/day\n";
        std::cout << "  ΔVZ = " << deltaVel.z << " AU/day\n";
        std::cout << "  |Δv| = " << deltaVel.magnitude() 
                  << " AU/day = " << std::fixed << std::setprecision(3)
                  << deltaVel.magnitude() * 149597870.7 / 86400.0 * 1000.0 << " mm/s\n\n";
        
        // Valutazione
        double posErrorKm = deltaPos.magnitude() * 149597870.7;
        double velErrorMms = deltaVel.magnitude() * 149597870.7 / 86400.0 * 1000.0;
        
        printSeparator();
        std::cout << "   VALUTAZIONE\n";
        printSeparator();
        std::cout << "\n";
        
        std::cout << "Errore posizione: " << std::fixed << std::setprecision(2) 
                  << posErrorKm << " km\n";
        std::cout << "Errore velocità:  " << std::setprecision(3) 
                  << velErrorMms << " mm/s\n\n";
        
        if (posErrorKm < 100) {
            std::cout << "✓ ECCELLENTE: Errore < 100 km\n";
        } else if (posErrorKm < 1000) {
            std::cout << "✓ BUONO: Errore < 1,000 km\n";
        } else if (posErrorKm < 10000) {
            std::cout << "⚠ ACCETTABILE: Errore < 10,000 km\n";
        } else {
            std::cout << "✗ ALTO: Errore > 10,000 km - verificare parametri\n";
        }
        
        std::cout << "\n";
        printSeparator();
        
    } catch (const std::exception& e) {
        std::cerr << "\nERRORE: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
