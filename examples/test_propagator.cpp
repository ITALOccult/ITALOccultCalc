#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/ephemeris.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main(int argc, char** argv) {
    std::string asteroidName = "433";  // Eros
    double days = 365.25;  // 1 anno
    
    if (argc > 1) asteroidName = argv[1];
    if (argc > 2) days = std::atof(argv[2]);
    
    std::cout << "=== TEST PROPAGATORE ORBITALE ===\n";
    std::cout << "Asteroide: " << asteroidName << "\n";
    std::cout << "Propagazione: " << days << " giorni\n\n";
    
    // 1. Carica elementi da AstDyS
    std::cout << "1. Caricamento elementi orbitali...\n";
    AstDysClient astdys;
    auto elements = astdys.getElements(asteroidName);
    
    auto kep = elements.toKeplerian();
    std::cout << "   Elementi all'epoca " << elements.epoch.jd << ":\n";
    std::cout << "   a = " << std::fixed << std::setprecision(6) << kep.a << " AU\n";
    std::cout << "   e = " << kep.e << "\n";
    std::cout << "   i = " << kep.i * 180/M_PI << "°\n\n";
    
    // 2. Setup propagatore
    std::cout << "2. Setup propagatore numerico...\n";
    PropagatorOptions opts;
    opts.integrator = IntegratorType::RK4;
    opts.stepSize = 1.0;  // 1 giorno
    opts.usePlanetaryPerturbations = true;
    opts.useRelativisticCorrections = false;
    
    OrbitPropagator propagator(opts);
    
    // Progress callback
    propagator.setProgressCallback([](double progress, const OrbitState& state) {
        std::cout << "\r   Progresso: " << std::fixed << std::setprecision(1) 
                  << progress * 100 << "%" << std::flush;
    });
    
    // 3. Propaga con metodo numerico
    std::cout << "3. Propagazione numerica...\n";
    JulianDate targetEpoch(elements.epoch.jd + days);
    
    auto finalState = propagator.propagate(elements, targetEpoch);
    
    std::cout << "\n\n   Stato finale (JD " << finalState.epoch.jd << "):\n";
    std::cout << "   Posizione (AU): (" 
              << std::setprecision(8) << finalState.position.x << ", "
              << finalState.position.y << ", "
              << finalState.position.z << ")\n";
    std::cout << "   Velocità (AU/day): ("
              << finalState.velocity.x << ", "
              << finalState.velocity.y << ", "
              << finalState.velocity.z << ")\n";
    
    auto stats = propagator.getLastStats();
    std::cout << "\n   Statistiche propagazione:\n";
    std::cout << "   - Passi integrazione: " << stats.nSteps << "\n";
    std::cout << "   - Step finale: " << stats.finalStepSize << " giorni\n";
    std::cout << "   - Tempo calcolo: " << std::fixed << std::setprecision(3) 
              << stats.computeTime << " secondi\n\n";
    
    // 4. Confronto con propagazione Kepleriana semplice
    std::cout << "4. Confronto con propagazione Kepleriana...\n";
    Ephemeris eph(elements);
    auto ephData = eph.compute(targetEpoch);
    
    std::cout << "   Posizione Kepleriana (AU): ("
              << ephData.heliocentricPos.x << ", "
              << ephData.heliocentricPos.y << ", "
              << ephData.heliocentricPos.z << ")\n";
    
    // Differenza
    double dx = finalState.position.x - ephData.heliocentricPos.x;
    double dy = finalState.position.y - ephData.heliocentricPos.y;
    double dz = finalState.position.z - ephData.heliocentricPos.z;
    double diff = sqrt(dx*dx + dy*dy + dz*dz);
    
    std::cout << "\n   Differenza posizione: " << std::scientific 
              << diff << " AU = " << diff * 149597870.7 << " km\n";
    
    if (diff * 149597870.7 < 100) {
        std::cout << "   ✓ Differenza < 100 km - propagazione accurata!\n";
    } else if (diff * 149597870.7 < 10000) {
        std::cout << "   ⚠ Differenza < 10,000 km - precisione accettabile\n";
    } else {
        std::cout << "   ✗ Differenza significativa - verificare parametri\n";
    }
    
    // 5. Test con diversi step sizes
    std::cout << "\n5. Test sensitività step size...\n";
    double stepSizes[] = {0.1, 0.5, 1.0, 2.0, 5.0};
    
    std::cout << "   Step(days)  Passi   Tempo(s)  Diff(km)\n";
    std::cout << "   " << std::string(45, '-') << "\n";
    
    for (double step : stepSizes) {
        opts.stepSize = step;
        OrbitPropagator testProp(opts);
        
        auto testState = testProp.propagate(elements, targetEpoch);
        auto testStats = testProp.getLastStats();
        
        double testDiff = (testState.position - finalState.position).magnitude() * 149597870.7;
        
        std::cout << "   " << std::fixed << std::setprecision(1) << std::setw(10) << step
                  << std::setw(8) << testStats.nSteps
                  << std::setw(10) << std::setprecision(4) << testStats.computeTime
                  << std::setw(12) << std::setprecision(2) << testDiff << "\n";
    }
    
    std::cout << "\n=== TEST COMPLETATO ===\n";
    
    return 0;
}
