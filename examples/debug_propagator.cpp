#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/astdys_client.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main() {
    std::cout << "=== DEBUG PROPAGATORE ===\n\n";
    
    // Carica elementi
    AstDysClient astdys;
    auto elements = astdys.getElements("433");
    
    auto kep = elements.toKeplerian();
    std::cout << "Elementi Kepleriani:\n";
    std::cout << "  a = " << std::fixed << std::setprecision(8) << kep.a << " AU\n";
    std::cout << "  e = " << kep.e << "\n";
    std::cout << "  i = " << kep.i * 180/M_PI << "°\n";
    std::cout << "  Ω = " << kep.Omega * 180/M_PI << "°\n";
    std::cout << "  ω = " << kep.omega * 180/M_PI << "°\n";
    std::cout << "  M = " << kep.M * 180/M_PI << "°\n";
    std::cout << "  Epoca: JD " << elements.epoch.jd << "\n\n";
    
    // Crea propagatore
    PropagatorOptions opts;
    opts.integrator = IntegratorType::RK4;
    opts.stepSize = 1.0;
    opts.usePlanetaryPerturbations = false;  // Solo 2-body per debug
    opts.useRelativisticCorrections = false;
    
    OrbitPropagator propagator(opts);
    
    // Converti elementi in stato
    std::cout << "Conversione elementi → stato:\n";
    auto initialState = propagator.elementsToState(elements);
    
    std::cout << "  Posizione (AU): (" 
              << std::setprecision(10) << initialState.position.x << ", "
              << initialState.position.y << ", "
              << initialState.position.z << ")\n";
    std::cout << "  r = " << initialState.position.magnitude() << " AU\n";
    
    std::cout << "  Velocità (AU/day): ("
              << initialState.velocity.x << ", "
              << initialState.velocity.y << ", "
              << initialState.velocity.z << ")\n";
    std::cout << "  v = " << initialState.velocity.magnitude() << " AU/day\n\n";
    
    // Verifica energia
    double r = initialState.position.magnitude();
    double v = initialState.velocity.magnitude();
    double GM = 2.959122082834965e-04;  // AU³/day²
    double energy = v*v/2.0 - GM/r;
    double a_from_energy = -GM/(2.0*energy);
    
    std::cout << "Verifica energia:\n";
    std::cout << "  E = v²/2 - μ/r = " << std::scientific << energy << " AU²/day²\n";
    std::cout << "  a da energia = " << std::fixed << a_from_energy << " AU\n";
    std::cout << "  (dovrebbe essere " << kep.a << " AU)\n\n";
    
    // Propaga 1 giorno
    std::cout << "Propagazione di 1 giorno:\n";
    JulianDate target1(elements.epoch.jd + 1.0);
    auto state1 = propagator.propagate(initialState, target1);
    
    std::cout << "  Posizione: (" << state1.position.x << ", "
              << state1.position.y << ", " << state1.position.z << ")\n";
    std::cout << "  r = " << state1.position.magnitude() << " AU\n";
    
    r = state1.position.magnitude();
    v = state1.velocity.magnitude();
    energy = v*v/2.0 - GM/r;
    a_from_energy = -GM/(2.0*energy);
    
    std::cout << "  E = " << std::scientific << energy << "\n";
    std::cout << "  a da energia = " << std::fixed << a_from_energy << " AU\n";
    std::cout << "  Differenza a: " << (a_from_energy - kep.a) << " AU\n\n";
    
    // Propaga 10 giorni
    std::cout << "Propagazione di 10 giorni:\n";
    JulianDate target10(elements.epoch.jd + 10.0);
    auto state10 = propagator.propagate(initialState, target10);
    
    std::cout << "  r = " << state10.position.magnitude() << " AU\n";
    std::cout << "  v = " << state10.velocity.magnitude() << " AU/day\n";
    
    r = state10.position.magnitude();
    v = state10.velocity.magnitude();
    energy = v*v/2.0 - GM/r;
    a_from_energy = -GM/(2.0*energy);
    
    std::cout << "  a da energia = " << a_from_energy << " AU\n";
    std::cout << "  Differenza a: " << (a_from_energy - kep.a) << " AU\n";
    std::cout << "  Errore %: " << std::setprecision(3) 
              << (a_from_energy - kep.a) / kep.a * 100 << "%\n\n";
    
    // Propaga 100 giorni
    std::cout << "Propagazione di 100 giorni:\n";
    JulianDate target100(elements.epoch.jd + 100.0);
    auto state100 = propagator.propagate(initialState, target100);
    
    std::cout << "  r = " << state100.position.magnitude() << " AU\n";
    std::cout << "  v = " << state100.velocity.magnitude() << " AU/day\n";
    
    r = state100.position.magnitude();
    v = state100.velocity.magnitude();
    energy = v*v/2.0 - GM/r;
    a_from_energy = -GM/(2.0*energy);
    
    std::cout << "  a da energia = " << a_from_energy << " AU\n";
    std::cout << "  Differenza a: " << (a_from_energy - kep.a) << " AU\n";
    std::cout << "  Errore %: " << (a_from_energy - kep.a) / kep.a * 100 << "%\n\n";
    
    return 0;
}
