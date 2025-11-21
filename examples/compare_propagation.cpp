#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/coordinates.h"
#include <iostream>
#include <iomanip>
#include <cmath>

using namespace ioccultcalc;

double radToDeg(double rad) { return rad * 180.0 / M_PI; }
double radToArcsec(double rad) { return rad * 180.0 / M_PI * 3600.0; }

int main() {
    std::cout << "=== CONFRONTO PROPAGATORE CON OSSERVAZIONI ===\n\n";
    
    // 1. Carica elementi
    AstDysClient astdys;
    auto elements = astdys.getElements("433");
    
    std::cout << "Elementi all'epoca JD " << std::fixed << std::setprecision(2) 
              << elements.epoch.jd << ":\n";
    auto kep = elements.toKeplerian();
    std::cout << "  a = " << std::setprecision(6) << kep.a << " AU\n";
    std::cout << "  e = " << kep.e << "\n";
    std::cout << "  i = " << kep.i * 180/M_PI << "°\n\n";
    
    // 2. Scegli un'epoca di test (+100 giorni dall'epoca elementi)
    JulianDate testEpoch(elements.epoch.jd + 100.0);
    
    std::cout << "Epoca di test:\n";
    std::cout << "  JD " << testEpoch.jd << " (Δt = +100 giorni)\n\n";
    
    // 3. Propaga con metodo numerico (con perturbazioni)
    std::cout << "Propagazione numerica (con perturbazioni)...\n";
    PropagatorOptions optsNum;
    optsNum.stepSize = 1.0;
    optsNum.usePlanetaryPerturbations = true;
    optsNum.useRelativisticCorrections = false;
    
    OrbitPropagator propagatorNum(optsNum);
    auto stateNum = propagatorNum.propagate(elements, testEpoch);
    
    std::cout << "  Posizione (AU): ("
              << std::setprecision(8) << stateNum.position.x << ", "
              << stateNum.position.y << ", "
              << stateNum.position.z << ")\n";
    std::cout << "  r = " << stateNum.position.magnitude() << " AU\n";
    std::cout << "  v = " << stateNum.velocity.magnitude() << " AU/day\n\n";
    
    // 4. Propaga solo 2-body (senza perturbazioni)
    std::cout << "Propagazione 2-body (senza perturbazioni)...\n";
    PropagatorOptions opts2body;
    opts2body.stepSize = 1.0;
    opts2body.usePlanetaryPerturbations = false;
    opts2body.useRelativisticCorrections = false;
    
    OrbitPropagator propagator2body(opts2body);
    auto state2body = propagator2body.propagate(elements, testEpoch);
    
    std::cout << "  Posizione (AU): ("
              << state2body.position.x << ", "
              << state2body.position.y << ", "
              << state2body.position.z << ")\n";
    std::cout << "  r = " << state2body.position.magnitude() << " AU\n";
    std::cout << "  v = " << state2body.velocity.magnitude() << " AU/day\n\n";
    
    // 5. Propagazione Kepleriana (Ephemeris classico)
    std::cout << "Propagazione Kepleriana (Ephemeris)...\n";
    Ephemeris eph(elements);
    auto ephData = eph.compute(testEpoch);
    
    std::cout << "  Posizione (AU): ("
              << ephData.heliocentricPos.x << ", "
              << ephData.heliocentricPos.y << ", "
              << ephData.heliocentricPos.z << ")\n";
    std::cout << "  r = " << ephData.heliocentricPos.magnitude() << " AU\n\n";
    
    // 6. Confronti
    std::cout << "CONFRONTO DIFFERENZE:\n\n";
    
    Vector3D diff_num_2body = stateNum.position - state2body.position;
    double dist_num_2body = diff_num_2body.magnitude();
    
    std::cout << "Numerico (pert) vs 2-body:\n";
    std::cout << "  Distanza: " << std::scientific << dist_num_2body 
              << " AU = " << std::fixed << std::setprecision(0)
              << dist_num_2body * 149597870.7 << " km\n";
    std::cout << "  % di r: " << std::setprecision(4) 
              << dist_num_2body / stateNum.position.magnitude() * 100 << "%\n\n";
    
    Vector3D diff_2body_kep = state2body.position - ephData.heliocentricPos;
    double dist_2body_kep = diff_2body_kep.magnitude();
    
    std::cout << "2-body vs Kepleriano:\n";
    std::cout << "  Distanza: " << std::scientific << dist_2body_kep 
              << " AU = " << std::fixed << dist_2body_kep * 149597870.7 << " km\n";
    std::cout << "  % di r: " << std::setprecision(4)
              << dist_2body_kep / state2body.position.magnitude() * 100 << "%\n\n";
    
    Vector3D diff_num_kep = stateNum.position - ephData.heliocentricPos;
    double dist_num_kep = diff_num_kep.magnitude();
    
    std::cout << "Numerico (pert) vs Kepleriano:\n";
    std::cout << "  Distanza: " << std::scientific << dist_num_kep 
              << " AU = " << std::fixed << dist_num_kep * 149597870.7 << " km\n";
    std::cout << "  % di r: " << std::setprecision(4)
              << dist_num_kep / stateNum.position.magnitude() * 100 << "%\n";
    
    return 0;
}
