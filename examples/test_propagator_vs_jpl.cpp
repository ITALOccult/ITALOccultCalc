/**
 * @file test_propagator_vs_jpl.cpp
 * @brief Test accuratezza propagatore con elementi JPL alla stessa epoca
 * 
 * Strategia:
 * 1. Scarica elementi orbitali da JPL Horizons all'epoca T0
 * 2. Scarica posizione/velocità JPL all'epoca T0 + ΔT
 * 3. Propaga localmente da T0 a T0 + ΔT
 * 4. Confronta i risultati
 */

#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/orbital_elements.h"
#include <iostream>
#include <iomanip>
#include <cmath>

using namespace ioccultcalc;

void printSeparator() {
    std::cout << std::string(70, '=') << "\n";
}

void printSubSeparator() {
    std::cout << std::string(70, '-') << "\n";
}

/**
 * @brief Converte vettori di stato in elementi orbitali equinoziali
 * 
 * @param pos Posizione (AU)
 * @param vel Velocità (AU/day)
 * @param epoch Epoca
 * @return AstDynEquinoctialElements
 */
AstDynEquinoctialElements stateToAstDynEquinoctialElements(const Vector3D& pos, 
                                               const Vector3D& vel,
                                               const JulianDate& epoch) {
    // Costante gravitazionale (AU³/day²)
    const double GM = 2.959122082834965e-04;
    
    // Modulo posizione e velocità
    double r = pos.magnitude();
    double v2 = vel.x*vel.x + vel.y*vel.y + vel.z*vel.z;
    
    // Momento angolare
    Vector3D h;
    h.x = pos.y * vel.z - pos.z * vel.y;
    h.y = pos.z * vel.x - pos.x * vel.z;
    h.z = pos.x * vel.y - pos.y * vel.x;
    double h_mag = h.magnitude();
    
    // Vettore eccentricità
    Vector3D e_vec;
    double rdotv = pos.x*vel.x + pos.y*vel.y + pos.z*vel.z;
    e_vec.x = (v2 - GM/r) * pos.x - rdotv * vel.x;
    e_vec.y = (v2 - GM/r) * pos.y - rdotv * vel.y;
    e_vec.z = (v2 - GM/r) * pos.z - rdotv * vel.z;
    e_vec.x /= GM;
    e_vec.y /= GM;
    e_vec.z /= GM;
    
    double e = e_vec.magnitude();
    
    // Energia specifica
    double energy = v2/2.0 - GM/r;
    
    // Semiasse maggiore
    double a = -GM / (2.0 * energy);
    
    // Nodo ascendente
    Vector3D n;
    n.x = -h.y;
    n.y = h.x;
    n.z = 0.0;
    double n_mag = sqrt(n.x*n.x + n.y*n.y);
    
    // Inclinazione
    double i = acos(h.z / h_mag);
    
    // Longitudine nodo ascendente
    double Omega = 0.0;
    if (n_mag > 1e-10) {
        Omega = acos(n.x / n_mag);
        if (n.y < 0) Omega = 2*M_PI - Omega;
    }
    
    // Argomento del pericentro
    double omega = 0.0;
    if (n_mag > 1e-10 && e > 1e-10) {
        double cos_omega = (n.x*e_vec.x + n.y*e_vec.y + n.z*e_vec.z) / (n_mag * e);
        omega = acos(cos_omega);
        if (e_vec.z < 0) omega = 2*M_PI - omega;
    }
    
    // Anomalia vera
    double nu = 0.0;
    if (e > 1e-10) {
        double cos_nu = (e_vec.x*pos.x + e_vec.y*pos.y + e_vec.z*pos.z) / (e * r);
        nu = acos(cos_nu);
        if (rdotv < 0) nu = 2*M_PI - nu;
    } else {
        // Orbita circolare
        nu = atan2(pos.y, pos.x);
    }
    
    // Anomalia media
    double E = 2.0 * atan2(sqrt(1.0-e) * sin(nu/2.0), sqrt(1.0+e) * cos(nu/2.0));
    double M = E - e * sin(E);
    
    // Converti in elementi equinoziali
    AstDynEquinoctialElements elements;
    elements.a = a;
    elements.h = e * sin(omega + Omega);
    elements.k = e * cos(omega + Omega);
    elements.p = tan(i/2.0) * sin(Omega);
    elements.q = tan(i/2.0) * cos(Omega);
    elements.lambda = M + omega + Omega;
    elements.epoch = epoch;
    
    return elements;
}

int main(int argc, char** argv) {
    std::string asteroid = "433";  // Eros di default
    double testDays = 40.0;        // Intervallo test
    
    if (argc > 1) asteroid = argv[1];
    if (argc > 2) testDays = std::atof(argv[2]);
    
    printSeparator();
    std::cout << "   TEST PROPAGATORE CON ELEMENTI JPL ALLA STESSA EPOCA\n";
    printSeparator();
    std::cout << "\n";
    
    try {
        // 1. Setup JPL Horizons
        std::cout << "1. Connessione a JPL Horizons...\n";
        JPLHorizonsClient horizons;
        horizons.setTimeout(60);
        
        if (!horizons.isTargetAvailable(asteroid)) {
            std::cerr << "   ERRORE: Oggetto " << asteroid << " non trovato\n";
            return 1;
        }
        std::cout << "   Oggetto (" << asteroid << ") disponibile\n\n";
        
        // 2. Scegli epoca iniziale (oggi)
        JulianDate epoch0(2460000.0);  // ~2023
        std::cout << "2. Epoca iniziale: JD " << std::fixed << std::setprecision(2) 
                  << epoch0.jd << "\n\n";
        
        // 3. Scarica stato iniziale da JPL
        std::cout << "3. Scaricamento stato iniziale da JPL...\n";
        auto [pos0_jpl, vel0_jpl] = horizons.getStateVectors(asteroid, epoch0, "@sun");
        
        std::cout << "   Posizione (AU): (" 
                  << std::setprecision(10) << pos0_jpl.x << ", "
                  << pos0_jpl.y << ", "
                  << pos0_jpl.z << ")\n";
        std::cout << "   Velocità (AU/day): ("
                  << vel0_jpl.x << ", "
                  << vel0_jpl.y << ", "
                  << vel0_jpl.z << ")\n";
        std::cout << "   r = " << pos0_jpl.magnitude() << " AU\n";
        std::cout << "   v = " << vel0_jpl.magnitude() << " AU/day\n\n";
        
        // 4. Converti in elementi orbitali
        std::cout << "4. Conversione in elementi orbitali equinoziali...\n";
        auto elements = stateToAstDynEquinoctialElements(pos0_jpl, vel0_jpl, epoch0);
        
        auto kep = elements.toKeplerian();
        std::cout << "   a = " << std::setprecision(8) << kep.a << " AU\n";
        std::cout << "   e = " << kep.e << "\n";
        std::cout << "   i = " << std::setprecision(4) << kep.i * 180/M_PI << "°\n";
        std::cout << "   Ω = " << kep.Omega * 180/M_PI << "°\n";
        std::cout << "   ω = " << kep.omega * 180/M_PI << "°\n";
        std::cout << "   M = " << kep.M * 180/M_PI << "°\n\n";
        
        // 5. Setup propagatore
        std::cout << "5. Setup propagatore numerico...\n";
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RK4;
        opts.stepSize = 1.0;
        opts.usePlanetaryPerturbations = true;
        opts.useRelativisticCorrections = false;
        
        OrbitPropagator propagator(opts);
        std::cout << "   Integratore: RK4\n";
        std::cout << "   Step size: " << opts.stepSize << " giorni\n";
        std::cout << "   Perturbazioni planetarie: ON\n\n";
        
        // 6. Test a diversi intervalli
        printSeparator();
        std::cout << "   TEST DI ACCURATEZZA\n";
        printSeparator();
        std::cout << "\n";
        
        std::vector<double> testIntervals = {1, 5, 10, 20, testDays};
        
        std::cout << std::setw(12) << "Δt (giorni)"
                  << std::setw(18) << "Err Pos (km)"
                  << std::setw(18) << "Err Pos (m)"
                  << std::setw(18) << "Err Vel (mm/s)"
                  << std::setw(12) << "Tempo (s)\n";
        printSubSeparator();
        
        for (double dt : testIntervals) {
            // Epoca target
            JulianDate targetEpoch(epoch0.jd + dt);
            
            // Propagazione locale
            auto startTime = std::chrono::high_resolution_clock::now();
            auto localState = propagator.propagate(elements, targetEpoch);
            auto endTime = std::chrono::high_resolution_clock::now();
            double computeTime = std::chrono::duration<double>(endTime - startTime).count();
            
            // Effemeridi JPL all'epoca target
            auto [posT_jpl, velT_jpl] = horizons.getStateVectors(asteroid, targetEpoch, "@sun");
            
            // Calcola errori
            auto [posErrorKm, velErrorMms] = compareWithHorizons(
                localState.position, localState.velocity,
                posT_jpl, velT_jpl
            );
            
            // Output
            std::cout << std::setw(12) << std::fixed << std::setprecision(1) << dt
                      << std::setw(18) << std::setprecision(6) << posErrorKm
                      << std::setw(18) << std::setprecision(2) << posErrorKm * 1000.0
                      << std::setw(18) << std::setprecision(3) << velErrorMms
                      << std::setw(12) << std::setprecision(4) << computeTime << "\n";
        }
        
        std::cout << "\n";
        
        // 7. Test dettagliato all'intervallo specificato
        printSeparator();
        std::cout << "   DETTAGLIO TEST " << static_cast<int>(testDays) << " GIORNI\n";
        printSeparator();
        std::cout << "\n";
        
        JulianDate testEpoch(epoch0.jd + testDays);
        
        std::cout << "Propagazione locale da JD " << epoch0.jd 
                  << " a JD " << testEpoch.jd << "...\n";
        auto localState = propagator.propagate(elements, testEpoch);
        auto stats = propagator.getLastStats();
        
        std::cout << "  Posizione (AU): ("
                  << std::setprecision(12) << localState.position.x << ", "
                  << localState.position.y << ", "
                  << localState.position.z << ")\n";
        std::cout << "  Velocità (AU/day): ("
                  << localState.velocity.x << ", "
                  << localState.velocity.y << ", "
                  << localState.velocity.z << ")\n";
        std::cout << "  Passi integrazione: " << stats.nSteps << "\n";
        std::cout << "  Tempo calcolo: " << std::setprecision(4) 
                  << stats.computeTime << " s\n\n";
        
        std::cout << "Effemeridi JPL Horizons a JD " << testEpoch.jd << "...\n";
        auto [posT_jpl, velT_jpl] = horizons.getStateVectors(asteroid, testEpoch, "@sun");
        
        std::cout << "  Posizione (AU): ("
                  << std::setprecision(12) << posT_jpl.x << ", "
                  << posT_jpl.y << ", "
                  << posT_jpl.z << ")\n";
        std::cout << "  Velocità (AU/day): ("
                  << velT_jpl.x << ", "
                  << velT_jpl.y << ", "
                  << velT_jpl.z << ")\n\n";
        
        // Differenze
        Vector3D deltaPos = localState.position - posT_jpl;
        Vector3D deltaVel = localState.velocity - velT_jpl;
        
        std::cout << "Differenze:\n";
        std::cout << "  ΔX = " << std::scientific << std::setprecision(6) 
                  << deltaPos.x << " AU = " << std::fixed << std::setprecision(3)
                  << deltaPos.x * 149597870.7 << " km = "
                  << deltaPos.x * 149597870.7 * 1000.0 << " m\n";
        std::cout << "  ΔY = " << std::scientific << deltaPos.y 
                  << " AU = " << std::fixed << deltaPos.y * 149597870.7 << " km = "
                  << deltaPos.y * 149597870.7 * 1000.0 << " m\n";
        std::cout << "  ΔZ = " << std::scientific << deltaPos.z 
                  << " AU = " << std::fixed << deltaPos.z * 149597870.7 << " km = "
                  << deltaPos.z * 149597870.7 * 1000.0 << " m\n";
        std::cout << "  |Δr| = " << std::scientific << deltaPos.magnitude()
                  << " AU = " << std::fixed << std::setprecision(6) 
                  << deltaPos.magnitude() * 149597870.7 << " km\n";
        std::cout << "       = " << std::setprecision(2) 
                  << deltaPos.magnitude() * 149597870.7 * 1000.0 << " m\n\n";
        
        std::cout << "  ΔVX = " << std::scientific << std::setprecision(6) << deltaVel.x << " AU/day\n";
        std::cout << "  ΔVY = " << deltaVel.y << " AU/day\n";
        std::cout << "  ΔVZ = " << deltaVel.z << " AU/day\n";
        std::cout << "  |Δv| = " << deltaVel.magnitude() 
                  << " AU/day = " << std::fixed << std::setprecision(4)
                  << deltaVel.magnitude() * 149597870.7 / 86400.0 * 1000.0 << " mm/s\n\n";
        
        // Valutazione
        double posErrorKm = deltaPos.magnitude() * 149597870.7;
        double posErrorM = posErrorKm * 1000.0;
        double velErrorMms = deltaVel.magnitude() * 149597870.7 / 86400.0 * 1000.0;
        
        printSeparator();
        std::cout << "   VALUTAZIONE\n";
        printSeparator();
        std::cout << "\n";
        
        std::cout << "Errore posizione: " << std::setprecision(6) << posErrorKm << " km"
                  << " = " << std::setprecision(2) << posErrorM << " m\n";
        std::cout << "Errore velocità:  " << std::setprecision(4) << velErrorMms << " mm/s\n\n";
        
        if (posErrorKm < 0.001) {
            std::cout << "✓ ECCEZIONALE: Errore < 1 m - precisione sub-metrica!\n";
        } else if (posErrorKm < 0.1) {
            std::cout << "✓ ECCELLENTE: Errore < 100 m - precisione ottimale\n";
        } else if (posErrorKm < 1.0) {
            std::cout << "✓ OTTIMO: Errore < 1 km - precisione molto buona\n";
        } else if (posErrorKm < 100) {
            std::cout << "✓ BUONO: Errore < 100 km - precisione adeguata\n";
        } else if (posErrorKm < 1000) {
            std::cout << "⚠ ACCETTABILE: Errore < 1,000 km\n";
        } else {
            std::cout << "✗ ALTO: Errore > 1,000 km - verificare parametri\n";
        }
        
        std::cout << "\n";
        
        // Stima errore per arco temporale più lungo
        double errorRateKmPerDay = posErrorKm / testDays;
        std::cout << "Tasso di crescita errore: " << std::setprecision(3) 
                  << errorRateKmPerDay << " km/giorno\n";
        std::cout << "Stima errore a 1 anno: " << std::setprecision(1) 
                  << errorRateKmPerDay * 365.25 << " km\n";
        std::cout << "Stima errore a 10 anni: " << std::setprecision(0) 
                  << errorRateKmPerDay * 3652.5 << " km\n";
        
        std::cout << "\n";
        printSeparator();
        
    } catch (const std::exception& e) {
        std::cerr << "\nERRORE: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
