/**
 * @file test_nongrav.cpp
 * @brief Test accelerazioni non gravitazionali (VFCC17 model)
 * 
 * Confronta propagazione con/senza parametri A1, A2, A3 (Yarkovsky)
 * Usa Apophis (99942) che ha parametri non gravitazionali noti
 */

#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/jpl_horizons_client.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

void printSeparator() {
    std::cout << std::string(70, '=') << "\n";
}

int main() {
    printSeparator();
    std::cout << "   TEST ACCELERAZIONI NON GRAVITAZIONALI (VFCC17)\n";
    printSeparator();
    std::cout << "\nAsteroide: 99942 Apophis (NEA, diametro ~370m)\n\n";
    
    try {
        JPLHorizonsClient horizons;
        
        // Epoca: JD 2461000.5 (epoca elementi JPL #281)
        JulianDate epoch0(2461000.5);
        
        // Elementi orbitali da JPL SBDB (orbit_id 281, epoca 2461000.5)
        EquinoctialElements elements;
        elements.epoch = epoch0;
        elements.a = 0.9223815982266211;        // AU
        elements.designation = "99942";
        elements.name = "Apophis";
        
        // Conversione da elementi Kepleriani
        double e = 0.1911676516894895;
        double i = 3.339601096662309 * M_PI / 180.0;  // rad
        double Omega = 204.4459761496291 * M_PI / 180.0;
        double omega = 126.404394025686 * M_PI / 180.0;
        double M = 228.3776978652904 * M_PI / 180.0;
        
        elements = EquinoctialElements::fromKeplerian(elements.a, e, i, omega, Omega, M, epoch0);
        
        // Parametri non gravitazionali da JPL SBDB (orbit_id 281)
        elements.A1 = 5.0e-13;              // AU/day² (radial)
        elements.A2 = -2.901766637153165e-14; // AU/day² (transverse)
        elements.A3 = 0.0;                  // AU/day² (normal)
        
        std::cout << "Elementi orbitali (epoca JD " << epoch0.jd << "):\n";
        std::cout << "  a = " << elements.a << " AU\n";
        std::cout << "  e = " << e << "\n";
        std::cout << "  i = " << (i * 180/M_PI) << " deg\n\n";
        
        std::cout << "Parametri non gravitazionali (VFCC17):\n";
        std::cout << "  A1 = " << std::scientific << elements.A1 << " AU/day² (radial)\n";
        std::cout << "  A2 = " << elements.A2 << " AU/day² (transverse)\n";
        std::cout << "  A3 = " << elements.A3 << " AU/day² (normal)\n\n";
        
        // Conversione a stato cartesiano
        std::cout << "Conversione a stato cartesiano...\n";
        PropagatorOptions opts_tmp;
        OrbitPropagator prop_tmp(opts_tmp);
        OrbitState state0 = prop_tmp.elementsToState(elements);
        
        std::cout << "Stato cartesiano:\n";
        std::cout << "  r = (" << state0.position.x << ", " << state0.position.y << ", " << state0.position.z << ") AU\n";
        std::cout << "  v = (" << state0.velocity.x << ", " << state0.velocity.y << ", " << state0.velocity.z << ") AU/day\n\n";
        
        // Test 1: Propagazione SENZA parametri non gravitazionali
        PropagatorOptions opts_no_ng;
        opts_no_ng.integrator = IntegratorType::RA15;
        opts_no_ng.stepSize = 0.1;
        opts_no_ng.tolerance = 1e-12;
        opts_no_ng.usePlanetaryPerturbations = true;
        opts_no_ng.useRelativisticCorrections = true;
        opts_no_ng.useNonGravitational = false;  // DISATTIVATO
        
        OrbitPropagator prop_no_ng(opts_no_ng);
        
        // Test 2: Propagazione CON parametri non gravitazionali
        PropagatorOptions opts_with_ng = opts_no_ng;
        opts_with_ng.useNonGravitational = true;  // ATTIVATO
        
        OrbitPropagator prop_with_ng(opts_with_ng);
        
        std::cout << "TEST PROPAGAZIONE (30 giorni):\n";
        std::cout << std::string(70, '-') << "\n";
        std::cout << std::setw(12) << "Δt (giorni)"
                  << std::setw(22) << "Δ Posizione (km)"
                  << std::setw(22) << "Effetto A1/A2 (%)\n";
        std::cout << std::string(70, '-') << "\n";
        
        constexpr double AU_TO_KM = 149597870.7;
        std::vector<double> intervals = {1, 5, 10, 20, 30};
        
        for (double dt : intervals) {
            JulianDate targetEpoch(epoch0.jd + dt);
            
            // Propaga SENZA A1/A2
            OrbitState result_no_ng = prop_no_ng.propagate(elements, targetEpoch);
            
            // Propaga CON A1/A2
            OrbitState result_with_ng = prop_with_ng.propagate(elements, targetEpoch);
            
            // Differenza tra le due propagazioni
            Vector3D delta = result_with_ng.position - result_no_ng.position;
            double delta_km = delta.magnitude() * AU_TO_KM;
            
            // Distanza percorsa (per riferimento)
            Vector3D displacement = result_no_ng.position - state0.position;
            double dist_km = displacement.magnitude() * AU_TO_KM;
            double effect_pct = (delta_km / dist_km) * 100.0;
            
            std::cout << std::setw(12) << std::fixed << std::setprecision(1) << dt
                      << std::setw(22) << std::setprecision(3) << delta_km
                      << std::setw(22) << std::scientific << std::setprecision(2) << effect_pct << "\n";
        }
        
        std::cout << std::string(70, '-') << "\n";
        std::cout << "\nNota: L'effetto di A1/A2 (Yarkovsky) si accumula nel tempo.\n";
        std::cout << "      Per Apophis (370m), l'effetto è dell'ordine di ~km in 30 giorni.\n";
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
