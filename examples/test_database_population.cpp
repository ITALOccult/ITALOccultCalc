/**
 * @file test_database_population.cpp
 * @brief Esempio di come popolare database con dati Horizons
 * 
 * Dimostra la strategia finale:
 * 1. Usa AstDysClient::getStateFromHorizons() per ottenere vettori
 * 2. Converte in elementi osculanti con OrbitPropagator::stateToElements()
 * 3. Salva nel database
 * 
 * Vantaggi:
 * - Usa vettori Horizons (già testati e funzionanti)
 * - Evita problemi di parsing elementi complessi
 * - Veloce e affidabile
 */

#include <iostream>
#include <iomanip>
#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/orbit_propagator.h"

using namespace ioccultcalc;

int main() {
    try {
        std::cout << "=== STRATEGIA POPOLA DATABASE CON HORIZONS ===\n\n";
        
        // Epoca per database: usiamo epoca recente (2025-11-21)
        JulianDate epoch;
        epoch.jd = 2461000.5;  // MJD 61000
        
        std::cout << "Epoca database: JD " << epoch.jd << " (MJD " << epoch.toMJD() << ")\n";
        std::cout << "Asteroide: (704) Interamnia\n\n";
        
        // STEP 1: Ottieni vettori di stato da Horizons
        std::cout << "STEP 1: Scarica vettori di stato da Horizons...\n";
        AstDysClient client;
        OrbitState state = client.getStateFromHorizons("704", epoch);
        
        std::cout << "\n  Stato ottenuto:\n";
        std::cout << "    r = (" << state.position.x << ", "
                  << state.position.y << ", "
                  << state.position.z << ") AU\n";
        std::cout << "    v = (" << state.velocity.x << ", "
                  << state.velocity.y << ", "
                  << state.velocity.z << ") AU/day\n\n";
        
        // STEP 2: Converti in elementi osculanti equinoziali
        std::cout << "STEP 2: Converte vettori → elementi osculanti...\n";
        PropagatorOptions opts;
        OrbitPropagator prop(opts);
        
        EquinoctialElements elements = prop.stateToElements(state);
        
        std::cout << "  Elementi calcolati:\n";
        std::cout << "    a      = " << std::fixed << std::setprecision(6) << elements.a << " AU\n";
        std::cout << "    h      = " << elements.h << "\n";
        std::cout << "    k      = " << elements.k << "\n";
        double ecc = sqrt(elements.h * elements.h + elements.k * elements.k);
        std::cout << "    e      = " << ecc << "\n";
        std::cout << "    p      = " << elements.p << "\n";
        std::cout << "    q      = " << elements.q << "\n";
        double inc = 2.0 * atan(sqrt(elements.p * elements.p + elements.q * elements.q)) * 180.0 / M_PI;
        std::cout << "    i      = " << inc << "°\n";
        std::cout << "    lambda = " << elements.lambda * 180.0 / M_PI << "°\n";
        std::cout << "    epoch  = JD " << elements.epoch.jd << "\n\n";
        
        // STEP 3: Verifica riconversione elementi → stato
        std::cout << "STEP 3: Verifica riconversione elementi → stato...\n";
        OrbitState state_check = prop.elementsToState(elements);
        
        std::cout << "  Stato riconvertito:\n";
        std::cout << "    r = (" << state_check.position.x << ", "
                  << state_check.position.y << ", "
                  << state_check.position.z << ") AU\n";
        
        // Calcola errore
        double pos_error = (state.position - state_check.position).magnitude();
        double vel_error = (state.velocity - state_check.velocity).magnitude();
        
        std::cout << "\n  Errore riconversione:\n";
        std::cout << "    Posizione: " << pos_error * 1.496e8 << " km\n";
        std::cout << "    Velocità:  " << vel_error * 1731.456 << " m/s\n\n";
        
        if (pos_error < 1e-6 && vel_error < 1e-6) {
            std::cout << "✓ CONVERSIONE PERFETTA!\n\n";
        } else {
            std::cout << "⚠ Errore conversione (verificare algoritmo)\n\n";
        }
        
        // STEP 4: Mostra come salvare nel database
        std::cout << "STEP 4: Salvataggio nel database (pseudo-codice):\n";
        std::cout << "  Database::insert(\n";
        std::cout << "    asteroid_id: 704,\n";
        std::cout << "    name: 'Interamnia',\n";
        std::cout << "    epoch: " << epoch.jd << ",\n";
        std::cout << "    a: " << elements.a << ",\n";
        std::cout << "    h: " << elements.h << ",\n";
        std::cout << "    k: " << elements.k << ",\n";
        std::cout << "    p: " << elements.p << ",\n";
        std::cout << "    q: " << elements.q << ",\n";
        std::cout << "    lambda: " << elements.lambda << ",\n";
        std::cout << "    source: 'JPL Horizons'\n";
        std::cout << "  )\n\n";
        
        std::cout << "CONCLUSIONE:\n";
        std::cout << "- Usa getStateFromHorizons() per popolare database\n";
        std::cout << "- Converte con stateToElements() prima di salvare\n";
        std::cout << "- Elementi osculanti risultanti sono direttamente usabili\n";
        std::cout << "- NO problemi con mean elements di AstDyS\n";
        std::cout << "- NO necessità di propagazione OrbFit\n";
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
