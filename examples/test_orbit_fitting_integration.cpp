/**
 * Test minimale orbit fitting integration
 * Verifica che le osservazioni .rwo vengano caricate e usate per il fitting
 */

#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/observation.h"
#include "ioccultcalc/orbit_fitter.h"
#include "ioccultcalc/mpc_client.h"
#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <set>
#include <cstdio>

using namespace ioccultcalc;

int main() {
    std::cout << "\n═══════════════════════════════════════════════════════\n";
    std::cout << " TEST INTEGRAZIONE ORBIT FITTING\n";
    std::cout << "═══════════════════════════════════════════════════════\n\n";
    
    try {
        // FASE 1: Inizializza AstDysClient con directory locali
        std::cout << "FASE 1: Inizializzazione AstDysClient\n";
        std::cout << "----------------------------------------\n";
        AstDysClient client;
        client.setLocalEQ1Directory("test_astdys_download");
        client.setLocalRWODirectory("test_astdys_download");
        std::cout << "✓ Directory configurate\n\n";
        
        // FASE 2: Carica elementi orbitali
        std::cout << "FASE 2: Caricamento Elementi Orbitali\n";
        std::cout << "----------------------------------------\n";
        auto elements = client.getElements("433");
        auto tempKep = elements.toKeplerian();
        std::cout << "✓ Elementi caricati per (433) Eros\n";
        std::cout << "  a = " << std::fixed << std::setprecision(8) << elements.a << " AU\n";
        std::cout << "  e = " << tempKep.e << "\n";
        std::cout << "  i = " << std::setprecision(6) << tempKep.i << "°\n";
        std::cout << "  Epoca = MJD " << std::setprecision(2) << elements.epoch.toMJD() << "\n\n";
        
        // FASE 3: Carica osservazioni
        std::cout << "FASE 3: Caricamento Osservazioni\n";
        std::cout << "----------------------------------------\n";
        auto obsLines = client.getObservations("433");
        std::cout << "✓ File .rwo caricato: " << obsLines.size() << " righe\n";
        
        // FASE 4: Parsa osservazioni usando MPCClient.loadFromRWOFile()
        std::cout << "\nFASE 4: Parsing Osservazioni\n";
        std::cout << "----------------------------------------\n";
        
        // Salva temporaneamente in un file per usare loadFromRWOFile
        std::string tempFile = "temp_433.rwo";
        std::ofstream tempOut(tempFile);
        for (const auto& line : obsLines) {
            tempOut << line << "\n";
        }
        tempOut.close();
        
        MPCClient mpcClient;
        ObservationSet obsSet = mpcClient.loadFromRWOFile(tempFile);
        
        // Rimuovi file temporaneo
        std::remove(tempFile.c_str());
        
        std::cout << "✓ Osservazioni parsate con MPCClient::loadFromRWOFile()\n";
        std::cout << "  Numero osservazioni: " << obsSet.numberOfObservations << "\n";
        std::cout << "  Arco temporale: " << std::setprecision(1) 
                  << (obsSet.arcLength / 365.25) << " anni\n";
        std::cout << "  Osservatori distinti: " << obsSet.numberOfObservatories << "\n\n";
        
        // FASE 5: Converti AstDynEquinoctialElements → OrbitalElements
        std::cout << "FASE 5: Conversione Elementi\n";
        std::cout << "----------------------------------------\n";
        OrbitalElements kepElements = elements.toKeplerian();
        std::cout << "✓ Elementi convertiti in formato Kepleriano\n\n";
        
        // FASE 6: Orbit Fitting
        std::cout << "FASE 6: Orbit Fitting\n";
        std::cout << "========================================\n";
        OrbitFitter fitter;
        OrbitFitOptions options;
        options.maxIterations = 10;
        options.convergenceThreshold = 1.0e-6;
        options.outlierSigma = 3.0;
        options.rejectOutliers = true;
        
        std::cout << "Eseguo fitting con:\n";
        std::cout << "  Osservazioni: " << obsSet.numberOfObservations << "\n";
        std::cout << "  Max iterazioni: " << options.maxIterations << "\n";
        std::cout << "  Soglia outlier: " << options.outlierSigma << " σ\n\n";
        
        OrbitFitResult result = fitter.fit(kepElements, obsSet, options);
        
        // FASE 7: Risultati
        std::cout << "\n";
        std::cout << "FASE 7: Risultati\n";
        std::cout << "========================================\n";
        std::cout << "  Convergenza: " << (result.converged ? "✓ SÌ" : "✗ NO") << "\n";
        std::cout << "  Iterazioni: " << result.iterations << "\n";
        std::cout << "  RMS residui: " << std::setprecision(2) << result.rmsResidual << " arcsec\n";
        std::cout << "  Max residuo: " << result.maxResidual << " arcsec\n";
        std::cout << "  χ²: " << std::setprecision(4) << result.chi2 << "\n";
        std::cout << "  Osservazioni usate: " << result.nObservations << "\n";
        std::cout << "  Outliers rigettati: " << result.nOutliers << "\n\n";
        
        if (result.converged && result.rmsResidual < 2.0) {
            std::cout << "✅ SUCCESSO: Fitting convergente con RMS accettabile\n";
            std::cout << "\nElementi migliorati:\n";
            std::cout << "  a = " << std::setprecision(8) << result.fittedElements.a << " AU";
            std::cout << "  (Δ = " << std::scientific << std::setprecision(2) 
                      << (result.fittedElements.a - kepElements.a) << ")\n";
            std::cout << "  e = " << std::fixed << std::setprecision(8) << result.fittedElements.e;
            std::cout << "  (Δ = " << std::scientific << std::setprecision(2) 
                      << (result.fittedElements.e - kepElements.e) << ")\n";
            std::cout << "  i = " << std::fixed << std::setprecision(6) << result.fittedElements.i << "°";
            std::cout << "  (Δ = " << std::scientific << std::setprecision(2) 
                      << (result.fittedElements.i - kepElements.i) << "°)\n";
        } else {
            std::cout << "⚠️  ATTENZIONE: Fitting non convergente o RMS alto\n";
        }
        
        std::cout << "\n═══════════════════════════════════════════════════════\n";
        std::cout << " ✓ TEST COMPLETATO\n";
        std::cout << "═══════════════════════════════════════════════════════\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n✗ ERRORE: " << e.what() << "\n\n";
        return 1;
    }
}
