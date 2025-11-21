/**
 * @file orbit_improvement.cpp
 * @brief Esempio di miglioramento orbitale usando osservazioni astrometriche
 * 
 * Questo esempio dimostra come:
 * 1. Caricare elementi orbitali iniziali da AstDyS2
 * 2. Scaricare osservazioni astrometriche da MPC
 * 3. Eseguire fit differenziale per migliorare gli elementi
 * 4. Confrontare elementi iniziali vs migliorati
 * 5. Valutare qualità dell'orbita e incertezze
 */

#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/mpc_client.h"
#include "ioccultcalc/orbit_fitter.h"
#include "ioccultcalc/ephemeris.h"
#include <iostream>
#include <iomanip>
#include <fstream>

using namespace ioccultcalc;



void printFitResult(const OrbitFitResult& result, const OrbitalElements& initial) {
    std::cout << "\n=== RISULTATI FIT ORBITALE ===\n";
    std::cout << "Convergenza: " << (result.converged ? "SI" : "NO") << "\n";
    std::cout << "Iterazioni: " << result.iterations << "\n";
    std::cout << "Osservazioni usate: " << result.nObservations << "\n";
    std::cout << "Outliers rimossi: " << result.nOutliers << "\n";
    
    std::cout << std::fixed << std::setprecision(4);
    std::cout << "\nResiduali:\n";
    std::cout << "  RMS totale: " << result.rmsResidual << " arcsec\n";
    std::cout << "  RMS max:    " << result.maxResidual << " arcsec\n";
    std::cout << "  Chi-quadro: " << result.chi2 << "\n";
    
    std::cout << std::setprecision(8);
    std::cout << "\nIncertezze elementi (1-sigma):\n";
    std::cout << "  sigma_a:      " << result.sigma_a * AU << " km\n";
    std::cout << "  sigma_e:      " << result.sigma_e << "\n";
    std::cout << "  sigma_i:      " << result.sigma_i * RAD_TO_DEG << " deg\n";
    std::cout << "  sigma_Omega:  " << result.sigma_Omega * RAD_TO_DEG << " deg\n";
    std::cout << "  sigma_omega:  " << result.sigma_omega * RAD_TO_DEG << " deg\n";
    std::cout << "  sigma_M:      " << result.sigma_M * RAD_TO_DEG << " deg\n";
    
    std::cout << "\n=== CONFRONTO ELEMENTI ===\n";
    std::cout << std::fixed << std::setprecision(8);
    std::cout << "                  Iniziali          Migliorati        Variazione\n";
    std::cout << "  a (AU):         " << std::setw(15) << initial.a 
              << "  " << std::setw(15) << result.fittedElements.a
              << "  " << std::setw(15) << (result.fittedElements.a - initial.a) << "\n";
    std::cout << "  e:              " << std::setw(15) << initial.e 
              << "  " << std::setw(15) << result.fittedElements.e
              << "  " << std::setw(15) << (result.fittedElements.e - initial.e) << "\n";
    std::cout << "  i (deg):        " << std::setw(15) << initial.i * RAD_TO_DEG
              << "  " << std::setw(15) << result.fittedElements.i * RAD_TO_DEG
              << "  " << std::setw(15) << (result.fittedElements.i - initial.i) * RAD_TO_DEG << "\n";
}

int main(int argc, char** argv) {
    try {
        std::string asteroidName = "433"; // Eros - ben osservato
        
        if (argc > 1) {
            asteroidName = argv[1];
        }
        
        std::cout << "=== ORBIT IMPROVEMENT EXAMPLE ===\n";
        std::cout << "Asteroide: " << asteroidName << "\n\n";
        
        // 1. Carica elementi orbitali iniziali da AstDyS2
        std::cout << "1. Caricamento elementi orbitali da AstDyS2...\n";
        AstDysClient astdys;
        auto initialEquinoctial = astdys.getElements(asteroidName);
        
        // Converti in elementi Kepleriani
        auto initialElements = initialEquinoctial.toKeplerian();
        
        std::cout << "   Elementi caricati con successo\n";
        std::cout << "   a=" << initialElements.a << " AU, e=" << initialElements.e 
                  << ", i=" << initialElements.i * RAD_TO_DEG << "°\n";
        
        // 2. Scarica osservazioni da AstDyS/MPC
        std::cout << "\n2. Download osservazioni astrometriche...\n";
        MPCClient mpc;
        auto observations = mpc.getObservations(asteroidName);
        
        observations.computeStatistics();
        
        std::cout << "   Scaricate " << observations.numberOfObservations 
                  << " osservazioni\n";
        std::cout << "   Arc: " << (observations.lastObservation.jd - observations.firstObservation.jd) << " giorni\n";
        std::cout << "   Osservatori: " << observations.numberOfObservatories << "\n";
        
        // 3. Setup orbit fitter
        std::cout << "\n3. Setup differential corrector...\n";
        OrbitFitter fitter;
        
        // Configura opzioni fit
        OrbitFitOptions options;
        options.maxIterations = 10;
        options.convergenceThreshold = 0.01;  // 0.01 arcsec
        options.rejectOutliers = true;
        options.outlierSigma = 3.0;
        options.useWeights = true;
        options.includeJupiter = true;
        options.includeSaturn = true;
        
        // 4. Esegui fit
        std::cout << "\n4. Esecuzione differential correction...\n";
        auto result = fitter.fit(initialElements, observations, options);
        
        // 5. Mostra risultati
        printFitResult(result, initialElements);
        
        // 6. Statistiche residui
        std::cout << "\n=== ANALISI RESIDUI ===\n";
        double meanRA, meanDec, rmsRA, rmsDec, rmsTotal;
        ResidualAnalysis::computeStatistics(result.observations, 
                                           meanRA, meanDec, rmsRA, rmsDec, rmsTotal);
        
        std::cout << std::fixed << std::setprecision(3);
        std::cout << "  Mean RA:  " << meanRA << "\" (bias: " 
                  << (fabs(meanRA) / rmsRA) << "σ)\n";
        std::cout << "  Mean Dec: " << meanDec << "\" (bias: " 
                  << (fabs(meanDec) / rmsDec) << "σ)\n";
        std::cout << "  RMS RA:   " << rmsRA << "\"\n";
        std::cout << "  RMS Dec:  " << rmsDec << "\"\n";
        std::cout << "  RMS Tot:  " << rmsTotal << "\"\n";
        
        double biasRA, biasDec;
        bool hasBias = ResidualAnalysis::detectSystematicBias(result.observations,
                                                               biasRA, biasDec);
        if (hasBias) {
            std::cout << "\n⚠  ATTENZIONE: Rilevato bias sistematico!\n";
            std::cout << "   Bias RA:  " << biasRA << "\"\n";
            std::cout << "   Bias Dec: " << biasDec << "\"\n";
        }
        
        std::cout << "\n=== COMPLETATO ===\n";
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << "\n";
        return 1;
    }
}
