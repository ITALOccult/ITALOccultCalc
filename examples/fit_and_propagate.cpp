/**
 * @file fit_and_propagate.cpp
 * @brief Fit elementi orbitali da osservazioni RWO e test propagazione
 * 
 * Workflow:
 * 1. Scarica osservazioni RWO da AstDyS
 * 2. Carica elementi orbitali nominali
 * 3. Fitta elementi usando osservazioni
 * 4. Propaga con elementi fittati e confronta con JPL
 */

#include "ioccultcalc/orbit_fitter.h"
#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/mpc_client.h"
#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/astdys_client.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

void printSeparator() {
    std::cout << std::string(75, '=') << "\n";
}

int main(int argc, char** argv) {
    std::string asteroid = "433";  // Eros
    double propagationDays = 40.0;
    
    if (argc > 1) asteroid = argv[1];
    if (argc > 2) propagationDays = std::atof(argv[2]);
    
    printSeparator();
    std::cout << "   FIT ORBITALE DA OSSERVAZIONI RWO + PROPAGAZIONE\n";
    printSeparator();
    std::cout << "\nAsteroide: " << asteroid << "\n";
    std::cout << "Propagazione: " << propagationDays << " giorni\n\n";
    
    try {
        // =====================================================================
        // STEP 1: Scarica osservazioni RWO da AstDyS
        // =====================================================================
        std::cout << "STEP 1: Scaricamento osservazioni da AstDyS...\n";
        std::cout << std::string(75, '-') << "\n";
        
        MPCClient mpcClient;
        ObservationSet observationsAll = mpcClient.getObservations(asteroid);
        
        std::cout << "✓ Osservazioni scaricate: " << observationsAll.observations.size() << "\n";
        std::cout << "  Prima osservazione: JD " << std::fixed << std::setprecision(2) 
                  << observationsAll.firstObservation.jd << "\n";
        std::cout << "  Ultima osservazione: JD " << observationsAll.lastObservation.jd << "\n";
        std::cout << "  Arco osservativo: " << std::setprecision(1) 
                  << observationsAll.arcLength << " giorni ("
                  << (observationsAll.arcLength / 365.25) << " anni)\n";
        
        // Filtra solo osservazioni recenti (ultimi 3 anni)
        double cutoffJD = observationsAll.lastObservation.jd - 3 * 365.25;
        ObservationSet observations;
        observations.objectDesignation = observationsAll.objectDesignation;
        
        for (const auto& obs : observationsAll.observations) {
            if (obs.epoch.jd >= cutoffJD) {
                observations.observations.push_back(obs);
            }
        }
        
        if (!observations.observations.empty()) {
            observations.firstObservation = observations.observations.front().epoch;
            observations.lastObservation = observations.observations.back().epoch;
            observations.arcLength = observations.lastObservation.jd - observations.firstObservation.jd;
            observations.numberOfObservations = observations.observations.size();
        }
        
        std::cout << "\n✓ Filtrate osservazioni recenti (ultimi 3 anni): " 
                  << observations.observations.size() << "\n";
        std::cout << "  Arc filtrato: " << observations.arcLength << " giorni\n\n";
        
        if (observations.observations.empty()) {
            std::cerr << "ERRORE: Nessuna osservazione recente disponibile!\n";
            return 1;
        }
        
        // =====================================================================
        // STEP 2: Carica elementi orbitali nominali da AstDyS
        // =====================================================================
        std::cout << "STEP 2: Caricamento elementi orbitali nominali...\n";
        std::cout << std::string(75, '-') << "\n";
        
        AstDysClient astdysClient;
        AstDynEquinoctialElements nominalElementsEq = astdysClient.getElements(asteroid);
        OrbitalElements nominalElements = nominalElementsEq.toKeplerian();
        
        std::cout << "✓ Elementi nominali caricati (epoca JD " 
                  << nominalElements.epoch.jd << "):\n";
        std::cout << "  a = " << std::setprecision(10) << nominalElements.a << " AU\n";
        std::cout << "  e = " << nominalElements.e << "\n";
        std::cout << "  i = " << (nominalElements.i * 180.0 / M_PI) << " deg\n";
        std::cout << "  Ω = " << (nominalElements.Omega * 180.0 / M_PI) << " deg\n";
        std::cout << "  ω = " << (nominalElements.omega * 180.0 / M_PI) << " deg\n";
        std::cout << "  M = " << (nominalElements.M * 180.0 / M_PI) << " deg\n\n";
        
        // =====================================================================
        // STEP 3: Fit degli elementi orbitali con osservazioni
        // =====================================================================
        std::cout << "STEP 3: Fit differenziale degli elementi orbitali...\n";
        std::cout << std::string(75, '-') << "\n";
        
        OrbitFitter fitter;
        OrbitFitOptions fitOptions;
        fitOptions.maxIterations = 20;
        fitOptions.convergenceThreshold = 0.01;  // 0.01 arcsec
        fitOptions.rejectOutliers = true;
        fitOptions.outlierSigma = 3.0;
        fitOptions.includeJupiter = true;
        fitOptions.includeSaturn = true;
        
        // Callback per monitorare progresso
        fitter.setProgressCallback([](int iter, const OrbitFitResult& result) {
            std::cout << "  Iterazione " << std::setw(2) << iter 
                      << ": RMS = " << std::fixed << std::setprecision(4) 
                      << result.rmsResidual << " arcsec"
                      << ", χ² = " << std::setprecision(2) << result.chi2 
                      << "\n";
        });
        
        auto fitResult = fitter.fit(nominalElements, observations, fitOptions);
        
        std::cout << "\n";
        if (fitResult.converged) {
            std::cout << "✓ FIT CONVERGITO!\n";
        } else {
            std::cout << "⚠ FIT NON CONVERGITO: " << fitResult.convergenceMessage << "\n";
        }
        
        std::cout << "\nStatistiche fit:\n";
        std::cout << "  Iterazioni: " << fitResult.iterations << "\n";
        std::cout << "  RMS residuo: " << std::setprecision(4) << fitResult.rmsResidual 
                  << " arcsec\n";
        std::cout << "  Max residuo: " << fitResult.maxResidual << " arcsec\n";
        std::cout << "  χ²: " << std::setprecision(2) << fitResult.chi2 << "\n";
        std::cout << "  Osservazioni usate: " << fitResult.nObservations << "\n";
        std::cout << "  Outlier rigettati: " << fitResult.nOutliers << "\n";
        
        std::cout << "\nElementi fittati (epoca JD " << fitResult.fittedElements.epoch.jd << "):\n";
        std::cout << "  a = " << std::setprecision(10) << fitResult.fittedElements.a 
                  << " ± " << std::scientific << std::setprecision(2) 
                  << fitResult.sigma_a << " AU\n";
        std::cout << "  e = " << std::fixed << std::setprecision(10) 
                  << fitResult.fittedElements.e 
                  << " ± " << std::scientific << fitResult.sigma_e << "\n";
        std::cout << "  i = " << std::fixed << std::setprecision(8) 
                  << (fitResult.fittedElements.i * 180.0 / M_PI)
                  << " ± " << std::scientific << (fitResult.sigma_i * 180.0 / M_PI) 
                  << " deg\n\n";
        
        // =====================================================================
        // STEP 4: Propagazione con elementi nominali vs fittati
        // =====================================================================
        std::cout << "STEP 4: Test propagazione (elementi nominali vs fittati)...\n";
        std::cout << std::string(75, '-') << "\n";
        
        // Epoca di partenza: usa ultima osservazione o epoca fissa
        JulianDate epoch0(2460000.0);  // 2023-02-25
        std::cout << "Epoca iniziale: JD " << epoch0.jd << "\n";
        std::cout << "Test propagazione: " << propagationDays << " giorni\n\n";
        
        // Propaga elementi nominali all'epoca iniziale
        std::cout << "Propagazione elementi nominali a epoca iniziale...\n";
        OrbitalElements nominalAtEpoch0 = fitter.propagate(nominalElements, epoch0, true);
        
        // Propaga elementi fittati all'epoca iniziale
        std::cout << "Propagazione elementi fittati a epoca iniziale...\n";
        OrbitalElements fittedAtEpoch0 = fitter.propagate(fitResult.fittedElements, epoch0, true);
        
        // Setup propagatore con RA15 + perturbazioni complete
        PropagatorOptions propOpts;
        propOpts.integrator = IntegratorType::RA15;
        propOpts.stepSize = 0.1;
        propOpts.tolerance = 1e-12;
        propOpts.usePlanetaryPerturbations = true;
        propOpts.useRelativisticCorrections = true;
        propOpts.useNonGravitational = false;  // Eros non ha A1/A2
        
        OrbitPropagator propagator(propOpts);
        
        // Test a vari intervalli
        std::cout << "\nRISULTATI PROPAGAZIONE:\n";
        std::cout << std::string(75, '-') << "\n";
        std::cout << std::setw(10) << "Δt (gg)"
                  << std::setw(20) << "Err Nominali (km)"
                  << std::setw(20) << "Err Fittati (km)"
                  << std::setw(20) << "Miglioramento\n";
        std::cout << std::string(75, '-') << "\n";
        
        JPLHorizonsClient horizons;
        std::vector<double> testIntervals = {1, 5, 10, 20, propagationDays};
        
        for (double dt : testIntervals) {
            JulianDate targetEpoch(epoch0.jd + dt);
            
            // Propaga con elementi nominali
            auto elemNomEq = AstDynEquinoctialElements::fromKeplerian(nominalAtEpoch0);
            OrbitState stateNom = propagator.propagate(elemNomEq, targetEpoch);
            
            // Propaga con elementi fittati
            auto elemFitEq = AstDynEquinoctialElements::fromKeplerian(fittedAtEpoch0);
            OrbitState stateFit = propagator.propagate(elemFitEq, targetEpoch);
            
            // Stato JPL Horizons per riferimento
            auto jplState = horizons.getStateVectors(asteroid, targetEpoch);
            Vector3D posJPL = jplState.first;
            
            // Calcola errori
            Vector3D deltaNom = stateNom.position - posJPL;
            Vector3D deltaFit = stateFit.position - posJPL;
            
            constexpr double AU_TO_KM = 149597870.7;
            double errNom_km = deltaNom.magnitude() * AU_TO_KM;
            double errFit_km = deltaFit.magnitude() * AU_TO_KM;
            double improvement = errNom_km - errFit_km;
            
            std::cout << std::setw(10) << std::fixed << std::setprecision(1) << dt
                      << std::setw(20) << std::setprecision(2) << errNom_km
                      << std::setw(20) << errFit_km
                      << std::setw(20) << improvement << "\n";
        }
        
        std::cout << std::string(75, '-') << "\n";
        std::cout << "\n✓ Test completato!\n\n";
        
        printSeparator();
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\nERRORE: " << e.what() << "\n";
        return 1;
    }
}
