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
#include "ioccultcalc/orbit_determination.h"
#include "ioccultcalc/ephemeris.h"
#include <iostream>
#include <iomanip>
#include <fstream>

using namespace ioccultcalc;

void printElements(const EquinoctialElements& elem, const std::string& label) {
    std::cout << "\n" << label << ":\n";
    std::cout << std::fixed << std::setprecision(8);
    std::cout << "  a (AU):       " << elem.a << "\n";
    std::cout << "  h:            " << elem.h << "\n";
    std::cout << "  k:            " << elem.k << "\n";
    std::cout << "  p:            " << elem.p << "\n";
    std::cout << "  q:            " << elem.q << "\n";
    std::cout << "  lambda (deg): " << elem.lambda * RAD_TO_DEG << "\n";
    std::cout << "  Epoch (JD):   " << elem.epoch.jd << "\n";
}

void printObservationStats(const ObservationSet& obs) {
    std::cout << "\nStatistiche osservazioni:\n";
    std::cout << "  Numero totale: " << obs.numberOfObservations << "\n";
    std::cout << "  Arco (giorni): " << obs.arcLength << "\n";
    std::cout << "  Prima:         " << obs.firstObservation.jd << "\n";
    std::cout << "  Ultima:        " << obs.lastObservation.jd << "\n";
    std::cout << "  RMS totale:    " << obs.getRMSResidual() << " arcsec\n";
    std::cout << "  RMS RA:        " << obs.getRAResidualRMS() << " arcsec\n";
    std::cout << "  RMS Dec:       " << obs.getDecResidualRMS() << " arcsec\n";
}

void printFitResult(const OrbitFitResult& result) {
    std::cout << "\n=== RISULTATI FIT ORBITALE ===\n";
    std::cout << "Convergenza: " << (result.converged ? "SI" : "NO") << "\n";
    std::cout << "Iterazioni: " << result.iterations << "\n";
    std::cout << "Osservazioni usate: " << result.numberOfObservations << "\n";
    std::cout << "Outliers rimossi: " << result.numberOfOutliers << "\n";
    std::cout << "Gradi di libertà: " << result.degreesOfFreedom << "\n";
    
    std::cout << std::fixed << std::setprecision(4);
    std::cout << "\nResiduali:\n";
    std::cout << "  RMS totale: " << result.rmsResidual << " arcsec\n";
    std::cout << "  RMS RA:     " << result.raRMS << " arcsec\n";
    std::cout << "  RMS Dec:    " << result.decRMS << " arcsec\n";
    std::cout << "  Chi-quadro: " << result.chi2 << "\n";
    
    std::cout << std::setprecision(8);
    std::cout << "\nIncertezze elementi (1-sigma):\n";
    std::cout << "  sigma_a:      " << result.sigma_a * AU << " km\n";
    std::cout << "  sigma_h:      " << result.sigma_h << "\n";
    std::cout << "  sigma_k:      " << result.sigma_k << "\n";
    std::cout << "  sigma_p:      " << result.sigma_p << "\n";
    std::cout << "  sigma_q:      " << result.sigma_q << "\n";
    std::cout << "  sigma_lambda: " << result.sigma_lambda * RAD_TO_DEG << " deg\n";
    
    printElements(result.initialElements, "Elementi iniziali");
    printElements(result.improvedElements, "Elementi migliorati");
    
    // Miglioramento
    double deltaA = (result.improvedElements.a - result.initialElements.a) * AU;
    std::cout << "\nVariazioni:\n";
    std::cout << "  Delta a: " << deltaA << " km\n";
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
        auto initialElements = astdys.getElements(asteroidName);
        
        std::cout << "   Elementi caricati con successo\n";
        printElements(initialElements, "Elementi AstDyS2");
        
        // 2. Scarica osservazioni da MPC
        std::cout << "\n2. Download osservazioni astrometriche da MPC...\n";
        MPCClient mpc;
        auto observations = mpc.getObservations(asteroidName);
        
        std::cout << "   Scaricate " << observations.numberOfObservations 
                  << " osservazioni\n";
        printObservationStats(observations);
        
        // 3. Setup orbit determination
        std::cout << "\n3. Setup orbit determination...\n";
        OrbitDetermination od;
        od.setInitialElements(initialElements);
        od.setObservations(observations);
        
        // Configura opzioni fit
        OrbitDetermination::FitOptions options;
        options.useWeights = true;           // Usa pesi basati su incertezze
        options.removeOutliers = true;       // Rimuovi outliers
        options.outlierThreshold = 3.0;      // 3-sigma
        
        od.setFitOptions(options);
        
        // 4. Esegui fit
        std::cout << "\n4. Esecuzione differential correction...\n";
        auto result = od.fitOrbit();
        
        // 5. Mostra risultati
        printFitResult(result);
        
        // 6. Valuta qualità orbita
        std::cout << "\n=== VALUTAZIONE QUALITA' ===\n";
        
        int conditionCode = OrbitQuality::computeConditionCode(observations);
        std::cout << "Condition code MPC: " << conditionCode << "\n";
        
        bool reliable = OrbitQuality::isOrbitReliable(
            result, observations, 
            30.0,  // minimo 30 giorni arco
            10     // minimo 10 osservazioni
        );
        std::cout << "Orbita affidabile: " << (reliable ? "SI" : "NO") << "\n";
        
        // Stima incertezza effemeridi tra 30 giorni
        JulianDate futureDate;
        futureDate.jd = result.improvedElements.epoch.jd + 30.0;
        
        double uncertainty30d = OrbitQuality::computeUncertaintyParameter(
            result, 30.0
        );
        std::cout << "Incertezza stimata a +30 giorni: " 
                  << uncertainty30d << " arcsec\n";
        
        // Calcola ellisse errore
        double semiMajor, semiMinor, angle;
        result.getErrorEllipse(futureDate, semiMajor, semiMinor, angle);
        
        std::cout << "\nEllisse errore a +30 giorni:\n";
        std::cout << "  Semi-asse maggiore: " << semiMajor << " arcsec\n";
        std::cout << "  Semi-asse minore:   " << semiMinor << " arcsec\n";
        std::cout << "  Angolo posizione:   " << angle << " deg\n";
        
        // 7. Confronto effemeridi
        std::cout << "\n=== CONFRONTO EFFEMERIDI ===\n";
        std::cout << "Effemeridi tra 7 giorni:\n";
        
        JulianDate testDate;
        testDate.jd = result.improvedElements.epoch.jd + 7.0;
        
        Ephemeris ephInitial(result.initialElements);
        Ephemeris ephImproved(result.improvedElements);
        
        auto dataInitial = ephInitial.compute(testDate);
        auto dataImproved = ephImproved.compute(testDate);
        
        std::cout << std::fixed << std::setprecision(6);
        std::cout << "\nElementi iniziali:\n";
        std::cout << "  RA:       " << dataInitial.geocentricPos.ra * RAD_TO_DEG << " deg\n";
        std::cout << "  Dec:      " << dataInitial.geocentricPos.dec * RAD_TO_DEG << " deg\n";
        std::cout << "  Distanza: " << dataInitial.distance << " AU\n";
        
        std::cout << "\nElementi migliorati:\n";
        std::cout << "  RA:       " << dataImproved.geocentricPos.ra * RAD_TO_DEG << " deg\n";
        std::cout << "  Dec:      " << dataImproved.geocentricPos.dec * RAD_TO_DEG << " deg\n";
        std::cout << "  Distanza: " << dataImproved.distance << " AU\n";
        
        double dRA = (dataImproved.geocentricPos.ra - dataInitial.geocentricPos.ra) 
                     * cos(dataInitial.geocentricPos.dec) * RAD_TO_DEG * 3600.0;
        double dDec = (dataImproved.geocentricPos.dec - dataInitial.geocentricPos.dec) 
                      * RAD_TO_DEG * 3600.0;
        double dPos = sqrt(dRA * dRA + dDec * dDec);
        
        std::cout << "\nDifferenza posizionale:\n";
        std::cout << "  Delta RA:  " << dRA << " arcsec\n";
        std::cout << "  Delta Dec: " << dDec << " arcsec\n";
        std::cout << "  Totale:    " << dPos << " arcsec\n";
        
        // 8. Salva risultati
        std::cout << "\n=== SALVATAGGIO RISULTATI ===\n";
        
        // Salva osservazioni con residui
        std::string obsFile = asteroidName + "_observations.txt";
        observations.saveToFile(obsFile);
        std::cout << "Osservazioni salvate in: " << obsFile << "\n";
        
        // Salva elementi migliorati (formato semplice)
        std::string elemFile = asteroidName + "_improved_elements.txt";
        std::ofstream out(elemFile);
        out << std::fixed << std::setprecision(12);
        out << "# Elementi orbitali migliorati per " << asteroidName << "\n";
        out << "# Epoca (JD): " << result.improvedElements.epoch.jd << "\n";
        out << "# RMS fit: " << result.rmsResidual << " arcsec\n";
        out << "# Osservazioni: " << result.numberOfObservations << "\n";
        out << "# Arco: " << observations.arcLength << " giorni\n\n";
        out << "a      " << result.improvedElements.a << " +/- " 
            << result.sigma_a * AU << " km\n";
        out << "h      " << result.improvedElements.h << " +/- " 
            << result.sigma_h << "\n";
        out << "k      " << result.improvedElements.k << " +/- " 
            << result.sigma_k << "\n";
        out << "p      " << result.improvedElements.p << " +/- " 
            << result.sigma_p << "\n";
        out << "q      " << result.improvedElements.q << " +/- " 
            << result.sigma_q << "\n";
        out << "lambda " << result.improvedElements.lambda << " +/- " 
            << result.sigma_lambda << " rad\n";
        out.close();
        
        std::cout << "Elementi migliorati salvati in: " << elemFile << "\n";
        
        std::cout << "\n=== COMPLETATO ===\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << "\n";
        return 1;
    }
}
