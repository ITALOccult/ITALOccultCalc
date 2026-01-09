#ifndef IOCCULTCALC_ORBIT_DETERMINATION_H
#define IOCCULTCALC_ORBIT_DETERMINATION_H

#include "observation.h"
#include "orbital_elements.h"
#include "ephemeris.h"
#include <vector>
#include <functional>

namespace ioccultcalc {

// Risultati del fit orbitale
struct OrbitFitResult {
    AstDynEquinoctialElements improvedElements;  // Elementi migliorati
    AstDynEquinoctialElements initialElements;   // Elementi iniziali
    
    // Matrice covarianza (6x6 per i 6 elementi)
    std::vector<std::vector<double>> covarianceMatrix;
    
    // Incertezze 1-sigma su ogni elemento
    double sigma_a, sigma_h, sigma_k, sigma_p, sigma_q, sigma_lambda;
    
    // Statistiche del fit
    double rmsResidual;        // RMS totale (arcsec)
    double raRMS;              // RMS solo RA
    double decRMS;             // RMS solo Dec
    double chi2;               // Chi-quadro
    int degreesOfFreedom;      // Gradi di libertà
    int numberOfObservations;  // Numero osservazioni usate
    int numberOfOutliers;      // Numero outliers rimossi
    
    bool converged;            // Se il fit è convergito
    int iterations;            // Numero iterazioni
    
    OrbitFitResult() 
        : sigma_a(0), sigma_h(0), sigma_k(0), sigma_p(0), sigma_q(0), sigma_lambda(0),
          rmsResidual(0), raRMS(0), decRMS(0), chi2(0),
          degreesOfFreedom(0), numberOfObservations(0), numberOfOutliers(0),
          converged(false), iterations(0) {}
    
    // Calcola incertezza posizionale al tempo t (km)
    double getPositionUncertainty(const JulianDate& time) const;
    
    // Ellisse di errore (semi-assi maggiore/minore, angolo) al tempo t
    void getErrorEllipse(const JulianDate& time,
                        double& semiMajor, double& semiMinor, double& angle) const;
};

class OrbitDetermination {
public:
    OrbitDetermination();
    ~OrbitDetermination();
    
    // Imposta elementi orbitali iniziali (approssimati)
    void setInitialElements(const AstDynEquinoctialElements& elements);
    
    // Imposta osservazioni
    void setObservations(const ObservationSet& observations);
    
    // Esegue il fit orbitale (differential correction)
    OrbitFitResult fitOrbit();
    
    // Differential correction con Gauss-Newton
    OrbitFitResult differentialCorrection(
        int maxIterations = 20,
        double convergenceTolerance = 1e-10,
        bool robustFit = true  // Usa sigma-clipping
    );
    
    // Calcola residui per elementi dati
    void computeResiduals(const AstDynEquinoctialElements& elements,
                         ObservationSet& observations);
    
    // Orbit determination iniziale da 3 osservazioni (metodo Gauss)
    static AstDynEquinoctialElements gaussMethod(
        const AstrometricObservation& obs1,
        const AstrometricObservation& obs2,
        const AstrometricObservation& obs3
    );
    
    // Orbit determination da 2 osservazioni molto distanti (metodo Laplace)
    static AstDynEquinoctialElements laplaceMethod(
        const AstrometricObservation& obs1,
        const AstrometricObservation& obs2
    );
    
    // Opzioni di fitting
    struct FitOptions {
        bool fitSemiMajorAxis;
        bool fitEccentricity;
        bool fitInclination;
        bool fitLongitudes;
        bool useWeights;
        bool removeOutliers;
        double outlierThreshold;  // sigma
        
        FitOptions() 
            : fitSemiMajorAxis(true), fitEccentricity(true),
              fitInclination(true), fitLongitudes(true),
              useWeights(true), removeOutliers(true),
              outlierThreshold(3.0) {}
    };
    
    void setFitOptions(const FitOptions& options);
    FitOptions getFitOptions() const;
    
    // Propagazione con integrazione numerica
    void enableNumericalIntegration(bool enable = true);
    
    // Considera perturbazioni planetarie
    void enablePlanetaryPerturbations(bool enable = true);
    
private:
    class Impl;
    std::unique_ptr<Impl> pImpl;
    
    // Calcola matrice Jacobiana (derivate parziali)
    std::vector<std::vector<double>> computeJacobian(
        const AstDynEquinoctialElements& elements,
        const ObservationSet& observations);
    
    // Calcola vettore residui
    std::vector<double> computeResidualVector(
        const AstDynEquinoctialElements& elements,
        const ObservationSet& observations);
    
    // Risolve sistema lineare per correzioni
    std::vector<double> solveLeastSquares(
        const std::vector<std::vector<double>>& jacobian,
        const std::vector<double>& residuals,
        const std::vector<double>& weights);
    
    // Calcola effemeridi per osservazione
    EquatorialCoordinates computeEphemeris(
        const AstDynEquinoctialElements& elements,
        const AstrometricObservation& obs);
};

// Utilità per valutazione qualità orbita
class OrbitQuality {
public:
    // Calcola Condition Code (U parameter) 0-9
    static int computeConditionCode(const ObservationSet& observations);
    
    // Calcola Uncertainty Parameter (stima incertezza ephemeris)
    static double computeUncertaintyParameter(
        const OrbitFitResult& fitResult,
        double daysFromLastObs);
    
    // Valuta se l'orbita è affidabile per predizioni
    static bool isOrbitReliable(
        const OrbitFitResult& fitResult,
        const ObservationSet& observations,
        double minimumArcDays = 10.0,
        int minimumObservations = 3);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ORBIT_DETERMINATION_H
