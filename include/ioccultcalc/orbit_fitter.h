#ifndef IOCCULTCALC_ORBIT_FITTER_H
#define IOCCULTCALC_ORBIT_FITTER_H

#include "ioccultcalc/orbital_elements.h"
#include "ioccultcalc/observation.h"
#include "ioccultcalc/ephemeris.h"
#include <vector>
#include <functional>

namespace ioccultcalc {

// Risultato del fit orbitale
struct OrbitFitResult {
    OrbitalElements fittedElements;     // Elementi orbitali corretti
    ObservationSet observations;         // Osservazioni con residui aggiornati
    
    // Statistiche del fit
    int iterations;                      // Numero di iterazioni
    double rmsResidual;                  // RMS dei residui finali (arcsec)
    double maxResidual;                  // Residuo massimo (arcsec)
    double chi2;                         // Chi-quadro del fit
    int nObservations;                   // Numero di osservazioni usate
    int nOutliers;                       // Numero di outlier rigettati
    
    // Matrice di covarianza degli elementi (6x6)
    std::vector<std::vector<double>> covariance;
    
    // Incertezze formali sugli elementi
    double sigma_a, sigma_e, sigma_i;
    double sigma_Omega, sigma_omega, sigma_M;
    
    bool converged;                      // Se il fit è convergito
    std::string convergenceMessage;
    
    OrbitFitResult() : iterations(0), rmsResidual(0), maxResidual(0),
                       chi2(0), nObservations(0), nOutliers(0),
                       sigma_a(0), sigma_e(0), sigma_i(0),
                       sigma_Omega(0), sigma_omega(0), sigma_M(0),
                       converged(false) {}
};

// Parametri per il fit orbitale
struct OrbitFitOptions {
    int maxIterations;                   // Massimo numero di iterazioni
    double convergenceThreshold;         // Soglia di convergenza (arcsec)
    double outlierSigma;                 // Sigma per rigetto outlier
    bool rejectOutliers;                 // Se rigettare outlier
    bool useWeights;                     // Se usare pesi delle osservazioni
    
    // Perturbatori da includere
    bool includeJupiter;
    bool includeSaturn;
    bool includeUranus;
    bool includeNeptune;
    
    OrbitFitOptions() 
        : maxIterations(20),
          convergenceThreshold(0.01),    // 0.01 arcsec
          outlierSigma(3.0),
          rejectOutliers(true),
          useWeights(true),
          includeJupiter(true),
          includeSaturn(true),
          includeUranus(false),
          includeNeptune(false) {}
};

// Classe per il fit orbitale differenziale
class OrbitFitter {
public:
    OrbitFitter();
    ~OrbitFitter();
    
    // Fit degli elementi orbitali usando le osservazioni
    OrbitFitResult fit(const OrbitalElements& initialElements,
                       const ObservationSet& observations,
                       const OrbitFitOptions& options = OrbitFitOptions());
    
    // Calcola solo i residui senza correggere gli elementi
    void computeResiduals(const OrbitalElements& elements,
                         ObservationSet& observations);
    
    // Propaga elementi orbitali a una nuova epoca
    OrbitalElements propagate(const OrbitalElements& elements,
                             const JulianDate& targetEpoch,
                             bool includePerturbations = true);
    
    // Calcola matrice di covarianza degli elementi
    std::vector<std::vector<double>> computeCovariance(
        const OrbitalElements& elements,
        const ObservationSet& observations);
    
    // Callback per monitorare il progresso
    using ProgressCallback = std::function<void(int iteration, const OrbitFitResult&)>;
    void setProgressCallback(ProgressCallback callback) { progressCallback_ = callback; }
    
private:
    class Impl;
    Impl* pImpl;
    
    ProgressCallback progressCallback_;
    
    // Calcola effemeridi per tutte le osservazioni
    void computeEphemerides(const OrbitalElements& elements,
                           const ObservationSet& observations,
                           std::vector<EquatorialCoordinates>& computed);
    
    // Calcola matrice Jacobiana (design matrix)
    void computeJacobian(const OrbitalElements& elements,
                        const ObservationSet& observations,
                        std::vector<std::vector<double>>& jacobian);
    
    // Risolve sistema normale (least squares)
    bool solveNormalEquations(const std::vector<std::vector<double>>& A,
                             const std::vector<double>& b,
                             std::vector<double>& x);
    
    // Aggiorna elementi orbitali con correzioni
    OrbitalElements updateElements(const OrbitalElements& elements,
                                  const std::vector<double>& corrections);
    
    // Identifica e marca outlier
    void detectOutliers(ObservationSet& observations, double sigmaCutoff);
};

// Utility per analisi residui
class ResidualAnalysis {
public:
    // Calcola statistiche sui residui
    static void computeStatistics(const ObservationSet& observations,
                                  double& meanRA, double& meanDec,
                                  double& rmsRA, double& rmsDec,
                                  double& rmsTotal);
    
    // Identifica trend sistematici nei residui
    static bool detectSystematicBias(const ObservationSet& observations,
                                     double& biasRA, double& biasDec);
    
    // Genera istogramma dei residui
    static std::vector<int> residualHistogram(const ObservationSet& observations,
                                              int nBins, double& binWidth);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ORBIT_FITTER_H
