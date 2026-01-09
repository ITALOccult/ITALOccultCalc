/**
 * @file uncertainty_propagation.h
 * @brief Propagazione incertezze con metodi Monte Carlo e analitici
 * 
 * Implementa:
 * - Monte Carlo sampling da distribuzioni multivariate
 * - Propagazione lineare (STM)
 * - Propagazione non-lineare (Unscented Transform)
 * - Stima probabilità bayesiana
 */

#ifndef IOCCULTCALC_UNCERTAINTY_PROPAGATION_H
#define IOCCULTCALC_UNCERTAINTY_PROPAGATION_H

#include "ioccultcalc/types.h"
#include "ioccultcalc/orbital_elements.h"
#include "ioccultcalc/numerical_integrator.h"
#include <vector>
#include <random>

namespace ioccultcalc {

/**
 * @struct UncertaintyEllipsoid
 * @brief Ellissoide di incertezza 3D
 */
struct UncertaintyEllipsoid {
    Vector3D center;          // Centro ellissoide
    Vector3D semiAxes;        // Semiassi (a ≥ b ≥ c)
    std::vector<std::vector<double>> rotation; // Matrice orientamento
    double sigma;             // Livello confidenza (1, 2, 3-sigma)
    
    /**
     * @brief Calcola volume ellissoide
     */
    double volume() const;
    
    /**
     * @brief Verifica se punto è dentro ellissoide
     */
    bool contains(const Vector3D& point) const;
    
    /**
     * @brief Proietta su piano
     * @param normal Normale al piano
     * @return Ellisse 2D proiettata
     */
    struct Ellipse2D {
        double x, y;           // Centro
        double semiMajor;      // Semiasse maggiore
        double semiMinor;      // Semiasse minore
        double angle;          // Angolo (rad)
    };
    
    Ellipse2D projectToPlane(const Vector3D& normal) const;
};

/**
 * @class MonteCarloSampler
 * @brief Campionamento Monte Carlo da distribuzioni
 */
class MonteCarloSampler {
public:
    /**
     * @brief Inizializza con seed random
     */
    MonteCarloSampler(unsigned int seed = std::random_device{}());
    
    /**
     * @brief Campiona da distribuzione normale multivariata
     * @param mean Vettore media
     * @param covariance Matrice covarianza
     * @param nSamples Numero campioni
     * @return Matrice campioni (nSamples × dim)
     */
    std::vector<std::vector<double>> sampleMultivariateNormal(
        const std::vector<double>& mean,
        const std::vector<std::vector<double>>& covariance,
        int nSamples);
    
    /**
     * @brief Campiona elementi orbitali da incertezze
     * @param nominalElements Elementi nominali
     * @param covariance Matrice 6×6 covarianza
     * @param nSamples Numero campioni
     * @return Vettore elementi campionati
     */
    std::vector<AstDynEquinoctialElements> sampleOrbitalElements(
        const AstDynEquinoctialElements& nominalElements,
        const std::vector<std::vector<double>>& covariance,
        int nSamples);
    
    /**
     * @brief Campiona uniforme su sfera (per direzioni)
     */
    Vector3D sampleUniformSphere();
    
    /**
     * @brief Campiona da distribuzione Chi-quadro
     */
    double sampleChiSquare(int degreesOfFreedom);
    
private:
    std::mt19937 rng;
    
    // Decomposizione Cholesky per multivariata normale
    std::vector<std::vector<double>> choleskyDecomposition(
        const std::vector<std::vector<double>>& matrix);
};

/**
 * @struct OccultationProbability
 * @brief Risultato calcolo probabilità occultazione
 */
struct OccultationProbability {
    double probability;          // Probabilità totale [0,1]
    double probabilityUmbra;     // Probabilità ombra totale
    double probabilityPenumbra;  // Probabilità penombra
    
    // Path nominale
    std::vector<GeographicCoordinates> nominalPath;
    
    // Envelope incertezza (1, 2, 3-sigma)
    std::vector<std::vector<GeographicCoordinates>> uncertaintyBands;
    
    // Mappe probabilità 2D
    struct ProbabilityMap {
        std::vector<std::vector<double>> grid;  // Valori probabilità
        double minLat, maxLat, minLon, maxLon;  // Limiti griglia
        double resolution;                       // Risoluzione (gradi)
    };
    ProbabilityMap probabilityMap;
    
    // Statistiche Monte Carlo
    int numberOfTrials;
    int numberOfHits;
    double confidenceInterval;  // Errore stima probabilità
};

/**
 * @class UncertaintyPropagator
 * @brief Propagazione incertezze orbitali
 */
class UncertaintyPropagator {
public:
    /**
     * @brief Propaga incertezze linearmente (STM)
     * 
     * Usa State Transition Matrix per propagare covarianza
     * Veloce ma approssimato per grandi incertezze
     * 
     * @param initialState Stato iniziale
     * @param initialCovariance Covarianza iniziale (6×6)
     * @param finalTime Tempo finale
     * @param forceFunc Funzione forza
     * @return Covarianza finale
     */
    static std::vector<std::vector<double>> propagateLinear(
        const OrbitalState& initialState,
        const std::vector<std::vector<double>>& initialCovariance,
        const JulianDate& finalTime,
        const ForceFunction& forceFunc);
    
    /**
     * @brief Propaga con Unscented Transform
     * 
     * Metodo deterministico che usa sigma-points
     * Più accurato di linearizzazione per non-linearità moderate
     * 
     * @param initialState Stato iniziale
     * @param initialCovariance Covarianza iniziale
     * @param finalTime Tempo finale
     * @param forceFunc Funzione forza
     * @return Stato e covarianza finali
     */
    static std::pair<OrbitalState, std::vector<std::vector<double>>>
    propagateUnscented(
        const OrbitalState& initialState,
        const std::vector<std::vector<double>>& initialCovariance,
        const JulianDate& finalTime,
        const ForceFunction& forceFunc);
    
    /**
     * @brief Propaga con Monte Carlo
     * 
     * Metodo più accurato ma computazionalmente costoso
     * Campiona dalla distribuzione iniziale e propaga ogni sample
     * 
     * @param initialState Stato iniziale
     * @param initialCovariance Covarianza iniziale
     * @param finalTime Tempo finale
     * @param forceFunc Funzione forza
     * @param nSamples Numero campioni MC
     * @return Distribuzione stati finali
     */
    static std::vector<OrbitalState> propagateMonteCarlo(
        const OrbitalState& initialState,
        const std::vector<std::vector<double>>& initialCovariance,
        const JulianDate& finalTime,
        const ForceFunction& forceFunc,
        int nSamples = 1000);
    
    /**
     * @brief Estrae covarianza da campioni MC
     */
    static std::vector<std::vector<double>> computeCovarianceFromSamples(
        const std::vector<OrbitalState>& samples);
    
    /**
     * @brief Calcola ellissoide incertezza posizionale
     */
    static UncertaintyEllipsoid extractPositionEllipsoid(
        const std::vector<std::vector<double>>& covariance,
        double sigma = 1.0);
};

/**
 * @class OccultationProbabilityCalculator
 * @brief Calcola probabilità occultazione con incertezze
 */
class OccultationProbabilityCalculator {
public:
    /**
     * @brief Calcola probabilità con metodo analitico (Gaussiano)
     * 
     * Assume distribuzioni gaussiane e calcola probabilità
     * che shadow path passi entro distanza dal sito
     * 
     * @param nominalPath Path nominale
     * @param pathCovariance Covarianza path
     * @param observerLocation Posizione osservatore
     * @return Probabilità [0,1]
     */
    static double computeGaussianProbability(
        const std::vector<GeographicCoordinates>& nominalPath,
        const std::vector<std::vector<double>>& pathCovariance,
        const GeographicCoordinates& observerLocation);
    
    /**
     * @brief Calcola probabilità con Monte Carlo completo
     * 
     * Campiona elementi orbitali, propaga ciascuno,
     * calcola shadow path e conta hits
     * 
     * @param nominalElements Elementi nominali
     * @param covariance Covarianza elementi (6×6)
     * @param starPos Posizione stella
     * @param asteroidShape Forma asteroide
     * @param centralEpoch Epoca centrale evento
     * @param duration Durata totale (ore)
     * @param nTrials Numero trial MC
     * @return Risultato completo probabilità
     */
    static OccultationProbability computeMonteCarloProbability(
        const AstDynEquinoctialElements& nominalElements,
        const std::vector<std::vector<double>>& covariance,
        const EquatorialCoordinates& starPos,
        const AsteroidShape& asteroidShape,
        const JulianDate& centralEpoch,
        double duration,
        int nTrials = 10000);
    
    /**
     * @brief Genera mappa probabilità 2D
     * 
     * Crea griglia lat/lon e calcola probabilità per ogni cella
     * 
     * @param result Risultato MC con trials
     * @param resolution Risoluzione griglia (gradi)
     * @return Mappa probabilità
     */
    static OccultationProbability::ProbabilityMap generateProbabilityMap(
        const std::vector<std::vector<GeographicCoordinates>>& mcPaths,
        double resolution = 0.1);
    
    /**
     * @brief Calcola intervallo confidenza su probabilità
     * 
     * Usa statistica binomiale: σ_p = sqrt(p(1-p)/n)
     * 
     * @param probability Probabilità stimata
     * @param nTrials Numero trials
     * @param confidenceLevel Livello confidenza (default: 0.95)
     * @return Intervallo [p_low, p_high]
     */
    static std::pair<double, double> computeConfidenceInterval(
        double probability,
        int nTrials,
        double confidenceLevel = 0.95);
};

/**
 * @class BayesianUpdater
 * @brief Aggiornamento Bayesiano di elementi orbitali da osservazioni
 * 
 * Usa filtro Kalman esteso o particelle per aggiornare
 * distribuzione posteriore elementi da nuove osservazioni
 */
class BayesianUpdater {
public:
    /**
     * @brief Update con Extended Kalman Filter
     * 
     * @param priorState Stato prior
     * @param priorCovariance Covarianza prior
     * @param observation Nuova osservazione astrometrica
     * @return Stato e covarianza posteriori
     */
    static std::pair<OrbitalState, std::vector<std::vector<double>>>
    updateExtendedKalman(
        const OrbitalState& priorState,
        const std::vector<std::vector<double>>& priorCovariance,
        const AstrometricObservation& observation);
    
    /**
     * @brief Update con Particle Filter
     * 
     * @param priorParticles Particelle prior
     * @param observation Nuova osservazione
     * @return Particelle posteriori (resampled)
     */
    static std::vector<OrbitalState> updateParticleFilter(
        const std::vector<OrbitalState>& priorParticles,
        const AstrometricObservation& observation);
    
    /**
     * @brief Calcola likelihood osservazione
     */
    static double computeLikelihood(
        const OrbitalState& state,
        const AstrometricObservation& observation);
};

/**
 * @class CovarianceAnalysis
 * @brief Analisi e manipolazione matrici covarianza
 */
class CovarianceAnalysis {
public:
    /**
     * @brief Decomposizione autovalori
     * @return Autovalori e autovettori
     */
    static std::pair<std::vector<double>, std::vector<std::vector<double>>>
    eigenDecomposition(const std::vector<std::vector<double>>& covariance);
    
    /**
     * @brief Calcola correlazioni da covarianza
     * @return Matrice correlazione
     */
    static std::vector<std::vector<double>> computeCorrelation(
        const std::vector<std::vector<double>>& covariance);
    
    /**
     * @brief Verifica positività definita
     */
    static bool isPositiveDefinite(
        const std::vector<std::vector<double>>& matrix);
    
    /**
     * @brief Regolarizza matrice (aggiunge ridge)
     */
    static std::vector<std::vector<double>> regularize(
        const std::vector<std::vector<double>>& matrix,
        double epsilon = 1e-10);
    
    /**
     * @brief Calcola numero condizione
     */
    static double conditionNumber(
        const std::vector<std::vector<double>>& matrix);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_UNCERTAINTY_PROPAGATION_H
