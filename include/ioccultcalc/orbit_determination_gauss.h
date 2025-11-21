/**
 * @file orbit_determination_gauss.h
 * @brief Determinazione orbitale con metodo di Gauss
 * 
 * Implementa il metodo classico di Gauss per determinazione orbitale
 * iniziale da 3 osservazioni astrometriche. Ispirato a Find_Orb di
 * Project Pluto (https://www.projectpluto.com/find_orb.htm)
 * 
 * Flusso:
 * 1. Seleziona 3 osservazioni ben distribuite
 * 2. Metodo di Gauss per orbita preliminare
 * 3. Differential correction con full n-body
 * 4. Outlier rejection robusto
 * 5. Iterazione fino a convergenza
 * 
 * Riferimenti:
 * - Gauss, "Theoria Motus Corporum Coelestium" (1809)
 * - Danby, "Fundamentals of Celestial Mechanics" (1988)
 * - Find_Orb source code (Bill Gray, Project Pluto)
 */

#ifndef IOCCULTCALC_ORBIT_DETERMINATION_GAUSS_H
#define IOCCULTCALC_ORBIT_DETERMINATION_GAUSS_H

#include "ioccultcalc/observation.h"
#include "ioccultcalc/orbital_elements.h"
#include "ioccultcalc/types.h"
#include <vector>
#include <functional>

namespace ioccultcalc {

/**
 * @struct GaussOrbitResult
 * @brief Risultato determinazione orbitale metodo Gauss
 */
struct GaussOrbitResult {
    bool success;
    EquinoctialElements elements;
    
    // Statistiche fit
    double rmsResidual;         // arcsec
    int nObservationsUsed;
    int nIterations;
    
    // Selezione osservazioni Gauss
    int obs1Index;  // Prima osservazione
    int obs2Index;  // Seconda osservazione (centrale)
    int obs3Index;  // Terza osservazione
    
    // Diagnostica
    std::string message;
    double conditionNumber;     // Condizionamento sistema
    bool reliableOrbit;         // Orbita affidabile?
    
    GaussOrbitResult() : success(false), rmsResidual(0), 
                        nObservationsUsed(0), nIterations(0),
                        obs1Index(-1), obs2Index(-1), obs3Index(-1),
                        conditionNumber(0), reliableOrbit(false) {}
};

/**
 * @class GaussOrbitDeterminer
 * @brief Determinazione orbitale con metodo di Gauss + differential correction
 * 
 * Strategia robusta ispirata a Find_Orb:
 * - Usa 3 osservazioni ben spaziate per orbita iniziale
 * - Differential correction con full model
 * - Outlier rejection progressivo
 * - Convergenza garantita o fallimento controllato
 */
class GaussOrbitDeterminer {
public:
    GaussOrbitDeterminer();
    ~GaussOrbitDeterminer();
    
    /**
     * @brief Determina orbita da set di osservazioni
     * 
     * @param observations Osservazioni astrometriche
     * @param maxIterations Max iterazioni differential correction (default 20)
     * @param convergenceThreshold Soglia convergenza RMS (arcsec, default 0.1)
     * @return Risultato con elementi orbitali e statistiche
     */
    GaussOrbitResult determineOrbit(const ObservationSet& observations,
                                   int maxIterations = 20,
                                   double convergenceThreshold = 0.1);
    
    /**
     * @brief Seleziona 3 osservazioni ottimali per Gauss
     * 
     * Strategia:
     * - Prima e ultima osservazione per massimo arc
     * - Osservazione centrale a ~metà arc
     * - Evita osservazioni troppo vicine temporalmente
     * 
     * @param observations Set completo
     * @param idx1 Output: indice prima osservazione
     * @param idx2 Output: indice osservazione centrale
     * @param idx3 Output: indice terza osservazione
     * @return true se selezione riuscita
     */
    bool selectThreeObservations(const ObservationSet& observations,
                                 int& idx1, int& idx2, int& idx3);
    
    /**
     * @brief Metodo di Gauss per orbita preliminare
     * 
     * Input: 3 osservazioni (RA, Dec, tempo)
     * Output: elementi orbitali preliminari
     * 
     * @param obs1 Prima osservazione
     * @param obs2 Seconda osservazione (centrale)
     * @param obs3 Terza osservazione
     * @param elements Output: elementi determinati
     * @return true se determinazione riuscita
     */
    bool gaussMethod(const AstrometricObservation& obs1,
                    const AstrometricObservation& obs2,
                    const AstrometricObservation& obs3,
                    EquinoctialElements& elements);
    
    /**
     * @brief Differential correction con modello completo
     * 
     * Migliora elementi da Gauss usando tutte le osservazioni
     * 
     * @param initialElements Elementi da Gauss
     * @param observations Tutte le osservazioni
     * @param maxIterations Max iterazioni
     * @param threshold Soglia convergenza (arcsec)
     * @return Elementi migliorati e statistiche
     */
    GaussOrbitResult differentialCorrection(const EquinoctialElements& initialElements,
                                           const ObservationSet& observations,
                                           int maxIterations,
                                           double threshold);
    
    /**
     * @brief Valida qualità orbita determinata
     * 
     * Criteri (ispirato a Find_Orb):
     * - RMS < 5 arcsec
     * - Eccentricità fisica (e < 0.99)
     * - Semiasse maggiore ragionevole (0.1 < a < 100 AU)
     * - Condition number < 1e6
     * 
     * @param result Risultato da validare
     * @return true se orbita accettabile
     */
    bool validateOrbit(GaussOrbitResult& result);
    
    /**
     * @brief Callback per progresso iterazione
     */
    void setProgressCallback(std::function<void(int, double)> callback) {
        progressCallback_ = callback;
    }
    
private:
    class Impl;
    Impl* pImpl;
    
    std::function<void(int, double)> progressCallback_;
    
    // Metodi interni
    Vector3D computeEarthPosition(const JulianDate& jd);
    Vector3D computeSunPosition(const JulianDate& jd);
    double solveKeplerEquation(double M, double e, double tol = 1e-12);
    
    // Gauss helpers
    bool computeSlantRanges(const AstrometricObservation& obs1,
                           const AstrometricObservation& obs2,
                           const AstrometricObservation& obs3,
                           double& rho1, double& rho2, double& rho3);
    
    bool vectorsToElements(const Vector3D& r, const Vector3D& v,
                          const JulianDate& epoch,
                          EquinoctialElements& elements);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ORBIT_DETERMINATION_GAUSS_H
