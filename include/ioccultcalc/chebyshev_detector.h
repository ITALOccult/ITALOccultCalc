/**/**#ifndef IOCCULTCALC_CHEBYSHEV_DETECTOR_H

 * @file chebyshev_detector.h

 * @brief Rilevamento veloce di occultazioni con approssimazione di Chebyshev * @file chebyshev_detector.h#define IOCCULTCALC_CHEBYSHEV_DETECTOR_H

 * 

 * Implementa la FASE 1 della strategia di ricerca a due fasi: * @brief Rilevazione veloce di occultazioni usando approssimazione di Chebyshev

 * - Usa polinomi di Chebyshev per approssimare la traiettoria

 * - Trova candidati vicini alle stelle (~100-1000x più veloce dell'integrazione) * #include "types.h"

 * - I candidati vengono poi raffinati con predictOccultation (FASE 2)

 *  * Implementa la FASE 1 della strategia di ricerca a due fasi:#include "ephemeris.h"

 * @author Michele Bigi

 * @date 2025-11-29 * 1. Approssima la traiettoria dell'asteroide con polinomi di Chebyshev#include "chebyshev_approximation.h"

 */

 * 2. Cerca intersezioni algebriche tra curva e cerchi attorno alle stelle#include <vector>

#ifndef IOCCULTCALC_CHEBYSHEV_DETECTOR_H

#define IOCCULTCALC_CHEBYSHEV_DETECTOR_H * 3. Trova candidati in modo molto più veloce (~100-1000x) dell'integrazione#include <memory>



#include "chebyshev_approximation.h" * #include <functional>

#include "ephemeris.h"

#include <vector> * I candidati trovati vengono poi raffinati nella FASE 2 con l'integratore

#include <utility>

 * preciso (RKF78, GAUSS_RADAU, etc.) tramite predictOccultation().namespace ioccultcalc {

namespace ioccultcalc {

 * 

/**

 * @brief Risultato di una ricerca candidati * @author Michele Bigi/**

 */

struct OccultationCandidate { * @date 2025-11-29 * Polinomio Chebyshev 1D con supporto per derivata e ricerca radici

    int starIndex;              ///< Indice stella nel catalogo

    double jd;                  ///< JD approssimato della close approach */ */

    double minDistArcsec;       ///< Distanza minima in arcsecondi

    double uncertaintyArcsec;   ///< Incertezza sulla distanzaclass ChebyshevPolynomial {

    

    OccultationCandidate() #ifndef IOCCULTCALC_CHEBYSHEV_DETECTOR_Hpublic:

        : starIndex(-1), jd(0), minDistArcsec(1e10), uncertaintyArcsec(0) {}

};#define IOCCULTCALC_CHEBYSHEV_DETECTOR_H    ChebyshevPolynomial() : m_t0(0), m_t1(1) {}



/**    ChebyshevPolynomial(double t0, double t1, const std::vector<double>& coeffs);

 * @brief Detector di occultazioni usando approssimazione di Chebyshev

 * #include "chebyshev_approximation.h"    

 * Strategia:

 * 1. Costruisce approssimazione Chebyshev della traiettoria#include "types.h"    /**

 * 2. Per ogni stella, cerca intersezioni tra traiettoria e cerchio attorno alla stella

 * 3. Restituisce candidati che richiedono refinement con integratore preciso#include <vector>     * Valuta polinomio al tempo t (in JD)

 */

class ChebyshevOccultationDetector {#include <memory>     */

public:

    /**    double evaluate(double t) const;

     * @brief Configurazione detector

     */namespace ioccultcalc {    

    struct Config {

        int order;                  ///< Ordine polinomio Chebyshev (8-15)    /**

        double segmentDays;         ///< Durata segmento (giorni)

        double thresholdArcsec;     ///< Soglia ricerca iniziale (arcsec)/**     * Calcola derivata (restituisce nuovo polinomio)

        double refinementArcsec;    ///< Soglia per refinement (arcsec)

        bool verbose;               ///< Output debug * @brief Candidato occultazione trovato con metodo Chebyshev     * Usa: T'ₙ(x) = n × U_{n-1}(x) dove Uₙ è Chebyshev 2a specie

        

        Config()  */     * Ma più semplice: d/dt p(t) con cambio scala

            : order(11), segmentDays(1.0), 

              thresholdArcsec(300.0), refinementArcsec(60.0),struct ChebyshevCandidate {     */

              verbose(false) {}

    };    int starIndex;              ///< Indice stella nel catalogo    ChebyshevPolynomial derivative() const;

    

    /**    double jd;                  ///< JD approssimativo dell'occultazione    

     * @brief Costruttore

     * @param config Configurazione    double minDistArcsec;       ///< Distanza minima in arcsec    /**

     */

    explicit ChebyshevOccultationDetector(const Config& config);    EquatorialCoordinates asteroidPos;  ///< Posizione asteroide al JD     * Trova radici del polinomio in [t0, t1]

    

    ~ChebyshevOccultationDetector();         * Usa metodo companion matrix + eigenvalues

    

    /**    ChebyshevCandidate() : starIndex(-1), jd(0), minDistArcsec(1e9) {}     */

     * @brief Inizializza il detector per un asteroide

     * @param ephemeris Ephemeris dell'asteroide};    std::vector<double> findRoots() const;

     * @param startJD JD inizio periodo

     * @param endJD JD fine periodo    

     * @return true se successo

     *//**    /**

    bool initialize(const Ephemeris& ephemeris, double startJD, double endJD);

     * @brief Configurazione per ChebyshevOccultationDetector     * Trova minimi locali in [t0, t1]

    /**

     * @brief Trova candidati occultazione per lista di stelle */     * Calcola radici della derivata e verifica segno derivata seconda

     * @param stars Vettore di coppie (RA, Dec) in gradi

     * @return Lista di candidati che richiedono refinementstruct ChebyshevDetectorConfig {     */

     */

    std::vector<OccultationCandidate> findCandidates(    int order;                  ///< Ordine polinomio Chebyshev (11-15)    std::vector<double> findMinima() const;

        const std::vector<std::pair<double, double>>& stars);

        double segmentDays;         ///< Durata segmenti (giorni)    

    /**

     * @brief Trova candidati per una singola stella    double thresholdArcsec;     ///< Threshold ricerca candidati (arcsec)    /**

     * @param ra RA stella (gradi)

     * @param dec Dec stella (gradi)    double refinementArcsec;    ///< Threshold per refinement (arcsec)     * Sottrae costante dal polinomio

     * @param starIndex Indice stella nel catalogo

     * @return Candidato, o starIndex=-1 se nessuno trovato    bool verbose;               ///< Output diagnostico     */

     */

    OccultationCandidate findCandidate(double ra, double dec, int starIndex);        ChebyshevPolynomial subtract(double value) const;

    

    /**    ChebyshevDetectorConfig()    

     * @brief Ottiene l'approssimazione Chebyshev utilizzata

     */        : order(11), segmentDays(1.0),     /**

    const ChebyshevApproximation& getApproximation() const { 

        return *approximation_;           thresholdArcsec(300.0), refinementArcsec(60.0),     * Moltiplica per altro polinomio

    }

              verbose(false) {}     */

private:

    Config config_;};    ChebyshevPolynomial multiply(const ChebyshevPolynomial& other) const;

    std::unique_ptr<ChebyshevApproximation> approximation_;

    double startJD_;    

    double endJD_;

    bool initialized_;/**    /**

    

    /** * @brief Detector di occultazioni usando approssimazione di Chebyshev     * Somma con altro polinomio

     * @brief Calcola distanza angolare in arcsecondi

     */ *      */

    double angularDistance(double ra1, double dec1, double ra2, double dec2) const;

     * Strategia di ricerca ottimizzata:    ChebyshevPolynomial add(const ChebyshevPolynomial& other) const;

    /**

     * @brief Trova il minimo di distanza usando ricerca binaria * 1. Divide il periodo in segmenti (es. 1 giorno ciascuno)    

     * @param ra RA target (gradi)

     * @param dec Dec target (gradi) * 2. Per ogni segmento, approssima traiettoria con Chebyshev    /**

     * @param jdStart JD inizio intervallo

     * @param jdEnd JD fine intervallo * 3. Per ogni stella, cerca intersezioni algebriche curva-cerchio     * Intervallo temporale

     * @return Coppia (JD minimo, distanza minima arcsec)

     */ * 4. Ritorna candidati che richiedono refinement preciso     */

    std::pair<double, double> findMinDistance(double ra, double dec,

                                              double jdStart, double jdEnd) const; *     double t0() const { return m_t0; }

};

 * Performance: ~100-1000x più veloce rispetto a loop diretto con integratore    double t1() const { return m_t1; }

} // namespace ioccultcalc

 */    

#endif // IOCCULTCALC_CHEBYSHEV_DETECTOR_H

class ChebyshevOccultationDetector {    /**

public:     * Coefficienti

    using Config = ChebyshevDetectorConfig;     */

        const std::vector<double>& coeffs() const { return m_coeffs; }

    /**    int order() const { return m_coeffs.size() - 1; }

     * @brief Costruttore    

     * @param config Configurazione detectorprivate:

     */    double m_t0, m_t1;           // Intervallo temporale [JD]

    explicit ChebyshevOccultationDetector(const Config& config = Config());    std::vector<double> m_coeffs; // Coefficienti Chebyshev

        

    ~ChebyshevOccultationDetector();    /**

         * Normalizza tempo t ∈ [t0, t1] → x ∈ [-1, 1]

    /**     */

     * @brief Inizializza il detector con traiettoria asteroide    double normalizeTime(double t) const;

     * @param ephemeris Oggetto Ephemeris dell'asteroide    

     * @param startJD JD inizio periodo ricerca    /**

     * @param endJD JD fine periodo ricerca     * De-normalizza x ∈ [-1, 1] → t ∈ [t0, t1]

     * @return true se successo, false altrimenti     */

     */    double denormalizeTime(double x) const;

    bool initialize(const Ephemeris& ephemeris, double startJD, double endJD);    

        /**

    /**     * Valuta polinomio in x normalizzato

     * @brief Cerca candidati occultazione per una lista di stelle     */

     * @param stars Lista di coordinate stelle (RA, Dec in gradi)    double evaluateNormalized(double x) const;

     * @return Lista di candidati trovati};

     */

    std::vector<ChebyshevCandidate> findCandidates(/**

        const std::vector<std::pair<double, double>>& stars) const; * Segmento Chebyshev per RA/Dec (coordinate angolari)

     */

    /**class ChebyshevRADecSegment {

     * @brief Cerca candidati per una singola stellapublic:

     * @param ra RA stella (gradi)    ChebyshevRADecSegment(double t0, double t1, int order);

     * @param dec Dec stella (gradi)    

     * @return Candidato se trovato, starIndex=-1 se nessun candidato    /**

     */     * Calcola coefficienti da ephemeris

    ChebyshevCandidate findCandidatesForStar(double ra, double dec) const;     */

        void computeCoefficients(Ephemeris& ephemeris);

    /**    

     * @brief Ottiene l'approssimazione Chebyshev generata    /**

     * @return Riferimento all'approssimazione     * Valuta RA, Dec al tempo t

     */     */

    const ChebyshevApproximation& getApproximation() const { return *approximation_; }    void evaluate(double t, double& ra, double& dec) const;

        

    /**    /**

     * @brief Valuta qualità approssimazione     * Restituisce polinomi RA e Dec

     * @param ephemeris Ephemeris originale per confronto     */

     * @return Errore massimo in arcsec    const ChebyshevPolynomial& raPolynomial() const { return m_raCheb; }

     */    const ChebyshevPolynomial& decPolynomial() const { return m_decCheb; }

    double evaluateQuality(const Ephemeris& ephemeris) const;    

        /**

private:     * Intervallo

    Config config_;     */

    std::unique_ptr<ChebyshevApproximation> approximation_;    double t0() const { return m_t0; }

    double startJD_;    double t1() const { return m_t1; }

    double endJD_;    bool contains(double t) const { return t >= m_t0 && t <= m_t1; }

        

    /**private:

     * @brief Trova la distanza minima tra traiettoria e stella in un segmento    double m_t0, m_t1;

     * @param segment Segmento Chebyshev    int m_order;

     * @param starRA RA stella (gradi)    ChebyshevPolynomial m_raCheb;

     * @param starDec Dec stella (gradi)    ChebyshevPolynomial m_decCheb;

     * @param minDistArcsec [out] Distanza minima trovata};

     * @param jdMin [out] JD del punto di minima distanza

     * @return true se trovato candidato entro threshold/**

     */ * Candidato occultazione trovato da detector

    bool findMinDistanceInSegment(const ChebyshevSegment& segment, */

                                  double starRA, double starDec,struct OccultationCandidate {

                                  double& minDistArcsec, double& jdMin) const;    double jd;              // Tempo minima distanza angolare

        double minDistArcsec;   // Distanza minima in arcsec

    /**    double starRA;          // RA stella (rad)

     * @brief Cerca zeri della derivata della distanza (punti critici)    double starDec;         // Dec stella (rad)

     * @param segment Segmento Chebyshev    int starIndex;          // Indice stella nel catalogo

     * @param starRA RA stella (gradi)};

     * @param starDec Dec stella (gradi)

     * @return Lista di JD corrispondenti a punti critici/**

     */ * Detector occultazioni basato su intersezione Chebyshev

    std::vector<double> findCriticalPoints(const ChebyshevSegment& segment, * 

                                          double starRA, double starDec) const; * Algoritmo:

     * 1. Interpola traiettoria asteroide (RA, Dec) con Chebyshev

    /** * 2. Per ogni stella S, calcola polinomio D²(t) = (RA(t)-RA_s)² + (Dec(t)-Dec_s)²

     * @brief Calcola distanza angolare tra due posizioni equatoriali * 3. Trova minimi di D²(t) (radici di dD²/dt)

     * @param ra1 RA prima posizione (gradi) * 4. Se min(D²) < threshold² → candidato occultazione

     * @param dec1 Dec prima posizione (gradi) */

     * @param ra2 RA seconda posizione (gradi)class ChebyshevOccultationDetector {

     * @param dec2 Dec seconda posizione (gradi)public:

     * @return Distanza angolare (arcsec)    struct Config {

     */        int order = 11;                    // Ordine polinomio Chebyshev

    double angularDistance(double ra1, double dec1, double ra2, double dec2) const;        double segmentDays = 1.0;          // Durata segmento (giorni)

            double thresholdArcsec = 300.0;    // Threshold ricerca (arcsec) - ~5 arcmin

    /**        double refinementArcsec = 60.0;    // Threshold per refine (arcsec)

     * @brief Risolve equazione per trovare minimo distanza        bool verbose = false;

     * @param segment Segmento Chebyshev    };

     * @param starRA RA stella (gradi)    

     * @param starDec Dec stella (gradi)    explicit ChebyshevOccultationDetector(const Config& config);

     * @param xStart Valore iniziale normalizzato [-1, 1]    

     * @param xMin [out] Valore normalizzato del minimo    /**

     * @return true se convergenza, false altrimenti     * Inizializza detector per intervallo temporale

     */     * Pre-calcola segmenti Chebyshev per l'asteroide

    bool solveForMinimum(const ChebyshevSegment& segment,     */

                        double starRA, double starDec,    void initialize(Ephemeris& ephemeris, double startJD, double endJD);

                        double xStart, double& xMin) const;    

};    /**

     * Cerca occultazioni con lista di stelle

} // namespace ioccultcalc     * Restituisce candidati ordinati per tempo

     */

#endif // IOCCULTCALC_CHEBYSHEV_DETECTOR_H    std::vector<OccultationCandidate> findCandidates(

        const std::vector<std::pair<double, double>>& stars  // (RA, Dec) in radianti
    ) const;
    
    /**
     * Cerca occultazione con singola stella
     * Restituisce tutti i passaggi entro threshold
     */
    std::vector<OccultationCandidate> findCandidatesForStar(
        double starRA, double starDec, int starIndex
    ) const;
    
    /**
     * Valuta distanza angolare asteroide-stella al tempo t
     */
    double angularDistance(double t, double starRA, double starDec) const;
    
    /**
     * Statistiche
     */
    int segmentCount() const { return m_segments.size(); }
    bool isInitialized() const { return m_initialized; }
    
private:
    Config m_config;
    std::vector<std::shared_ptr<ChebyshevRADecSegment>> m_segments;
    double m_startJD, m_endJD;
    bool m_initialized;
    
    /**
     * Trova segmento contenente tempo t
     */
    std::shared_ptr<ChebyshevRADecSegment> findSegment(double t) const;
    
    /**
     * Calcola polinomio distanza² da stella per un segmento
     * D²(t) = (RA(t) - RA_s)² × cos²(Dec_avg) + (Dec(t) - Dec_s)²
     */
    ChebyshevPolynomial computeDistanceSquared(
        const ChebyshevRADecSegment& segment,
        double starRA, double starDec
    ) const;
    
    /**
     * Refine tempo minimo usando Newton-Raphson
     */
    double refineMinimum(double t_approx, double starRA, double starDec) const;
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_CHEBYSHEV_DETECTOR_H
