/**/**#ifndef IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H

 * @file chebyshev_approximation.h

 * @brief Approssimazione di Chebyshev per traiettorie asteroidali * @file chebyshev_approximation.h#define IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H

 * 

 * Implementa l'approssimazione polinomiale di Chebyshev per rappresentare * @brief Approssimazione di Chebyshev per traiettorie asteroidali

 * la traiettoria di un asteroide in modo efficiente. Permette di calcolare

 * rapidamente la posizione dell'asteroide senza integrazioni numeriche costose. * #include "types.h"

 * 

 * Utilizzato nella FASE 1 della strategia di ricerca occultazioni: * Implementa l'approssimazione polinomiale di Chebyshev per rappresentare#include "ephemeris.h"

 * - Approssima la traiettoria con polinomi di Chebyshev

 * - Permette ricerca veloce (~100-1000x più veloce dell'integrazione) * la traiettoria di un asteroide in modo efficiente. Permette di calcolare#include <vector>

 * - I candidati trovati vengono poi raffinati con l'integratore preciso (FASE 2)

 *  * rapidamente la posizione dell'asteroide senza integrazioni numeriche costose.#include <memory>

 * @author Michele Bigi

 * @date 2025-11-29 * 

 */

 * Utilizzato nella FASE 1 della strategia di ricerca occultazioni:namespace ioccultcalc {

#ifndef IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H

#define IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H * - Approssima la traiettoria con polinomi di Chebyshev



#include "types.h" * - Permette ricerca veloce (~100-1000x più veloce dell'integrazione)/**

#include "ephemeris.h"

#include <vector> * - I candidati trovati vengono poi raffinati con l'integratore preciso (FASE 2) * Configurazione approssimazione Chebyshev

#include <memory>

 *  */

namespace ioccultcalc {

 * @author Michele Bigistruct ChebyshevConfig {

/**

 * @brief Configurazione per l'approssimazione di Chebyshev * @date 2025-11-29    bool enabled = false;              // Abilita Chebyshev approximation

 */

struct ChebyshevConfig { */    int order = 11;                    // Ordine polinomio (7-15, LinOccult usa 11)

    int order;              ///< Ordine del polinomio (tipicamente 11-15)

    double segmentDays;     ///< Durata di ogni segmento (giorni)    double intervalDays = 1.0;         // Intervallo per segmento (0.5-2.0 giorni)

    bool geocentric;        ///< Se true, usa coordinate geocentriche

    #ifndef IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H    double fineTimestepMinutes = 15.0; // Timestep fine per scanning

    ChebyshevConfig() 

        : order(11), segmentDays(1.0), geocentric(true) {}#define IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H    bool precisionCheck = false;       // Valida errore vs integrazione diretta

};

    double maxErrorKm = 10.0;          // Errore massimo accettabile (km)

/**

 * @brief Singolo segmento di approssimazione Chebyshev#include "types.h"};

 * 

 * Rappresenta un intervallo temporale approssimato con polinomi di Chebyshev.#include "ephemeris.h"

 * Per intervalli più lunghi, si usano multipli segmenti concatenati.

 */#include <vector>/**

struct ChebyshevSegment {

    double startJD;         ///< JD inizio segmento#include <memory> * Singolo segmento di approssimazione Chebyshev

    double endJD;           ///< JD fine segmento

    double midJD;           ///< JD centro segmento (per normalizzazione) * Rappresenta un polinomio per intervallo temporale [t0, t1]

    double halfSpan;        ///< Metà durata segmento (per normalizzazione)

    namespace ioccultcalc { */

    // Coefficienti Chebyshev per ciascuna coordinata

    std::vector<double> coeffX;  ///< Coefficienti per X (RA o X geocentrico)class ChebyshevSegment {

    std::vector<double> coeffY;  ///< Coefficienti per Y (Dec o Y geocentrico)

    std::vector<double> coeffZ;  ///< Coefficienti per Z (distanza o Z geocentrico)/**public:

    

    ChebyshevSegment() : startJD(0), endJD(0), midJD(0), halfSpan(0) {} * @brief Configurazione per l'approssimazione di Chebyshev    ChebyshevSegment(const JulianDate& t0, const JulianDate& t1, int order);

};

 */    

/**

 * @brief Approssimazione completa di una traiettoria con polinomi di Chebyshevstruct ChebyshevConfig {    /**

 * 

 * Gestisce multipli segmenti per coprire intervalli temporali estesi.    int order;              ///< Ordine del polinomio (tipicamente 11-15)     * Calcola coefficienti Chebyshev per questo segmento

 * Fornisce metodi per valutare rapidamente la posizione in qualsiasi istante.

 */    double segmentDays;     ///< Durata di ogni segmento (giorni)     * Usa punti campionati da ephemeris.compute()

class ChebyshevApproximation {

public:    bool geocentric;        ///< Se true, usa coordinate geocentriche     */

    /**

     * @brief Costruttore        void computeCoefficients(Ephemeris& ephemeris);

     * @param config Configurazione approssimazione

     */    ChebyshevConfig()     

    explicit ChebyshevApproximation(const ChebyshevConfig& config = ChebyshevConfig());

            : order(11), segmentDays(1.0), geocentric(true) {}    /**

    ~ChebyshevApproximation();

    };     * Valuta posizione/velocità al tempo t usando interpolazione Chebyshev

    /**

     * @brief Genera l'approssimazione per una traiettoria     * Restituisce posizione geocentrica in coordinate equatoriali

     * @param ephemeris Oggetto Ephemeris per l'asteroide

     * @param startJD JD inizio periodo/**     */

     * @param endJD JD fine periodo

     * @return true se successo, false altrimenti * @brief Singolo segmento di approssimazione Chebyshev    EphemerisData evaluate(const JulianDate& t) const;

     */

    bool generate(const Ephemeris& ephemeris, double startJD, double endJD); *     

    

    /** * Rappresenta un intervallo temporale approssimato con polinomi di Chebyshev.    /**

     * @brief Valuta la posizione geocentrica all'istante specificato

     * @param jd Julian Date * Per intervalli più lunghi, si usano multipli segmenti concatenati.     * Verifica se tempo t è contenuto in questo segmento

     * @return Coordinate equatoriali geocentriche (RA, Dec in gradi, distanza in AU)

     */ */     */

    EquatorialCoordinates evaluate(double jd) const;

    struct ChebyshevSegment {    bool contains(const JulianDate& t) const;

    /**

     * @brief Valuta posizione e velocità geocentriche    double startJD;         ///< JD inizio segmento    

     * @param jd Julian Date

     * @param pos [out] Posizione (X, Y, Z) in AU    double endJD;           ///< JD fine segmento    /**

     * @param vel [out] Velocità (Vx, Vy, Vz) in AU/day

     */    double midJD;           ///< JD centro segmento (per normalizzazione)     * Errore stimato dell'approssimazione (km)

    void evaluateStateVector(double jd, Vector3D& pos, Vector3D& vel) const;

        double halfSpan;        ///< Metà durata segmento (per normalizzazione)     */

    /**

     * @brief Trova il segmento che contiene il JD specificato        double estimatedError() const { return m_estimatedError; }

     * @param jd Julian Date

     * @return Puntatore al segmento, nullptr se fuori range    // Coefficienti Chebyshev per ciascuna coordinata    

     */

    const ChebyshevSegment* findSegment(double jd) const;    std::vector<double> coeffX;  ///< Coefficienti per X (RA o X geocentrico)    /**

    

    /**    std::vector<double> coeffY;  ///< Coefficienti per Y (Dec o Y geocentrico)     * Intervallo temporale

     * @brief Ottiene l'intervallo temporale coperto

     * @param startJD [out] JD inizio    std::vector<double> coeffZ;  ///< Coefficienti per Z (distanza o Z geocentrico)     */

     * @param endJD [out] JD fine

     */        double startJD() const { return m_t0.jd; }

    void getTimeSpan(double& startJD, double& endJD) const;

        ChebyshevSegment() : startJD(0), endJD(0), midJD(0), halfSpan(0) {}    double endJD() const { return m_t1.jd; }

    /**

     * @brief Ottiene il numero di segmenti};    

     */

    size_t getSegmentCount() const { return segments_.size(); }private:

    

    /**/**    JulianDate m_t0, m_t1;    // Intervallo temporale

     * @brief Valuta l'errore massimo dell'approssimazione

     * @param ephemeris Ephemeris originale per confronto * @brief Approssimazione completa di una traiettoria con polinomi di Chebyshev    int m_order;              // Ordine polinomio

     * @param numTestPoints Numero di punti di test

     * @return Errore massimo in arcsec *     double m_estimatedError;  // Errore stimato (km)

     */

    double estimateMaxError(const Ephemeris& ephemeris, int numTestPoints = 100) const; * Gestisce multipli segmenti per coprire intervalli temporali estesi.    

    

private: * Fornisce metodi per valutare rapidamente la posizione in qualsiasi istante.    // Coefficienti Chebyshev per X, Y, Z (posizione)

    ChebyshevConfig config_;

    std::vector<ChebyshevSegment> segments_; */    std::vector<double> m_coeffsX;

    

    /**class ChebyshevApproximation {    std::vector<double> m_coeffsY;

     * @brief Genera un singolo segmento

     * @param ephemeris Oggetto Ephemerispublic:    std::vector<double> m_coeffsZ;

     * @param startJD JD inizio segmento

     * @param endJD JD fine segmento    /**    

     * @return Segmento generato

     */     * @brief Costruttore    // Coefficienti per VX, VY, VZ (velocità) - opzionale

    ChebyshevSegment generateSegment(const Ephemeris& ephemeris, 

                                     double startJD, double endJD);     * @param config Configurazione approssimazione    std::vector<double> m_coeffsVX;

    

    /**     */    std::vector<double> m_coeffsVY;

     * @brief Calcola i coefficienti di Chebyshev per una serie di valori

     * @param values Valori della funzione in punti Chebyshev    explicit ChebyshevApproximation(const ChebyshevConfig& config = ChebyshevConfig());    std::vector<double> m_coeffsVZ;

     * @return Coefficienti di Chebyshev

     */        

    std::vector<double> computeChebyshevCoefficients(const std::vector<double>& values) const;

        ~ChebyshevApproximation();    /**

    /**

     * @brief Valuta un polinomio di Chebyshev         * Normalizza tempo t ∈ [t0, t1] → x ∈ [-1, 1]

     * @param coeffs Coefficienti del polinomio

     * @param x Valore normalizzato in [-1, 1]    /**     */

     * @return Valore del polinomio

     */     * @brief Genera l'approssimazione per una traiettoria    double normalizeTime(const JulianDate& t) const;

    double evaluateChebyshevPolynomial(const std::vector<double>& coeffs, double x) const;

         * @param ephemeris Oggetto Ephemeris per l'asteroide    

    /**

     * @brief Valuta la derivata di un polinomio di Chebyshev     * @param startJD JD inizio periodo    /**

     * @param coeffs Coefficienti del polinomio

     * @param x Valore normalizzato in [-1, 1]     * @param endJD JD fine periodo     * Valuta polinomio Chebyshev: Σ cᵢ × Tᵢ(x)

     * @return Valore della derivata

     */     * @return true se successo, false altrimenti     */

    double evaluateChebyshevDerivative(const std::vector<double>& coeffs, double x) const;

         */    double evaluatePolynomial(const std::vector<double>& coeffs, double x) const;

    /**

     * @brief Normalizza un JD all'intervallo [-1, 1] per un segmento    bool generate(const Ephemeris& ephemeris, double startJD, double endJD);    

     * @param jd Julian Date

     * @param segment Segmento di riferimento        /**

     * @return Valore normalizzato

     */    /**     * Calcola polinomi Chebyshev T₀, T₁, ..., Tₙ in x

    double normalizeTime(double jd, const ChebyshevSegment& segment) const;

};     * @brief Valuta la posizione geocentrica all'istante specificato     * Usa ricorsione: Tₙ₊₁(x) = 2x×Tₙ(x) - Tₙ₋₁(x)



} // namespace ioccultcalc     * @param jd Julian Date     */



#endif // IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H     * @return Coordinate equatoriali geocentriche (RA, Dec in gradi, distanza in AU)    void evaluateChebyshevBasis(double x, std::vector<double>& T) const;


     */};

    EquatorialCoordinates evaluate(double jd) const;

    /**

    /** * Cache di segmenti Chebyshev per intervallo temporale esteso

     * @brief Valuta posizione e velocità geocentriche * Gestisce pre-computazione e lookup efficiente

     * @param jd Julian Date */

     * @param pos [out] Posizione (X, Y, Z) in AUclass ChebyshevCache {

     * @param vel [out] Velocità (Vx, Vy, Vz) in AU/daypublic:

     */    explicit ChebyshevCache(const ChebyshevConfig& config);

    void evaluateStateVector(double jd, Vector3D& pos, Vector3D& vel) const;    

        /**

    /**     * Inizializza cache per intervallo temporale

     * @brief Trova il segmento che contiene il JD specificato     * Pre-computa tutti i segmenti necessari

     * @param jd Julian Date     */

     * @return Puntatore al segmento, nullptr se fuori range    void initialize(Ephemeris& ephemeris, 

     */                   const JulianDate& startJD,

    const ChebyshevSegment* findSegment(double jd) const;                   const JulianDate& endJD);

        

    /**    /**

     * @brief Ottiene l'intervallo temporale coperto     * Valuta posizione al tempo t usando segmento appropriato

     * @param startJD [out] JD inizio     * Restituisce nullptr se t fuori range

     * @param endJD [out] JD fine     */

     */    EphemerisData evaluate(const JulianDate& t) const;

    void getTimeSpan(double& startJD, double& endJD) const;    

        /**

    /**     * Genera timestep fine per scanning

     * @brief Ottiene il numero di segmenti     * Restituisce vettore di JD con risoluzione fineTimestepMinutes

     */     */

    size_t getSegmentCount() const { return segments_.size(); }    std::vector<JulianDate> generateFineTimesteps() const;

        

    /**    /**

     * @brief Valuta l'errore massimo dell'approssimazione     * Statistiche cache

     * @param ephemeris Ephemeris originale per confronto     */

     * @param numTestPoints Numero di punti di test    int segmentCount() const { return m_segments.size(); }

     * @return Errore massimo in arcsec    double maxError() const { return m_maxError; }

     */    double avgError() const { return m_avgError; }

    double estimateMaxError(const Ephemeris& ephemeris, int numTestPoints = 100) const;    bool isInitialized() const { return m_initialized; }

        

private:    /**

    ChebyshevConfig config_;     * Configurazione

    std::vector<ChebyshevSegment> segments_;     */

        const ChebyshevConfig& config() const { return m_config; }

    /**    

     * @brief Genera un singolo segmentoprivate:

     * @param ephemeris Oggetto Ephemeris    ChebyshevConfig m_config;

     * @param startJD JD inizio segmento    std::vector<std::shared_ptr<ChebyshevSegment>> m_segments;

     * @param endJD JD fine segmento    JulianDate m_startJD, m_endJD;

     * @return Segmento generato    bool m_initialized;

     */    double m_maxError, m_avgError;

    ChebyshevSegment generateSegment(const Ephemeris& ephemeris,     

                                     double startJD, double endJD);    /**

         * Trova segmento contenente tempo t

    /**     * Usa binary search per O(log n)

     * @brief Calcola i coefficienti di Chebyshev per una serie di valori     */

     * @param values Valori della funzione in punti Chebyshev    std::shared_ptr<ChebyshevSegment> findSegment(const JulianDate& t) const;

     * @return Coefficienti di Chebyshev};

     */

    std::vector<double> computeChebyshevCoefficients(const std::vector<double>& values) const;/**

     * Helper per fitting coefficienti Chebyshev

    /** * Usa trasformata discreta di Chebyshev

     * @brief Valuta un polinomio di Chebyshev */

     * @param coeffs Coefficienti del polinomioclass ChebyshevFitter {

     * @param x Valore normalizzato in [-1, 1]public:

     * @return Valore del polinomio    /**

     */     * Calcola coefficienti Chebyshev dato set di punti campionati

    double evaluateChebyshevPolynomial(const std::vector<double>& coeffs, double x) const;     * 

         * @param order Ordine polinomio

    /**     * @param t Vettore di tempi normalizzati in [-1, 1]

     * @brief Valuta la derivata di un polinomio di Chebyshev     * @param y Vettore di valori campionati

     * @param coeffs Coefficienti del polinomio     * @return Coefficienti c₀, c₁, ..., cₙ

     * @param x Valore normalizzato in [-1, 1]     */

     * @return Valore della derivata    static std::vector<double> fit(int order, 

     */                                   const std::vector<double>& t,

    double evaluateChebyshevDerivative(const std::vector<double>& coeffs, double x) const;                                   const std::vector<double>& y);

        

    /**    /**

     * @brief Normalizza un JD all'intervallo [-1, 1] per un segmento     * Calcola punti di campionamento Chebyshev ottimali in [-1, 1]

     * @param jd Julian Date     * Usa nodi: xᵢ = cos(π(2i+1)/(2n+2)) per i=0..n

     * @param segment Segmento di riferimento     */

     * @return Valore normalizzato    static std::vector<double> chebyshevNodes(int order);

     */    

    double normalizeTime(double jd, const ChebyshevSegment& segment) const;    /**

};     * Valuta errore approssimazione confrontando con punti test

     */

} // namespace ioccultcalc    static double evaluateError(const std::vector<double>& coeffs,

                                const std::vector<double>& t_test,

#endif // IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H                                const std::vector<double>& y_test);

};

} // namespace ioccultcalc

#endif // IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H
