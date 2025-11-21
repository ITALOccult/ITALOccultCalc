/**
 * @file ra15_integrator.hpp
 * @brief Implementazione integratore RA15 (Radau 15° ordine di Everhart)
 * 
 * Basato su OrbFit 5.0.8 (Andrea Milani et al., Università di Pisa)
 * File originale: src/propag/ra15_mod.f90
 * 
 * Riferimento:
 * Everhart, E. (1985). "An efficient integrator that uses Gauss-Radau spacings".
 * IAU Colloq. 83: Dynamics of Comets, pp. 185-202.
 */

#ifndef IOCCULTCALC_RA15_INTEGRATOR_HPP
#define IOCCULTCALC_RA15_INTEGRATOR_HPP

#include "ioccultcalc/time_utils.h"
#include "ioccultcalc/types.h"
#include <array>
#include <functional>

namespace ioccultcalc {

// Forward declaration (OrbitState è definito in orbit_propagator.h)
struct OrbitState;
struct JulianDate;

/**
 * @brief Opzioni per integratore RA15
 */
struct RA15Options {
    int llev = 10;              ///< Sequence level: ss = 10^(-llev), tipico 6-12
    double h_init = 0.1;        ///< Stepsize iniziale [giorni]
    double eprk = 1e-12;        ///< Tolleranza convergenza iterazioni
    int lit1 = 10;              ///< Max iterazioni primo step
    int lit2 = 4;               ///< Max iterazioni step successivi
    bool fixed_step = false;    ///< Se true, usa stepsize fisso h_init
    int max_steps = 100000;     ///< Numero massimo step (protezione loop infiniti)
    bool verbose = false;       ///< Output diagnostico
};

/**
 * @brief Integratore RA15 di Everhart (Radau 15° ordine)
 * 
 * Integratore implicito di ordine 15 specificamente progettato per
 * problemi di meccanica celeste a lungo termine.
 * 
 * Caratteristiche:
 * - Ordine 15 (vs ordine 4 di RK4)
 * - Metodo implicito con punti Gauss-Radau
 * - Step size adattivo con controllo errore
 * - Extrapolazione coefficienti tra step
 * 
 * Usato da OrbFit/NEODyS/AstDyS per propagazione orbite asteroidali.
 * 
 * Esempio:
 * @code
 * RA15Integrator integrator;
 * auto accel_func = [&](const JulianDate& t, const Vector3D& pos, const Vector3D& vel) {
 *     return computeAcceleration(t, pos, vel);
 * };
 * 
 * OrbitState state0{pos0, vel0, epoch0};
 * OrbitState final = integrator.integrate(state0, target_epoch, accel_func);
 * @endcode
 */
class RA15Integrator {
public:
    /**
     * @brief Tipo funzione per calcolo accelerazione
     * 
     * @param epoch Epoca corrente [JD]
     * @param position Posizione [AU]
     * @param velocity Velocità [AU/day]
     * @return Accelerazione [AU/day²]
     */
    using AccelerationFunction = std::function<Vector3D(
        const JulianDate& epoch,
        const Vector3D& position,
        const Vector3D& velocity
    )>;

    /**
     * @brief Costruttore con opzioni
     * @param options Opzioni integratore
     */
    explicit RA15Integrator(const RA15Options& options = RA15Options{});

    /**
     * @brief Integra da stato iniziale a epoca finale
     * 
     * @param initial_state Stato iniziale (posizione, velocità, epoca)
     * @param target_epoch Epoca finale [JD]
     * @param accel_func Funzione per calcolo accelerazione
     * @return Stato finale all'epoca target
     * 
     * @throws std::runtime_error se integrazione fallisce
     */
    OrbitState integrate(
        const OrbitState& initial_state,
        const JulianDate& target_epoch,
        AccelerationFunction accel_func
    );

    /**
     * @brief Ottieni statistiche ultima integrazione
     * @return Struct con numero step, valutazioni, etc.
     */
    struct Statistics {
        int num_steps = 0;           ///< Numero step eseguiti
        int num_evals = 0;           ///< Numero valutazioni accelerazione
        int num_rejections = 0;      ///< Numero step rifiutati
        double final_stepsize = 0.0; ///< Stepsize finale [giorni]
        double avg_iterations = 0.0; ///< Media iterazioni per step
    };
    
    Statistics getStatistics() const { return stats_; }

private:
    // Costanti Gauss-Radau (calcolate una sola volta)
    std::array<double, 8> h_;   ///< Spaziature punti Gauss-Radau
    std::array<double, 7> w_;   ///< Pesi integrazione
    std::array<double, 7> u_;   ///< Coefficienti ausiliari
    std::array<double, 21> c_;  ///< Coefficienti trasformazione
    std::array<double, 21> d_;  ///< Coefficienti derivate
    std::array<double, 21> r_;  ///< Coefficienti ricorrenza

    // Storage arrays per coefficienti B, G, E (7 livelli × 6 componenti)
    std::array<std::array<double, 6>, 7> b_;   ///< Coefficienti principali
    std::array<std::array<double, 6>, 7> g_;   ///< Coefficienti ausiliari
    std::array<std::array<double, 6>, 7> e_;   ///< Coefficienti extrapolati
    std::array<std::array<double, 6>, 7> bd_;  ///< Derivate coefficienti B

    RA15Options options_;  ///< Opzioni configurazione
    Statistics stats_;     ///< Statistiche ultima integrazione
    
    AccelerationFunction accel_func_;  ///< Funzione accelerazione corrente

    /**
     * @brief Calcola costanti Gauss-Radau
     * 
     * Inizializza array h_, w_, u_, c_, d_, r_ con valori specifici
     * del metodo Radau a 8 punti.
     */
    void computeRadauConstants();

    /**
     * @brief Calcola array G da array B
     * @param b Coefficienti B correnti
     * @param g Output: coefficienti G
     */
    void computeG(
        const std::array<std::array<double, 6>, 7>& b,
        std::array<std::array<double, 6>, 7>& g
    );

    /**
     * @brief Esegue un'iterazione del metodo implicito
     * 
     * @param iter Numero iterazione corrente (1-based)
     * @param t Stepsize corrente [giorni]
     * @param tm Tempo dall'inizio integrazione [giorni]
     * @param tini Epoca iniziale [JD]
     * @param x Posizione corrente [AU] (input/output)
     * @param v Velocità corrente [AU/day] (input/output)
     * @param f1 Accelerazione inizio step [AU/day²]
     * @return Parametro di controllo convergenza epsilon
     */
    double iterate(
        int iter,
        double t,
        double tm,
        const JulianDate& tini,
        std::array<double, 6>& x_and_v,
        const std::array<double, 6>& f1
    );

    /**
     * @brief Predice stato finale dello step
     * 
     * @param x Posizione [AU] (input/output)
     * @param v Velocità [AU/day] (input/output)
     * @param t Stepsize [giorni]
     * @param f1 Accelerazione inizio step [AU/day²]
     */
    void predict(
        std::array<double, 6>& x_and_v,
        double t,
        const std::array<double, 6>& f1
    );

    /**
     * @brief Estrapola coefficienti B per prossimo step
     * 
     * @param q Rapporto step_new / step_old
     * @param ns Numero step eseguiti
     */
    void extrapolate(double q, int ns);

    /**
     * @brief Calcola nuovo stepsize adattivo
     * 
     * @param t Stepsize corrente [giorni]
     * @param dir Direzione tempo (+1 o -1)
     * @return Nuovo stepsize proposto [giorni]
     */
    double computeNewStepsize(double t, double dir);

    /**
     * @brief Resetta storage arrays a zero
     */
    void resetArrays();
    
    /**
     * @brief Calcola indice per matrice triangolare
     * @param j Riga (1-based)
     * @param l Colonna (0-based)
     * @return Indice lineare
     */
    inline int getIndex(int j, int l) const;
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_RA15_INTEGRATOR_HPP
