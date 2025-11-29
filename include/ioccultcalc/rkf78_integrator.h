/**
 * @file rkf78_integrator.h
 * @brief Runge-Kutta-Fehlberg 7(8) adaptive integrator
 * 
 * High-precision adaptive step integrator for long-term orbital propagation.
 * Based on ITALOccultLibrary implementation by Michele Bigi.
 * 
 * Reference: Fehlberg (1968) NASA TR R-287
 */

#ifndef IOCCULTCALC_RKF78_INTEGRATOR_H
#define IOCCULTCALC_RKF78_INTEGRATOR_H

#include <functional>
#include <vector>
#include <cmath>
#include <stdexcept>

namespace ioccultcalc {

/**
 * @brief State vector [x, y, z, vx, vy, vz] in AU and AU/day
 */
struct StateVector {
    double x, y, z;       // Position [AU]
    double vx, vy, vz;    // Velocity [AU/day]
    
    StateVector() : x(0), y(0), z(0), vx(0), vy(0), vz(0) {}
    StateVector(double x_, double y_, double z_, double vx_, double vy_, double vz_)
        : x(x_), y(y_), z(z_), vx(vx_), vy(vy_), vz(vz_) {}
    
    double norm() const {
        return std::sqrt(x*x + y*y + z*z + vx*vx + vy*vy + vz*vz);
    }
    
    StateVector operator+(const StateVector& other) const {
        return StateVector(x + other.x, y + other.y, z + other.z,
                          vx + other.vx, vy + other.vy, vz + other.vz);
    }
    
    StateVector operator-(const StateVector& other) const {
        return StateVector(x - other.x, y - other.y, z - other.z,
                          vx - other.vx, vy - other.vy, vz - other.vz);
    }
    
    StateVector operator*(double scalar) const {
        return StateVector(x * scalar, y * scalar, z * scalar,
                          vx * scalar, vy * scalar, vz * scalar);
    }
    
    StateVector& operator+=(const StateVector& other) {
        x += other.x; y += other.y; z += other.z;
        vx += other.vx; vy += other.vy; vz += other.vz;
        return *this;
    }
};

/**
 * @brief Derivative function signature: dY/dt = f(t, Y)
 * 
 * @param t Time (MJD TDB)
 * @param y State vector [x, y, z, vx, vy, vz]
 * @return Derivative [vx, vy, vz, ax, ay, az]
 */
using DerivativeFunction = std::function<StateVector(double t, const StateVector& y)>;

/**
 * @brief Integration statistics
 */
struct IntegrationStatistics {
    int num_steps = 0;
    int num_function_evals = 0;
    int num_rejected_steps = 0;
    double min_step_size = 0.0;
    double max_step_size = 0.0;
    double final_time = 0.0;
    
    void reset() {
        num_steps = 0;
        num_function_evals = 0;
        num_rejected_steps = 0;
        min_step_size = 0.0;
        max_step_size = 0.0;
        final_time = 0.0;
    }
};

/**
 * @brief Runge-Kutta-Fehlberg 7(8) adaptive integrator
 * 
 * High-precision integrator with automatic step size control.
 * Uses 7th/8th order formulas to estimate local truncation error.
 * 
 * Features:
 * - Adaptive step size (adjusts based on error estimate)
 * - Bidirectional integration (forward/backward in time)
 * - Error control (relative tolerance, default 1e-12)
 * - Safety limits (min/max step size)
 * 
 * Typical usage:
 * @code
 *   RKF78Integrator integrator(0.1, 1e-12);  // initial_step=0.1 days, tol=1e-12
 *   
 *   auto derivatives = [](double t, const StateVector& y) {
 *       // Compute accelerations from forces
 *       return StateVector(y.vx, y.vy, y.vz, ax, ay, az);
 *   };
 *   
 *   StateVector y0 = ...; // Initial state
 *   StateVector yf = integrator.integrate(derivatives, y0, t0, tf);
 * @endcode
 */
class RKF78Integrator {
public:
    /**
     * @brief Construct RKF78 integrator
     * 
     * @param initial_step Initial step size guess [days]
     * @param tolerance Relative error tolerance (default 1e-12)
     * @param min_step Minimum step size [days] (default 1e-6 ~= 0.1 seconds)
     * @param max_step Maximum step size [days] (default 100.0)
     */
    explicit RKF78Integrator(double initial_step,
                            double tolerance = 1e-12,
                            double min_step = 1e-6,
                            double max_step = 100.0);
    
    /**
     * @brief Integrate from t0 to tf
     * 
     * Automatically handles forward (tf > t0) or backward (tf < t0) integration.
     * Step size adapts to maintain specified tolerance.
     * 
     * @param f Derivative function dY/dt = f(t, Y)
     * @param y0 Initial state at t0
     * @param t0 Initial time [MJD TDB]
     * @param tf Final time [MJD TDB]
     * @return Final state at tf
     * @throws std::runtime_error if integration fails
     */
    StateVector integrate(const DerivativeFunction& f,
                         const StateVector& y0,
                         double t0,
                         double tf);
    
    /**
     * @brief Get statistics from last integration
     */
    const IntegrationStatistics& statistics() const { return stats_; }
    
    /**
     * @brief Reset statistics counters
     */
    void reset_statistics() { stats_.reset(); }
    
    /**
     * @brief Update tolerance
     */
    void set_tolerance(double tol) { tolerance_ = tol; }
    double tolerance() const { return tolerance_; }
    
    /**
     * @brief Update step size limits
     */
    void set_min_step(double h_min) { h_min_ = h_min; }
    void set_max_step(double h_max) { h_max_ = h_max; }
    
private:
    /**
     * @brief Adaptive RKF78 step with error control
     * 
     * Attempts one integration step. If error exceeds tolerance,
     * step is rejected and h is reduced. Otherwise step is accepted,
     * t and y are updated, and h is increased for next step.
     * 
     * @param f Derivative function
     * @param t Current time [in/out]
     * @param y Current state [in/out]
     * @param h Step size [in/out]
     * @param t_target Don't overshoot this time
     * @return true if step accepted, false if rejected
     */
    bool adaptive_step(const DerivativeFunction& f,
                      double& t,
                      StateVector& y,
                      double& h,
                      double t_target);
    
    double h_initial_;  // Initial step size
    double tolerance_;  // Relative error tolerance
    double h_min_;      // Minimum step size
    double h_max_;      // Maximum step size
    
    IntegrationStatistics stats_;
    
    // RKF78 Butcher tableau coefficients (Fehlberg 1968)
    static const double c_[13];      // Time nodes
    static const double a_[13][12];  // Matrix A
    static const double b7_[13];     // 7th order weights
    static const double b8_[13];     // 8th order weights
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_RKF78_INTEGRATOR_H
