/**
 * @file rkf78_integrator.cpp
 * @brief Implementation of RKF78 adaptive integrator
 * 
 * Based on ITALOccultLibrary by Michele Bigi
 * Reference: Fehlberg (1968) NASA TR R-287
 */

#include "ioccultcalc/rkf78_integrator.h"
#include <algorithm>
#include <sstream>

namespace ioccultcalc {

// ============================================================================
// Fehlberg 7(8) Butcher Tableau Coefficients
// ============================================================================

const double RKF78Integrator::c_[13] = {
    0.0,
    2.0/27.0,
    1.0/9.0,
    1.0/6.0,
    5.0/12.0,
    0.5,
    5.0/6.0,
    1.0/6.0,
    2.0/3.0,
    1.0/3.0,
    1.0,
    0.0,
    1.0
};

const double RKF78Integrator::a_[13][12] = {
    {},
    {2.0/27.0},
    {1.0/36.0, 1.0/12.0},
    {1.0/24.0, 0.0, 1.0/8.0},
    {5.0/12.0, 0.0, -25.0/16.0, 25.0/16.0},
    {1.0/20.0, 0.0, 0.0, 1.0/4.0, 1.0/5.0},
    {-25.0/108.0, 0.0, 0.0, 125.0/108.0, -65.0/27.0, 125.0/54.0},
    {31.0/300.0, 0.0, 0.0, 0.0, 61.0/225.0, -2.0/9.0, 13.0/900.0},
    {2.0, 0.0, 0.0, -53.0/6.0, 704.0/45.0, -107.0/9.0, 67.0/90.0, 3.0},
    {-91.0/108.0, 0.0, 0.0, 23.0/108.0, -976.0/135.0, 311.0/54.0, -19.0/60.0, 17.0/6.0, -1.0/12.0},
    {2383.0/4100.0, 0.0, 0.0, -341.0/164.0, 4496.0/1025.0, -301.0/82.0, 2133.0/4100.0, 45.0/82.0, 45.0/164.0, 18.0/41.0},
    {3.0/205.0, 0.0, 0.0, 0.0, 0.0, -6.0/41.0, -3.0/205.0, -3.0/41.0, 3.0/41.0, 6.0/41.0, 0.0},
    {-1777.0/4100.0, 0.0, 0.0, -341.0/164.0, 4496.0/1025.0, -289.0/82.0, 2193.0/4100.0, 51.0/82.0, 33.0/164.0, 12.0/41.0, 0.0, 1.0}
};

const double RKF78Integrator::b7_[13] = {
    41.0/840.0, 0.0, 0.0, 0.0, 0.0, 34.0/105.0, 9.0/35.0,
    9.0/35.0, 9.0/280.0, 9.0/280.0, 41.0/840.0, 0.0, 0.0
};

const double RKF78Integrator::b8_[13] = {
    0.0, 0.0, 0.0, 0.0, 0.0, 34.0/105.0, 9.0/35.0,
    9.0/35.0, 9.0/280.0, 9.0/280.0, 0.0, 41.0/840.0, 41.0/840.0
};

// ============================================================================
// RKF78Integrator Implementation
// ============================================================================

RKF78Integrator::RKF78Integrator(double initial_step,
                                 double tolerance,
                                 double min_step,
                                 double max_step)
    : h_initial_(initial_step),
      tolerance_(tolerance),
      h_min_(min_step),
      h_max_(max_step) {
    if (h_initial_ <= 0.0) {
        throw std::invalid_argument("RKF78: Initial step size must be positive");
    }
    if (tolerance_ <= 0.0) {
        throw std::invalid_argument("RKF78: Tolerance must be positive");
    }
}

bool RKF78Integrator::adaptive_step(const DerivativeFunction& f,
                                    double& t,
                                    StateVector& y,
                                    double& h,
                                    double t_target) {
    std::vector<StateVector> k(13);
    
    // Determine direction (forward or backward)
    double direction = (h >= 0.0) ? 1.0 : -1.0;
    
    // Compute 13 stages (k0 through k12)
    k[0] = f(t, y);
    stats_.num_function_evals++;
    
    for (int i = 1; i < 13; ++i) {
        StateVector y_temp = y;
        for (int j = 0; j < i; ++j) {
            y_temp += k[j] * (h * a_[i][j]);
        }
        k[i] = f(t + c_[i] * h, y_temp);
        stats_.num_function_evals++;
    }
    
    // 7th order solution
    StateVector y7 = y;
    for (int i = 0; i < 13; ++i) {
        y7 += k[i] * (h * b7_[i]);
    }
    
    // 8th order solution
    StateVector y8 = y;
    for (int i = 0; i < 13; ++i) {
        y8 += k[i] * (h * b8_[i]);
    }
    
    // Error estimate: |y8 - y7|
    StateVector error = y8 - y7;
    double error_norm = error.norm();
    double y_norm = y.norm();
    
    // Scale by state magnitude (avoid division by zero)
    double scale = (y_norm > 1.0) ? y_norm : 1.0;
    double relative_error = error_norm / scale;
    
    // Step size control factor (conservative)
    double safety_factor = 0.9;
    double h_new_abs = std::abs(h);
    
    if (relative_error > tolerance_) {
        // REJECT STEP: error too large, reduce step size
        h_new_abs = safety_factor * std::abs(h) * std::pow(tolerance_ / relative_error, 1.0 / 8.0);
        h_new_abs = std::max(h_new_abs, h_min_);
        h = direction * h_new_abs;  // Preserve direction
        stats_.num_rejected_steps++;
        return false;
    } else {
        // ACCEPT STEP: use higher-order solution (8th order)
        y = y8;
        t += h;
        stats_.num_steps++;
        
        // Increase step size for next iteration
        if (relative_error > 0.0) {
            h_new_abs = safety_factor * std::abs(h) * std::pow(tolerance_ / relative_error, 1.0 / 8.0);
        } else {
            // Error extremely small, use max step
            h_new_abs = h_max_;
        }
        h_new_abs = std::min(h_new_abs, h_max_);
        h_new_abs = std::max(h_new_abs, h_min_);
        
        // Update statistics
        stats_.min_step_size = (stats_.num_steps == 1) ? std::abs(h) : std::min(stats_.min_step_size, std::abs(h));
        stats_.max_step_size = std::max(stats_.max_step_size, std::abs(h));
        
        h = direction * h_new_abs;  // Preserve direction for next step
        return true;
    }
}

StateVector RKF78Integrator::integrate(const DerivativeFunction& f,
                                       const StateVector& y0,
                                       double t0,
                                       double tf) {
    stats_.reset();
    stats_.min_step_size = std::abs(h_initial_);
    stats_.max_step_size = std::abs(h_initial_);
    
    double t = t0;
    StateVector y = y0;
    
    // Determine direction: forward (tf > t0) or backward (tf < t0)
    double direction = (tf > t0) ? 1.0 : -1.0;
    double h = direction * std::abs(h_initial_);
    
    // Safety limits to prevent infinite loops
    const int max_iterations = 1000000;  // 1 million iterations
    int iteration_count = 0;
    int consecutive_rejections = 0;
    
    while (std::abs(tf - t) > 1e-14) {
        // Check iteration limit
        if (++iteration_count > max_iterations) {
            std::ostringstream oss;
            oss << "RKF78Integrator: Maximum iterations (" << max_iterations << ") exceeded.\n"
                << "  Integration from t=" << t0 << " to t=" << tf << " failed at t=" << t << "\n"
                << "  Steps: " << stats_.num_steps 
                << ", Rejections: " << stats_.num_rejected_steps
                << ", Current h: " << h;
            throw std::runtime_error(oss.str());
        }
        
        // Don't overshoot target time
        if (std::abs(tf - t) < std::abs(h)) {
            h = tf - t;
        }
        
        // Attempt integration step (may be rejected)
        bool accepted = adaptive_step(f, t, y, h, tf);
        
        if (accepted) {
            consecutive_rejections = 0;
        } else {
            consecutive_rejections++;
            
            // Too many consecutive rejections indicates a problem
            if (consecutive_rejections >= 1000) {
                std::ostringstream oss;
                oss << "RKF78Integrator: Too many consecutive step rejections (" 
                    << consecutive_rejections << ").\n"
                    << "  Integration may be stuck. Current t=" << t 
                    << ", h=" << h << ", h_min=" << h_min_;
                throw std::runtime_error(oss.str());
            }
        }
        
        // If rejected: t and y unchanged, h reduced, loop retries
        // If accepted: t and y updated, h adjusted for next step
    }
    
    stats_.final_time = t;
    return y;
}

} // namespace ioccultcalc
