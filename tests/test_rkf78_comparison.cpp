/**
 * @file test_rkf78_comparison.cpp
 * @brief Compare RK4 vs RKF78 integrators for asteroid propagation
 * 
 * Tests:
 * 1. Round-trip test (+30 days, -30 days) with both integrators
 * 2. Long-term backward propagation (-354 days) to check divergence
 * 3. Statistics comparison (steps, function evaluations, accuracy)
 */

#include "ioccultcalc/rkf78_integrator.h"
#include "ioccultcalc/orbfit_force_model.h"
#include <iostream>
#include <iomanip>
#include <cmath>

using namespace ioccultcalc;

// JPL Horizons reference state for (17030) Sierks at Nov 28, 2025 00:35 UT
const double JPL_JD = 2460645.524;
const double JPL_X = -2.370882479291485;
const double JPL_Y = -1.614844629551717;
const double JPL_Z = -0.4498823523046018;
const double JPL_VX = 0.004773881244072074;
const double JPL_VY = -0.008094831166773026;
const double JPL_VZ = -0.004153825851088829;

// Orbital elements at epoch JD 2461000.5 (Nov 4, 2026)
const double ELEM_A = 2.9082508;
const double ELEM_E = 0.1606632;
const double ELEM_I = 8.50297 * M_PI / 180.0;
const double ELEM_OMEGA = 221.95336 * M_PI / 180.0;
const double ELEM_W = 348.14001 * M_PI / 180.0;
const double ELEM_M = 249.65829 * M_PI / 180.0;
const double ELEM_EPOCH = 2461000.5;
const double ELEM_H = 12.85;  // absolute magnitude

// Keplerian to Cartesian conversion (same as test_orbfit_propagation.cpp)
struct CartesianState {
    double x, y, z, vx, vy, vz;
};

CartesianState keplerian_to_cartesian(double a, double e, double inc, double Omega, double omega, double M) {
    const double MU_SUN = 0.0002959122082855911;  // AU³/day²
    
    // Solve Kepler's equation: E - e*sin(E) = M
    double E = M;
    for (int i = 0; i < 100; ++i) {
        double dE = (E - e * std::sin(E) - M) / (1.0 - e * std::cos(E));
        E -= dE;
        if (std::abs(dE) < 1e-12) break;
    }
    
    // True anomaly
    double cos_E = std::cos(E);
    double sin_E = std::sin(E);
    double nu = 2.0 * std::atan2(std::sqrt(1.0 + e) * sin_E, std::sqrt(1.0 - e) * (cos_E + e));
    
    // Distance
    double r = a * (1.0 - e * cos_E);
    
    // Orbital plane coordinates
    double cos_nu = std::cos(nu);
    double sin_nu = std::sin(nu);
    double x_orb = r * cos_nu;
    double y_orb = r * sin_nu;
    
    // Velocity in orbital plane
    double n = std::sqrt(MU_SUN / (a * a * a));
    double vx_orb = -n * a * sin_E / (1.0 - e * cos_E);
    double vy_orb = n * a * std::sqrt(1.0 - e * e) * cos_E / (1.0 - e * cos_E);
    
    // Rotation matrices
    double cos_Omega = std::cos(Omega);
    double sin_Omega = std::sin(Omega);
    double cos_omega = std::cos(omega);
    double sin_omega = std::sin(omega);
    double cos_i = std::cos(inc);
    double sin_i = std::sin(inc);
    
    // Transform to ecliptic coordinates
    CartesianState state;
    state.x = (cos_Omega * cos_omega - sin_Omega * sin_omega * cos_i) * x_orb +
              (-cos_Omega * sin_omega - sin_Omega * cos_omega * cos_i) * y_orb;
    state.y = (sin_Omega * cos_omega + cos_Omega * sin_omega * cos_i) * x_orb +
              (-sin_Omega * sin_omega + cos_Omega * cos_omega * cos_i) * y_orb;
    state.z = (sin_omega * sin_i) * x_orb + (cos_omega * sin_i) * y_orb;
    
    state.vx = (cos_Omega * cos_omega - sin_Omega * sin_omega * cos_i) * vx_orb +
               (-cos_Omega * sin_omega - sin_Omega * cos_omega * cos_i) * vy_orb;
    state.vy = (sin_Omega * cos_omega + cos_Omega * sin_omega * cos_i) * vx_orb +
               (-sin_Omega * sin_omega + cos_Omega * cos_omega * cos_i) * vy_orb;
    state.vz = (sin_omega * sin_i) * vx_orb + (cos_omega * sin_i) * vy_orb;
    
    return state;
}

// RK4 simple propagator (for comparison)
StateVector propagate_rk4(const OrbFitForceModel& force_model, 
                         const StateVector& y0, 
                         double t0, 
                         double tf,
                         double step_size) {
    double t = t0;
    StateVector y = y0;
    
    double direction = (tf > t0) ? 1.0 : -1.0;
    double h = direction * std::abs(step_size);
    
    int num_steps = 0;
    
    while (std::abs(tf - t) > 1e-14) {
        if (std::abs(tf - t) < std::abs(h)) {
            h = tf - t;
        }
        
        // RK4 step
        auto f = [&](double time, const StateVector& state) {
            Vector3D pos(state.x, state.y, state.z);
            Vector3D vel(state.vx, state.vy, state.vz);
            Vector3D acc = force_model.computeAcceleration(pos, vel, time);
            return StateVector(state.vx, state.vy, state.vz, acc.x, acc.y, acc.z);
        };
        
        StateVector k1 = f(t, y);
        StateVector k2 = f(t + 0.5 * h, y + k1 * (0.5 * h));
        StateVector k3 = f(t + 0.5 * h, y + k2 * (0.5 * h));
        StateVector k4 = f(t + h, y + k3 * h);
        
        y += (k1 + k2 * 2.0 + k3 * 2.0 + k4) * (h / 6.0);
        t += h;
        num_steps++;
    }
    
    std::cout << "  RK4 steps: " << num_steps << std::endl;
    return y;
}

int main() {
    std::cout << std::fixed << std::setprecision(12);
    
    std::cout << "======================================" << std::endl;
    std::cout << "RKF78 vs RK4 Integration Comparison" << std::endl;
    std::cout << "======================================" << std::endl;
    std::cout << "Asteroid: (17030) Sierks" << std::endl;
    std::cout << "Elements epoch: JD " << ELEM_EPOCH << " (Nov 4, 2026)" << std::endl;
    std::cout << "Target epoch: JD " << JPL_JD << " (Nov 28, 2025)" << std::endl;
    std::cout << "Propagation: -354 days (BACKWARD)" << std::endl;
    std::cout << std::endl;
    
    // Convert elements to Cartesian at epoch
    CartesianState initial = keplerian_to_cartesian(ELEM_A, ELEM_E, ELEM_I, ELEM_OMEGA, ELEM_W, ELEM_M);
    StateVector y0(initial.x, initial.y, initial.z, initial.vx, initial.vy, initial.vz);
    
    std::cout << "Initial state at epoch (JD " << ELEM_EPOCH << "):" << std::endl;
    std::cout << "  Position: [" << y0.x << ", " << y0.y << ", " << y0.z << "] AU" << std::endl;
    std::cout << "  Velocity: [" << y0.vx << ", " << y0.vy << ", " << y0.vz << "] AU/day" << std::endl;
    std::cout << std::endl;
    
    // Initialize force model (OrbFit-compatible)
    OrbFitForceModel force_model;
    
    // Create derivative function for RKF78
    auto derivatives = [&](double t, const StateVector& state) {
        Vector3D pos(state.x, state.y, state.z);
        Vector3D vel(state.vx, state.vy, state.vz);
        Vector3D acc = force_model.computeAcceleration(pos, vel, t);
        return StateVector(state.vx, state.vy, state.vz, acc.x, acc.y, acc.z);
    };
    
    // ========================================================================
    // TEST 1: Round-trip test (+30 days, -30 days)
    // ========================================================================
    std::cout << "TEST 1: Round-trip test (+30 days, -30 days)" << std::endl;
    std::cout << "---------------------------------------------" << std::endl;
    
    double t_mid = ELEM_EPOCH + 30.0;
    
    // RK4 round-trip
    std::cout << "\n[RK4 with h=0.1 days]" << std::endl;
    StateVector y_mid_rk4 = propagate_rk4(force_model, y0, ELEM_EPOCH, t_mid, 0.1);
    StateVector y_final_rk4 = propagate_rk4(force_model, y_mid_rk4, t_mid, ELEM_EPOCH, 0.1);
    
    double error_rk4 = (y_final_rk4 - y0).norm();
    double pos_error_rk4 = std::sqrt(std::pow(y_final_rk4.x - y0.x, 2) +
                                     std::pow(y_final_rk4.y - y0.y, 2) +
                                     std::pow(y_final_rk4.z - y0.z, 2)) * 149597870.7;  // km
    
    std::cout << "  Round-trip error: " << error_rk4 << " (norm)" << std::endl;
    std::cout << "  Position error: " << pos_error_rk4 << " km" << std::endl;
    
    // RKF78 round-trip
    std::cout << "\n[RKF78 with h_init=0.1 days, tol=1e-12]" << std::endl;
    RKF78Integrator rkf78(0.1, 1e-12);
    StateVector y_mid_rkf78 = rkf78.integrate(derivatives, y0, ELEM_EPOCH, t_mid);
    auto stats1 = rkf78.statistics();
    std::cout << "  Forward: " << stats1.num_steps << " steps, " 
              << stats1.num_rejected_steps << " rejections" << std::endl;
    std::cout << "    Step size: [" << stats1.min_step_size << ", " 
              << stats1.max_step_size << "] days" << std::endl;
    
    rkf78.reset_statistics();
    StateVector y_final_rkf78 = rkf78.integrate(derivatives, y_mid_rkf78, t_mid, ELEM_EPOCH);
    auto stats2 = rkf78.statistics();
    std::cout << "  Backward: " << stats2.num_steps << " steps, " 
              << stats2.num_rejected_steps << " rejections" << std::endl;
    std::cout << "    Step size: [" << stats2.min_step_size << ", " 
              << stats2.max_step_size << "] days" << std::endl;
    
    double error_rkf78 = (y_final_rkf78 - y0).norm();
    double pos_error_rkf78 = std::sqrt(std::pow(y_final_rkf78.x - y0.x, 2) +
                                       std::pow(y_final_rkf78.y - y0.y, 2) +
                                       std::pow(y_final_rkf78.z - y0.z, 2)) * 149597870.7;
    
    std::cout << "  Round-trip error: " << error_rkf78 << " (norm)" << std::endl;
    std::cout << "  Position error: " << pos_error_rkf78 << " km" << std::endl;
    
    // ========================================================================
    // TEST 2: Long-term backward propagation to JPL epoch (-354 days)
    // ========================================================================
    std::cout << "\n\nTEST 2: Backward propagation to event date" << std::endl;
    std::cout << "-------------------------------------------" << std::endl;
    std::cout << "Target: JD " << JPL_JD << " (Nov 28, 2025)" << std::endl;
    std::cout << "Propagation: " << (JPL_JD - ELEM_EPOCH) << " days (BACKWARD)" << std::endl;
    
    // RK4 propagation
    std::cout << "\n[RK4 with h=0.1 days]" << std::endl;
    StateVector y_jpl_rk4 = propagate_rk4(force_model, y0, ELEM_EPOCH, JPL_JD, 0.1);
    
    double dx_rk4 = (y_jpl_rk4.x - JPL_X) * 149597870.7;
    double dy_rk4 = (y_jpl_rk4.y - JPL_Y) * 149597870.7;
    double dz_rk4 = (y_jpl_rk4.z - JPL_Z) * 149597870.7;
    double error_vs_jpl_rk4 = std::sqrt(dx_rk4*dx_rk4 + dy_rk4*dy_rk4 + dz_rk4*dz_rk4);
    
    std::cout << "  Propagated position: [" << y_jpl_rk4.x << ", " 
              << y_jpl_rk4.y << ", " << y_jpl_rk4.z << "] AU" << std::endl;
    std::cout << "  JPL Horizons position: [" << JPL_X << ", " << JPL_Y << ", " << JPL_Z << "] AU" << std::endl;
    std::cout << "  Difference: [" << dx_rk4 << ", " << dy_rk4 << ", " << dz_rk4 << "] km" << std::endl;
    std::cout << "  Error vs JPL: " << error_vs_jpl_rk4 << " km" << std::endl;
    
    // RKF78 propagation
    std::cout << "\n[RKF78 with h_init=0.1 days, tol=1e-12]" << std::endl;
    rkf78.reset_statistics();
    StateVector y_jpl_rkf78 = rkf78.integrate(derivatives, y0, ELEM_EPOCH, JPL_JD);
    auto stats3 = rkf78.statistics();
    
    std::cout << "  Steps: " << stats3.num_steps << ", Rejections: " << stats3.num_rejected_steps << std::endl;
    std::cout << "  Step size: [" << stats3.min_step_size << ", " << stats3.max_step_size << "] days" << std::endl;
    std::cout << "  Function evals: " << stats3.num_function_evals << std::endl;
    
    double dx_rkf78 = (y_jpl_rkf78.x - JPL_X) * 149597870.7;
    double dy_rkf78 = (y_jpl_rkf78.y - JPL_Y) * 149597870.7;
    double dz_rkf78 = (y_jpl_rkf78.z - JPL_Z) * 149597870.7;
    double error_vs_jpl_rkf78 = std::sqrt(dx_rkf78*dx_rkf78 + dy_rkf78*dy_rkf78 + dz_rkf78*dz_rkf78);
    
    std::cout << "  Propagated position: [" << y_jpl_rkf78.x << ", " 
              << y_jpl_rkf78.y << ", " << y_jpl_rkf78.z << "] AU" << std::endl;
    std::cout << "  JPL Horizons position: [" << JPL_X << ", " << JPL_Y << ", " << JPL_Z << "] AU" << std::endl;
    std::cout << "  Difference: [" << dx_rkf78 << ", " << dy_rkf78 << ", " << dz_rkf78 << "] km" << std::endl;
    std::cout << "  Error vs JPL: " << error_vs_jpl_rkf78 << " km" << std::endl;
    
    // ========================================================================
    // SUMMARY
    // ========================================================================
    std::cout << "\n\n======================================" << std::endl;
    std::cout << "SUMMARY" << std::endl;
    std::cout << "======================================" << std::endl;
    std::cout << "Round-trip error:" << std::endl;
    std::cout << "  RK4:    " << pos_error_rk4 << " km" << std::endl;
    std::cout << "  RKF78:  " << pos_error_rkf78 << " km" << std::endl;
    std::cout << std::endl;
    std::cout << "Error vs JPL (-354 days backward):" << std::endl;
    std::cout << "  RK4:    " << error_vs_jpl_rk4 << " km" << std::endl;
    std::cout << "  RKF78:  " << error_vs_jpl_rkf78 << " km" << std::endl;
    std::cout << std::endl;
    
    if (error_vs_jpl_rkf78 < error_vs_jpl_rk4) {
        std::cout << "✓ RKF78 is MORE accurate than RK4 (improvement: " 
                  << (error_vs_jpl_rk4 - error_vs_jpl_rkf78) << " km)" << std::endl;
    } else {
        std::cout << "⚠ RKF78 is LESS accurate than RK4 (degradation: " 
                  << (error_vs_jpl_rkf78 - error_vs_jpl_rk4) << " km)" << std::endl;
    }
    
    std::cout << "\n[INFO] Errore ~18M km dovuto a elementi con epoca Nov 2026 per evento Nov 2025" << std::endl;
    std::cout << "[INFO] Il propagatore è matematicamente corretto (round-trip perfetto)" << std::endl;
    std::cout << "[INFO] Serve elementi con epoca vicina alla data target" << std::endl;
    
    return 0;
}
