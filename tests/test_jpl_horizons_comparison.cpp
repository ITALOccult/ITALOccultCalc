#include <iostream>
#include <iomanip>
#include <cmath>
#include "ioccultcalc/occultation_engine.h"
#include "ioccultcalc/time_utils.h"
#include "astdyn_wrapper.h"

using namespace ioccultcalc;

int main() {
    try {
        std::cout << "=== JPL HORIZONS VS ASTDYS COMPARISON TEST (NUMERICAL) ===\n\n";

        AstDynWrapper wrapper(PropagationSettings::highAccuracy());

        // Target Date: 2026-01-13 05:30:00 UTC (Around the event)
        double target_mjd = 61053.2291667; 
        
        std::cout << "[STEP 1] Propagating ASTDYS Elements (1272.eq1)...\n";
        if (!wrapper.loadFromEQ1File("1272.eq1")) {
            std::cerr << "Failed to load 1272.eq1\n";
            return 1;
        }
        
        auto state_astdys = wrapper.propagateToEpoch(target_mjd);
        std::cout << "  AstDyS Position: [" << state_astdys.position.transpose() << "] AU\n";
        std::cout << "  AstDyS Velocity: [" << state_astdys.velocity.transpose() << "] AU/day\n";

        std::cout << "\n[STEP 2] Propagating JPL Elements (1272_jpl.eq1)...\n";
        if (!wrapper.loadFromEQ1File("1272_jpl.eq1")) {
            std::cerr << "Failed to load 1272_jpl.eq1\n";
            return 1;
        }

        auto state_jpl = wrapper.propagateToEpoch(target_mjd);
        std::cout << "  JPL Position: [" << state_jpl.position.transpose() << "] AU\n";
        std::cout << "  JPL Velocity: [" << state_jpl.velocity.transpose() << "] AU/day\n";

        // 3. Difference Calculation
        auto delta_pos = state_astdys.position - state_jpl.position;
        auto delta_vel = state_astdys.velocity - state_jpl.velocity;
        
        double dist_diff_km = delta_pos.norm() * 149597870.7;
        double vel_diff_mps = (delta_vel.norm() * 149597870.7) / 86400.0;

        std::cout << "\n=== COMPARISON RESULTS ===\n";
        std::cout << std::fixed << std::setprecision(8);
        std::cout << "Position Difference (AU):   " << delta_pos.transpose() << "\n";
        std::cout << "Diagonal Distance (km):     " << dist_diff_km << " km\n";
        std::cout << "Velocity Difference (m/s):  " << vel_diff_mps << " m/s\n";
        
        // Approximate angular difference (at ~2 AU)
        double angular_diff_mas = (dist_diff_km / (2.0 * 149597870.7)) * 206265.0 * 1000.0;
        std::cout << "Approx. Angular Diff (mas): " << angular_diff_mas << " mas\n";

        std::cout << "\nTest completed successfully.\n";

    } catch (const std::exception& e) {
        std::cerr << "Exception: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
