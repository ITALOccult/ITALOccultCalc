/**
 * @file test_propagation_vs_horizons.cpp
 * @brief Test propagazione -60 a +60 giorni vs JPL Horizons
 * 
 * Test completo:
 * - Usa elementi JPL a epoca centrale
 * - Propaga backward -60 giorni
 * - Propaga forward +60 giorni
 * - Confronta ogni 10 giorni con JPL Horizons
 * - Calcola RMS error
 */

#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/orbital_elements.h"
#include <iostream>
#include <iomanip>
#include <cmath>
#include <vector>

using namespace ioccultcalc;

// JPL Horizons data per (1) Ceres a diverse epoche
// Epoca centrale: JD 2460645.5 (Nov 28, 2025 00:00 UT)
struct HorizonsState {
    double jd;
    double x, y, z;      // AU heliocentric ecliptic J2000
    double vx, vy, vz;   // AU/day
};

// Dati JPL Horizons per Ceres ogni 10 giorni da -60 a +60 giorni
// Epoca centrale: 2460645.5 (Nov 28, 2025)
std::vector<HorizonsState> horizons_data = {
    // JD 2460585.5 (Sep 29, 2025) - 60 giorni prima
    {2460585.5, -2.545938478797629, -0.861179558315839, 0.474883655876448,
     0.003677467502883, -0.008915289158916, -0.004305842224016},
    
    // JD 2460595.5 (Oct 9, 2025) - 50 giorni prima
    {2460595.5, -2.509511544627891, -0.951119583950210, 0.431672859745363,
     0.003927768829656, -0.008770046044678, -0.004258540876717},
    
    // JD 2460605.5 (Oct 19, 2025) - 40 giorni prima
    {2460605.5, -2.470670863098733, -1.039721768945134, 0.387766756044098,
     0.004172746862982, -0.008617530854989, -0.004207787863398},
    
    // JD 2460615.5 (Oct 29, 2025) - 30 giorni prima
    {2460615.5, -2.429450925419775, -1.126897649168751, 0.343178982683014,
     0.004412237819087, -0.008457872806824, -0.004153632698142},
    
    // JD 2460625.5 (Nov 8, 2025) - 20 giorni prima
    {2460625.5, -2.385886848686050, -1.212560338032421, 0.297923264887481,
     0.004646082651149, -0.008291204394994, -0.004096127327431},
    
    // JD 2460635.5 (Nov 18, 2025) - 10 giorni prima
    {2460635.5, -2.340015229398518, -1.296623517305090, 0.252013318072819,
     0.004874125016149, -0.008117661706078, -0.004035328882177},
    
    // JD 2460645.5 (Nov 28, 2025) - EPOCA CENTRALE
    {2460645.5, -2.291873060113913, -1.379002429251408, 0.205462850619584,
     0.005096212324551, -0.007937383157085, -0.003971294969012},
    
    // JD 2460655.5 (Dec 8, 2025) + 10 giorni dopo
    {2460655.5, -2.241497699606127, -1.459613912391936, 0.158286563447056,
     0.005312195858893, -0.007750508577816, -0.003904082777767},
    
    // JD 2460665.5 (Dec 18, 2025) + 20 giorni dopo
    {2460665.5, -2.188927898962669, -1.538376401085679, 0.110499141652965,
     0.005521930760649, -0.007557180382846, -0.003833746824894},
    
    // JD 2460675.5 (Dec 28, 2025) + 30 giorni dopo
    {2460675.5, -2.134203746080994, -1.615209952785742, 0.062115250730829,
     0.005725276056009, -0.007357544145699, -0.003760341447491},
    
    // JD 2460685.5 (Jan 7, 2026) + 40 giorni dopo
    {2460685.5, -2.077366604309208, -1.690035273047652, 0.013149537825878,
     0.005922094654890, -0.007151747231485, -0.003683921433063},
    
    // JD 2460695.5 (Jan 17, 2026) + 50 giorni dopo
    {2460695.5, -2.018459070859449, -1.762774649028638, -0.036383074066453,
     0.006112253352476, -0.006939938428949, -0.003604541003264},
    
    // JD 2460705.5 (Jan 27, 2026) + 60 giorni dopo
    {2460705.5, -1.957525857896785, -1.833352025746054, -0.086465086733028,
     0.006295621825181, -0.006722267036823, -0.003522254809349}
};

// Elementi orbitali Ceres all'epoca centrale (JD 2460645.5)
// Estratti da JPL Small-Body Database
EquinoctialElements getCeresElements() {
    // Elementi Keplerian da JPL SBDB (epoca 2460645.5)
    double a = 2.7665435;  // AU
    double e = 0.0755869;
    double inc = 10.59407 * M_PI / 180.0;  // radianti
    double Omega = 80.30554 * M_PI / 180.0;
    double omega = 73.85895 * M_PI / 180.0;
    double M = 304.38642 * M_PI / 180.0;
    
    // Converti in equinoziali
    EquinoctialElements elem;
    elem.a = a;
    elem.h = e * std::sin(omega + Omega);
    elem.k = e * std::cos(omega + Omega);
    elem.p = std::tan(inc / 2.0) * std::sin(Omega);
    elem.q = std::tan(inc / 2.0) * std::cos(Omega);
    elem.lambda = M + omega + Omega;
    
    // Normalizza lambda
    while (elem.lambda < 0) elem.lambda += 2.0 * M_PI;
    while (elem.lambda >= 2.0 * M_PI) elem.lambda -= 2.0 * M_PI;
    
    elem.epoch = JulianDate(2460645.5);
    elem.designation = "(1) Ceres";
    elem.H = 3.34;
    elem.G = 0.12;
    
    return elem;
}

int main() {
    std::cout << std::fixed << std::setprecision(9);
    
    std::cout << "========================================================" << std::endl;
    std::cout << "Propagation Test vs JPL Horizons" << std::endl;
    std::cout << "========================================================" << std::endl;
    std::cout << "Object: (1) Ceres" << std::endl;
    std::cout << "Epoch: JD 2460645.5 (Nov 28, 2025 00:00 UT)" << std::endl;
    std::cout << "Range: -60 to +60 days" << std::endl;
    std::cout << "Comparison: every 10 days" << std::endl;
    std::cout << "Reference: JPL Horizons" << std::endl;
    std::cout << std::endl;
    
    // Ottieni elementi Ceres
    EquinoctialElements elements = getCeresElements();
    
    std::cout << "Orbital elements (Equinoctial, Ecliptic J2000):" << std::endl;
    std::cout << "  a = " << elements.a << " AU" << std::endl;
    std::cout << "  h = " << elements.h << std::endl;
    std::cout << "  k = " << elements.k << std::endl;
    std::cout << "  p = " << elements.p << std::endl;
    std::cout << "  q = " << elements.q << std::endl;
    std::cout << "  lambda = " << elements.lambda << " rad" << std::endl;
    double ecc = std::sqrt(elements.h * elements.h + elements.k * elements.k);
    std::cout << "  e = " << ecc << " (derived)" << std::endl;
    std::cout << std::endl;
    
    // Setup propagator con RKF78
    PropagatorOptions opts;
    opts.integrator = IntegratorType::RKF78;
    opts.stepSize = 0.1;  // 0.1 giorni step iniziale
    opts.tolerance = 1e-12;
    opts.usePlanetaryPerturbations = true;
    opts.useRelativisticCorrections = false;
    
    OrbitPropagator propagator(opts);
    
    std::cout << "Propagator settings:" << std::endl;
    std::cout << "  Integrator: RKF78" << std::endl;
    std::cout << "  Initial step: " << opts.stepSize << " days" << std::endl;
    std::cout << "  Tolerance: " << opts.tolerance << std::endl;
    std::cout << "  Planetary perturbations: enabled" << std::endl;
    std::cout << std::endl;
    
    // Converti elementi a stato iniziale
    OrbitState state0 = propagator.elementsToState(elements);
    
    std::cout << "Initial state at epoch:" << std::endl;
    std::cout << "  Position: [" << state0.position.x << ", " 
              << state0.position.y << ", " << state0.position.z << "] AU" << std::endl;
    std::cout << "  Velocity: [" << state0.velocity.x << ", " 
              << state0.velocity.y << ", " << state0.velocity.z << "] AU/day" << std::endl;
    std::cout << std::endl;
    
    // Propaga a tutte le epoche e confronta
    std::cout << "========================================================" << std::endl;
    std::cout << "Propagation Results" << std::endl;
    std::cout << "========================================================" << std::endl;
    std::cout << std::setw(12) << "JD" 
              << std::setw(10) << "Days" 
              << std::setw(15) << "Error (km)" 
              << std::setw(12) << "Steps" 
              << std::setw(12) << "Func Evals"
              << std::setw(10) << "Time (s)" << std::endl;
    std::cout << std::string(70, '-') << std::endl;
    
    std::vector<double> errors;
    double total_time = 0.0;
    int total_steps = 0;
    int total_evals = 0;
    
    for (const auto& horizons : horizons_data) {
        JulianDate target_epoch(horizons.jd);
        double days_from_epoch = horizons.jd - elements.epoch.jd;
        
        // Propaga
        auto start = std::chrono::high_resolution_clock::now();
        OrbitState state = propagator.propagate(state0, target_epoch);
        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> elapsed = end - start;
        
        auto stats = propagator.getLastStats();
        
        // Calcola errore vs Horizons
        double dx = (state.position.x - horizons.x) * 149597870.7;
        double dy = (state.position.y - horizons.y) * 149597870.7;
        double dz = (state.position.z - horizons.z) * 149597870.7;
        double error = std::sqrt(dx*dx + dy*dy + dz*dz);
        
        errors.push_back(error);
        total_time += elapsed.count();
        total_steps += stats.nSteps;
        total_evals += stats.nEvaluations;
        
        // Output
        std::cout << std::setw(12) << std::setprecision(1) << std::fixed << horizons.jd
                  << std::setw(10) << std::setprecision(1) << days_from_epoch
                  << std::setw(15) << std::setprecision(3) << error
                  << std::setw(12) << stats.nSteps
                  << std::setw(12) << stats.nEvaluations
                  << std::setw(10) << std::setprecision(6) << elapsed.count() << std::endl;
    }
    
    std::cout << std::string(70, '-') << std::endl;
    
    // Calcola statistiche
    double sum_error = 0.0;
    double max_error = 0.0;
    for (double err : errors) {
        sum_error += err * err;
        if (err > max_error) max_error = err;
    }
    double rms_error = std::sqrt(sum_error / errors.size());
    
    std::cout << std::endl;
    std::cout << "========================================================" << std::endl;
    std::cout << "Summary Statistics" << std::endl;
    std::cout << "========================================================" << std::endl;
    std::cout << "Number of epochs: " << errors.size() << std::endl;
    std::cout << "RMS error: " << rms_error << " km" << std::endl;
    std::cout << "Max error: " << max_error << " km" << std::endl;
    std::cout << "Total propagation time: " << total_time << " seconds" << std::endl;
    std::cout << "Average time per epoch: " << (total_time / errors.size()) << " seconds" << std::endl;
    std::cout << "Total RKF78 steps: " << total_steps << std::endl;
    std::cout << "Total function evaluations: " << total_evals << std::endl;
    std::cout << "Average steps per propagation: " << (total_steps / static_cast<double>(errors.size())) << std::endl;
    std::cout << std::endl;
    
    // Valutazione accuracy
    std::cout << "Accuracy Assessment:" << std::endl;
    if (rms_error < 1.0) {
        std::cout << "✓ EXCELLENT: RMS < 1 km (professional-grade accuracy)" << std::endl;
    } else if (rms_error < 10.0) {
        std::cout << "✓ GOOD: RMS < 10 km (acceptable for most applications)" << std::endl;
    } else if (rms_error < 100.0) {
        std::cout << "⚠ FAIR: RMS < 100 km (usable for occultation predictions)" << std::endl;
    } else if (rms_error < 1000.0) {
        std::cout << "⚠ POOR: RMS < 1000 km (marginal accuracy)" << std::endl;
    } else {
        std::cout << "✗ BAD: RMS > 1000 km (unacceptable accuracy)" << std::endl;
    }
    std::cout << std::endl;
    
    // Confronto elementi a epoche estreme
    std::cout << "========================================================" << std::endl;
    std::cout << "Round-trip Test" << std::endl;
    std::cout << "========================================================" << std::endl;
    
    // Propaga -60, +60, ritorna a epoca centrale
    JulianDate epoch_minus60(2460585.5);
    JulianDate epoch_plus60(2460705.5);
    
    OrbitState state_m60 = propagator.propagate(state0, epoch_minus60);
    OrbitState state_back = propagator.propagate(state_m60, elements.epoch);
    
    double dx_rt = (state_back.position.x - state0.position.x) * 149597870.7;
    double dy_rt = (state_back.position.y - state0.position.y) * 149597870.7;
    double dz_rt = (state_back.position.z - state0.position.z) * 149597870.7;
    double error_rt = std::sqrt(dx_rt*dx_rt + dy_rt*dy_rt + dz_rt*dz_rt);
    
    std::cout << "Round-trip (-60 days, +60 days back):" << std::endl;
    std::cout << "  Position error: " << error_rt << " km" << std::endl;
    
    if (error_rt < 1.0) {
        std::cout << "✓ PASS: Round-trip error < 1 km" << std::endl;
    } else {
        std::cout << "⚠ WARNING: Round-trip error = " << error_rt << " km" << std::endl;
    }
    
    return 0;
}
