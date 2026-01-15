#include "astdyn_wrapper.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main() {
    AstDynWrapper wrapper;
    
    // Set initial elements (Jan 1, 2026 = MJD 61041.0)
    double a = 2.44875;
    double e = 0.14334;
    double i = 8.46 * M_PI / 180.0;
    double Omega = 67.8 * M_PI / 180.0;
    double omega = 269.9 * M_PI / 180.0;
    double M_0 = 59.11 * M_PI / 180.0;
    double epoch_0 = 61041.0;
    
    wrapper.setKeplerianElements(a, e, i, Omega, omega, M_0, epoch_0, "13477_TEST");
    
    std::cout << "[VERIFY] Initial M: " << wrapper.getKeplerianElements().mean_anomaly * 180.0 / M_PI << " deg" << std::endl;
    
    // Propagate 10 days
    double epoch_1 = epoch_0 + 10.0;
    wrapper.propagateToEpoch(epoch_1);
    
    double M_1 = wrapper.getKeplerianElements().mean_anomaly * 180.0 / M_PI;
    std::cout << "[VERIFY] After 10 days propagation (MJD " << epoch_1 << "):" << std::endl;
    std::cout << "  - M: " << M_1 << " deg" << std::endl;
    std::cout << "  - Delta M: " << (M_1 - 59.11) << " deg" << std::endl;
    
    // Expected Delta M (roughly): 10 days * (0.9856 / a^1.5)
    double n = 0.9856 * std::pow(a, -1.5) * 180.0 / M_PI; // deg/day
    std::cout << "  - Expected Delta M (2-body approx): " << n * 10.0 << " deg" << std::endl;

    if (std::abs(M_1 - 59.11) < 0.01) {
        std::cout << "[VERIFY] FAILURE: Mean Anomaly did not update!" << std::endl;
    } else {
        std::cout << "[VERIFY] SUCCESS: Mean Anomaly updated." << std::endl;
    }

    return 0;
}
