#include "astdyn/propagation/OrbitalElements.hpp"
#include "astdyn/core/Constants.hpp"
#include <iostream>
#include <iomanip>

int main() {
    using namespace astdyn::propagation;
    using namespace astdyn::constants;
    
    // Mean elements for 13477 at MJD 61041.0
    KeplerianElements mean;
    mean.semi_major_axis = 2.448744005;
    mean.eccentricity = 0.14332836;
    mean.inclination = 8.46011 * PI / 180.0;
    mean.longitude_ascending_node = 67.77114 * PI / 180.0;
    mean.argument_perihelion = 271.18569 * PI / 180.0;
    mean.mean_anomaly = 69.64531 * PI / 180.0;
    mean.epoch_mjd_tdb = 61041.0;
    
    // TEST 1: mu = 0 (Should be corrected by safety check)
    mean.gravitational_parameter = 0.0;
    KeplerianElements osc1 = mean_to_osculating(mean);
    
    std::cout << "TEST 1 (mu=0):" << std::endl;
    std::cout << "  Mean a: " << std::fixed << std::setprecision(10) << mean.semi_major_axis << std::endl;
    std::cout << "  Osc  a: " << std::fixed << std::setprecision(10) << osc1.semi_major_axis << std::endl;
    std::cout << "  Delta a: " << (osc1.semi_major_axis - mean.semi_major_axis) << std::endl;
    
    // TEST 2: mu = 1.0 (Should NOT be corrected by 1e-10 threshold!)
    mean.gravitational_parameter = 1.0;
    KeplerianElements osc2 = mean_to_osculating(mean);
    
    std::cout << "\nTEST 2 (mu=1.0):" << std::endl;
    std::cout << "  Mean a: " << mean.semi_major_axis << std::endl;
    std::cout << "  Osc  a: " << osc2.semi_major_axis << std::endl;
    std::cout << "  Delta a: " << (osc2.semi_major_axis - mean.semi_major_axis) << std::endl;
    
    // TEST 3: Correct mu
    mean.gravitational_parameter = GMS;
    KeplerianElements osc3 = mean_to_osculating(mean);
    
    std::cout << "\nTEST 3 (mu=GMS):" << std::endl;
    std::cout << "  Osc  a: " << osc3.semi_major_axis << std::endl;
    std::cout << "  Delta a: " << (osc3.semi_major_axis - mean.semi_major_axis) << std::endl;

    // TEST 4: Default initialized struct (should now have GMS)
    KeplerianElements mean4;
    mean4.semi_major_axis = 2.448744005;
    mean4.eccentricity = 0.14332836;
    mean4.inclination = 8.46011 * PI / 180.0;
    mean4.longitude_ascending_node = 67.77114 * PI / 180.0;
    mean4.argument_perihelion = 271.18569 * PI / 180.0;
    mean4.mean_anomaly = 69.64531 * PI / 180.0;
    mean4.epoch_mjd_tdb = 61041.0;
    // gravitational_parameter NOT SET
    
    KeplerianElements osc4 = mean_to_osculating(mean4);
    std::cout << "\nTEST 4 (Default MU):" << std::endl;
    std::cout << "  Default MU: " << mean4.gravitational_parameter << std::endl;
    std::cout << "  Osc  a: " << osc4.semi_major_axis << std::endl;
    std::cout << "  Delta a: " << (osc4.semi_major_axis - mean4.semi_major_axis) << std::endl;

    return 0;
}
