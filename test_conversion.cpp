#include <iostream>
#include <iomanip>
#include "astdyn/propagation/OrbitalElements.hpp"
#include "astdyn/core/Constants.hpp"

using namespace astdyn::propagation;

int main() {
    KeplerianElements kep;
    kep.epoch_mjd_tdb = 61000.0;
    kep.semi_major_axis = 2.804117;
    kep.eccentricity = 0.173159;
    kep.inclination = 15.0 * M_PI / 180.0;
    kep.longitude_ascending_node = 30.0 * M_PI / 180.0;
    kep.argument_perihelion = 45.0 * M_PI / 180.0;
    kep.mean_anomaly = 60.0 * M_PI / 180.0;
    kep.gravitational_parameter = astdyn::constants::GMS;

    std::cout << "Initial a: " << std::fixed << std::setprecision(6) << kep.semi_major_axis << " AU\n";
    std::cout << "Initial mu: " << std::scientific << kep.gravitational_parameter << "\n";

    CartesianElements cart = keplerian_to_cartesian(kep);
    std::cout << "Cartesian pos: " << cart.position.transpose() << "\n";
    std::cout << "Cartesian vel: " << cart.velocity.transpose() << "\n";
    std::cout << "Cartesian mu: " << std::scientific << cart.gravitational_parameter << "\n";

    KeplerianElements kep2 = cartesian_to_keplerian(cart);
    std::cout << "Recovered a: " << std::fixed << std::setprecision(6) << kep2.semi_major_axis << " AU\n";
    std::cout << "Recovered mu: " << std::scientific << kep2.gravitational_parameter << "\n";

    if (std::abs(kep.semi_major_axis - kep2.semi_major_axis) > 1e-6) {
        std::cout << "!!! ERROR: Identity failed! Delta = " << (kep2.semi_major_axis - kep.semi_major_axis) << "\n";
    } else {
        std::cout << "✓ Identity successful.\n";
    }

    return 0;
}
