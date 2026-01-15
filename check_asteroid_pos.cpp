#include <iostream>
#include <vector>
#include <astdyn/AstDyn.hpp>
#include <astdyn/api/OrbitFitAPI.hpp>
#include <astdyn/propagation/HighPrecisionPropagator.hpp>

int main() {
    // 1. Setup Propagator
    astdyn::propagation::HighPrecisionPropagator::Config config;
    config.de441_path = std::string(getenv("HOME")) + "/.ioccultcalc/ephemerides/de441_part-2.bsp";
    config.perturbations_planets = true;
    config.relativity = true;
    astdyn::propagation::HighPrecisionPropagator propagator(config);

    // 2. Load Elements from 1272_jpl.eq1
    try {
        auto equ = astdyn::api::OrbitFitAPI::parse_eq1("1272_jpl.eq1");
        auto initial_kep_ecl = astdyn::propagation::equinoctial_to_keplerian(equ);
        initial_kep_ecl.gravitational_parameter = 2.959122082855911e-04;

        // 3. Calculate Position at Occult4 Time
        // XML: <ID>20260113_1194-1,61044.49</ID>
        // <Elements>...0.54,2026,1,13,0.2296683...
        double event_mjd = 61053.2296683;
        double event_jd = event_mjd + 2400000.5;

        std::cout << "Calculating state for 1272 at MJD=" << event_mjd << " (JD=" << event_jd << ")...\n";
        
        auto obs = propagator.calculateGeocentricObservation(
            initial_kep_ecl, 
            event_jd, 
            astdyn::propagation::HighPrecisionPropagator::InputFrame::ECLIPTIC
        );

        std::cout << "Asteroid Position (Geocentric App): RA=" << obs.ra_deg << " Dec=" << obs.dec_deg << " Dist=" << obs.distance_au << " AU\n";
        
        // Occult4 Star Position
        // <Star>...8.21574020,25.0585092...
        double star_ra_h = 8.21574020;
        double star_dec_deg = 25.0585092;
        double star_ra_deg = star_ra_h * 15.0;

        std::cout << "Occult4 Star Position (Apparent?): RA=" << star_ra_deg << " Dec=" << star_dec_deg << "\n";

        // Distance
        double d_ra = (obs.ra_deg - star_ra_deg) * std::cos(star_dec_deg * M_PI / 180.0);
        double d_dec = (obs.dec_deg - star_dec_deg);
        double dist_deg = std::sqrt(d_ra*d_ra + d_dec*d_dec);
        
        std::cout << "Distance: " << dist_deg * 3600.0 << " arcsec\n";

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
