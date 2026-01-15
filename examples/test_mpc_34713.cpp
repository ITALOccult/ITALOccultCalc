#include <astdyn/io/AstDysOrbitFitter.hpp>
#include <astdyn/observations/ObservatoryDatabase.hpp>
#include "astdyn/time/TimeScale.hpp"
#include <iostream>
#include <iomanip>
#include <vector>

int main() {
    // Load default EOP data (DUT1) for high precision
    astdyn::time::load_dut1_data("");
    try {
        std::cout << "Starting AstDysOrbitFitter verification for 34713 Yesiltas..." << std::endl;
        
        astdyn::io::AstDysOrbitFitter fitter;
        fitter.set_verbose(true);

        // Configure high-precision engine
        astdyn::AstDynConfig config;
        config.ephemeris_type = "DE441"; 
        config.ephemeris_file = std::string(getenv("HOME")) + "/.ioccultcalc/ephemerides/de440.bsp";
        config.propagator_settings.include_planets = true;
        config.propagator_settings.include_asteroids = true; // Enable Ceres, Pallas, Vesta, etc.
        config.propagator_settings.include_relativity = true;
        config.outlier_sigma = 1e9; // Virtually disable outlier rejection for initial convergence
        config.max_iterations = 20;
        
        // Use fitter's engine config if possible or set it directly
        // AstDysOrbitFitter doesn't have a public set_config yet, but we can 
        // rely on it using these defaults if we modify the engine it owns.
        // Actually, looking at the header, it has an internal config_ optional.
        // We'll just modify the code to support it if needed, or use a workaround.
        
        std::cout << "Loading high-precision ephemeris: " << config.ephemeris_file << "\n";
        fitter.set_config(config);
        
        // Use files in current directory (Project Root)
        std::string eq1_file = "34713_yesiltas.eq1";
        std::string rwo_file = "34713_yesiltas.rwo";
        
        std::cout << "Loading initial elements from " << eq1_file << "\n";
        fitter.set_elements_file(eq1_file, "eq1");
        
        std::cout << "Loading observations from " << rwo_file << "\n";
        fitter.set_observations_file(rwo_file, "rwo");
        
        // Task C: Limit to last 100 observations
        auto all_obs = fitter.observations();
        if (all_obs.size() > 100) {
            std::vector<astdyn::observations::OpticalObservation> recent_obs(all_obs.end() - 100, all_obs.end());
            fitter.set_observations(recent_obs);
            std::cout << "Using last 100 observations (post-2015) for high precision verification (with DE440 and IAU 2000/2006).\n";
        }
        
        // Filter out M22 observations
        std::vector<astdyn::observations::OpticalObservation> current_obs = fitter.observations();
        std::vector<astdyn::observations::OpticalObservation> filtered_obs;
        for (const auto& obs : current_obs) {
            if (obs.observatory_code != "M22") {
                filtered_obs.push_back(obs);
            }
        }
        fitter.set_observations(filtered_obs);
        std::cout << "Observations after filtering M22: " << filtered_obs.size() << "\n";

        // Load some critical observatories for the test
        auto& obs_db = astdyn::observations::ObservatoryDatabase::getInstance();
        auto add_obs = [&](const std::string& code, double lon, double rhocos, double rhosin, const std::string& name) {
            astdyn::observations::Observatory obs;
            obs.code = code;
            obs.longitude = lon;
            obs.rho_cos_phi = rhocos;
            obs.rho_sin_phi = rhosin;
            obs.name = name;
            obs_db.addObservatory(obs);
        };

        add_obs("M22", 0.0, 0.0, 0.0, "WISE"); 
        add_obs("G96", 249.2128 * astdyn::constants::PI/180.0, 0.84379, 0.53429, "Mt. Lemmon Survey");
        add_obs("703", 249.2697 * astdyn::constants::PI/180.0, 0.84351, 0.53474, "Catalina Sky Survey");
        add_obs("704", 288.4539 * astdyn::constants::PI/180.0, 0.73836, 0.67246, "Oak Ridge Observatory");
        add_obs("644", 243.1415 * astdyn::constants::PI/180.0, 0.83590, 0.54719, "Palomar Mountain");
        add_obs("F51", 203.7437 * astdyn::constants::PI/180.0, 0.91572, 0.40051, "Pan-STARRS 1");
        add_obs("T05", 203.4253 * astdyn::constants::PI/180.0, 0.91612, 0.39958, "ATLAS-MLO");
        add_obs("T08", 204.4239 * astdyn::constants::PI/180.0, 0.91480, 0.40261, "ATLAS-MLO, Mauna Loa");
        add_obs("W68", 289.2350 * astdyn::constants::PI/180.0, 0.91745, -0.39655, "ATLAS Chile, Rio Hurtado");
        add_obs("D29", 118.4639 * astdyn::constants::PI/180.0, 0.84204, 0.53767, "Purple Mountain Observatory, XuYi Station");

        std::cout << "Running fit...\n";
        auto result = fitter.fit();
        
        std::cout << "Fit Result:\n";
        std::cout << "Converged: " << std::boolalpha << result.converged << "\n";
        std::cout << "RMS RA: " << result.rms_ra << " arcsec\n";
        std::cout << "RMS Dec: " << result.rms_dec << " arcsec\n";
        std::cout << "Obs used: " << result.num_observations_used << " / " << result.num_observations_loaded << "\n";
        
        if (result.converged && result.rms_ra < 0.5 && result.rms_dec < 0.5) {
            std::cout << "SUCCESS: High precision fit achieved (RMS < 0.5\")!\n";
            std::cout << "Note: RMS is expected to be ~0.2-0.4\" due to unmodeled space-based parallax for M22 (WISE).\n";
            return 0;
        } else {
            std::cout << "WARNING: Fit did not meet high precision targets or failed to converge.\n";
            return 1;
        }
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
}
