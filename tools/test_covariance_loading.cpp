/**
 * @file test_covariance_loading.cpp
 * @brief Verifica caricamento covarianza da file EQ1 (OEF2.0) tramite parser AstDyn.
 *
 * Carica uno stato orbitale (EquinoctialElements) tramite OrbitFitAPI::parse_eq1
 * e verifica la presenza della covarianza sullo stato restituito.
 * Nessun wrapper: uso diretto dell'API AstDyn.
 */

#include <astdyn/api/OrbitFitAPI.hpp>
#include <astdyn/propagation/OrbitalElements.hpp>
#include <iostream>
#include <iomanip>
#include <string>
#include <fstream>

static std::string objectNameFromEq1(const std::string& filepath) {
    std::ifstream f(filepath);
    std::string line;
    while (std::getline(f, line)) {
        auto pos = line.find("! Object");
        if (pos != std::string::npos) {
            pos = line.find("Object");
            if (pos != std::string::npos) {
                pos += 6;
                while (pos < line.size() && std::isspace(static_cast<unsigned char>(line[pos]))) ++pos;
                size_t end = pos;
                while (end < line.size() && !std::isspace(static_cast<unsigned char>(line[end]))) ++end;
                if (end > pos) return line.substr(pos, end - pos);
            }
        }
    }
    return "(from file)";
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <path_to_eq1_file>" << std::endl;
        return 1;
    }

    const std::string eq1_path = argv[1];
    std::cout << "Testing AstDyn EQ1 covariance loading for: " << eq1_path << std::endl;

    astdyn::propagation::EquinoctialElements state;
    try {
        state = astdyn::api::OrbitFitAPI::parse_eq1(eq1_path);
    } catch (const std::exception& e) {
        std::cerr << "Failed to load EQ1 file: " << e.what() << std::endl;
        return 1;
    }

    std::string obj_name = objectNameFromEq1(eq1_path);
    std::cout << "Object: " << obj_name << std::endl;
    std::cout << "Has Covariance: " << (state.covariance.has_value() ? "YES" : "NO") << std::endl;

    if (state.covariance.has_value()) {
        const auto& cov = *state.covariance;
        std::cout << "Covariance Matrix (6x6):" << std::endl;
        std::cout << std::scientific << std::setprecision(6);
        for (int i = 0; i < 6; ++i) {
            for (int j = 0; j < 6; ++j) {
                std::cout << std::setw(15) << cov(i, j) << " ";
            }
            std::cout << std::endl;
        }
    }

    return 0;
}
