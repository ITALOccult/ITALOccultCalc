#include <iostream>
#include <iomanip>
#include "astdyn_wrapper.h"

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <path_to_eq1_file>" << std::endl;
        return 1;
    }

    std::string eq1_path = argv[1];
    std::cout << "Testing AstDynWrapper covariance loading for: " << eq1_path << std::endl;

    ioccultcalc::AstDynWrapper wrapper;
    if (!wrapper.loadFromEQ1File(eq1_path)) {
        std::cerr << "Failed to load EQ1 file." << std::endl;
        return 1;
    }

    std::cout << "Object: " << wrapper.getObjectName() << std::endl;
    std::cout << "Has Covariance: " << (wrapper.hasCovariance() ? "YES" : "NO") << std::endl;

    if (wrapper.hasCovariance()) {
        auto cov = wrapper.getCovariance();
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
