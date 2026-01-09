/**
 * @file test_astdys.cpp
 * @brief Test program per verificare lo scaricamento da AstDyS
 */

#include "ioccultcalc/astdys_client.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Uso: " << argv[0] << " <asteroid_number>\n";
        std::cerr << "Esempio: " << argv[0] << " 433\n";
        return 1;
    }
    
    std::string asteroidNumber = argv[1];
    
    std::cout << "========================================\n";
    std::cout << "Test scaricamento elementi da AstDyS\n";
    std::cout << "========================================\n\n";
    std::cout << "Asteroide: " << asteroidNumber << "\n\n";
    
    try {
        AstDysClient client;
        client.setTimeout(30);
        
        std::cout << "Scaricamento elementi orbitali...\n\n";
        
        AstDynEquinoctialElements elem = client.getElements(asteroidNumber);
        
        std::cout << "\n========================================\n";
        std::cout << "SUCCESSO!\n";
        std::cout << "========================================\n\n";
        
        std::cout << "Elementi orbitali scaricati:\n";
        std::cout << "----------------------------\n";
        std::cout << "Designation: " << elem.designation << "\n";
        std::cout << "Epoca (JD):  " << std::fixed << std::setprecision(2) << elem.epoch.jd << "\n";
        std::cout << "\nElementi equinoziali:\n";
        std::cout << "  a (AU):    " << std::scientific << std::setprecision(10) << elem.a << "\n";
        std::cout << "  h:         " << elem.h << "\n";
        std::cout << "  k:         " << elem.k << "\n";
        std::cout << "  p:         " << elem.p << "\n";
        std::cout << "  q:         " << elem.q << "\n";
        std::cout << "  lambda:    " << elem.lambda << "\n";
        
        std::cout << "\nParametri assoluti:\n";
        std::cout << "  H (mag):   " << std::fixed << std::setprecision(2) << elem.H << "\n";
        std::cout << "  G:         " << elem.G << "\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n========================================\n";
        std::cerr << "ERRORE!\n";
        std::cerr << "========================================\n\n";
        std::cerr << "Errore: " << e.what() << "\n";
        return 1;
    }
}
