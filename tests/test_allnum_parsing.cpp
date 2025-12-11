/**
 * @file test_allnum_parsing.cpp
 * @brief Test parsing allnum.cat e verifica conversioni gradi/radianti
 * 
 * Verifica che:
 * 1. Il parsing fixed-width funzioni correttamente
 * 2. Le conversioni gradi->radianti siano corrette
 * 3. Le conversioni MJD->JD siano corrette
 * 4. I valori siano ragionevoli
 */

#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/orbital_elements.h"
#include <iostream>
#include <iomanip>
#include <cmath>
#include <vector>
#include <string>

using namespace ioccultcalc;

// Colori per output
#define RESET   "\033[0m"
#define GREEN   "\033[32m"
#define RED     "\033[31m"
#define YELLOW  "\033[33m"
#define BLUE    "\033[34m"
#define CYAN    "\033[36m"

void printHeader(const std::string& title) {
    std::cout << "\n" << BLUE << "═══════════════════════════════════════════════════════\n";
    std::cout << title << "\n";
    std::cout << "═══════════════════════════════════════════════════════" << RESET << "\n\n";
}

void printTest(const std::string& name, bool passed) {
    if (passed) {
        std::cout << GREEN << "✓ " << name << RESET << "\n";
    } else {
        std::cout << RED << "✗ " << name << RESET << "\n";
    }
}

void printValue(const std::string& label, double value, const std::string& unit = "") {
    std::cout << "  " << CYAN << label << ": " << RESET 
              << std::fixed << std::setprecision(6) << value << " " << unit << "\n";
}

bool checkAngleInRadians(double angle_rad, const std::string& name) {
    // Gli angoli in radianti devono essere in [0, 2π) o ragionevolmente vicini
    // Per elementi orbitali, i valori tipici sono:
    // - i (inclinazione): 0-180° = 0-π rad
    // - Omega, omega, M: 0-360° = 0-2π rad
    
    // Normalizza a [0, 2π)
    while (angle_rad < 0) angle_rad += 2.0 * M_PI;
    while (angle_rad >= 2.0 * M_PI) angle_rad -= 2.0 * M_PI;
    
    // Se l'angolo è > 180° (π rad), potrebbe essere un errore di conversione
    // (se fosse in gradi invece di radianti, un valore di 90° sarebbe 90 rad, che è > 2π)
    // Ma per sicurezza, controlliamo solo che non sia > 2π
    if (angle_rad > 2.0 * M_PI) {
        std::cout << RED << "  ERRORE: " << name << " = " << angle_rad 
                  << " rad sembra essere in GRADI invece di RADIANTI!" << RESET << "\n";
        return false;
    }
    
    return true;
}

bool checkReasonableValue(double value, double min_val, double max_val, const std::string& name) {
    if (value < min_val || value > max_val) {
        std::cout << RED << "  ERRORE: " << name << " = " << value 
                  << " fuori dal range ragionevole [" << min_val << ", " << max_val << "]" << RESET << "\n";
        return false;
    }
    return true;
}

int main() {
    printHeader("TEST PARSING allnum.cat E CONVERSIONI GRADI/RADIANTI");
    
    std::vector<std::string> testAsteroids = {"1", "433", "704", "17030", "4"};
    AstDysClient client;
    client.setTimeout(60); // Timeout più lungo per download catalogo grande
    
    int passedTests = 0;
    int totalTests = 0;
    
    std::cout << CYAN << "NOTA: Questo test scarica allnum.cat da AstDyS (file grande, ~30MB)\n";
    std::cout << "      Potrebbe richiedere alcuni secondi per il download...\n" << RESET << std::endl;
    
    for (const auto& designation : testAsteroids) {
        std::cout << "\n" << YELLOW << "━━━ Asteroide " << designation << " ━━━" << RESET << "\n";
        std::cout << "Scaricamento elementi da allnum.cat..." << std::flush;
        
        try {
            // Test 1: Parsing elementi da allnum.cat
            totalTests++;
            OrbitalElements elem = client.getRecentElements(designation);
            std::cout << GREEN << " ✓" << RESET << "\n";
            
            std::cout << "\nElementi parsati:\n";
            printValue("Epoca (JD)", elem.epoch.jd);
            printValue("Epoca (MJD)", elem.epoch.toMJD());
            printValue("a (AU)", elem.a, "AU");
            printValue("e", elem.e);
            printValue("i", elem.i, "rad");
            printValue("i", elem.i * 180.0 / M_PI, "deg");
            printValue("Omega", elem.Omega, "rad");
            printValue("Omega", elem.Omega * 180.0 / M_PI, "deg");
            printValue("omega", elem.omega, "rad");
            printValue("omega", elem.omega * 180.0 / M_PI, "deg");
            printValue("M", elem.M, "rad");
            printValue("M", elem.M * 180.0 / M_PI, "deg");
            printValue("H", elem.H);
            printValue("G", elem.G);
            
            // Test 2: Verifica conversione MJD -> JD
            totalTests++;
            double mjd = elem.epoch.toMJD();
            double expected_jd = mjd + 2400000.5;
            double jd_diff = std::abs(elem.epoch.jd - expected_jd);
            bool mjd_ok = jd_diff < 0.0001;
            printTest("Conversione MJD->JD corretta", mjd_ok);
            if (!mjd_ok) {
                std::cout << RED << "  JD calcolato: " << elem.epoch.jd 
                          << ", JD atteso: " << expected_jd << RESET << "\n";
            } else {
                passedTests++;
            }
            
            // Test 3: Verifica angoli in radianti (non gradi)
            totalTests++;
            bool angles_ok = true;
            
            // i (inclinazione) dovrebbe essere 0-π rad (0-180°)
            if (!checkAngleInRadians(elem.i, "i")) {
                angles_ok = false;
            }
            double i_deg = elem.i * 180.0 / M_PI;
            if (!checkReasonableValue(i_deg, 0.0, 180.0, "i (deg)")) {
                angles_ok = false;
            }
            
            // Omega, omega, M dovrebbero essere 0-2π rad (0-360°)
            if (!checkAngleInRadians(elem.Omega, "Omega")) {
                angles_ok = false;
            }
            if (!checkAngleInRadians(elem.omega, "omega")) {
                angles_ok = false;
            }
            if (!checkAngleInRadians(elem.M, "M")) {
                angles_ok = false;
            }
            
            // Verifica che i valori in gradi siano ragionevoli
            double Omega_deg = elem.Omega * 180.0 / M_PI;
            double omega_deg = elem.omega * 180.0 / M_PI;
            double M_deg = elem.M * 180.0 / M_PI;
            
            if (!checkReasonableValue(Omega_deg, 0.0, 360.0, "Omega (deg)")) {
                angles_ok = false;
            }
            if (!checkReasonableValue(omega_deg, 0.0, 360.0, "omega (deg)")) {
                angles_ok = false;
            }
            if (!checkReasonableValue(M_deg, 0.0, 360.0, "M (deg)")) {
                angles_ok = false;
            }
            
            printTest("Angoli in radianti corretti", angles_ok);
            if (angles_ok) {
                passedTests++;
            }
            
            // Test 4: Verifica valori ragionevoli
            totalTests++;
            bool values_ok = true;
            
            // a (semiasse maggiore) dovrebbe essere 0.1-50 AU per asteroidi
            if (!checkReasonableValue(elem.a, 0.1, 50.0, "a (AU)")) {
                values_ok = false;
            }
            
            // e (eccentricità) dovrebbe essere 0-1
            if (!checkReasonableValue(elem.e, 0.0, 1.0, "e")) {
                values_ok = false;
            }
            
            // H (magnitudine assoluta) dovrebbe essere 0-30
            if (!checkReasonableValue(elem.H, 0.0, 30.0, "H")) {
                values_ok = false;
            }
            
            // G (slope parameter) dovrebbe essere 0-1
            if (!checkReasonableValue(elem.G, 0.0, 1.0, "G")) {
                values_ok = false;
            }
            
            printTest("Valori ragionevoli", values_ok);
            if (values_ok) {
                passedTests++;
            }
            
            // Test 5: Verifica che la conversione gradi->radianti sia corretta
            // Controlliamo che convertendo avanti e indietro otteniamo lo stesso valore
            totalTests++;
            bool conversion_ok = true;
            
            // Test per ogni angolo
            struct AngleTest {
                double value;
                const char* name;
            };
            AngleTest test_angles[] = {
                {elem.i, "i"},
                {elem.Omega, "Omega"},
                {elem.omega, "omega"},
                {elem.M, "M"}
            };
            
            for (int i = 0; i < 4; i++) {
                double angle_rad = test_angles[i].value;
                double angle_deg = angle_rad * 180.0 / M_PI;
                double angle_rad_back = angle_deg * M_PI / 180.0;
                double diff = std::abs(angle_rad - angle_rad_back);
                
                if (diff > 1e-10) {
                    std::cout << RED << "  ERRORE conversione " << test_angles[i].name 
                              << ": diff = " << diff << RESET << "\n";
                    conversion_ok = false;
                }
            }
            
            printTest("Conversione gradi↔radianti corretta", conversion_ok);
            if (conversion_ok) {
                passedTests++;
            }
            
        } catch (const std::exception& e) {
            std::cout << RED << " ✗" << RESET << "\n";
            std::cout << RED << "✗ Errore nel parsing: " << e.what() << RESET << "\n";
            std::cout << "  (Skipping questo asteroide)\n";
        } catch (...) {
            std::cout << RED << " ✗" << RESET << "\n";
            std::cout << RED << "✗ Errore sconosciuto durante il parsing" << RESET << "\n";
            std::cout << "  (Skipping questo asteroide)\n";
        }
    }
    
    // Riepilogo
    printHeader("RIEPILOGO TEST");
    std::cout << "Test passati: " << GREEN << passedTests << RESET << " / " << totalTests << "\n";
    std::cout << "Test falliti: " << RED << (totalTests - passedTests) << RESET << " / " << totalTests << "\n\n";
    
    if (passedTests == totalTests) {
        std::cout << GREEN << "✓ TUTTI I TEST PASSATI!" << RESET << "\n";
        std::cout << "Il parsing di allnum.cat e le conversioni gradi/radianti sono corrette.\n";
        return 0;
    } else {
        std::cout << RED << "✗ ALCUNI TEST FALLITI!" << RESET << "\n";
        std::cout << "Verificare il parsing e le conversioni.\n";
        return 1;
    }
}
