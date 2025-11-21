#include <iostream>
#include <cassert>
#include <cmath>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

void test_iso_to_jd() {
    std::cout << "Test: ISO to JD conversion... ";
    
    // J2000.0: 2000-01-01 12:00:00 = JD 2451545.0
    JulianDate jd = TimeUtils::isoToJD("2000-01-01 12:00:00");
    
    assert(std::abs(jd.jd - 2451545.0) < 0.001);
    
    std::cout << "OK\n";
}

void test_jd_to_iso() {
    std::cout << "Test: JD to ISO conversion... ";
    
    JulianDate jd(2451545.0);
    std::string iso = TimeUtils::jdToISO(jd);
    
    // Dovrebbe essere circa 2000-01-01 12:00:00
    assert(iso.find("2000-01-01") != std::string::npos);
    
    std::cout << "OK (ISO: " << iso << ")\n";
}

void test_round_trip() {
    std::cout << "Test: Round trip conversion... ";
    
    std::string original = "2025-11-21 00:00:00";
    JulianDate jd = TimeUtils::isoToJD(original);
    std::string converted = TimeUtils::jdToISO(jd);
    
    // Le stringhe dovrebbero essere molto simili
    assert(converted.find("2025-11-21") != std::string::npos);
    
    std::cout << "OK\n";
}

void test_gmst() {
    std::cout << "Test: GMST calculation... ";
    
    // Test per J2000.0
    JulianDate jd(2451545.0);
    double gmst = TimeUtils::gmst(jd);
    
    // GMST dovrebbe essere un valore ragionevole in radianti
    assert(gmst >= 0 && gmst < 2 * M_PI);
    
    std::cout << "OK (GMST: " << (gmst * 180.0 / M_PI) << " degrees)\n";
}

int main() {
    std::cout << "IOccultCalc - Time Utils Tests\n";
    std::cout << "================================\n\n";
    
    try {
        test_iso_to_jd();
        test_jd_to_iso();
        test_round_trip();
        test_gmst();
        
        std::cout << "\nAll tests passed!\n";
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\nTest failed: " << e.what() << std::endl;
        return 1;
    }
}
