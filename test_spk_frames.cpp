// Quick test SPK coordinate systems
#include "ioccultcalc/spice_spk_reader.h"
#include "ioccultcalc/time_utils.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main() {
    JulianDate jd = TimeUtils::calendarToJD(2024, 12, 10, 2, 30, 0);
    
    SPICESPKReader spk;
    spk.ensureFileLoaded("de440s.bsp");
    
    std::cout << std::fixed << std::setprecision(6);
    
    // Test vari centri
    std::cout << "Terra (399) rispetto a:\n\n";
    
    std::cout << "SSB (0 = Solar System Barycenter):\n";
    auto [pos0, vel0] = spk.getState(399, jd.jd, 0);
    std::cout << "  [" << pos0.x << ", " << pos0.y << ", " << pos0.z << "]\n\n";
    
    std::cout << "Sole (10):\n";
    auto [pos10, vel10] = spk.getState(399, jd.jd, 10);
    std::cout << "  [" << pos10.x << ", " << pos10.y << ", " << pos10.z << "]\n\n";
    
    std::cout << "Horizons (@sun):\n";
    std::cout << "  [0.203452, 0.963555, -0.000055633]\n";
    
    return 0;
}
