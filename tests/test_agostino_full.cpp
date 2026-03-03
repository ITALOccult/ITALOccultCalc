#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/spice_spk_reader.h"
#include <iostream>
using namespace ioccultcalc;
int main() {
    auto r = std::make_shared<SPICESPKReader>();
    r->ensureFileLoaded("de440.bsp");
    Ephemeris e(r);
    auto p = e.getEarthPosition(JulianDate(2461045.0));
    std::cout << "Dist: " << p.magnitude() << " AU" << std::endl;
    return 0;
}
