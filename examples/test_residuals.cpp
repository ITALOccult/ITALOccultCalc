#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/mpc_client.h"
#include "ioccultcalc/ephemeris.h"
#include <iostream>
#include <iomanip>
#include <cmath>

using namespace ioccultcalc;

int main() {
    // Carica elementi e osservazioni
    AstDysClient astdys;
    auto elements = astdys.getElements("433");
    
    MPCClient mpc;
    auto obs = mpc.getObservations("433");
    
    Ephemeris eph(elements);
    
    std::cout << "Elementi: epoch=" << elements.epoch.jd << "\n";
    std::cout << "Osservazioni: " << obs.observations.size() << "\n";
    std::cout << "Prima osservazione: " << obs.observations[0].epoch.jd << "\n";
    std::cout << "Ultima osservazione: " << obs.observations.back().epoch.jd << "\n\n";
    
    // Calcola residui per prime 10 osservazioni vicine all'epoca
    std::vector<double> residuals;
    for (const auto& ob : obs.observations) {
        double dt = fabs(ob.epoch.jd - elements.epoch.jd);
        if (dt < 365.0) {  // Entro 1 anno dall'epoca
            auto ephem = eph.compute(ob.epoch);
            double dRA = (ob.obs.ra - ephem.geocentricPos.ra) * cos(ob.obs.dec) * 180/M_PI * 3600;
            double dDec = (ob.obs.dec - ephem.geocentricPos.dec) * 180/M_PI * 3600;
            double res = sqrt(dRA*dRA + dDec*dDec);
            residuals.push_back(res);
            
            if (residuals.size() <= 10) {
                std::cout << "JD=" << std::fixed << std::setprecision(2) << ob.epoch.jd 
                          << " dt=" << std::setw(6) << (int)dt 
                          << " giorni, residuo=" << std::setw(8) << (int)res << "\"\n";
            }
        }
    }
    
    if (!residuals.empty()) {
        double sum = 0;
        for (double r : residuals) sum += r*r;
        double rms = sqrt(sum / residuals.size());
        std::cout << "\nRMS (osservazioni entro 1 anno): " << rms << "\" su " 
                  << residuals.size() << " osservazioni\n";
    }
    
    return 0;
}
