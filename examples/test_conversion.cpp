#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/mpc_client.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main() {
    // 1. Carica elementi
    AstDysClient astdys;
    auto elements = astdys.getElements("433");
    
    std::cout << "Elementi equinoziali:\n";
    std::cout << "  a = " << elements.a << " AU\n";
    std::cout << "  h = " << elements.h << "\n";
    std::cout << "  k = " << elements.k << "\n";
    std::cout << "  epoch = " << elements.epoch.jd << "\n\n";
    
    // 2. Converti in Keplerian
    auto kep = elements.toKeplerian();
    std::cout << "Elementi Kepleriani:\n";
    std::cout << "  a = " << kep.a << " AU\n";
    std::cout << "  e = " << kep.e << "\n";
    std::cout << "  i = " << kep.i * 180/M_PI << " deg\n";
    std::cout << "  epoch = " << kep.epoch.jd << "\n\n";
    
    // 3. Riconverti in equinoziali
    auto eq2 = kep.toEquinoctial();
    std::cout << "Riconvertito in equinoziali:\n";
    std::cout << "  a = " << eq2.a << " AU\n";
    std::cout << "  h = " << eq2.h << "\n";
    std::cout << "  k = " << eq2.k << "\n\n";
    
    // 4. Calcola posizione con elementi originali
    Ephemeris eph1(elements);
    MPCClient mpc;
    auto obs = mpc.getObservations("433");
    
    std::cout << "Epoca elementi: " << elements.epoch.jd << "\n";
    
    if (!obs.observations.empty()) {
        // Trova osservazione vicina all'epoca degli elementi
        size_t bestIdx = 0;
        double minDt = 999999;
        for (size_t i = 0; i < obs.observations.size(); i++) {
            double dt = fabs(obs.observations[i].epoch.jd - elements.epoch.jd);
            if (dt < minDt) {
                minDt = dt;
                bestIdx = i;
            }
        }
        
        auto bestObs = obs.observations[bestIdx];
        auto ephem1 = eph1.compute(bestObs.epoch);
        
        std::cout << "Osservazione più vicina (Δt=" << minDt << " giorni): " << bestObs.epoch.jd << "\n";
        std::cout << "  Osservato:  RA=" << std::setprecision(6) << bestObs.obs.ra * 180/M_PI 
                  << " deg, Dec=" << bestObs.obs.dec * 180/M_PI << " deg\n";
        std::cout << "  Calcolato:  RA=" << ephem1.geocentricPos.ra * 180/M_PI 
                  << " deg, Dec=" << ephem1.geocentricPos.dec * 180/M_PI << " deg\n";
        
        double dRA = (bestObs.obs.ra - ephem1.geocentricPos.ra) * cos(bestObs.obs.dec) * 180/M_PI * 3600;
        double dDec = (bestObs.obs.dec - ephem1.geocentricPos.dec) * 180/M_PI * 3600;
        std::cout << "  Residui:    dRA=" << dRA << "\", dDec=" << dDec << "\"\n\n";
        
        // 5. Prova con elementi Keplerian -> equinoziali
        Ephemeris eph2(eq2);
        auto ephem2 = eph2.compute(bestObs.epoch);
        std::cout << "  Con riconversione:\n";
        std::cout << "    RA=" << ephem2.geocentricPos.ra * 180/M_PI 
                  << " deg, Dec=" << ephem2.geocentricPos.dec * 180/M_PI << " deg\n";
        
        dRA = (bestObs.obs.ra - ephem2.geocentricPos.ra) * cos(bestObs.obs.dec) * 180/M_PI * 3600;
        dDec = (bestObs.obs.dec - ephem2.geocentricPos.dec) * 180/M_PI * 3600;
        std::cout << "    Residui: dRA=" << dRA << "\", dDec=" << dDec << "\"\n";
    }
    
    return 0;
}
