#include "astdyn_wrapper.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main() {
    AstDynWrapper wrapper;
    
    // Elementi JPL dal benchmark XML (JPL#62)
    // Epoca: MJD 61059.0 (19 Gen 2026)
    // Frame: EQUATORIAL ICRF
    double a = 2.449176313176686;
    double e = 0.143380720454395;
    double i = 27.08630138927896 * M_PI / 180.0;
    double Omega = 58.7511993466185 * M_PI / 180.0;
    double omega = 277.4206536643689 * M_PI / 180.0;
    double M = 74.24851239617265 * M_PI / 180.0;
    double epoch = 61059.0;
    
    std::cout << "[VERIFY] Caricamento elementi JPL (ICRF)..." << std::endl;
    wrapper.setKeplerianElements(a, e, i, Omega, omega, M, epoch, "13477_JPL", 
                                astdyn::propagation::HighPrecisionPropagator::InputFrame::EQUATORIAL);
    
    // Target: Jan 20, 2026 
    double target_epoch = 61060.00657; 
    
    std::cout << "[VERIFY] Propagazione al " << target_epoch << "..." << std::endl;
    auto state = wrapper.propagateToEpoch(target_epoch);
    
    // Posizione apparente
    auto app = wrapper.getApparentStateGeocentric(target_epoch);
    std::cout << "\n--- Posizione Apparente Geocentrica (ICRF) ---" << std::endl;
    std::cout << "RA:  " << app.ra_deg << " deg (Target: 44.256)" << std::endl;
    std::cout << "Dec: " << app.dec_deg << " deg (Target: 17.245)" << std::endl;
    
    double ra_err_asec = (app.ra_deg - 44.256) * 3600.0 * std::cos(app.dec_deg * M_PI / 180.0);
    double dec_err_asec = (app.dec_deg - 17.245) * 3600.0;
    
    std::cout << "\n--- Errori vs Target ---" << std::endl;
    std::cout << "Errore RAcosD: " << ra_err_asec << " arcsec" << std::endl;
    std::cout << "Errore Dec:    " << dec_err_asec << " arcsec" << std::endl;
    
    return 0;
}
