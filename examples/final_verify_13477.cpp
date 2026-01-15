#include "astdyn_wrapper.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main() {
    AstDynWrapper wrapper;
    
    // Elementi AstDyS estratti dal DB per 13477 Utkin
    // Epoca: MJD 61000.0 (21 Nov 2025)
    // Frame: Ecliptic J2000
    double a = 2.4485080003194;
    double e = 0.143329820474419;
    double i = 0.147662766626375; // rad (8.46045 deg)
    double Omega = 1.18281763160276; // rad (67.770 deg)
    double omega = 4.73269210633698; // rad (271.163 deg)
    double M = 1.03166444813114; // rad (59.110 deg)
    double epoch = 61000.0;
    
    std::cout << "[VERIFY] Caricamento elementi AstDyS corriggi epoca (MJD 61000.0)..." << std::endl;
    wrapper.setKeplerianElements(a, e, i, Omega, omega, M, epoch, "13477", 
                                astdyn::propagation::HighPrecisionPropagator::InputFrame::ECLIPTIC);
    
    // Target: Jan 20, 2026 (Benchmark epoch)
    double target_epoch = 61060.00657; 
    
    std::cout << "[VERIFY] Propagazione al " << target_epoch << "..." << std::endl;
    auto state = wrapper.propagateToEpoch(target_epoch);
    
    // Ottieni elementi finali (Equatorial ICRF)
    auto final_kep = wrapper.getKeplerianElements();
    
    std::cout << "\n--- Elementi Finali (Equatorial ICRF) ---" << std::endl;
    std::cout << "MJD:   " << std::fixed << std::setprecision(5) << final_kep.epoch_mjd_tdb << std::endl;
    std::cout << "a:     " << final_kep.semi_major_axis << " AU" << std::endl;
    std::cout << "e:     " << final_kep.eccentricity << std::endl;
    std::cout << "i:     " << final_kep.inclination * 180.0 / M_PI << " deg (Target: ~27.08)" << std::endl;
    std::cout << "Omega: " << final_kep.longitude_asc_node * 180.0 / M_PI << " deg" << std::endl;
    std::cout << "omega: " << final_kep.argument_perihelion * 180.0 / M_PI << " deg" << std::endl;
    std::cout << "M:     " << final_kep.mean_anomaly * 180.0 / M_PI << " deg (Target Benchmark: ~74.45)" << std::endl;
    
    // Posizione apparente
    auto app = wrapper.getApparentStateGeocentric(target_epoch);
    std::cout << "\n--- Posizione Apparente Geocentrica (ICRF) ---" << std::endl;
    std::cout << "RA:  " << app.ra_deg << " deg (Target HIP 13752: 44.256)" << std::endl;
    std::cout << "Dec: " << app.dec_deg << " deg (Target HIP 13752: 17.245)" << std::endl;
    
    double ra_err_asec = (app.ra_deg - 44.2565) * 3600.0 * std::cos(app.dec_deg * M_PI / 180.0);
    double dec_err_asec = (app.dec_deg - 17.2452) * 3600.0;
    
    std::cout << "\n--- Errori vs Stella HIP 13752 ---" << std::endl;
    std::cout << "Errore RAcosD: " << ra_err_asec << " arcsec" << std::endl;
    std::cout << "Errore Dec:    " << dec_err_asec << " arcsec" << std::endl;
    
    return 0;
}
