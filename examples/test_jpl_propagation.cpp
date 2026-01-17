#include "astdyn_wrapper.h"
#include <iostream>
#include <iomanip>

int main() {
    using namespace ioccultcalc;
    auto astdyn = std::make_shared<AstDynWrapper>();
    
    // High Precision State for 13477 (JPL #62)
    // Epoch: MJD 58413.0 (2458413.5 JD TDB)
    double epoch = 58413.0;
    
    // Equivalent ICRF heliocentric cartesian coordinates (au, au/d)
    Eigen::Vector3d pos(-7.318092516491381E-01,  2.208447534461022E+00,  1.221168056068470E+00);
    Eigen::Vector3d vel(-9.991989309288787E-03, -2.071950592819523E-03,  4.981877610044968E-04);
    
    std::cout << "--- JPL High Precision Calibration Test (13477) ---" << std::endl;
    astdyn->setCartesianElements(pos, vel, epoch, "13477", 
                                astdyn::propagation::HighPrecisionPropagator::InputFrame::EQUATORIAL);
    
    // Target Time (Horizons UT): 2026-Jan-20 03:20:53
    // MJD = 61060.13950231
    double target_mjd = 61060.13950231;
    auto state = astdyn->getApparentStateGeocentric(target_mjd);
    
    std::cout << "Target Time (UT): MJD " << std::fixed << std::setprecision(6) << target_mjd << std::endl;
    std::cout << "RA Calculated:  " << std::setprecision(8) << state.ra_deg << " deg" << std::endl;
    std::cout << "Dec Calculated: " << std::setprecision(8) << state.dec_deg << " deg" << std::endl;
    std::cout << "Dist Calculated: " << std::setprecision(8) << state.distance_au << " AU" << std::endl;

    // Truth from Horizons Snippet:
    // RA (ICRF): 02 57 06.40 -> 44.27666667 deg
    // Dec (ICRF): +17 15 04.5 -> 17.25125000 deg
    // Delta (AU): 1.92235469
    double truth_ra = 44.27666667;
    double truth_dec = 17.25125000;
    double truth_dist = 1.92235469;
    
    double dra = (state.ra_deg - truth_ra) * 3600.0 * std::cos(truth_dec * M_PI / 180.0);
    double ddec = (state.dec_deg - truth_dec) * 3600.0;
    double dist_err = std::sqrt(dra*dra + ddec*ddec);
    
    std::cout << "--- Comparison with Horizons TRUTH ---" << std::endl;
    std::cout << "dRA:  " << std::setprecision(3) << dra << " arcsec" << std::endl;
    std::cout << "dDec: " << std::setprecision(3) << ddec << " arcsec" << std::endl;
    std::cout << "Total Offset: " << std::setprecision(3) << dist_err << " arcsec" << std::endl;
    std::cout << "Dist Error:   " << std::setprecision(8) << (state.distance_au - truth_dist) << " AU" << std::endl;
    
    return 0;
}
