#include "ioccultcalc/chebyshev_detector.h"
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/types.h"
#include <cmath>
#include <algorithm>

namespace ioccultcalc {

ChebyshevOccultationDetector::ChebyshevOccultationDetector(const Config& cfg)
    : config_(cfg) {}

bool ChebyshevOccultationDetector::initialize(Ephemeris& ephemeris, double startJD, double endJD) {
    approximation_ = std::make_unique<ChebyshevApproximation>(config_.order);
    
    // Sample positions for the fitting
    std::vector<Eigen::Vector3d> positions;
    int nPoints = std::max(16, (int)config_.order * 2);
    for (int i = 0; i < nPoints; ++i) {
        double jd = startJD + (endJD - startJD) * i / (nPoints - 1);
        EphemerisData data = ephemeris.compute(JulianDate(jd));
        
        // Convert to Cartesian (Geocentric Equatorial J2000)
        double r = data.distance;
        double ra_rad = data.geocentricPos.ra;
        double dec_rad = data.geocentricPos.dec;
        
        double x = r * std::cos(dec_rad) * std::cos(ra_rad);
        double y = r * std::cos(dec_rad) * std::sin(ra_rad);
        double z = r * std::sin(dec_rad);
        
        positions.push_back(Eigen::Vector3d(x, y, z));
    }

    bool success = approximation_->fit(positions, startJD - 2400000.5, endJD - 2400000.5);
    
    if (success) {
        startJD_ = startJD;
        endJD_ = endJD;
    }
    return success;
}

std::vector<OccultationCandidate> ChebyshevOccultationDetector::findCandidates(
    const std::vector<std::pair<double, double>>& stars) const {
    
    if (!approximation_) return {};
    
    std::vector<OccultationCandidate> candidates;
    candidates.reserve(stars.size() / 100);
    
    for (size_t starIdx = 0; starIdx < stars.size(); starIdx++) {
        double ra = stars[starIdx].first;
        double dec = stars[starIdx].second;
        
        OccultationCandidate candidate = findCandidate(starIdx, ra, dec);
        
        if (candidate.minDistArcsec < config_.thresholdArcsec) {
            candidates.push_back(candidate);
        }
    }
    return candidates;
}

OccultationCandidate ChebyshevOccultationDetector::findCandidate(int starIndex, double ra, double dec) const {
    OccultationCandidate result;
    result.starIndex = starIndex;
    result.jd = 0.0;
    result.minDistArcsec = 1e10;
    result.uncertaintyArcsec = 0.0;
    
    if (!approximation_) return result;
    
    double jdStart = startJD_;
    double jdEnd = endJD_;
    int nSamples = 100;
    
    double minDist = 1e10;
    double minJD = (jdStart + jdEnd) / 2.0;
    
    for (int i = 0; i < nSamples; i++) {
        double jd = jdStart + (jdEnd - jdStart) * i / (nSamples - 1);
        
        // Evaluate position using MJD
        double mjd = jd - 2400000.5;
        Eigen::Vector3d pos = approximation_->evaluatePosition(mjd);
        double r = pos.norm();
        double ast_dec = std::asin(pos.z() / r) * 180.0 / M_PI;
        double ast_ra = std::atan2(pos.y(), pos.x()) * 180.0 / M_PI;
        if (ast_ra < 0) ast_ra += 360.0;
        
        double dist = angularDistance(ast_ra, ast_dec, ra, dec);
        
        if (dist < minDist) {
            minDist = dist;
            minJD = jd;
        }
    }
    
    if (minDist < config_.refinementArcsec) {
        jdStart = minJD - 0.1;
        jdEnd = minJD + 0.1;
        nSamples = 50;
        
        for (int i = 0; i < nSamples; i++) {
            double jd = jdStart + (jdEnd - jdStart) * i / (nSamples - 1);
            
            // Evaluate position using MJD
            double mjd = jd - 2400000.5;
            Eigen::Vector3d pos = approximation_->evaluatePosition(mjd);
            double r = pos.norm();
            double ast_dec = std::asin(pos.z() / r) * 180.0 / M_PI;
            double ast_ra = std::atan2(pos.y(), pos.x()) * 180.0 / M_PI;
            if (ast_ra < 0) ast_ra += 360.0;
            
            double dist = angularDistance(ast_ra, ast_dec, ra, dec);
            
            if (dist < minDist) {
                minDist = dist;
                minJD = jd;
            }
        }
    }
    
    result.jd = minJD;
    result.minDistArcsec = minDist;
    result.uncertaintyArcsec = 10.0;
    return result;
}

double ChebyshevOccultationDetector::angularDistance(double ra1, double dec1, double ra2, double dec2) const {
    double ra1_rad = ra1 * M_PI / 180.0;
    double dec1_rad = dec1 * M_PI / 180.0;
    double ra2_rad = ra2 * M_PI / 180.0;
    double dec2_rad = dec2 * M_PI / 180.0;
    
    double dra = ra2_rad - ra1_rad;
    double ddec = dec2_rad - dec1_rad;
    
    double a = sin(ddec/2.0) * sin(ddec/2.0) +
               cos(dec1_rad) * cos(dec2_rad) *
               sin(dra/2.0) * sin(dra/2.0);
    
    double c = 2.0 * atan2(sqrt(a), sqrt(1.0 - a));
    return c * 180.0 / M_PI * 3600.0;
}

} // namespace ioccultcalc
