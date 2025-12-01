#include "ioccultcalc/chebyshev_detector.h"
#include <cmath>
#include <algorithm>

namespace ioccultcalc {

ChebyshevOccultationDetector::ChebyshevOccultationDetector(const Config& cfg)
    : config_(cfg) {}

bool ChebyshevOccultationDetector::initialize(Ephemeris& ephemeris, double startJD, double endJD) {
    ChebyshevConfig chebConfig;
    chebConfig.order = config_.order;
    chebConfig.segmentDays = config_.segmentDays;
    chebConfig.geocentric = true;
    
    approximation_ = std::make_unique<ChebyshevApproximation>(chebConfig);
    bool success = approximation_->generate(ephemeris, startJD, endJD);
    
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
        
        double ast_ra, ast_dec, ast_dist;
        if (!approximation_->evaluate(jd, ast_ra, ast_dec, ast_dist)) continue;
        
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
            
            double ast_ra, ast_dec, ast_dist;
            if (!approximation_->evaluate(jd, ast_ra, ast_dec, ast_dist)) continue;
            
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
