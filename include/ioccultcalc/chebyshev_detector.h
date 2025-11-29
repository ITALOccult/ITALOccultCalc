#ifndef IOCCULTCALC_CHEBYSHEV_DETECTOR_H
#define IOCCULTCALC_CHEBYSHEV_DETECTOR_H

#include "chebyshev_approximation.h"
#include <vector>
#include <memory>

namespace ioccultcalc {

class Ephemeris;

struct OccultationCandidate {
    int starIndex;
    double jd;
    double minDistArcsec;
    double uncertaintyArcsec;
};

class ChebyshevOccultationDetector {
public:
    struct Config {
        int order = 11;
        double segmentDays = 1.0;
        double thresholdArcsec = 300.0;
        double refinementArcsec = 60.0;
        bool verbose = false;
    };
    
    explicit ChebyshevOccultationDetector(const Config& cfg);
    
    bool initialize(Ephemeris& ephemeris, double startJD, double endJD);
    
    std::vector<OccultationCandidate> findCandidates(const std::vector<std::pair<double, double>>& stars) const;
    
    OccultationCandidate findCandidate(int starIndex, double ra, double dec) const;

private:
    Config config_;
    std::unique_ptr<ChebyshevApproximation> approximation_;
    double startJD_;
    double endJD_;
    
    double angularDistance(double ra1, double dec1, double ra2, double dec2) const;
};

} // namespace ioccultcalc

#endif
