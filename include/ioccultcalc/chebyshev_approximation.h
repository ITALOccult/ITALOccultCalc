#ifndef IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H
#define IOCCULTCALC_CHEBYSHEV_APPROXIMATION_H

#include <vector>
#include <memory>

namespace ioccultcalc {

class Ephemeris;

struct ChebyshevConfig {
    int order = 11;
    double segmentDays = 1.0;
    bool geocentric = true;
};

struct ChebyshevSegment {
    double startJD;
    double endJD;
    double midJD;
    double halfSpan;
    std::vector<double> coeffX;
    std::vector<double> coeffY;
    std::vector<double> coeffZ;
};

class ChebyshevApproximation {
public:
    explicit ChebyshevApproximation(const ChebyshevConfig& cfg);
    
    bool generate(Ephemeris& ephemeris, double startJD, double endJD);
    
    bool evaluate(double jd, double& ra, double& dec, double& distance) const;
    
    bool evaluateStateVector(double jd, double pos[3], double vel[3]) const;
    
    double estimateMaxError(Ephemeris& ephemeris) const;
    
    const std::vector<ChebyshevSegment>& getSegments() const { return segments_; }
    
    const ChebyshevConfig& getConfig() const { return config_; }

private:
    ChebyshevConfig config_;
    std::vector<ChebyshevSegment> segments_;
    
    bool generateSegment(Ephemeris& ephemeris, double startJD, double endJD, ChebyshevSegment& segment);
    
    std::vector<double> computeChebyshevCoefficients(const std::vector<double>& y) const;
    
    double evaluateChebyshevPolynomial(const std::vector<double>& coeffs, double x) const;
    
    double evaluateChebyshevDerivative(const std::vector<double>& coeffs, double x) const;
    
    double normalizeTime(double jd, const ChebyshevSegment& segment) const;
};

} // namespace ioccultcalc

#endif
