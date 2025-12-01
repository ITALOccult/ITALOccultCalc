#include "ioccultcalc/chebyshev_approximation.h"
#include "ioccultcalc/ephemeris.h"
#include <cmath>
#include <stdexcept>
#include <algorithm>

namespace ioccultcalc {

ChebyshevApproximation::ChebyshevApproximation(const ChebyshevConfig& cfg)
    : config_(cfg) {}

bool ChebyshevApproximation::generate(Ephemeris& ephemeris, double startJD, double endJD) {
    segments_.clear();
    if (endJD <= startJD) return false;
    
    double currentJD = startJD;
    while (currentJD < endJD) {
        double segmentEnd = std::min(currentJD + config_.segmentDays, endJD);
        ChebyshevSegment segment;
        if (!generateSegment(ephemeris, currentJD, segmentEnd, segment)) return false;
        segments_.push_back(segment);
        currentJD = segmentEnd;
    }
    return !segments_.empty();
}

bool ChebyshevApproximation::generateSegment(Ephemeris& ephemeris, double startJD, double endJD, ChebyshevSegment& segment) {
    segment.startJD = startJD;
    segment.endJD = endJD;
    segment.midJD = (startJD + endJD) / 2.0;
    segment.halfSpan = (endJD - startJD) / 2.0;
    
    int n = config_.order + 1;
    std::vector<double> xVals(n), yVals(n), zVals(n);
    
    for (int i = 0; i < n; i++) {
        double theta = M_PI * (i + 0.5) / n;
        double x = -std::cos(theta);
        double jd = segment.midJD + x * segment.halfSpan;
        
        JulianDate epoch;
        epoch.jd = jd;
        EphemerisData eph = ephemeris.compute(epoch);
        
        double px, py, pz;
        if (config_.geocentric) {
            // NOTA: eph.geocentricPos.ra/dec sono GIÀ in radianti (vedi types.h)
            double ra_rad = eph.geocentricPos.ra;
            double dec_rad = eph.geocentricPos.dec;
            double r = eph.distance;
            px = r * std::cos(dec_rad) * std::cos(ra_rad);
            py = r * std::cos(dec_rad) * std::sin(ra_rad);
            pz = r * std::sin(dec_rad);
        } else {
            throw std::runtime_error("Solo geocentrico supportato");
        }
        xVals[i] = px;
        yVals[i] = py;
        zVals[i] = pz;
    }
    
    segment.coeffX = computeChebyshevCoefficients(xVals);
    segment.coeffY = computeChebyshevCoefficients(yVals);
    segment.coeffZ = computeChebyshevCoefficients(zVals);
    return true;
}

std::vector<double> ChebyshevApproximation::computeChebyshevCoefficients(const std::vector<double>& y) const {
    int n = y.size();
    std::vector<double> coeffs(config_.order + 1, 0.0);
    
    for (int j = 0; j <= config_.order; j++) {
        double sum = 0.0;
        for (int i = 0; i < n; i++) {
            double theta = M_PI * (i + 0.5) / n;
            sum += y[i] * std::cos(j * theta);
        }
        coeffs[j] = (j == 0) ? (sum / n) : (2.0 * sum / n);
    }
    return coeffs;
}

bool ChebyshevApproximation::evaluate(double jd, double& ra, double& dec, double& distance) const {
    const ChebyshevSegment* segment = nullptr;
    for (const auto& seg : segments_) {
        if (jd >= seg.startJD && jd <= seg.endJD) {
            segment = &seg;
            break;
        }
    }
    if (!segment) return false;
    
    double t = normalizeTime(jd, *segment);
    double x = evaluateChebyshevPolynomial(segment->coeffX, t);
    double y = evaluateChebyshevPolynomial(segment->coeffY, t);
    double z = evaluateChebyshevPolynomial(segment->coeffZ, t);
    
    distance = std::sqrt(x*x + y*y + z*z);
    if (distance == 0.0) return false;
    
    ra = std::atan2(y, x) * 180.0 / M_PI;
    if (ra < 0.0) ra += 360.0;
    dec = std::asin(z / distance) * 180.0 / M_PI;
    return true;
}

bool ChebyshevApproximation::evaluateStateVector(double jd, double pos[3], double vel[3]) const {
    const ChebyshevSegment* segment = nullptr;
    for (const auto& seg : segments_) {
        if (jd >= seg.startJD && jd <= seg.endJD) {
            segment = &seg;
            break;
        }
    }
    if (!segment) return false;
    
    double t = normalizeTime(jd, *segment);
    pos[0] = evaluateChebyshevPolynomial(segment->coeffX, t);
    pos[1] = evaluateChebyshevPolynomial(segment->coeffY, t);
    pos[2] = evaluateChebyshevPolynomial(segment->coeffZ, t);
    vel[0] = evaluateChebyshevDerivative(segment->coeffX, t) / segment->halfSpan;
    vel[1] = evaluateChebyshevDerivative(segment->coeffY, t) / segment->halfSpan;
    vel[2] = evaluateChebyshevDerivative(segment->coeffZ, t) / segment->halfSpan;
    return true;
}

double ChebyshevApproximation::evaluateChebyshevPolynomial(const std::vector<double>& coeffs, double x) const {
    if (coeffs.empty()) return 0.0;
    if (coeffs.size() == 1) return coeffs[0];
    
    double b_k2 = 0.0, b_k1 = 0.0;
    for (int k = coeffs.size() - 1; k >= 0; k--) {
        double b_k = coeffs[k] + 2.0 * x * b_k1 - b_k2;
        b_k2 = b_k1;
        b_k1 = b_k;
    }
    return b_k1 - x * b_k2;
}

double ChebyshevApproximation::evaluateChebyshevDerivative(const std::vector<double>& coeffs, double x) const {
    if (coeffs.size() <= 1) return 0.0;
    std::vector<double> derivCoeffs(coeffs.size() - 1);
    for (size_t n = 1; n < coeffs.size(); n++) {
        derivCoeffs[n - 1] = n * coeffs[n];
    }
    return evaluateChebyshevPolynomial(derivCoeffs, x);
}

double ChebyshevApproximation::estimateMaxError(Ephemeris& ephemeris) const {
    double maxError = 0.0;
    for (const auto& seg : segments_) {
        for (int i = 0; i < 20; i++) {
            double fraction = (i + 0.3) / 20.0;
            double jd = seg.startJD + fraction * (seg.endJD - seg.startJD);
            
            JulianDate epoch;
            epoch.jd = jd;
            EphemerisData exact = ephemeris.compute(epoch);
            
            double ra_approx, dec_approx, dist_approx;
            if (!evaluate(jd, ra_approx, dec_approx, dist_approx)) continue;
            
            double dRA = (exact.geocentricPos.ra - ra_approx) * std::cos(exact.geocentricPos.dec * M_PI / 180.0);
            double dDec = exact.geocentricPos.dec - dec_approx;
            double error = std::sqrt(dRA*dRA + dDec*dDec) * 3600.0;
            maxError = std::max(maxError, error);
        }
    }
    return maxError;
}

double ChebyshevApproximation::normalizeTime(double jd, const ChebyshevSegment& segment) const {
    return (jd - segment.midJD) / segment.halfSpan;
}

} // namespace ioccultcalc
