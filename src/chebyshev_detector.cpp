/**/**#include "ioccultcalc/chebyshev_detector.h"

 * @file chebyshev_detector.cpp

 * @brief Implementazione detector occultazioni con Chebyshev * @file chebyshev_detector.cpp#include <cmath>

 * 

 * @author Michele Bigi * @brief Implementazione detector occultazioni con Chebyshev#include <algorithm>

 * @date 2025-11-29

 */ */#include <stdexcept>



#include "ioccultcalc/chebyshev_detector.h"#include <iostream>

#include <cmath>

#include <algorithm>#include "ioccultcalc/chebyshev_detector.h"

#include <stdexcept>

#include <cmath>namespace ioccultcalc {

namespace ioccultcalc {

#include <algorithm>

ChebyshevOccultationDetector::ChebyshevOccultationDetector(const Config& config)

    : config_(config), startJD_(0), endJD_(0), initialized_(false) {#include <stdexcept>// ============================================================================

}

// ChebyshevPolynomial Implementation

ChebyshevOccultationDetector::~ChebyshevOccultationDetector() {

}namespace ioccultcalc {// ============================================================================



bool ChebyshevOccultationDetector::initialize(const Ephemeris& ephemeris, 

                                              double startJD, double endJD) {

    startJD_ = startJD;ChebyshevOccultationDetector::ChebyshevOccultationDetector(const Config& config)ChebyshevPolynomial::ChebyshevPolynomial(double t0, double t1, 

    endJD_ = endJD;

        : config_(config), startJD_(0), endJD_(0) {                                         const std::vector<double>& coeffs)

    // Crea configurazione Chebyshev

    ChebyshevConfig chebConfig;        : m_t0(t0), m_t1(t1), m_coeffs(coeffs) {

    chebConfig.order = config_.order;

    chebConfig.segmentDays = config_.segmentDays;    // Crea l'approssimazione con configurazione appropriata}

    

    // Genera approssimazione    ChebyshevConfig approxConfig;

    approximation_ = std::make_unique<ChebyshevApproximation>(chebConfig);

    bool success = approximation_->generate(ephemeris, startJD, endJD);    approxConfig.order = config_.order;double ChebyshevPolynomial::normalizeTime(double t) const {

    

    initialized_ = success;    approxConfig.segmentDays = config_.segmentDays;    return 2.0 * (t - m_t0) / (m_t1 - m_t0) - 1.0;

    return success;

}    approxConfig.geocentric = true;  // Usa sempre coordinate geocentriche}



std::vector<OccultationCandidate> ChebyshevOccultationDetector::findCandidates(    

    const std::vector<std::pair<double, double>>& stars) {

        approximation_ = std::make_unique<ChebyshevApproximation>(approxConfig);double ChebyshevPolynomial::denormalizeTime(double x) const {

    if (!initialized_) {

        throw std::runtime_error("ChebyshevOccultationDetector not initialized");}    return m_t0 + (x + 1.0) * (m_t1 - m_t0) / 2.0;

    }

    }

    std::vector<OccultationCandidate> candidates;

    candidates.reserve(stars.size() / 10);  // Stima: ~10% candidatiChebyshevOccultationDetector::~ChebyshevOccultationDetector() {

    

    for (size_t i = 0; i < stars.size(); i++) {}double ChebyshevPolynomial::evaluateNormalized(double x) const {

        OccultationCandidate candidate = findCandidate(stars[i].first, 

                                                       stars[i].second, i);    // Algoritmo di Clenshaw per stabilità numerica

        if (candidate.starIndex >= 0) {

            candidates.push_back(candidate);bool ChebyshevOccultationDetector::initialize(const Ephemeris& ephemeris,    int n = m_coeffs.size() - 1;

        }

    }                                               double startJD, double endJD) {    if (n < 0) return 0.0;

    

    return candidates;    startJD_ = startJD;    

}

    endJD_ = endJD;    double b_k2 = 0.0;

OccultationCandidate ChebyshevOccultationDetector::findCandidate(

    double ra, double dec, int starIndex) {        double b_k1 = 0.0;

    

    if (!initialized_) {    // Genera l'approssimazione di Chebyshev per il periodo    

        throw std::runtime_error("ChebyshevOccultationDetector not initialized");

    }    bool success = approximation_->generate(ephemeris, startJD, endJD);    for (int k = n; k >= 1; k--) {

    

    // Cerca minimo distanza nell'intervallo temporale            double b_k = 2.0 * x * b_k1 - b_k2 + m_coeffs[k];

    auto [jdMin, distMin] = findMinDistance(ra, dec, startJD_, endJD_);

        if (config_.verbose && success) {        b_k2 = b_k1;

    // Verifica se sotto soglia

    if (distMin < config_.thresholdArcsec) {        double maxError = approximation_->estimateMaxError(ephemeris, 50);        b_k1 = b_k;

        OccultationCandidate candidate;

        candidate.starIndex = starIndex;        // Note: std::cout output removed for production, add logging if needed    }

        candidate.jd = jdMin;

        candidate.minDistArcsec = distMin;    }    

        candidate.uncertaintyArcsec = config_.refinementArcsec;

        return candidate;        return x * b_k1 - b_k2 + m_coeffs[0];

    }

        return success;}

    return OccultationCandidate();  // starIndex = -1

}}



std::pair<double, double> ChebyshevOccultationDetector::findMinDistance(double ChebyshevPolynomial::evaluate(double t) const {

    double ra, double dec, double jdStart, double jdEnd) const {

    std::vector<ChebyshevCandidate> ChebyshevOccultationDetector::findCandidates(    double x = normalizeTime(t);

    const int nSamples = 100;

    double minDist = 1e10;    const std::vector<std::pair<double, double>>& stars) const {    return evaluateNormalized(x);

    double minJD = (jdStart + jdEnd) / 2.0;

        }

    // Prima pass: campionamento grossolano

    for (int i = 0; i < nSamples; i++) {    std::vector<ChebyshevCandidate> candidates;

        double jd = jdStart + (jdEnd - jdStart) * i / (nSamples - 1);

        EquatorialCoordinates asteroidPos = approximation_->evaluate(jd);    candidates.reserve(stars.size() / 100);  // Stima conservativaChebyshevPolynomial ChebyshevPolynomial::derivative() const {

        double dist = angularDistance(asteroidPos.ra, asteroidPos.dec, ra, dec);

                // La derivata di un polinomio Chebyshev p(x) = Σ cₖ Tₖ(x)

        if (dist < minDist) {

            minDist = dist;    for (size_t i = 0; i < stars.size(); i++) {    // è p'(x) = Σ c'ₖ Tₖ(x) dove:

            minJD = jd;

        }        ChebyshevCandidate candidate = findCandidatesForStar(stars[i].first, stars[i].second);    // c'ₙ₋₁ = 2n cₙ

    }

                // c'ₖ = c'ₖ₊₂ + 2(k+1) cₖ₊₁  per k = n-2, ..., 0

    // Seconda pass: refinement locale

    double window = (jdEnd - jdStart) / nSamples * 2.0;        if (candidate.starIndex >= 0) {    

    double refineStart = std::max(jdStart, minJD - window);

    double refineEnd = std::min(jdEnd, minJD + window);            candidate.starIndex = static_cast<int>(i);    int n = m_coeffs.size() - 1;

    

    for (int i = 0; i < 50; i++) {            candidates.push_back(candidate);    if (n <= 0) {

        double jd = refineStart + (refineEnd - refineStart) * i / 49.0;

        EquatorialCoordinates asteroidPos = approximation_->evaluate(jd);        }        return ChebyshevPolynomial(m_t0, m_t1, {0.0});

        double dist = angularDistance(asteroidPos.ra, asteroidPos.dec, ra, dec);

            }    }

        if (dist < minDist) {

            minDist = dist;        

            minJD = jd;

        }    return candidates;    std::vector<double> dcoeffs(n, 0.0);

    }

    }    

    return {minJD, minDist};

}    if (n >= 1) {



double ChebyshevOccultationDetector::angularDistance(ChebyshevCandidate ChebyshevOccultationDetector::findCandidatesForStar(        dcoeffs[n-1] = 2.0 * n * m_coeffs[n];

    double ra1, double dec1, double ra2, double dec2) const {

        double ra, double dec) const {    }

    // Converti a radianti

    double ra1_rad = ra1 * M_PI / 180.0;        

    double dec1_rad = dec1 * M_PI / 180.0;

    double ra2_rad = ra2 * M_PI / 180.0;    ChebyshevCandidate best;    for (int k = n - 2; k >= 0; k--) {

    double dec2_rad = dec2 * M_PI / 180.0;

        best.minDistArcsec = 1e9;        dcoeffs[k] = (k + 2 < n ? dcoeffs[k + 2] : 0.0) + 2.0 * (k + 1) * m_coeffs[k + 1];

    // Formula haversine

    double dra = ra2_rad - ra1_rad;        }

    double ddec = dec2_rad - dec1_rad;

        // Cerca in ogni segmento    

    double a = sin(ddec/2) * sin(ddec/2) + 

               cos(dec1_rad) * cos(dec2_rad) * sin(dra/2) * sin(dra/2);    for (size_t i = 0; i < approximation_->getSegmentCount(); i++) {    // Correggere per il cambio di scala: d/dt = d/dx × dx/dt = d/dx × 2/(t1-t0)

    double c = 2 * atan2(sqrt(a), sqrt(1-a));

            const ChebyshevSegment* segment =     double scale = 2.0 / (m_t1 - m_t0);

    return c * 180.0 / M_PI * 3600.0;  // Converti a arcsec

}            approximation_->findSegment(startJD_ + i * config_.segmentDays);    for (double& c : dcoeffs) {



} // namespace ioccultcalc                c *= scale;


        if (!segment) continue;    }

            

        double minDist, jdMin;    return ChebyshevPolynomial(m_t0, m_t1, dcoeffs);

        if (findMinDistanceInSegment(*segment, ra, dec, minDist, jdMin)) {}

            // Trovato candidato in questo segmento

            if (minDist < best.minDistArcsec) {std::vector<double> ChebyshevPolynomial::findRoots() const {

                best.minDistArcsec = minDist;    // Metodo numerico: sampling + bisection

                best.jd = jdMin;    // Per polinomi Chebyshev, campiona densamente e usa bisection

                best.asteroidPos = approximation_->evaluate(jdMin);    // dove c'è cambio di segno

                best.starIndex = 0;  // Verrà sovrascritto dal chiamante    

            }    int n = m_coeffs.size();

        }    if (n == 0) return {};

    }    if (n == 1) return {};  // Costante

        

    // Verifica se sotto threshold    // Numero di campioni basato sull'ordine del polinomio

    if (best.minDistArcsec < config_.thresholdArcsec) {    int numSamples = std::max(50, 10 * n);

        return best;    double step = (m_t1 - m_t0) / numSamples;

    }    

        std::vector<double> roots;

    // Nessun candidato trovato    

    best.starIndex = -1;    double prevT = m_t0;

    return best;    double prevVal = evaluate(m_t0);

}    

    for (int i = 1; i <= numSamples; i++) {

bool ChebyshevOccultationDetector::findMinDistanceInSegment(        double t = m_t0 + i * step;

    const ChebyshevSegment& segment,        double val = evaluate(t);

    double starRA, double starDec,        

    double& minDistArcsec, double& jdMin) const {        // Cambio di segno → c'è una radice

            if (prevVal * val < 0) {

    // Strategia: cerca punti critici risolvendo d(distanza)/dt = 0            // Bisection per trovare la radice

                double lo = prevT, hi = t;

    // Campiona il segmento per trovare approssimazioni iniziali            double loVal = prevVal, hiVal = val;

    const int numSamples = 20;            

    double bestDist = 1e9;            for (int iter = 0; iter < 50; iter++) {

    double bestJD = segment.midJD;                double mid = (lo + hi) / 2.0;

                    double midVal = evaluate(mid);

    for (int i = 0; i < numSamples; i++) {                

        double t = -1.0 + 2.0 * i / (numSamples - 1);                if (std::abs(midVal) < 1e-14 || (hi - lo) < 1e-12) {

        double jd = segment.midJD + segment.halfSpan * t;                    roots.push_back(mid);

                            break;

        EquatorialCoordinates astPos = approximation_->evaluate(jd);                }

        double dist = angularDistance(astPos.ra, astPos.dec, starRA, starDec);                

                        if (loVal * midVal < 0) {

        if (dist < bestDist) {                    hi = mid;

            bestDist = dist;                    hiVal = midVal;

            bestJD = jd;                } else {

        }                    lo = mid;

    }                    loVal = midVal;

                    }

    // Se già troppo lontano, esci subito            }

    if (bestDist > config_.thresholdArcsec) {        }

        return false;        

    }        prevT = t;

            prevVal = val;

    // Refine con ricerca locale    }

    double xStart = (bestJD - segment.midJD) / segment.halfSpan;    

    double xMin;    // Rimuovi duplicati

        std::sort(roots.begin(), roots.end());

    if (solveForMinimum(segment, starRA, starDec, xStart, xMin)) {    auto last = std::unique(roots.begin(), roots.end(), 

        jdMin = segment.midJD + segment.halfSpan * xMin;        [](double a, double b) { return std::abs(a - b) < 1e-10; });

        EquatorialCoordinates astPos = approximation_->evaluate(jdMin);    roots.erase(last, roots.end());

        minDistArcsec = angularDistance(astPos.ra, astPos.dec, starRA, starDec);    

            return roots;

        return minDistArcsec < config_.thresholdArcsec;}

    }

    std::vector<double> ChebyshevPolynomial::findMinima() const {

    // Fallback: usa il miglior campione    // I minimi sono dove la derivata è zero e la derivata seconda è positiva

    minDistArcsec = bestDist;    ChebyshevPolynomial dp = derivative();

    jdMin = bestJD;    std::vector<double> criticalPoints = dp.findRoots();

    return true;    

}    ChebyshevPolynomial d2p = dp.derivative();

    

bool ChebyshevOccultationDetector::solveForMinimum(    std::vector<double> minima;

    const ChebyshevSegment& segment,    for (double t : criticalPoints) {

    double starRA, double starDec,        if (d2p.evaluate(t) > 0) {

    double xStart, double& xMin) const {            minima.push_back(t);

            }

    // Metodo di Newton per trovare il minimo locale    }

    const int maxIter = 20;    

    const double tolerance = 1e-6;    // Aggiungi anche i bordi come potenziali minimi

        double v0 = evaluate(m_t0);

    double x = xStart;    double v1 = evaluate(m_t1);

        

    for (int iter = 0; iter < maxIter; iter++) {    // Controlla se i bordi sono minimi locali

        // Valuta posizione e derivata    // (sarebbero minimi se la funzione sta crescendo/decrescendo appropriatamente)

        double jd = segment.midJD + segment.halfSpan * x;    

        EquatorialCoordinates pos = approximation_->evaluate(jd);    return minima;

        }

        // Calcola distanza

        double dist = angularDistance(pos.ra, pos.dec, starRA, starDec);ChebyshevPolynomial ChebyshevPolynomial::subtract(double value) const {

            std::vector<double> newCoeffs = m_coeffs;

        // Calcola derivata numericamente (spostamento piccolo)    if (!newCoeffs.empty()) {

        const double h = 1e-5;        newCoeffs[0] -= value;  // T₀(x) = 1, quindi sottraiamo da c₀

        double jdPlus = segment.midJD + segment.halfSpan * (x + h);    }

        EquatorialCoordinates posPlus = approximation_->evaluate(jdPlus);    return ChebyshevPolynomial(m_t0, m_t1, newCoeffs);

        double distPlus = angularDistance(posPlus.ra, posPlus.dec, starRA, starDec);}

        

        double derivative = (distPlus - dist) / h;ChebyshevPolynomial ChebyshevPolynomial::add(const ChebyshevPolynomial& other) const {

            if (std::abs(m_t0 - other.m_t0) > 1e-10 || std::abs(m_t1 - other.m_t1) > 1e-10) {

        // Convergenza?        throw std::runtime_error("Cannot add polynomials with different intervals");

        if (std::abs(derivative) < tolerance) {    }

            xMin = x;    

            return true;    size_t maxSize = std::max(m_coeffs.size(), other.m_coeffs.size());

        }    std::vector<double> result(maxSize, 0.0);

            

        // Step di Newton    for (size_t i = 0; i < m_coeffs.size(); i++) {

        x = x - 0.5 * derivative;  // Fattore 0.5 per stabilità        result[i] += m_coeffs[i];

            }

        // Mantieni in range [-1, 1]    for (size_t i = 0; i < other.m_coeffs.size(); i++) {

        if (x < -1.0) x = -1.0;        result[i] += other.m_coeffs[i];

        if (x > 1.0) x = 1.0;    }

    }    

        return ChebyshevPolynomial(m_t0, m_t1, result);

    xMin = x;}

    return false;  // Non converge perfettamente ma xMin è ragionevole

}ChebyshevPolynomial ChebyshevPolynomial::multiply(const ChebyshevPolynomial& other) const {

    if (std::abs(m_t0 - other.m_t0) > 1e-10 || std::abs(m_t1 - other.m_t1) > 1e-10) {

double ChebyshevOccultationDetector::angularDistance(        throw std::runtime_error("Cannot multiply polynomials with different intervals");

    double ra1, double dec1, double ra2, double dec2) const {    }

        

    // Formula haversine per distanza angolare sulla sfera    // Moltiplicazione di polinomi Chebyshev:

    double ra1Rad = ra1 * M_PI / 180.0;    // Tₘ(x) × Tₙ(x) = ½[Tₘ₊ₙ(x) + T|ₘ₋ₙ|(x)]

    double dec1Rad = dec1 * M_PI / 180.0;    

    double ra2Rad = ra2 * M_PI / 180.0;    int n1 = m_coeffs.size();

    double dec2Rad = dec2 * M_PI / 180.0;    int n2 = other.m_coeffs.size();

        int n_result = n1 + n2 - 1;

    double dRA = ra2Rad - ra1Rad;    

    double dDec = dec2Rad - dec1Rad;    std::vector<double> result(n_result, 0.0);

        

    double a = std::sin(dDec / 2.0) * std::sin(dDec / 2.0) +    for (int i = 0; i < n1; i++) {

               std::cos(dec1Rad) * std::cos(dec2Rad) *        for (int j = 0; j < n2; j++) {

               std::sin(dRA / 2.0) * std::sin(dRA / 2.0);            double prod = m_coeffs[i] * other.m_coeffs[j];

                int sum_idx = i + j;

    double c = 2.0 * std::atan2(std::sqrt(a), std::sqrt(1.0 - a));            int diff_idx = std::abs(i - j);

                

    // Converti in arcsec            // Tᵢ × Tⱼ = ½(Tᵢ₊ⱼ + T|ᵢ₋ⱼ|)

    return c * 180.0 / M_PI * 3600.0;            if (sum_idx < n_result) {

}                result[sum_idx] += 0.5 * prod;

            }

double ChebyshevOccultationDetector::evaluateQuality(const Ephemeris& ephemeris) const {            result[diff_idx] += 0.5 * prod;

    return approximation_->estimateMaxError(ephemeris, 100);        }

}    }

    

} // namespace ioccultcalc    return ChebyshevPolynomial(m_t0, m_t1, result);

}

// ============================================================================
// ChebyshevRADecSegment Implementation
// ============================================================================

ChebyshevRADecSegment::ChebyshevRADecSegment(double t0, double t1, int order)
    : m_t0(t0), m_t1(t1), m_order(order) {
}

void ChebyshevRADecSegment::computeCoefficients(Ephemeris& ephemeris) {
    // Genera nodi Chebyshev
    std::vector<double> nodes;
    for (int i = 0; i <= m_order; i++) {
        double theta = M_PI * (2.0 * i + 1.0) / (2.0 * (m_order + 1));
        nodes.push_back(cos(theta));
    }
    
    // Campiona RA e Dec ai nodi
    std::vector<double> raValues, decValues;
    
    double prevRA = 0.0;
    bool first = true;
    
    for (double node : nodes) {
        double jd = m_t0 + (node + 1.0) * (m_t1 - m_t0) / 2.0;
        EphemerisData eph = ephemeris.compute(JulianDate(jd));
        
        double ra = eph.geocentricPos.ra;
        double dec = eph.geocentricPos.dec;
        
        // Gestisci wrap-around RA (0 → 2π)
        if (!first) {
            while (ra - prevRA > M_PI) ra -= 2 * M_PI;
            while (prevRA - ra > M_PI) ra += 2 * M_PI;
        }
        prevRA = ra;
        first = false;
        
        raValues.push_back(ra);
        decValues.push_back(dec);
    }
    
    // Fit coefficienti Chebyshev
    auto fitCoeffs = [this, &nodes](const std::vector<double>& values) {
        std::vector<double> coeffs(m_order + 1, 0.0);
        int n = nodes.size();
        
        for (int j = 0; j <= m_order; j++) {
            double sum = 0.0;
            for (int i = 0; i < n; i++) {
                double T_j = (j == 0) ? 1.0 : (j == 1) ? nodes[i] : 0.0;
                if (j >= 2) {
                    double T_prev2 = 1.0, T_prev1 = nodes[i];
                    for (int k = 2; k <= j; k++) {
                        T_j = 2.0 * nodes[i] * T_prev1 - T_prev2;
                        T_prev2 = T_prev1;
                        T_prev1 = T_j;
                    }
                }
                sum += values[i] * T_j;
            }
            coeffs[j] = (j == 0) ? (sum / n) : (2.0 * sum / n);
        }
        return coeffs;
    };
    
    m_raCheb = ChebyshevPolynomial(m_t0, m_t1, fitCoeffs(raValues));
    m_decCheb = ChebyshevPolynomial(m_t0, m_t1, fitCoeffs(decValues));
}

void ChebyshevRADecSegment::evaluate(double t, double& ra, double& dec) const {
    ra = m_raCheb.evaluate(t);
    dec = m_decCheb.evaluate(t);
}

// ============================================================================
// ChebyshevOccultationDetector Implementation
// ============================================================================

ChebyshevOccultationDetector::ChebyshevOccultationDetector(const Config& config)
    : m_config(config), m_startJD(0), m_endJD(0), m_initialized(false) {
}

void ChebyshevOccultationDetector::initialize(Ephemeris& ephemeris, 
                                              double startJD, double endJD) {
    m_startJD = startJD;
    m_endJD = endJD;
    m_segments.clear();
    
    if (m_config.verbose) {
        std::cout << "ChebyshevOccultationDetector: Initializing" << std::endl;
        std::cout << "  Interval: JD " << startJD << " - " << endJD 
                  << " (" << (endJD - startJD) << " days)" << std::endl;
        std::cout << "  Order: " << m_config.order << std::endl;
        std::cout << "  Segment size: " << m_config.segmentDays << " days" << std::endl;
    }
    
    // Crea segmenti RA/Dec
    for (double jd = startJD; jd < endJD; jd += m_config.segmentDays) {
        double jdEnd = std::min(jd + m_config.segmentDays, endJD);
        
        auto segment = std::make_shared<ChebyshevRADecSegment>(
            jd, jdEnd, m_config.order);
        segment->computeCoefficients(ephemeris);
        m_segments.push_back(segment);
    }
    
    m_initialized = true;
    
    if (m_config.verbose) {
        std::cout << "  Segments created: " << m_segments.size() << std::endl;
    }
}

std::shared_ptr<ChebyshevRADecSegment> 
ChebyshevOccultationDetector::findSegment(double t) const {
    // Binary search
    int left = 0;
    int right = m_segments.size() - 1;
    
    while (left <= right) {
        int mid = (left + right) / 2;
        if (m_segments[mid]->contains(t)) {
            return m_segments[mid];
        }
        if (t < m_segments[mid]->t0()) {
            right = mid - 1;
        } else {
            left = mid + 1;
        }
    }
    return nullptr;
}

double ChebyshevOccultationDetector::angularDistance(double t, 
                                                      double starRA, 
                                                      double starDec) const {
    auto segment = findSegment(t);
    if (!segment) return 1e10;
    
    double ra, dec;
    segment->evaluate(t, ra, dec);
    
    // Distanza angolare con formula del coseno sferico
    double cosDist = sin(dec) * sin(starDec) + 
                     cos(dec) * cos(starDec) * cos(ra - starRA);
    cosDist = std::max(-1.0, std::min(1.0, cosDist));
    
    return acos(cosDist);  // radianti
}

ChebyshevPolynomial ChebyshevOccultationDetector::computeDistanceSquared(
    const ChebyshevRADecSegment& segment,
    double starRA, double starDec) const {
    
    // D²(t) ≈ (RA(t) - RA_s)² × cos²(Dec_avg) + (Dec(t) - Dec_s)²
    // Approssimazione valida per piccoli angoli
    
    const auto& raCheb = segment.raPolynomial();
    const auto& decCheb = segment.decPolynomial();
    
    // Stima Dec media per fattore cos²
    double t_mid = (segment.t0() + segment.t1()) / 2.0;
    double dec_mid = decCheb.evaluate(t_mid);
    double cosDec2 = cos(dec_mid) * cos(dec_mid);
    
    // ΔRA = RA(t) - RA_s  → sottrai costante
    ChebyshevPolynomial deltaRA = raCheb.subtract(starRA);
    
    // ΔDec = Dec(t) - Dec_s
    ChebyshevPolynomial deltaDec = decCheb.subtract(starDec);
    
    // D² = ΔRA² × cos²(Dec) + ΔDec²
    ChebyshevPolynomial deltaRA2 = deltaRA.multiply(deltaRA);
    ChebyshevPolynomial deltaDec2 = deltaDec.multiply(deltaDec);
    
    // Scala ΔRA² per cos²(Dec)
    std::vector<double> scaledCoeffs = deltaRA2.coeffs();
    for (double& c : scaledCoeffs) {
        c *= cosDec2;
    }
    ChebyshevPolynomial scaledDeltaRA2(segment.t0(), segment.t1(), scaledCoeffs);
    
    return scaledDeltaRA2.add(deltaDec2);
}

double ChebyshevOccultationDetector::refineMinimum(double t_approx, 
                                                    double starRA, 
                                                    double starDec) const {
    // Newton-Raphson per trovare minimo preciso
    // Minimizza D(t) trovando zero di dD/dt
    
    const double h = 1e-6;  // 0.1 secondi
    const int maxIter = 10;
    const double tol = 1e-8;  // ~1 ms
    
    double t = t_approx;
    
    for (int iter = 0; iter < maxIter; iter++) {
        double D = angularDistance(t, starRA, starDec);
        double D_plus = angularDistance(t + h, starRA, starDec);
        double D_minus = angularDistance(t - h, starRA, starDec);
        
        double dD = (D_plus - D_minus) / (2 * h);
        double d2D = (D_plus - 2*D + D_minus) / (h * h);
        
        if (std::abs(d2D) < 1e-20) break;
        
        double dt = -dD / d2D;
        
        // Limita step
        dt = std::max(-0.01, std::min(0.01, dt));  // max 15 minuti
        
        t += dt;
        
        if (std::abs(dt) < tol) break;
    }
    
    // Assicurati che t sia nel range
    t = std::max(m_startJD, std::min(m_endJD, t));
    
    return t;
}

std::vector<OccultationCandidate> 
ChebyshevOccultationDetector::findCandidatesForStar(double starRA, 
                                                     double starDec, 
                                                     int starIndex) const {
    std::vector<OccultationCandidate> candidates;
    
    double thresholdRad = m_config.thresholdArcsec / 206265.0;
    double thresholdRad2 = thresholdRad * thresholdRad;
    
    // Per ogni segmento
    for (const auto& segment : m_segments) {
        // Calcola polinomio D²(t)
        ChebyshevPolynomial distSq = computeDistanceSquared(*segment, starRA, starDec);
        
        // Trova minimi
        std::vector<double> minima = distSq.findMinima();
        
        // Aggiungi anche i bordi come potenziali candidati
        minima.push_back(segment->t0());
        minima.push_back(segment->t1());
        
        for (double t : minima) {
            if (t < segment->t0() || t > segment->t1()) continue;
            
            // Refina il minimo
            double t_refined = refineMinimum(t, starRA, starDec);
            
            // Calcola distanza precisa
            double dist = angularDistance(t_refined, starRA, starDec);
            double distArcsec = dist * 206265.0;
            
            if (dist < thresholdRad) {
                OccultationCandidate cand;
                cand.jd = t_refined;
                cand.minDistArcsec = distArcsec;
                cand.starRA = starRA;
                cand.starDec = starDec;
                cand.starIndex = starIndex;
                candidates.push_back(cand);
            }
        }
    }
    
    // Rimuovi duplicati (eventi troppo vicini nel tempo)
    std::sort(candidates.begin(), candidates.end(), 
              [](const auto& a, const auto& b) { return a.jd < b.jd; });
    
    std::vector<OccultationCandidate> unique;
    for (const auto& c : candidates) {
        if (unique.empty() || (c.jd - unique.back().jd) > 0.001) {  // > 1.4 minuti
            unique.push_back(c);
        } else if (c.minDistArcsec < unique.back().minDistArcsec) {
            unique.back() = c;  // Mantieni il più vicino
        }
    }
    
    return unique;
}

std::vector<OccultationCandidate> 
ChebyshevOccultationDetector::findCandidates(
    const std::vector<std::pair<double, double>>& stars) const {
    
    if (!m_initialized) {
        throw std::runtime_error("Detector not initialized");
    }
    
    std::vector<OccultationCandidate> allCandidates;
    
    for (size_t i = 0; i < stars.size(); i++) {
        auto starCandidates = findCandidatesForStar(
            stars[i].first, stars[i].second, static_cast<int>(i));
        
        allCandidates.insert(allCandidates.end(), 
                            starCandidates.begin(), starCandidates.end());
    }
    
    // Ordina per tempo
    std::sort(allCandidates.begin(), allCandidates.end(),
              [](const auto& a, const auto& b) { return a.jd < b.jd; });
    
    return allCandidates;
}

} // namespace ioccultcalc
