/**/**#include "ioccultcalc/chebyshev_approximation.h"

 * @file chebyshev_approximation.cpp

 * @brief Implementazione approssimazione di Chebyshev * @file chebyshev_approximation.cpp#include "ioccultcalc/coordinates.h"

 * 

 * @author Michele Bigi * @brief Implementazione approssimazione di Chebyshev per traiettorie asteroidali#include <cmath>

 * @date 2025-11-29

 */ */#include <algorithm>



#include "ioccultcalc/chebyshev_approximation.h"#include <stdexcept>

#include "ioccultcalc/coordinates.h"

#include <cmath>#include "ioccultcalc/chebyshev_approximation.h"#include <iostream>

#include <algorithm>

#include <stdexcept>#include "ioccultcalc/coordinates.h"



namespace ioccultcalc {#include <cmath>namespace ioccultcalc {



ChebyshevApproximation::ChebyshevApproximation(const ChebyshevConfig& config)#include <stdexcept>

    : config_(config) {

}#include <algorithm>// ============================================================================



ChebyshevApproximation::~ChebyshevApproximation() {// ChebyshevSegment Implementation

}

namespace ioccultcalc {// ============================================================================

bool ChebyshevApproximation::generate(const Ephemeris& ephemeris, 

                                     double startJD, double endJD) {

    segments_.clear();

    ChebyshevApproximation::ChebyshevApproximation(const ChebyshevConfig& config)ChebyshevSegment::ChebyshevSegment(const JulianDate& t0, const JulianDate& t1, int order)

    // Dividi l'intervallo in segmenti

    double currentJD = startJD;    : config_(config) {    : m_t0(t0), m_t1(t1), m_order(order), m_estimatedError(0.0) {

    while (currentJD < endJD) {

        double segEndJD = std::min(currentJD + config_.segmentDays, endJD);}    

        segments_.push_back(generateSegment(ephemeris, currentJD, segEndJD));

        currentJD = segEndJD;    if (order < 5 || order > 25) {

    }

    ChebyshevApproximation::~ChebyshevApproximation() {        throw std::runtime_error("Chebyshev order must be 5-25");

    return !segments_.empty();

}}    }



ChebyshevSegment ChebyshevApproximation::generateSegment(    

    const Ephemeris& ephemeris, double startJD, double endJD) {

    bool ChebyshevApproximation::generate(const Ephemeris& ephemeris,     // Alloca spazio per coefficienti

    ChebyshevSegment segment;

    segment.startJD = startJD;                                      double startJD, double endJD) {    m_coeffsX.resize(order + 1);

    segment.endJD = endJD;

    segment.midJD = (startJD + endJD) / 2.0;    segments_.clear();    m_coeffsY.resize(order + 1);

    segment.halfSpan = (endJD - startJD) / 2.0;

            m_coeffsZ.resize(order + 1);

    int nSamples = config_.order + 1;

    std::vector<double> samplesX(nSamples);    if (endJD <= startJD) {    m_coeffsVX.resize(order + 1);

    std::vector<double> samplesY(nSamples);

    std::vector<double> samplesZ(nSamples);        return false;    m_coeffsVY.resize(order + 1);

    

    // Campiona nei punti di Chebyshev    }    m_coeffsVZ.resize(order + 1);

    for (int i = 0; i < nSamples; i++) {

        double x = -cos(M_PI * (i + 0.5) / nSamples);  // Chebyshev nodes    }

        double jd = segment.midJD + x * segment.halfSpan;

            // Divide il periodo in segmenti

        JulianDate epoch;

        epoch.jd = jd;    double currentJD = startJD;void ChebyshevSegment::computeCoefficients(Ephemeris& ephemeris) {

        

        EphemerisData data = ephemeris.compute(epoch);    while (currentJD < endJD) {    // Genera nodi di campionamento Chebyshev ottimali

        

        // Usa coordinate equatoriali (RA, Dec, distanza)        double segmentEnd = std::min(currentJD + config_.segmentDays, endJD);    std::vector<double> nodes = ChebyshevFitter::chebyshevNodes(m_order);

        samplesX[i] = data.position.ra;

        samplesY[i] = data.position.dec;            

        samplesZ[i] = data.distance;

    }        // Genera il segmento    // Campiona posizioni e velocità ai nodi

    

    // Calcola coefficienti        ChebyshevSegment segment = generateSegment(ephemeris, currentJD, segmentEnd);    std::vector<double> samplesX, samplesY, samplesZ;

    segment.coeffX = computeChebyshevCoefficients(samplesX);

    segment.coeffY = computeChebyshevCoefficients(samplesY);        segments_.push_back(segment);    std::vector<double> samplesVX, samplesVY, samplesVZ;

    segment.coeffZ = computeChebyshevCoefficients(samplesZ);

                

    return segment;

}        currentJD = segmentEnd;    for (double node : nodes) {



std::vector<double> ChebyshevApproximation::computeChebyshevCoefficients(    }        // Converti nodo [-1, 1] → tempo JD

    const std::vector<double>& values) const {

                double jd = m_t0.jd + (node + 1.0) * (m_t1.jd - m_t0.jd) / 2.0;

    int n = values.size();

    std::vector<double> coeffs(n, 0.0);    return !segments_.empty();        

    

    // Trasformata di Chebyshev discreta}        // Calcola ephemeris con integrazione esatta

    for (int j = 0; j < n; j++) {

        double sum = 0.0;        EphemerisData eph = ephemeris.compute(JulianDate(jd));

        for (int k = 0; k < n; k++) {

            double angle = M_PI * j * (k + 0.5) / n;ChebyshevSegment ChebyshevApproximation::generateSegment(const Ephemeris& ephemeris,        

            sum += values[k] * cos(angle);

        }                                                         double startJD, double endJD) {        // Estrai posizione geocentrica (assumendo già in km)

        coeffs[j] = 2.0 * sum / n;

    }    ChebyshevSegment segment;        Vector3D pos = Coordinates::equatorialToCartesian(eph.geocentricPos);

    coeffs[0] /= 2.0;

        segment.startJD = startJD;        pos = pos * (eph.distance * AU);  // Converti in km

    return coeffs;

}    segment.endJD = endJD;        



EquatorialCoordinates ChebyshevApproximation::evaluate(double jd) const {    segment.midJD = (startJD + endJD) / 2.0;        samplesX.push_back(pos.x);

    const ChebyshevSegment* seg = findSegment(jd);

    if (!seg) {    segment.halfSpan = (endJD - startJD) / 2.0;        samplesY.push_back(pos.y);

        throw std::runtime_error("JD out of range for Chebyshev approximation");

    }            samplesZ.push_back(pos.z);

    

    double x = normalizeTime(jd, *seg);    const int order = config_.order;        

    

    EquatorialCoordinates coords;    const int numPoints = order + 1;        // Velocità (km/s)

    coords.ra = evaluateChebyshevPolynomial(seg->coeffX, x);

    coords.dec = evaluateChebyshevPolynomial(seg->coeffY, x);            Vector3D vel = eph.heliocentricVel * (AU / 86400.0);

    coords.distance = evaluateChebyshevPolynomial(seg->coeffZ, x);

        // Calcola posizioni nei punti di Chebyshev        samplesVX.push_back(vel.x);

    return coords;

}    std::vector<double> valuesX(numPoints);        samplesVY.push_back(vel.y);



void ChebyshevApproximation::evaluateStateVector(double jd,     std::vector<double> valuesY(numPoints);        samplesVZ.push_back(vel.z);

                                                 Vector3D& pos, Vector3D& vel) const {

    const ChebyshevSegment* seg = findSegment(jd);    std::vector<double> valuesZ(numPoints);    }

    if (!seg) {

        throw std::runtime_error("JD out of range for Chebyshev approximation");        

    }

        for (int i = 0; i < numPoints; i++) {    // Fit coefficienti Chebyshev per ogni componente

    double x = normalizeTime(jd, *seg);

            // Punto di Chebyshev: x_i = cos(π(i + 0.5)/n)    m_coeffsX = ChebyshevFitter::fit(m_order, nodes, samplesX);

    // Posizione (RA, Dec, distanza)

    double ra = evaluateChebyshevPolynomial(seg->coeffX, x);        double x = std::cos(M_PI * (i + 0.5) / numPoints);    m_coeffsY = ChebyshevFitter::fit(m_order, nodes, samplesY);

    double dec = evaluateChebyshevPolynomial(seg->coeffY, x);

    double dist = evaluateChebyshevPolynomial(seg->coeffZ, x);        double jd = segment.midJD + segment.halfSpan * x;    m_coeffsZ = ChebyshevFitter::fit(m_order, nodes, samplesZ);

    

    // Velocità (derivate)            m_coeffsVX = ChebyshevFitter::fit(m_order, nodes, samplesVX);

    double dra = evaluateChebyshevDerivative(seg->coeffX, x) / seg->halfSpan;

    double ddec = evaluateChebyshevDerivative(seg->coeffY, x) / seg->halfSpan;        // Calcola ephemeris per questo istante    m_coeffsVY = ChebyshevFitter::fit(m_order, nodes, samplesVY);

    double ddist = evaluateChebyshevDerivative(seg->coeffZ, x) / seg->halfSpan;

            Ephemeris ephCopy = ephemeris;  // Copia per calcolo    m_coeffsVZ = ChebyshevFitter::fit(m_order, nodes, samplesVZ);

    // Converti a coordinate cartesiane

    double ra_rad = ra * M_PI / 180.0;        EphemerisData ephem = ephCopy.compute({jd});    

    double dec_rad = dec * M_PI / 180.0;

                // Stima errore usando punti test intermedi

    pos.x = dist * cos(dec_rad) * cos(ra_rad);

    pos.y = dist * cos(dec_rad) * sin(ra_rad);        if (config_.geocentric) {    std::vector<double> testNodes;

    pos.z = dist * sin(dec_rad);

                // Usa coordinate equatoriali geocentriche (RA, Dec, distanza)    std::vector<double> testX, testY, testZ;

    vel.x = dra;

    vel.y = ddec;            valuesX[i] = ephem.geocentricPos.ra;   // RA in gradi    

    vel.z = ddist;

}            valuesY[i] = ephem.geocentricPos.dec;  // Dec in gradi    for (int i = 0; i < m_order; i++) {



const ChebyshevSegment* ChebyshevApproximation::findSegment(double jd) const {            valuesZ[i] = ephem.distance;           // Distanza in AU        double nodeTest = -1.0 + (2.0 * i + 1.5) / (m_order + 1);

    for (const auto& seg : segments_) {

        if (jd >= seg.startJD && jd <= seg.endJD) {        } else {        testNodes.push_back(nodeTest);

            return &seg;

        }            // Usa posizione cartesiana eliocentrica        

    }

    return nullptr;            valuesX[i] = ephem.heliocentricPos.x;        double jd = m_t0.jd + (nodeTest + 1.0) * (m_t1.jd - m_t0.jd) / 2.0;

}

            valuesY[i] = ephem.heliocentricPos.y;        EphemerisData eph = ephemeris.compute(JulianDate(jd));

void ChebyshevApproximation::getTimeSpan(double& startJD, double& endJD) const {

    if (segments_.empty()) {            valuesZ[i] = ephem.heliocentricPos.z;        Vector3D pos = Coordinates::equatorialToCartesian(eph.geocentricPos);

        startJD = endJD = 0;

        return;        }        pos = pos * (eph.distance * AU);

    }

    startJD = segments_.front().startJD;    }        

    endJD = segments_.back().endJD;

}            testX.push_back(pos.x);



double ChebyshevApproximation::estimateMaxError(const Ephemeris& ephemeris,     // Calcola coefficienti di Chebyshev per ciascuna coordinata        testY.push_back(pos.y);

                                                int numTestPoints) const {

    double maxError = 0.0;    segment.coeffX = computeChebyshevCoefficients(valuesX);        testZ.push_back(pos.z);

    double startJD, endJD;

    getTimeSpan(startJD, endJD);    segment.coeffY = computeChebyshevCoefficients(valuesY);    }

    

    for (int i = 0; i < numTestPoints; i++) {    segment.coeffZ = computeChebyshevCoefficients(valuesZ);    

        double t = startJD + (endJD - startJD) * i / (numTestPoints - 1);

        JulianDate epoch;        double errX = ChebyshevFitter::evaluateError(m_coeffsX, testNodes, testX);

        epoch.jd = t;

            return segment;    double errY = ChebyshevFitter::evaluateError(m_coeffsY, testNodes, testY);

        EphemerisData exact = ephemeris.compute(epoch);

        EquatorialCoordinates approx = evaluate(t);}    double errZ = ChebyshevFitter::evaluateError(m_coeffsZ, testNodes, testZ);

        

        // Calcola errore angolare    

        double dra = (approx.ra - exact.position.ra) * cos(exact.position.dec * M_PI / 180.0);

        double ddec = approx.dec - exact.position.dec;std::vector<double> ChebyshevApproximation::computeChebyshevCoefficients(    m_estimatedError = sqrt(errX*errX + errY*errY + errZ*errZ);

        double error = sqrt(dra*dra + ddec*ddec) * 3600.0;  // arcsec

            const std::vector<double>& values) const {}

        maxError = std::max(maxError, error);

    }    

    

    return maxError;    const int n = values.size();EphemerisData ChebyshevSegment::evaluate(const JulianDate& t) const {

}

    std::vector<double> coeffs(n, 0.0);    if (!contains(t)) {

double ChebyshevApproximation::evaluateChebyshevPolynomial(

    const std::vector<double>& coeffs, double x) const {            throw std::runtime_error("Time outside segment range");

    

    if (coeffs.empty()) return 0.0;    // Trasformata discreta di Chebyshev (DCT-I)    }

    if (coeffs.size() == 1) return coeffs[0];

        for (int k = 0; k < n; k++) {    

    // Algoritmo di Clenshaw

    double b_k = 0.0;        double sum = 0.0;    // Normalizza tempo

    double b_k1 = 0.0;

            for (int j = 0; j < n; j++) {    double x = normalizeTime(t);

    for (int k = coeffs.size() - 1; k >= 1; k--) {

        double b_k2 = b_k1;            double x = std::cos(M_PI * (j + 0.5) / n);    

        b_k1 = b_k;

        b_k = 2.0 * x * b_k1 - b_k2 + coeffs[k];            double T_k = std::cos(k * std::acos(x));    // Valuta polinomi per posizione

    }

                sum += values[j] * T_k;    double posX = evaluatePolynomial(m_coeffsX, x);

    return x * b_k - b_k1 + coeffs[0];

}        }    double posY = evaluatePolynomial(m_coeffsY, x);



double ChebyshevApproximation::evaluateChebyshevDerivative(        coeffs[k] = (2.0 / n) * sum;    double posZ = evaluatePolynomial(m_coeffsZ, x);

    const std::vector<double>& coeffs, double x) const {

        }    

    if (coeffs.size() <= 1) return 0.0;

            // Valuta polinomi per velocità

    int n = coeffs.size();

    std::vector<double> derivCoeffs(n - 1);    // Corregge primo coefficiente    double velX = evaluatePolynomial(m_coeffsVX, x);

    

    // Calcola coefficienti della derivata    coeffs[0] *= 0.5;    double velY = evaluatePolynomial(m_coeffsVY, x);

    derivCoeffs[n-2] = 2.0 * (n-1) * coeffs[n-1];

    if (n > 2) {        double velZ = evaluatePolynomial(m_coeffsVZ, x);

        derivCoeffs[n-3] = 2.0 * (n-2) * coeffs[n-2];

    }    return coeffs;    

    

    for (int j = n - 4; j >= 0; j--) {}    // Converti back in coordinate equatoriali

        derivCoeffs[j] = derivCoeffs[j+2] + 2.0 * (j+1) * coeffs[j+1];

    }    Vector3D posKm(posX, posY, posZ);

    

    return evaluateChebyshevPolynomial(derivCoeffs, x);EquatorialCoordinates ChebyshevApproximation::evaluate(double jd) const {    double distance = posKm.magnitude();

}

    const ChebyshevSegment* segment = findSegment(jd);    Vector3D posDir = posKm.normalize();

double ChebyshevApproximation::normalizeTime(double jd, 

                                             const ChebyshevSegment& segment) const {    if (!segment) {    

    return (jd - segment.midJD) / segment.halfSpan;

}        throw std::runtime_error("JD fuori range approssimazione Chebyshev");    EphemerisData eph;



} // namespace ioccultcalc    }    eph.jd = t;


        eph.geocentricPos = Coordinates::cartesianToEquatorial(posDir);

    double x = normalizeTime(jd, *segment);    eph.distance = distance / AU;  // km → AU

        

    EquatorialCoordinates coords;    Vector3D velKmS(velX, velY, velZ);

    coords.ra = evaluateChebyshevPolynomial(segment->coeffX, x);    eph.heliocentricVel = velKmS * (86400.0 / AU);  // km/s → AU/day

    coords.dec = evaluateChebyshevPolynomial(segment->coeffY, x);    

        return eph;

    // Normalizza RA in [0, 360)}

    while (coords.ra < 0) coords.ra += 360.0;

    while (coords.ra >= 360.0) coords.ra -= 360.0;bool ChebyshevSegment::contains(const JulianDate& t) const {

        return t.jd >= m_t0.jd && t.jd <= m_t1.jd;

    // Clamp Dec in [-90, 90]}

    if (coords.dec < -90.0) coords.dec = -90.0;

    if (coords.dec > 90.0) coords.dec = 90.0;double ChebyshevSegment::normalizeTime(const JulianDate& t) const {

        // Mappa [t0, t1] → [-1, 1]

    return coords;    return 2.0 * (t.jd - m_t0.jd) / (m_t1.jd - m_t0.jd) - 1.0;

}}



void ChebyshevApproximation::evaluateStateVector(double jd, double ChebyshevSegment::evaluatePolynomial(const std::vector<double>& coeffs, double x) const {

                                                 Vector3D& pos,     // Valuta Σ cᵢ × Tᵢ(x) usando ricorsione Clenshaw

                                                 Vector3D& vel) const {    // Più stabile numericamente della formula diretta

    const ChebyshevSegment* segment = findSegment(jd);    

    if (!segment) {    int n = coeffs.size() - 1;

        throw std::runtime_error("JD fuori range approssimazione Chebyshev");    if (n < 0) return 0.0;

    }    

        double b_k2 = 0.0;

    double x = normalizeTime(jd, *segment);    double b_k1 = 0.0;

        

    if (config_.geocentric) {    for (int k = n; k >= 1; k--) {

        // Valuta coordinate equatoriali        double b_k = 2.0 * x * b_k1 - b_k2 + coeffs[k];

        double ra = evaluateChebyshevPolynomial(segment->coeffX, x);        b_k2 = b_k1;

        double dec = evaluateChebyshevPolynomial(segment->coeffY, x);        b_k1 = b_k;

        double dist = evaluateChebyshevPolynomial(segment->coeffZ, x);    }

            

        // Converti in cartesiane    return x * b_k1 - b_k2 + coeffs[0];

        double raRad = ra * M_PI / 180.0;}

        double decRad = dec * M_PI / 180.0;

        void ChebyshevSegment::evaluateChebyshevBasis(double x, std::vector<double>& T) const {

        pos.x = dist * std::cos(decRad) * std::cos(raRad);    T.resize(m_order + 1);

        pos.y = dist * std::cos(decRad) * std::sin(raRad);    

        pos.z = dist * std::sin(decRad);    if (m_order >= 0) T[0] = 1.0;

            if (m_order >= 1) T[1] = x;

        // Valuta derivate per velocità (in AU/day)    

        double dra_dt = evaluateChebyshevDerivative(segment->coeffX, x) / segment->halfSpan;    // Ricorsione: Tₙ₊₁(x) = 2x×Tₙ(x) - Tₙ₋₁(x)

        double ddec_dt = evaluateChebyshevDerivative(segment->coeffY, x) / segment->halfSpan;    for (int n = 2; n <= m_order; n++) {

        double ddist_dt = evaluateChebyshevDerivative(segment->coeffZ, x) / segment->halfSpan;        T[n] = 2.0 * x * T[n-1] - T[n-2];

            }

        // Calcola velocità in coordinate cartesiane (formula derivata)}

        vel.x = ddist_dt * std::cos(decRad) * std::cos(raRad)

              - dist * std::sin(decRad) * ddec_dt * std::cos(raRad)// ============================================================================

              - dist * std::cos(decRad) * std::sin(raRad) * dra_dt;// ChebyshevCache Implementation

        // ============================================================================

        vel.y = ddist_dt * std::cos(decRad) * std::sin(raRad)

              - dist * std::sin(decRad) * ddec_dt * std::sin(raRad)ChebyshevCache::ChebyshevCache(const ChebyshevConfig& config)

              + dist * std::cos(decRad) * std::cos(raRad) * dra_dt;    : m_config(config), m_initialized(false), m_maxError(0.0), m_avgError(0.0) {

        }

        vel.z = ddist_dt * std::sin(decRad) + dist * std::cos(decRad) * ddec_dt;

        void ChebyshevCache::initialize(Ephemeris& ephemeris, 

    } else {                               const JulianDate& startJD,

        // Coordinate cartesiane dirette                               const JulianDate& endJD) {

        pos.x = evaluateChebyshevPolynomial(segment->coeffX, x);    

        pos.y = evaluateChebyshevPolynomial(segment->coeffY, x);    m_startJD = startJD;

        pos.z = evaluateChebyshevPolynomial(segment->coeffZ, x);    m_endJD = endJD;

            m_segments.clear();

        vel.x = evaluateChebyshevDerivative(segment->coeffX, x) / segment->halfSpan;    

        vel.y = evaluateChebyshevDerivative(segment->coeffY, x) / segment->halfSpan;    std::cout << "Initializing Chebyshev cache:" << std::endl;

        vel.z = evaluateChebyshevDerivative(segment->coeffZ, x) / segment->halfSpan;    std::cout << "  Interval: JD " << startJD.jd << " - " << endJD.jd 

    }              << " (" << (endJD.jd - startJD.jd) << " days)" << std::endl;

}    std::cout << "  Order: " << m_config.order << std::endl;

    std::cout << "  Segment size: " << m_config.intervalDays << " days" << std::endl;

double ChebyshevApproximation::evaluateChebyshevPolynomial(    

    const std::vector<double>& coeffs, double x) const {    // Crea segmenti

        int segmentCount = 0;

    if (coeffs.empty()) {    double totalError = 0.0;

        return 0.0;    

    }    for (double jd = startJD.jd; jd < endJD.jd; jd += m_config.intervalDays) {

            double jdEnd = std::min(jd + m_config.intervalDays, endJD.jd);

    // Algoritmo di Clenshaw per valutazione stabile        

    const int n = coeffs.size();        auto segment = std::make_shared<ChebyshevSegment>(

    if (n == 1) {            JulianDate(jd), JulianDate(jdEnd), m_config.order);

        return coeffs[0];        

    }        segment->computeCoefficients(ephemeris);

            

    double b_k2 = 0.0;  // b_{k+2}        double error = segment->estimatedError();

    double b_k1 = 0.0;  // b_{k+1}        totalError += error;

            m_maxError = std::max(m_maxError, error);

    for (int k = n - 1; k >= 1; k--) {        

        double b_k = 2.0 * x * b_k1 - b_k2 + coeffs[k];        m_segments.push_back(segment);

        b_k2 = b_k1;        segmentCount++;

        b_k1 = b_k;        

    }        if (segmentCount % 10 == 0) {

                std::cout << "  Computed " << segmentCount << " segments..." << std::endl;

    return x * b_k1 - b_k2 + coeffs[0];        }

}    }

    

double ChebyshevApproximation::evaluateChebyshevDerivative(    m_avgError = totalError / segmentCount;

    const std::vector<double>& coeffs, double x) const {    m_initialized = true;

        

    const int n = coeffs.size();    std::cout << "Chebyshev cache initialized:" << std::endl;

    if (n <= 1) {    std::cout << "  Segments: " << segmentCount << std::endl;

        return 0.0;    std::cout << "  Max error: " << m_maxError << " km" << std::endl;

    }    std::cout << "  Avg error: " << m_avgError << " km" << std::endl;

        

    // Calcola coefficienti della derivata    if (m_maxError > m_config.maxErrorKm) {

    std::vector<double> derivCoeffs(n - 1);        std::cerr << "WARNING: Max Chebyshev error " << m_maxError 

    derivCoeffs[n - 2] = 2.0 * (n - 1) * coeffs[n - 1];                  << " km exceeds threshold " << m_config.maxErrorKm << " km" << std::endl;

        }

    if (n > 2) {}

        derivCoeffs[n - 3] = 2.0 * (n - 2) * coeffs[n - 2];

        EphemerisData ChebyshevCache::evaluate(const JulianDate& t) const {

        for (int k = n - 4; k >= 0; k--) {    if (!m_initialized) {

            derivCoeffs[k] = derivCoeffs[k + 2] + 2.0 * (k + 1) * coeffs[k + 1];        throw std::runtime_error("Chebyshev cache not initialized");

        }    }

    }    

        auto segment = findSegment(t);

    // Valuta polinomio derivata    if (!segment) {

    return evaluateChebyshevPolynomial(derivCoeffs, x);        throw std::runtime_error("Time outside cache range");

}    }

    

const ChebyshevSegment* ChebyshevApproximation::findSegment(double jd) const {    return segment->evaluate(t);

    for (const auto& seg : segments_) {}

        if (jd >= seg.startJD && jd <= seg.endJD) {

            return &seg;std::vector<JulianDate> ChebyshevCache::generateFineTimesteps() const {

        }    std::vector<JulianDate> timesteps;

    }    

    return nullptr;    double stepDays = m_config.fineTimestepMinutes / 1440.0;

}    

    for (double jd = m_startJD.jd; jd <= m_endJD.jd; jd += stepDays) {

void ChebyshevApproximation::getTimeSpan(double& startJD, double& endJD) const {        timesteps.push_back(JulianDate(jd));

    if (segments_.empty()) {    }

        startJD = endJD = 0.0;    

        return;    return timesteps;

    }}

    

    startJD = segments_.front().startJD;std::shared_ptr<ChebyshevSegment> ChebyshevCache::findSegment(const JulianDate& t) const {

    endJD = segments_.back().endJD;    // Binary search per O(log n)

}    int left = 0;

    int right = m_segments.size() - 1;

double ChebyshevApproximation::estimateMaxError(const Ephemeris& ephemeris,    

                                                int numTestPoints) const {    while (left <= right) {

    if (segments_.empty()) {        int mid = (left + right) / 2;

        return 0.0;        

    }        if (m_segments[mid]->contains(t)) {

                return m_segments[mid];

    double startJD, endJD;        }

    getTimeSpan(startJD, endJD);        

    double span = endJD - startJD;        if (t.jd < m_segments[mid]->startJD()) {

                right = mid - 1;

    double maxError = 0.0;        } else {

                left = mid + 1;

    for (int i = 0; i < numTestPoints; i++) {        }

        double jd = startJD + (span * i) / (numTestPoints - 1);    }

            

        // Valuta con Chebyshev    return nullptr;

        EquatorialCoordinates approx = evaluate(jd);}

        

        // Valuta con Ephemeris originale// ============================================================================

        Ephemeris ephCopy = ephemeris;// ChebyshevFitter Implementation

        EphemerisData exact = ephCopy.compute({jd});// ============================================================================

        

        // Calcola errore angolarestd::vector<double> ChebyshevFitter::fit(int order, 

        double dRA = (approx.ra - exact.geocentricPos.ra) *                                         const std::vector<double>& t,

                    std::cos(exact.geocentricPos.dec * M_PI / 180.0);                                        const std::vector<double>& y) {

        double dDec = approx.dec - exact.geocentricPos.dec;    

        double error = std::sqrt(dRA * dRA + dDec * dDec) * 3600.0;  // arcsec    if (t.size() != y.size() || t.size() < order + 1) {

                throw std::runtime_error("Insufficient data points for Chebyshev fit");

        maxError = std::max(maxError, error);    }

    }    

        std::vector<double> coeffs(order + 1, 0.0);

    return maxError;    int n = t.size();

}    

    // Discrete Chebyshev Transform (DCT)

double ChebyshevApproximation::normalizeTime(double jd,     // cⱼ = (2/n) × Σᵢ yᵢ × Tⱼ(xᵢ) con correzione per j=0

                                             const ChebyshevSegment& segment) const {    

    return (jd - segment.midJD) / segment.halfSpan;    for (int j = 0; j <= order; j++) {

}        double sum = 0.0;

        

} // namespace ioccultcalc        for (int i = 0; i < n; i++) {

            double T_j = 0.0;
            
            // Calcola Tⱼ(xᵢ) usando ricorsione
            if (j == 0) {
                T_j = 1.0;
            } else if (j == 1) {
                T_j = t[i];
            } else {
                double T_prev2 = 1.0;
                double T_prev1 = t[i];
                for (int k = 2; k <= j; k++) {
                    T_j = 2.0 * t[i] * T_prev1 - T_prev2;
                    T_prev2 = T_prev1;
                    T_prev1 = T_j;
                }
            }
            
            sum += y[i] * T_j;
        }
        
        coeffs[j] = (j == 0) ? (sum / n) : (2.0 * sum / n);
    }
    
    return coeffs;
}

std::vector<double> ChebyshevFitter::chebyshevNodes(int order) {
    std::vector<double> nodes;
    
    // Nodi di Chebyshev: xᵢ = cos(π(2i+1)/(2(order+1)))
    // Ottimali per minimizzare oscillazione di Runge
    
    for (int i = 0; i <= order; i++) {
        double theta = M_PI * (2.0 * i + 1.0) / (2.0 * (order + 1));
        double node = cos(theta);
        nodes.push_back(node);
    }
    
    return nodes;
}

double ChebyshevFitter::evaluateError(const std::vector<double>& coeffs,
                                     const std::vector<double>& t_test,
                                     const std::vector<double>& y_test) {
    
    if (t_test.size() != y_test.size()) {
        throw std::runtime_error("Test data size mismatch");
    }
    
    double sumSquaredError = 0.0;
    
    for (size_t i = 0; i < t_test.size(); i++) {
        // Valuta polinomio ai punti test
        double y_approx = 0.0;
        
        for (size_t j = 0; j < coeffs.size(); j++) {
            double T_j = 0.0;
            
            if (j == 0) {
                T_j = 1.0;
            } else if (j == 1) {
                T_j = t_test[i];
            } else {
                double T_prev2 = 1.0;
                double T_prev1 = t_test[i];
                for (size_t k = 2; k <= j; k++) {
                    T_j = 2.0 * t_test[i] * T_prev1 - T_prev2;
                    T_prev2 = T_prev1;
                    T_prev1 = T_j;
                }
            }
            
            y_approx += coeffs[j] * T_j;
        }
        
        double error = y_approx - y_test[i];
        sumSquaredError += error * error;
    }
    
    return sqrt(sumSquaredError / t_test.size());  // RMS error
}

} // namespace ioccultcalc
