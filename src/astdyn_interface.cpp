#include "ioccultcalc/astdyn_interface.h"
#include "astdyn/ephemeris/AsteroidFitter.hpp"
#include "astdyn/coordinates/ReferenceFrame.hpp"
#include "astdyn/propagation/OrbitalElements.hpp"
#include "astdyn/core/Constants.hpp"
#include "ioccultcalc/time_utils.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cmath>

namespace ioccultcalc {

// ============================================================================
// AstDySElements Implementation
// ============================================================================

OrbitalElements AstDySElements::toOrbitalElements() const {
    OrbitalElements elem;
    elem.designation = name;
    elem.epoch.jd = epoch_mjd + 2400000.5;
    elem.a = a;
    elem.e = e;
    elem.i = i * M_PI / 180.0;
    elem.Omega = Omega * M_PI / 180.0;
    elem.omega = omega * M_PI / 180.0;
    elem.M = M * M_PI / 180.0;
    elem.H = H;
    elem.G = G;
    return elem;
}

// TODO: Implement fromFile, download, etc.

// ============================================================================
// RWOObservation Implementation
// ============================================================================

AstrometricObservation RWOObservation::toObservation() const {
    AstrometricObservation obs;
    obs.epoch.jd = mjd_utc + 2400000.5;
    obs.obs.ra = ra_deg * M_PI / 180.0;
    obs.obs.dec = dec_deg * M_PI / 180.0;
    obs.raError = ra_sigma_arcsec;
    obs.decError = dec_sigma_arcsec;
    obs.observatoryCode = obs_code;
    return obs;
}

// TODO: Implement fromFile, download, etc.

// ============================================================================
// OrbitFitResult Implementation
// ============================================================================

std::string OrbitFitResult::toReport() const {
    std::stringstream ss;
    ss << "Orbit Fit Report (" << method << ")\n";
    ss << "------------------------------------------\n";
    ss << "Observations: " << n_used << " used, " << n_outliers << " outliers\n";
    ss << "RMS Total:    " << std::fixed << std::setprecision(3) << rms_total_arcsec << " arcsec\n";
    ss << "Chi2 Reduced: " << std::fixed << std::setprecision(2) << chi2_reduced << "\n";
    ss << "Time Span:    " << std::fixed << std::setprecision(1) << time_span_days << " days\n";
    ss << "Status:       " << (is_good_fit() ? "GOOD" : "WARNING") << "\n";
    return ss.str();
}

// ============================================================================
// AstDynPropagator Implementation
// ============================================================================

class AstDynPropagator::Impl {
public:
    double tolerance;
    bool usePlanets = true;
    bool useAsteroids = true;
    bool useRelativity = true;
    
    explicit Impl(double tol) : tolerance(tol) {}
};

AstDynPropagator::AstDynPropagator(double tolerance) 
    : pimpl_(std::make_unique<Impl>(tolerance)) {}

AstDynPropagator::~AstDynPropagator() = default;

void AstDynPropagator::setTolerance(double tol) { pimpl_->tolerance = tol; }
void AstDynPropagator::usePlanetPerturbations(bool enable) { pimpl_->usePlanets = enable; }
void AstDynPropagator::useAsteroidPerturbations(bool enable) { pimpl_->useAsteroids = enable; }
void AstDynPropagator::useRelativisticCorrections(bool enable) { pimpl_->useRelativity = enable; }

AstDySElements AstDynPropagator::propagate(const AstDySElements& elements, double target_mjd) {
    // TODO: Bridge to astdyn implementation
    return elements; // Stub
}

// ============================================================================
// AstDynOrbitFitter Implementation
// ============================================================================

class AstDynOrbitFitter::Impl {
public:
    double tolerance;
    double outlierThreshold = 10.0;
    int maxIterations = 20;
    double convTolerance = 1e-6;
    bool verbose = false;
    
    explicit Impl(double tol) : tolerance(tol) {}
};

AstDynOrbitFitter::AstDynOrbitFitter(double tolerance) 
    : pimpl_(std::make_unique<Impl>(tolerance)) {}

AstDynOrbitFitter::~AstDynOrbitFitter() = default;

void AstDynOrbitFitter::setOutlierThreshold(double sigma) { pimpl_->outlierThreshold = sigma; }
void AstDynOrbitFitter::setMaxIterations(int max_iter) { pimpl_->maxIterations = max_iter; }
void AstDynOrbitFitter::setConvergenceTolerance(double tol_au) { pimpl_->convTolerance = tol_au; }
void AstDynOrbitFitter::setVerbose(bool verbose) { pimpl_->verbose = verbose; }

OrbitFitResult AstDynOrbitFitter::fit(const AstDySElements& initial_elements,
                                     const std::vector<RWOObservation>& observations) {
    throw std::runtime_error("Memory-based fitting temporarily disabled due to AstDyn API change (requires file-based workflow)");
}

OrbitFitResult AstDynOrbitFitter::computeResidualsOnly(
    const AstDySElements& elements,
    const std::vector<RWOObservation>& observations) {
    // Similar to fit but with max_iterations = 0
    int original_iter = pimpl_->maxIterations;
    pimpl_->maxIterations = 0;
    auto res = fit(elements, observations);
    pimpl_->maxIterations = original_iter;
    return res;
}

// ============================================================================
// AstDySClient Partial Implementation (Stubs for now)
// ============================================================================

AstDySElements AstDySClient::downloadElements(int asteroid_number) {
    throw std::runtime_error("AstDySClient implementation pending");
}

AstDySElements AstDySClient::downloadElements(const std::string& designation) {
    throw std::runtime_error("AstDySClient implementation pending");
}

std::vector<RWOObservation> AstDySClient::downloadObservations(int asteroid_number) {
    throw std::runtime_error("AstDySClient implementation pending");
}

std::vector<RWOObservation> AstDySClient::downloadObservations(const std::string& designation) {
    throw std::runtime_error("AstDySClient implementation pending");
}

} // namespace ioccultcalc
