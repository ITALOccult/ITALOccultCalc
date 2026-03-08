#include "ioccultcalc/astdyn_interface.h"
#include "astdyn/ephemeris/AsteroidFitter.hpp"
#include "astdyn/coordinates/ReferenceFrame.hpp"
#include "astdyn/propagation/OrbitalElements.hpp"
#include "astdyn/propagation/HighPrecisionPropagator.hpp"
#include "astdyn/core/Constants.hpp"
#include "astdyn/core/Types.hpp"
#include "astdyn/time/TimeScale.hpp"
#include "ioccultcalc/time_utils.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <cstdlib>

namespace ioccultcalc {

namespace ac = astdyn::constants;
namespace at = astdyn::time;

// Forward declarations for conversion helpers (used by AstDynPropagator::propagate)
static astdyn::propagation::KeplerianElements toAstDynKeplerian(const AstDySElements& el);
static AstDySElements fromAstDynKeplerian(const astdyn::propagation::KeplerianElements& kep, const std::string& name);

// ============================================================================
// AstDySElements Implementation
// ============================================================================

OrbitalElements AstDySElements::toOrbitalElements() const {
    OrbitalElements elem;
    elem.designation = name;
    elem.epoch.jd = at::mjd_to_jd(epoch_mjd);
    elem.a = a;
    elem.e = e;
    elem.i = i * ac::DEG_TO_RAD;
    elem.Omega = Omega * ac::DEG_TO_RAD;
    elem.omega = omega * ac::DEG_TO_RAD;
    elem.M = M * ac::DEG_TO_RAD;
    elem.H = H;
    elem.G = G;
    elem.frame = frame;
    elem.type = type;
    return elem;
}

std::optional<AstDySElements> AstDySElements::tryFromFile(const std::string& filename) {
    try {
        auto equ = astdyn::api::OrbitFitAPI::parse_eq1(filename);
        AstDySElements out;
        out.name = "Unknown";
        out.number = 0;
        out.a = equ.a;
        auto kep = astdyn::propagation::equinoctial_to_keplerian(equ);
        out.a = kep.semi_major_axis;
        out.e = kep.eccentricity;
        out.i = kep.inclination * ac::RAD_TO_DEG;
        out.omega = kep.argument_perihelion * ac::RAD_TO_DEG;
        out.Omega = kep.longitude_ascending_node * ac::RAD_TO_DEG;
        out.M = kep.mean_anomaly * ac::RAD_TO_DEG;
        out.epoch_mjd = equ.epoch_mjd_tdb;
        out.has_covariance = false;
        return out;
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

AstDySElements AstDySElements::fromFile(const std::string& filename) {
    auto opt = tryFromFile(filename);
    if (!opt)
        throw std::runtime_error("AstDySElements::fromFile failed: cannot parse " + filename);
    return *opt;
}

// ============================================================================
// RWOObservation Implementation
// ============================================================================

AstrometricObservation RWOObservation::toObservation() const {
    AstrometricObservation obs;
    obs.epoch.jd = at::mjd_to_jd(mjd_utc);
    obs.obs.ra = ra_deg * ac::DEG_TO_RAD;
    obs.obs.dec = dec_deg * ac::DEG_TO_RAD;
    obs.raError = ra_sigma_arcsec;
    obs.decError = dec_sigma_arcsec;
    obs.observatoryCode = obs_code;
    return obs;
}

std::optional<std::vector<RWOObservation>> RWOObservation::tryFromFile(const std::string& filename) {
    try {
        auto internal_obs = astdyn::observations::RWOReader::readFile(filename);
        std::vector<RWOObservation> out;
        for (const auto& o : internal_obs) {
            RWOObservation r;
            r.designation = o.object_designation;
            r.mjd_utc = o.mjd_utc;
            r.ra_deg = o.ra * ac::RAD_TO_DEG;
            r.dec_deg = o.dec * ac::RAD_TO_DEG;
            r.ra_sigma_arcsec = o.sigma_ra * ac::RAD_TO_ARCSEC;
            r.dec_sigma_arcsec = o.sigma_dec * ac::RAD_TO_ARCSEC;
            r.obs_code = o.observatory_code;
            r.magnitude = o.magnitude.value_or(0.0);
            out.push_back(r);
        }
        return out;
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

std::vector<RWOObservation> RWOObservation::fromFile(const std::string& filename) {
    auto opt = tryFromFile(filename);
    if (!opt)
        throw std::runtime_error("RWOObservation::fromFile failed: cannot parse " + filename);
    return *opt;
}

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
    std::string de441_path;
    mutable std::unique_ptr<astdyn::propagation::HighPrecisionPropagator> hpp;

    explicit Impl(double tol) : tolerance(tol) {
        discoverDe441Path();
    }

    void discoverDe441Path() {
        const char* home = std::getenv("HOME");
        std::string cacheDir = home ? (std::string(home) + "/.ioccultcalc/ephemerides/") : "";
        std::vector<std::string> tryPaths = {
            cacheDir + "de441_part-2.bsp",
            cacheDir + "de440.bsp",
            cacheDir + "de441.bsp"
        };
        for (const auto& p : tryPaths) {
            if (p.empty()) continue;
            std::ifstream f(p);
            if (f.good()) {
                de441_path = p;
                return;
            }
        }
        de441_path.clear();
    }

    astdyn::propagation::HighPrecisionPropagator& getPropagator() {
        if (!hpp) {
            astdyn::propagation::HighPrecisionPropagator::Config config;
            config.de441_path = de441_path;
            config.tolerance = tolerance;
            config.perturbations_planets = usePlanets;
            config.perturbations_asteroids = useAsteroids;
            config.relativity = useRelativity;
            config.step_size = 0.5;
            hpp = std::make_unique<astdyn::propagation::HighPrecisionPropagator>(config);
        }
        return *hpp;
    }

    void invalidatePropagator() { hpp.reset(); }
};

AstDynPropagator::AstDynPropagator(double tolerance)
    : pimpl_(std::make_unique<Impl>(tolerance)) {}

AstDynPropagator::~AstDynPropagator() = default;

void AstDynPropagator::setTolerance(double tol) {
    pimpl_->tolerance = tol;
    pimpl_->invalidatePropagator();
}
void AstDynPropagator::usePlanetPerturbations(bool enable) {
    pimpl_->usePlanets = enable;
    pimpl_->invalidatePropagator();
}
void AstDynPropagator::useAsteroidPerturbations(bool enable) {
    pimpl_->useAsteroids = enable;
    pimpl_->invalidatePropagator();
}
void AstDynPropagator::useRelativisticCorrections(bool enable) {
    pimpl_->useRelativity = enable;
    pimpl_->invalidatePropagator();
}

std::optional<AstDySElements> AstDynPropagator::propagate(const AstDySElements& elements, double target_mjd) {
    try {
        astdyn::propagation::KeplerianElements kep = toAstDynKeplerian(elements);
        astdyn::propagation::HighPrecisionPropagator& hpp = pimpl_->getPropagator();
        astdyn::propagation::CartesianElements cart =
            hpp.propagate_cartesian(kep, target_mjd,
                                    astdyn::propagation::HighPrecisionPropagator::InputFrame::ECLIPTIC);
        astdyn::propagation::KeplerianElements kep_target =
            astdyn::propagation::cartesian_to_keplerian(cart);
        return fromAstDynKeplerian(kep_target, elements.name);
    } catch (const std::exception&) {
        return std::nullopt;
    }
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
// Geometric Constants
// Use OBLIQUITY_J2000 from types.h

// Conversion Helpers (bridge AstDySElements <-> AstDyn Keplerian/Cartesian)
struct Cartesian { double x, y, z, vx, vy, vz; };

namespace propagation = astdyn::propagation;

static propagation::KeplerianElements toAstDynKeplerian(const AstDySElements& el) {
    propagation::KeplerianElements kep;
    kep.epoch_mjd_tdb = el.epoch_mjd;
    kep.semi_major_axis = el.a;
    kep.eccentricity = el.e;
    kep.inclination = el.i * ac::DEG_TO_RAD;
    kep.longitude_ascending_node = el.Omega * ac::DEG_TO_RAD;
    kep.argument_perihelion = el.omega * ac::DEG_TO_RAD;
    kep.mean_anomaly = el.M * ac::DEG_TO_RAD;
    kep.gravitational_parameter = ac::GMS;
    return kep;
}

static AstDySElements fromAstDynKeplerian(const propagation::KeplerianElements& kep, const std::string& name) {
    AstDySElements el;
    el.name = name;
    el.epoch_mjd = kep.epoch_mjd_tdb;
    el.a = kep.semi_major_axis;
    el.e = kep.eccentricity;
    el.i = kep.inclination * ac::RAD_TO_DEG;
    el.Omega = kep.longitude_ascending_node * ac::RAD_TO_DEG;
    el.omega = kep.argument_perihelion * ac::RAD_TO_DEG;
    el.M = kep.mean_anomaly * ac::RAD_TO_DEG;
    return el;
}

static Cartesian keplerianToCartesianElem(const AstDySElements& el) {
    propagation::KeplerianElements kep = toAstDynKeplerian(el);
    propagation::CartesianElements cart = propagation::keplerian_to_cartesian(kep);
    Cartesian c;
    c.x = cart.position(0);
    c.y = cart.position(1);
    c.z = cart.position(2);
    c.vx = cart.velocity(0);
    c.vy = cart.velocity(1);
    c.vz = cart.velocity(2);
    return c;
}

static AstDySElements cartesianToKeplerianElem(const Cartesian& c, astdyn::MJD epoch, const std::string& name) {
    propagation::CartesianElements cart;
    cart.epoch_mjd_tdb = epoch;
    cart.position << c.x, c.y, c.z;
    cart.velocity << c.vx, c.vy, c.vz;
    cart.gravitational_parameter = ac::GMS;
    propagation::KeplerianElements kep = propagation::cartesian_to_keplerian(cart);
    return fromAstDynKeplerian(kep, name);
}

static AstDySElements convertEclipticToEquatorial(const AstDySElements& el) {
    Cartesian c = keplerianToCartesianElem(el);
    double jd = at::mjd_to_jd(el.epoch_mjd);
    double eps = meanObliquityOfEclipticRad(jd);
    double co = std::cos(eps);
    double so = std::sin(eps);

    Cartesian eq;
    eq.x  = c.x;
    eq.y  = c.y * co - c.z * so;
    eq.z  = c.y * so + c.z * co;
    eq.vx = c.vx;
    eq.vy = c.vy * co - c.vz * so;
    eq.vz = c.vy * so + c.vz * co;

    return cartesianToKeplerianElem(eq, el.epoch_mjd, el.name);
}

void AstDynOrbitFitter::setConvergenceTolerance(double tol_au) { pimpl_->convTolerance = tol_au; }
void AstDynOrbitFitter::setVerbose(bool verbose) { pimpl_->verbose = verbose; }

// Helper per scrivere file RWO (Strict OrbFit Format)
static void writeRWO(const std::string& path, const std::string& name, const std::vector<RWOObservation>& obs) {
    std::ofstream f(path);
    if (!f.is_open()) {
        std::cerr << "CRITICAL ERROR: Cannot open RWO file for writing: " << path << std::endl;
        return;
    }
    
    f << " OBJECT: " << name << "\n";
    f << " errmod: iau_2010\n";
    f << " version: 1.0\n";
    f << " END_OF_HEADER\n";
    
    for (const auto& o : obs) {
        // Conversione MJD -> Date
        int y, m; double d;
        double mjd = o.mjd_utc;
        double jd = at::mjd_to_jd(mjd);
        int Z = (int)(jd + 0.5);
        double F = jd + 0.5 - Z;
        int A = Z;
        if (Z >= 2299161) {
            int alpha = (int)((Z - 1867216.25) / 36524.25);
            A = Z + 1 + alpha - (int)(alpha / 4.0);
        }
        int B = A + 1524;
        int C = (int)((B - 122.1) / 365.25);
        int D = (int)(365.25 * C);
        int E = (int)((B - D) / 30.6001);
        d = B - D - (int)(30.6001 * E) + F;
        m = (E < 14) ? E - 1 : E - 13;
        y = (m > 2) ? C - 4716 : C - 4715;

        // Formattazione RA/Dec per colonne fisse
        int rh = (int)(o.ra_deg / 15.0);
        int rm = (int)((o.ra_deg / 15.0 - rh) * 60.0);
        double rs = ((o.ra_deg / 15.0 - rh) * 60.0 - rm) * 60.0;
        
        double dec = std::abs(o.dec_deg);
        char sign = (o.dec_deg >= 0) ? '+' : '-';
        int dd = (int)dec;
        int dm = (int)((dec - dd) * 60.0);
        double ds = ((dec - dd) * 60.0 - dm) * 60.0;

        // Construct line buffer initialized with spaces
        std::string line(160, ' ');

        // Name (1-indexed col 2-10 -> 0-indexed 1-9)
        std::string n_str = name;
        if (n_str.length() > 9) n_str = n_str.substr(0, 9);
        for(size_t i=0; i<n_str.length(); ++i) line[1+i] = n_str[i];

        // Type (col 12 -> 11)
        line[11] = 'O';

        // Date (col 18 -> 17). Format: "YYYY MM DD.dddddddddd" (21 chars)
        char date_buf[32];
        snprintf(date_buf, sizeof(date_buf), "%04d %02d %013.10f", y, m, d);
        for(int i=0; i<21 && date_buf[i]; ++i) line[17+i] = date_buf[i];
        
        // RA (col 51 -> 50). Format: "HH MM SS.sss" (12 chars)
        char ra_buf[32];
        snprintf(ra_buf, sizeof(ra_buf), "%02d %02d %06.3f", rh, rm, rs);
        for(int i=0; i<12 && ra_buf[i]; ++i) line[50+i] = ra_buf[i];

        // RA RMS (col 74 -> 73). Format: "xxxx.xxxx" (9 chars)
        char ra_rms[16];
        snprintf(ra_rms, sizeof(ra_rms), "%9.4f", o.ra_sigma_arcsec);
        for(int i=0; i<9 && ra_rms[i]; ++i) line[73+i] = ra_rms[i];

        // Dec (col 104 -> 103). Format: "sDD MM SS.ss" (13 chars)
        char dec_buf[32];
        snprintf(dec_buf, sizeof(dec_buf), "%c%02d %02d %05.2f", sign, dd, dm, ds);
        for(int i=0; i<13 && dec_buf[i]; ++i) line[103+i] = dec_buf[i];
        
        // Dec RMS (col 127 -> 126). Format: "xxxx.xxxx" (9 chars)
        char dec_rms[16];
        snprintf(dec_rms, sizeof(dec_rms), "%9.4f", o.dec_sigma_arcsec);
        for(int i=0; i<9 && dec_rms[i]; ++i) line[126+i] = dec_rms[i];

        // ObsCode (col 151 -> 150)
        std::string o_code = o.obs_code;
        if (o_code.empty()) o_code = "500";
        if (o_code.length() > 3) o_code = o_code.substr(0, 3);
        for(size_t i=0; i<o_code.length(); ++i) line[150+i] = o_code[i];

        line[199] = '\0'; // Ensure termination
        f << line << "\n";
    }
    f.close();
}

// Helper per scrivere file EQ1 (Equinoctial with formal headers)
static void writeEQ1(const std::string& path, const AstDySElements& el) {
    // Converti Keplerian (Degrees) -> Equinoctial
    double a = el.a;
    double e = el.e;
    double i_rad = el.i * ac::DEG_TO_RAD;
    double Omega_rad = el.Omega * ac::DEG_TO_RAD;
    double omega_rad = el.omega * ac::DEG_TO_RAD;
    double M_rad = el.M * ac::DEG_TO_RAD;
    
    double h = e * std::sin(omega_rad + Omega_rad);
    double k = e * std::cos(omega_rad + Omega_rad);
    double tan_i2 = std::tan(i_rad / 2.0);
    double p = tan_i2 * std::sin(Omega_rad);
    double q = tan_i2 * std::cos(Omega_rad);
    double lambda_rad = M_rad + omega_rad + Omega_rad;
    
    // Normalize lambda 0-2PI
    while (lambda_rad < 0.0) lambda_rad += ac::TWO_PI;
    while (lambda_rad >= ac::TWO_PI) lambda_rad -= ac::TWO_PI;

    std::ofstream f(path);
    if (!f.is_open()) {
        std::cerr << "CRITICAL ERROR: Cannot open EQ1 file for writing: " << path << std::endl;
        return;
    }

    // Header standard per evitare errori se il parser è pignolo
    f << "format  = 'OEF2.0'       ! file format\n";
    f << "rectype = 'ML'           ! record type (1L/ML)\n";
    f << "refsys  = 'ECLM J2000'   ! reference system (Mean Ecliptic)\n";
    f << "END_OF_HEADER\n";
    
    // Semplificato: parole chiave all'inizio della riga
    f << "EQU " << std::fixed << std::setprecision(16) 
      << a << " " << h << " " << k << " " << p << " " << q << " " << (lambda_rad * ac::RAD_TO_DEG) << "\n";
    f << "MJD " << std::fixed << std::setprecision(8) << el.epoch_mjd << " TDT\n";
    f.close();
}

OrbitFitResult AstDynOrbitFitter::fit(const AstDySElements& initial_elements,
                                     const std::vector<RWOObservation>& observations) {
    std::string tmp_eq1 = "/tmp/ioc_fit_init.eq1";
    std::string tmp_rwo = "/tmp/ioc_fit_obs.rwo";
    
    // Convert Ecliptic Elements -> Equatorial for Fitting
    AstDySElements init_eq = convertEclipticToEquatorial(initial_elements);
    
    // Debug Roundtrip
    // Convert back from Equatorial using inverse rotation
    double eps = meanObliquityOfEclipticRad(at::mjd_to_jd(init_eq.epoch_mjd));
    double co = std::cos(eps); // Positive for Eq -> Ecl
    double so = std::sin(eps);
    Cartesian eq_c = keplerianToCartesianElem(init_eq);
    Cartesian ecl_back;
    ecl_back.x = eq_c.x;
    ecl_back.y = eq_c.y * co + eq_c.z * so;
    ecl_back.z = -eq_c.y * so + eq_c.z * co;
    ecl_back.vx = eq_c.vx;
    ecl_back.vy = eq_c.vy * co + eq_c.vz * so;
    ecl_back.vz = -eq_c.vy * so + eq_c.vz * co;
    AstDySElements back = cartesianToKeplerianElem(ecl_back, init_eq.epoch_mjd, init_eq.name);
    
    std::cout << "Debug Rotation:\n"
              << " Orig: i=" << initial_elements.i << " O=" << initial_elements.Omega << " w=" << initial_elements.omega << "\n"
              << " Eq:   i=" << init_eq.i << " O=" << init_eq.Omega << " w=" << init_eq.omega << "\n"
              << " Back: i=" << back.i << " O=" << back.Omega << " w=" << back.omega << "\n";
              
    // Write ORIGINAL (Ecliptic) elements to EQ1 file.
    // OrbitFitAPI::run_fit will handle the Ecliptic -> Equatorial transformation.
    writeEQ1(tmp_eq1, initial_elements);
    writeRWO(tmp_rwo, init_eq.name, observations);
    
    // Debug info
    if (!observations.empty()) {
        double min_mjd = 1e9, max_mjd = -1e9;
        for (const auto& obs : observations) {
            if (obs.mjd_utc < min_mjd) min_mjd = obs.mjd_utc;
            if (obs.mjd_utc > max_mjd) max_mjd = obs.mjd_utc;
        }
        std::cout << "[AstDynOrbitFitter] Feeding " << observations.size() << " obs to OrbitFitAPI.\n"
                  << "   Time Range: MJD " << min_mjd << " to " << max_mjd << "\n"
                  << "   Initial Element Epoch: " << initial_elements.epoch_mjd << "\n";
    }
    
    try {
        auto res = astdyn::api::OrbitFitAPI::run_fit(tmp_eq1, tmp_rwo, "", true);
        
        std::cout << "[AstDynOrbitFitter] run_fit finished. success=" << res.success 
                  << " msg='" << res.message << "'\n";

        OrbitFitResult out;
        out.n_observations = 0;
        out.n_used = 0;
        out.n_outliers = 0;
        out.rms_total_arcsec = 0;
        
        if (res.success && res.num_observations > 0) {
            out.n_observations = res.num_observations;
            out.n_outliers = res.num_outliers;
            out.n_used = out.n_observations - out.n_outliers;
            out.rms_ra_arcsec = res.rms_ra;
            out.rms_dec_arcsec = res.rms_dec;
            out.rms_total_arcsec = std::sqrt(res.rms_ra*res.rms_ra + res.rms_dec*res.rms_dec);
            out.fitted_elements = initial_elements;
            
            // Aggiorna elementi con quelli fittati
            out.fitted_elements.a = res.fitted_orbit.semi_major_axis;
            out.fitted_elements.e = res.fitted_orbit.eccentricity;
            out.fitted_elements.i = res.fitted_orbit.inclination * ac::RAD_TO_DEG;
            out.fitted_elements.Omega = res.fitted_orbit.longitude_ascending_node * ac::RAD_TO_DEG;
            out.fitted_elements.omega = res.fitted_orbit.argument_perihelion * ac::RAD_TO_DEG;
            out.fitted_elements.M = res.fitted_orbit.mean_anomaly * ac::RAD_TO_DEG;
            out.fitted_elements.epoch_mjd = res.fitted_orbit.epoch_mjd_tdb;
            
            // CRITICAL FIX: The fitter always returns Equatorial ICRF elements
            out.fitted_elements.frame = FrameType::EQUATORIAL_ICRF;
            out.fitted_elements.type = ElementType::OSCULATING;

            // Estrarre covarianza se presente
            if (res.fitted_orbit.covariance.has_value()) {
                const auto& cov = *res.fitted_orbit.covariance;
                out.fitted_elements.has_covariance = true;
                out.fitted_elements.covariance.clear();
                out.fitted_elements.covariance.reserve(21); // Triangolo superiore 6x6
                for (int i = 0; i < 6; ++i) {
                    for (int j = i; j < 6; ++j) {
                        out.fitted_elements.covariance.push_back(cov(i, j));
                    }
                }
            } else {
                out.fitted_elements.has_covariance = false;
            }
        } else {
            out.n_used = 0;
            out.n_observations = 0;
            out.fitted_elements = initial_elements;
        }
        
        return out;
    } catch (const std::exception& e) {
        std::cerr << "AstDynOrbitFitter::fit EXCEPTION: " << e.what() << std::endl;
        OrbitFitResult err;
        err.n_used = 0;
        err.n_observations = 0;
        err.fitted_elements = initial_elements; // Fallback
        return err; // Return failure instead of crashing
    }
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

// ============================================================================
// astdyn_utils Implementation
// ============================================================================

namespace astdyn_utils {

AstDySElements toAstDySElements(const OrbitalElements& elem) {
    AstDySElements out;
    out.name = elem.designation;
    out.number = elem.number;
    out.a = elem.a;
    out.e = elem.e;
    out.i = elem.i * ac::RAD_TO_DEG;
    out.Omega = elem.Omega * ac::RAD_TO_DEG;
    out.omega = elem.omega * ac::RAD_TO_DEG;
    out.M = elem.M * ac::RAD_TO_DEG;
    out.epoch_mjd = at::jd_to_mjd(elem.epoch.jd);
    out.H = elem.H;
    out.G = elem.G;
    out.frame = elem.frame;
    out.type = elem.type;
    out.has_covariance = false; 

    // Se stiamo usando un framework che popola OrbitalElements con covarianza
    // dovremmo copiarla qui. Per ora assumiamo che provenga da AstDySElements.
    // Tuttavia, aggiungiamo lo scheletro per il futuro:
    /*
    if (elem.has_covariance) {
        out.has_covariance = true;
        out.covariance = elem.covariance;
    }
    */
    return out;
}

OrbitalElements fromAstDySElements(const AstDySElements& elem) {
    OrbitalElements out;
    out.number = elem.number;
    out.designation = elem.name;
    out.name = elem.name;
    out.a = elem.a;
    out.e = elem.e;
    out.i = elem.i * ac::RAD_TO_DEG;
    out.Omega = elem.Omega * ac::RAD_TO_DEG;
    out.omega = elem.omega * ac::RAD_TO_DEG;
    out.M = elem.M * ac::RAD_TO_DEG;
    out.epoch = JulianDate(at::mjd_to_jd(elem.epoch_mjd));
    out.H = elem.H;
    out.G = elem.G;
    out.frame = elem.frame;
    out.type = elem.type;
    return out;
}

// Stubs for other utils if needed, or leave for future implementation
RWOObservation toRWOObservation(const AstrometricObservation& obs) {
    // Already defined? No, this is the implementation file.
    RWOObservation rwo;
    rwo.mjd_utc = at::jd_to_mjd(obs.epoch.jd);
    rwo.ra_deg = obs.obs.ra * ac::RAD_TO_DEG;
    rwo.dec_deg = obs.obs.dec * ac::RAD_TO_DEG;
    rwo.ra_sigma_arcsec = obs.raError;
    rwo.dec_sigma_arcsec = obs.decError;
    rwo.obs_code = obs.observatoryCode;
    return rwo;
}

AstrometricObservation fromRWOObservation(const RWOObservation& rwo) {
    AstrometricObservation obs;
    obs.epoch.jd = at::mjd_to_jd(rwo.mjd_utc);
    obs.obs.ra = rwo.ra_deg * ac::DEG_TO_RAD;
    obs.obs.dec = rwo.dec_deg * ac::DEG_TO_RAD;
    obs.raError = rwo.ra_sigma_arcsec;
    obs.decError = rwo.dec_sigma_arcsec;
    obs.observatoryCode = rwo.obs_code;
    return obs;
}

std::string formatResidual(double arcsec) {
    std::stringstream ss;
    ss << std::fixed << std::setprecision(3) << arcsec;
    return ss.str();
}

std::string formatRMS(double arcsec) { return formatResidual(arcsec); }

std::string formatChi2(double chi2, int ndf) {
    std::stringstream ss;
    ss << std::fixed << std::setprecision(2) << chi2 << "/" << ndf;
    return ss.str();
}

std::optional<AstDySElements> parseEQ1File(const std::string& filename) {
    return AstDySElements::tryFromFile(filename);
}

std::optional<std::vector<RWOObservation>> parseRWOFile(const std::string& filename) {
    return RWOObservation::tryFromFile(filename);
}

} // namespace astdyn_utils

} // namespace ioccultcalc
