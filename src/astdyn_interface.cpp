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
    elem.i = i * DEG_TO_RAD;
    elem.Omega = Omega * DEG_TO_RAD;
    elem.omega = omega * DEG_TO_RAD;
    elem.M = M * DEG_TO_RAD;
    elem.H = H;
    elem.G = G;
    elem.frame = frame;
    elem.type = type;
    return elem;
}

AstDySElements AstDySElements::fromFile(const std::string& filename) {
    try {
        auto equ = astdyn::api::OrbitFitAPI::parse_eq1(filename);
        AstDySElements out;
        out.name = "Unknown"; // parse_eq1 doesn't return name yet
        out.number = 0;
        out.a = equ.a;
        // Convert Equinoctial to Keplerian for AstDySElements storage (Degrees)
        auto kep = astdyn::propagation::equinoctial_to_keplerian(equ);
        out.a = kep.semi_major_axis;
        out.e = kep.eccentricity;
        out.i = kep.inclination * RAD_TO_DEG;
        out.omega = kep.argument_perihelion * RAD_TO_DEG;
        out.Omega = kep.longitude_ascending_node * RAD_TO_DEG;
        out.M = kep.mean_anomaly * RAD_TO_DEG;
        out.epoch_mjd = equ.epoch_mjd_tdb;
        out.has_covariance = false;
        return out;
    } catch (const std::exception& e) {
        throw std::runtime_error("AstDySElements::fromFile failed: " + std::string(e.what()));
    }
}

// ============================================================================
// RWOObservation Implementation
// ============================================================================

AstrometricObservation RWOObservation::toObservation() const {
    AstrometricObservation obs;
    obs.epoch.jd = mjd_utc + 2400000.5;
    obs.obs.ra = ra_deg * DEG_TO_RAD;
    obs.obs.dec = dec_deg * DEG_TO_RAD;
    obs.raError = ra_sigma_arcsec;
    obs.decError = dec_sigma_arcsec;
    obs.observatoryCode = obs_code;
    return obs;
}

std::vector<RWOObservation> RWOObservation::fromFile(const std::string& filename) {
    auto internal_obs = astdyn::observations::RWOReader::readFile(filename);
    std::vector<RWOObservation> out;
    for (const auto& o : internal_obs) {
        RWOObservation r;
        r.designation = o.object_designation;
        r.mjd_utc = o.mjd_utc;
        r.ra_deg = o.ra * (180.0 / M_PI);
        r.dec_deg = o.dec * (180.0 / M_PI);
        r.ra_sigma_arcsec = o.sigma_ra * (180.0 / M_PI) * 3600.0;
        r.dec_sigma_arcsec = o.sigma_dec * (180.0 / M_PI) * 3600.0;
        r.obs_code = o.observatory_code;
        r.magnitude = o.magnitude.value_or(0.0);
        out.push_back(r);
    }
    return out;
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
// Geometric Constants
// Use OBLIQUITY_J2000 from types.h

// Conversion Helpers
struct Cartesian { double x, y, z, vx, vy, vz; };

static Cartesian keplerianToCartesianElem(const AstDySElements& el) {
    double a = el.a;
    double e = el.e;
    double i = el.i * DEG_TO_RAD;
    double O = el.Omega * DEG_TO_RAD;
    double w = el.omega * DEG_TO_RAD;
    double M = el.M * DEG_TO_RAD;
    
    // Solve Kepler
    double E = M;
    for(int k=0; k<15; k++) E = M + e*std::sin(E);
    
    double nu = 2.0 * std::atan(std::sqrt((1.0+e)/(1.0-e)) * std::tan(E/2.0));
    double r = a * (1.0 - e*std::cos(E));
    double mu = 0.01720209895 * 0.01720209895; // GMS
    double n = std::sqrt(mu / (a*a*a));
    
    // Pos/Vel in Orbital Plane
    double X = r * std::cos(nu);
    double Y = r * std::sin(nu);
    double p = a * (1.0 - e*e);
    double Vfac = std::sqrt(mu/p);
    double VX = -Vfac * std::sin(nu);
    double VY =  Vfac * (e + std::cos(nu));
    
    // Rotate to 3D (3-1-3 Euler: O, i, w) where w is argument of periapsis
    // Correction: The order is usually Rz(-O) Rx(-i) Rz(-w) to go Inertial -> Orbital.
    // So Orbital -> Inertial is Rz(O) Rx(i) Rz(w) ?? No.
    // Standard: 
    // x = r ( cO cw - sO sw ci ) - r sin(nu) ... no
    // Let's rotate vectors.
    // 1. Rotate by -w around Z (to align periapsis) -> actually vector is (X,Y,0) in Perifocal.
    //    Rotate by -w? No, 'w' is angle from Node to Periapsis.
    //    Perifocal to Nodal: Rz(-w)? No.
    //    Let's use standard direction cosine matrix P and Q vectors.
    // P = [ cO cw - sO sw ci ]
    //     [ sO cw + cO sw ci ]
    //     [ sw si            ]
    // Q = [ -cO sw - sO cw ci ]
    //     [ -sO sw + cO cw ci ]
    //     [ cw si             ]
    // r_vec = X*P + Y*Q
    // v_vec = VX*P + VY*Q
    
    double cw = std::cos(w), sw = std::sin(w);
    double cO = std::cos(O), sO = std::sin(O);
    double ci = std::cos(i), si = std::sin(i);
    
    double Px = cO*cw - sO*sw*ci;
    double Py = sO*cw + cO*sw*ci;
    double Pz = sw*si;
    
    double Qx = -cO*sw - sO*cw*ci;
    double Qy = -sO*sw + cO*cw*ci;
    double Qz = cw*si;
    
    Cartesian c;
    c.x = X*Px + Y*Qx;
    c.y = X*Py + Y*Qy;
    c.z = X*Pz + Y*Qz;
    c.vx = VX*Px + VY*Qx;
    c.vy = VX*Py + VY*Qy;
    c.vz = VX*Pz + VY*Qz;
    return c;
}

static AstDySElements cartesianToKeplerianElem(const Cartesian& c, double epoch, const std::string& name) {
    double mu = 0.01720209895 * 0.01720209895;
    double r = std::sqrt(c.x*c.x + c.y*c.y + c.z*c.z);
    double v2 = c.vx*c.vx + c.vy*c.vy + c.vz*c.vz;
    
    // h = r x v
    double hx = c.y*c.vz - c.z*c.vy;
    double hy = c.z*c.vx - c.x*c.vz;
    double hz = c.x*c.vy - c.y*c.vx;
    double h2 = hx*hx + hy*hy + hz*hz;
    double h = std::sqrt(h2);
    
    // i
    double i = std::acos(hz/h);
    
    // Omega
    double O = std::atan2(hx, -hy); // Node vector n = (-hy, hx, 0) -> No. n = k x h = (-hy, hx, 0).
    // O is angle of n. atan2(ny, nx) = atan2(hx, -hy). Correct.
    if (O < 0) O += 2*M_PI;
    
    // e vector
    // e = (1/mu) * ((v^2 - mu/r)*r - (r.v)*v)
    double rv = c.x*c.vx + c.y*c.vy + c.z*c.vz;
    double ex = (1.0/mu) * ((v2 - mu/r)*c.x - rv*c.vx);
    double ey = (1.0/mu) * ((v2 - mu/r)*c.y - rv*c.vy);
    double ez = (1.0/mu) * ((v2 - mu/r)*c.z - rv*c.vz);
    double e = std::sqrt(ex*ex + ey*ey + ez*ez);
    
    // omega
    // w is angle from n to e.
    // n = (-hy, hx, 0). n_mag = sqrt(hx^2+hy^2) = h sin i.
    // cos w = n.e / (n e). sin w = (n x e).k / (n e) ? No.
    // Easier: w = u - nu. u = arg of latitude.
    // Let's use dot products.
    // nx = -hy, ny = hx, nz = 0.
    // n.e = nx*ex + ny*ey.
    double nx = -hy, ny = hx;
    double nmag = std::sqrt(nx*nx + ny*ny);
    double w = 0;
    if (nmag > 1e-10 && e > 1e-10) {
        double c_w = (nx*ex + ny*ey) / (nmag*e);
        if (c_w > 1.0) c_w = 1.0; if (c_w < -1.0) c_w = -1.0;
        w = std::acos(c_w);
        if (ez < 0) w = 2*M_PI - w;
    }
    
    // a
    double a = 1.0 / (2.0/r - v2/mu);
    
    // M
    // find E first.
    // cos E = (1 - r/a) / e
    double E = 0;
     if (e > 1e-10) {
        double cE = (1.0 - r/a) / e;
        if(cE > 1.0) cE=1.0; if(cE < -1.0) cE=-1.0;
        E = std::acos(cE);
        if (rv < 0) E = 2*M_PI - E;
    }
    double M = E - e*std::sin(E);
    if(M<0) M += 2*M_PI;
    
    AstDySElements el;
    el.name = name;
    el.epoch_mjd = epoch;
    el.a = a;
    el.e = e;
    el.i = i * RAD_TO_DEG;
    el.Omega = O * RAD_TO_DEG;
    el.omega = w * RAD_TO_DEG;
    el.M = M * RAD_TO_DEG;
    return el;
}

static AstDySElements convertEclipticToEquatorial(const AstDySElements& el) {
    Cartesian c = keplerianToCartesianElem(el);
    
    // Rotate +Obliquity (Ecl -> Eq)
    // Rx(eps):
    // y' = y c - z s
    // z' = y s + z c
    double eps = OBLIQUITY_J2000;
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
        double jd = mjd + 2400000.5;
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
    double i_rad = el.i * DEG_TO_RAD;
    double Omega_rad = el.Omega * DEG_TO_RAD;
    double omega_rad = el.omega * DEG_TO_RAD;
    double M_rad = el.M * DEG_TO_RAD;
    
    double h = e * std::sin(omega_rad + Omega_rad);
    double k = e * std::cos(omega_rad + Omega_rad);
    double tan_i2 = std::tan(i_rad / 2.0);
    double p = tan_i2 * std::sin(Omega_rad);
    double q = tan_i2 * std::cos(Omega_rad);
    double lambda_rad = M_rad + omega_rad + Omega_rad;
    
    // Normalize lambda 0-2PI
    while (lambda_rad < 0.0) lambda_rad += 2.0*M_PI;
    while (lambda_rad >= 2.0*M_PI) lambda_rad -= 2.0*M_PI;

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
      << a << " " << h << " " << k << " " << p << " " << q << " " << (lambda_rad * RAD_TO_DEG) << "\n";
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
    double eps = OBLIQUITY_J2000;
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
            out.fitted_elements.i = res.fitted_orbit.inclination * RAD_TO_DEG;
            out.fitted_elements.Omega = res.fitted_orbit.longitude_ascending_node * RAD_TO_DEG;
            out.fitted_elements.omega = res.fitted_orbit.argument_perihelion * RAD_TO_DEG;
            out.fitted_elements.M = res.fitted_orbit.mean_anomaly * RAD_TO_DEG;
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
    out.i = elem.i * RAD_TO_DEG;
    out.Omega = elem.Omega * RAD_TO_DEG;
    out.omega = elem.omega * RAD_TO_DEG;
    out.M = elem.M * RAD_TO_DEG;
    out.epoch_mjd = elem.epoch.jd - 2400000.5;
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
    out.i = elem.i * M_PI / 180.0;
    out.Omega = elem.Omega * M_PI / 180.0;
    out.omega = elem.omega * M_PI / 180.0;
    out.M = elem.M * M_PI / 180.0;
    out.epoch = JulianDate(elem.epoch_mjd + 2400000.5);
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
    rwo.mjd_utc = obs.epoch.jd - 2400000.5;
    rwo.ra_deg = obs.obs.ra * RAD_TO_DEG;
    rwo.dec_deg = obs.obs.dec * RAD_TO_DEG;
    rwo.ra_sigma_arcsec = obs.raError;
    rwo.dec_sigma_arcsec = obs.decError;
    rwo.obs_code = obs.observatoryCode;
    return rwo;
}

AstrometricObservation fromRWOObservation(const RWOObservation& rwo) {
    AstrometricObservation obs;
    obs.epoch.jd = rwo.mjd_utc + 2400000.5;
    obs.obs.ra = rwo.ra_deg * DEG_TO_RAD;
    obs.obs.dec = rwo.dec_deg * DEG_TO_RAD;
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

AstDySElements parseEQ1File(const std::string& filename) {
     throw std::runtime_error("Not implemented");
}

std::vector<RWOObservation> parseRWOFile(const std::string& filename) {
     throw std::runtime_error("Not implemented");
}

} // namespace astdyn_utils

} // namespace ioccultcalc
