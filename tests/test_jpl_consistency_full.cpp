/**
 * @file test_jpl_consistency_full.cpp
 * @brief Test completo e puntiglioso: confronto Terra, stella (posizione astrometrica),
 *        asteroide (posizione astrometrica) con riferimenti JPL/esterni.
 *
 * Utilizza i file in tests/reference_data/:
 * - jpl_horizons_earth_2026mar10.txt (obbligatorio): Terra vs Sole, Ecliptic J2000, AU, AU/day
 * - star_reference_jd2461109.5.txt (opzionale): stella media + attesa apparente
 * - jpl_horizons_asteroid_2026mar10.txt (opzionale): asteroide, Ecliptic J2000, AU, AU/day
 * - asteroid_elements_*.txt (opzionale, se si testa l'asteroide): elementi per propagazione
 *
 * Scrive report dettagliato su stdout e in reference_data/jpl_consistency_report.txt
 * Exit 0 solo se tutti i confronti eseguiti passano le tolleranze.
 */

#include <cmath>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <map>

#include "ioccultcalc/coordinates.h"
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/orbital_elements.h"
#include "ioccultcalc/spice_spk_reader.h"
#include "ioccultcalc/star_catalog.h"
#include "ioccultcalc/types.h"

using namespace ioccultcalc;

static const double JD_TEST = 2461109.5;  // 2026-Mar-10 00:00 TDB

// Tolleranze (configurabili). Riferimento Terra/asteroide da JPL è DE441.
// Se si usa de440.bsp le differenze possono essere ~1e-4 AU; per match stretto usare de441.bsp.
struct Tolerances {
    double earth_pos_au = 5e-4;       // AU (~75e3 km); stringere a 1e-5 se stesso kernel
    double earth_vel_au_per_day = 5e-6;
    double star_app_arcsec = 0.1;     // posizione apparente (regressione stretta)
    double asteroid_pos_au = 1e-4;
    double asteroid_vel_au_per_day = 1e-6;
};

static std::ostringstream g_report;
static int g_fail_count = 0;
static Tolerances g_tol;

static void report(const std::string& s) {
    std::cout << s;
    g_report << s;
}

// Parse key = value (skip #, blank)
static bool parseRefFile(const std::string& path, std::map<std::string, double>& out) {
    std::ifstream f(path);
    if (!f) return false;
    std::string line;
    while (std::getline(f, line)) {
        if (line.empty() || line[0] == '#') continue;
        size_t eq = line.find('=');
        if (eq == std::string::npos) continue;
        std::string key = line.substr(0, eq);
        std::string val = line.substr(eq + 1);
        while (!key.empty() && (key.back() == ' ' || key.back() == '\t')) key.pop_back();
        while (!key.empty() && (key.front() == ' ' || key.front() == '\t')) key.erase(0, 1);
        while (!val.empty() && (val.front() == ' ' || val.front() == '\t')) val.erase(0, 1);
        try {
            out[key] = std::stod(val);
        } catch (...) { continue; }
    }
    return !out.empty();
}

// Legge riferimento Terra (X,Y,Z,VX,VY,VZ in AU, AU/day, frame Ecliptic J2000)
static bool loadEarthRef(const std::string& refDir, double& X, double& Y, double& Z,
                         double& VX, double& VY, double& VZ) {
    std::map<std::string, double> m;
    if (!parseRefFile(refDir + "/jpl_horizons_earth_2026mar10.txt", m)) return false;
    auto get = [&m](const std::string& k) -> double {
        auto it = m.find(k);
        return it != m.end() ? it->second : 0.0;
    };
    X = get("X"); Y = get("Y"); Z = get("Z");
    VX = get("VX"); VY = get("VY"); VZ = get("VZ");
    return (X != 0 || Y != 0 || Z != 0);
}

// Confronto Terra: nostro stato ICRF vs riferimento Ecliptic
static void testEarth(const std::string& refDir) {
    report("\n========== 1. TERRA (Earth vs Sun) ==========\n");
    report("Frame riferimento JPL: Ecliptic J2000.0. Nostro SPICE: ICRF J2000.\n");
    report("Confronto: convertiamo il nostro stato in Ecliptic e confrontiamo con JPL.\n\n");

    double refX, refY, refZ, refVX, refVY, refVZ;
    if (!loadEarthRef(refDir, refX, refY, refZ, refVX, refVY, refVZ)) {
        report("  [SKIP] File jpl_horizons_earth_2026mar10.txt non trovato o vuoto.\n");
        return;
    }

    Vector3D posRefEcl(refX, refY, refZ);
    Vector3D velRefEcl(refVX, refVY, refVZ);

    Vector3D posOurs = Ephemeris::getEarthPosition(JulianDate(JD_TEST));
    Vector3D velOurs = Ephemeris::getEarthVelocity(JulianDate(JD_TEST));

    // Nota: getEarthVelocity in branch SPICE potrebbe restituire km/s invece di AU/day
    // (bug noto: manca moltiplicazione KMS_TO_AUD). Il test riporta le unità effettive
    // confrontando con il riferimento in AU/day.
    Vector3D posOursEcl = Coordinates::equatorialToEcliptic(posOurs, JD_TEST);
    Vector3D velOursEcl = Coordinates::equatorialToEcliptic(velOurs, JD_TEST);

    Vector3D dPos = Vector3D(posOursEcl.x - refX, posOursEcl.y - refY, posOursEcl.z - refZ);
    Vector3D dVel = Vector3D(velOursEcl.x - refVX, velOursEcl.y - refVY, velOursEcl.z - refVZ);

    double dPosAu = std::sqrt(dPos.x*dPos.x + dPos.y*dPos.y + dPos.z*dPos.z);
    double dVelAud = std::sqrt(dVel.x*dVel.x + dVel.y*dVel.y + dVel.z*dVel.z);

    report("  Riferimento JPL (DE441, Ecliptic J2000) Pos [AU]:  ");
    report(std::to_string(refX) + " " + std::to_string(refY) + " " + std::to_string(refZ) + "\n");
    report("  Nostro (convertito Ecl.)  Pos [AU]:  ");
    report(std::to_string(posOursEcl.x) + " " + std::to_string(posOursEcl.y) + " " + std::to_string(posOursEcl.z) + "\n");
    report("  Delta pos [AU]: " + std::to_string(dPos.x) + " " + std::to_string(dPos.y) + " " + std::to_string(dPos.z) + "\n");
    report("  |Delta pos| [AU]: " + std::to_string(dPosAu) + "  (tolleranza " + std::to_string(g_tol.earth_pos_au) + ")\n");

    report("  JPL (Ecliptic) Vel [AU/day]: ");
    report(std::to_string(refVX) + " " + std::to_string(refVY) + " " + std::to_string(refVZ) + "\n");
    report("  Nostro (conv.) Vel [AU/day]: ");
    report(std::to_string(velOursEcl.x) + " " + std::to_string(velOursEcl.y) + " " + std::to_string(velOursEcl.z) + "\n");
    report("  |Delta vel| [AU/day]: " + std::to_string(dVelAud) + "  (tolleranza " + std::to_string(g_tol.earth_vel_au_per_day) + ")\n");

    if (dVelAud > 0.01 && dPosAu < 1e-4) {
        report("  NOTA: Se posizione OK ma velocità molto fuori, verificare unità: Ephemeris::getEarthVelocity\n");
        report("         con SPICE potrebbe restituire km/s invece di AU/day (manca KMS_TO_AUD).\n");
    }

    bool okPos = dPosAu <= g_tol.earth_pos_au;
    bool okVel = dVelAud <= g_tol.earth_vel_au_per_day;
    if (okPos && okVel) {
        report("  [PASS] Terra: posizione e velocità entro tolleranza.\n");
    } else {
        if (!okPos) { report("  [FAIL] Terra: posizione fuori tolleranza.\n"); g_fail_count++; }
        if (!okVel) { report("  [FAIL] Terra: velocità fuori tolleranza.\n"); g_fail_count++; }
    }
}

// Posizione apparente stella: mean (epoch + PM) + parallasse (stesso modello di Ephemeris / OccultationPredictor)
static void starApparentDirection(double ra_rad, double dec_rad, double ref_epoch_jd,
                                  double pmra_mas, double pmdec_mas, double parallax_mas,
                                  const JulianDate& jd, const Vector3D& earthHelio,
                                  double& app_ra_rad, double& app_dec_rad) {
    double ra = ra_rad, dec = dec_rad;
    Ephemeris::applyProperMotion(ra, dec, pmra_mas, pmdec_mas, ref_epoch_jd, jd.jd);
    Vector3D u(std::cos(ra)*std::cos(dec), std::sin(ra)*std::cos(dec), std::sin(dec));
    Vector3D appDir = Ephemeris::applyStellarParallax(u, parallax_mas, earthHelio);
    app_ra_rad = std::atan2(appDir.y, appDir.x);
    if (app_ra_rad < 0) app_ra_rad += 2.0 * M_PI;
    app_dec_rad = std::asin(std::max(-1.0, std::min(1.0, appDir.z)));
}

// Confronto stella: posizione astrometrica (media + PM + parallasse) vs attesa se fornita
static void testStar(const std::string& refDir) {
    report("\n========== 2. STELLA (posizione astrometrica / apparente) ==========\n");

    std::map<std::string, double> m;
    if (!parseRefFile(refDir + "/star_reference_jd2461109.5.txt", m)) {
        report("  [SKIP] File star_reference_jd2461109.5.txt non trovato.\n");
        report("  Formato atteso: ra_deg, dec_deg, ref_epoch_jd, pmra_mas, pmdec_mas, parallax_mas,\n");
        report("  opzionale: expected_app_ra_deg, expected_app_dec_deg\n");
        return;
    }

    auto get = [&m](const std::string& k, double def) -> double {
        auto it = m.find(k);
        return it != m.end() ? it->second : def;
    };

    double ra_deg = get("ra_deg", 0);
    double dec_deg = get("dec_deg", 0);
    double ref_epoch_jd = get("ref_epoch_jd", 2457388.5);
    double pmra = get("pmra_mas", 0);
    double pmdec = get("pmdec_mas", 0);
    double plx = get("parallax_mas", 0);
    double exp_app_ra_deg = get("expected_app_ra_deg", -999);
    double exp_app_dec_deg = get("expected_app_dec_deg", -999);

    double ra_rad = ra_deg * DEG_TO_RAD;
    double dec_rad = dec_deg * DEG_TO_RAD;

    Vector3D earthHelio = Ephemeris::getEarthPosition(JulianDate(JD_TEST));
    double app_ra_rad, app_dec_rad;
    starApparentDirection(ra_rad, dec_rad, ref_epoch_jd, pmra, pmdec, plx,
                         JulianDate(JD_TEST), earthHelio, app_ra_rad, app_dec_rad);

    double app_ra_deg = app_ra_rad * RAD_TO_DEG;
    double app_dec_deg = app_dec_rad * RAD_TO_DEG;

    report("  Stella: RA=" + std::to_string(ra_deg) + "° Dec=" + std::to_string(dec_deg) + "° (epoca JD " + std::to_string(ref_epoch_jd) + ")\n");
    report("  PM RA=" + std::to_string(pmra) + " mas/yr, PM Dec=" + std::to_string(pmdec) + " mas/yr, parallax=" + std::to_string(plx) + " mas\n");
    report("  Nostra posizione apparente a JD " + std::to_string(JD_TEST) + ": RA=" + std::to_string(app_ra_deg) + "° Dec=" + std::to_string(app_dec_deg) + "°\n");

    if (exp_app_ra_deg > -900 && exp_app_dec_deg > -900) {
        double dRa = (app_ra_deg - exp_app_ra_deg) * 3600.0 * std::cos(app_dec_rad); // arcsec
        double dDec = (app_dec_deg - exp_app_dec_deg) * 3600.0;
        double sep_arcsec = std::sqrt(dRa*dRa + dDec*dDec);
        report("  Riferimento atteso:     RA=" + std::to_string(exp_app_ra_deg) + "° Dec=" + std::to_string(exp_app_dec_deg) + "°\n");
        report("  Delta: " + std::to_string(sep_arcsec) + " arcsec (tolleranza " + std::to_string(g_tol.star_app_arcsec) + ")\n");
        if (sep_arcsec <= g_tol.star_app_arcsec) {
            report("  [PASS] Stella: posizione apparente entro tolleranza.\n");
        } else {
            report("  [FAIL] Stella: posizione apparente fuori tolleranza.\n");
            g_fail_count++;
        }
    } else {
        report("  (Nessun expected_app_ra/dec: riportato solo valore calcolato per verifica manuale.)\n");
    }

    // Auto-consistenza: ri-calcolo con formula minima inline e confronto
    if (plx > 0 && (pmra != 0 || pmdec != 0)) {
        double dt_yr = (JD_TEST - ref_epoch_jd) / 365.25;
        const double mas_rad = 1.0 / 1000.0 * ARCSEC_TO_RAD;
        double ra_inline = ra_rad + (pmra * mas_rad * dt_yr) / std::cos(dec_rad);
        double dec_inline = dec_rad + pmdec * mas_rad * dt_yr;
        Vector3D u_inline(std::cos(ra_inline)*std::cos(dec_inline), std::sin(ra_inline)*std::cos(dec_inline), std::sin(dec_inline));
        Vector3D app_inline = Ephemeris::applyStellarParallax(u_inline, plx, earthHelio);
        double ra_inline_out = std::atan2(app_inline.y, app_inline.x);
        if (ra_inline_out < 0) ra_inline_out += 2.0 * M_PI;
        double dec_inline_out = std::asin(std::max(-1.0, std::min(1.0, app_inline.z)));
        double dra = (app_ra_rad - ra_inline_out) * std::cos(app_dec_rad) * 3600.0 * 180.0 / M_PI;
        double ddec = (app_dec_rad - dec_inline_out) * 3600.0 * 180.0 / M_PI;
        double sep_inline = std::sqrt(dra*dra + ddec*ddec);
        report("  Auto-consistenza (PM inline + parallasse): delta " + std::to_string(sep_inline) + " arcsec\n");
        if (sep_inline > 0.01) {
            report("  [FAIL] Stella: discrepanza auto-consistenza (PM/parallasse).\n");
            g_fail_count++;
        } else {
            report("  [PASS] Stella: auto-consistenza OK.\n");
        }
    }
}

// Carica elementi asteroid da file semplice: una riga con epoch_jd a h k p q lambda (equinoctial)
// oppure da mappa chiave-valore
static bool loadAsteroidElements(const std::string& path, AstDynEquinoctialElements& elem) {
    std::map<std::string, double> m;
    if (!parseRefFile(path, m)) return false;
    auto get = [&m](const std::string& k, double def) -> double {
        auto it = m.find(k);
        return it != m.end() ? it->second : def;
    };
    elem.epoch = JulianDate(get("epoch_jd", JD_TEST));
    elem.a = get("a_au", 0);
    elem.h = get("h", 0);
    elem.k = get("k", 0);
    elem.p = get("p", 0);
    elem.q = get("q", 0);
    elem.lambda = get("lambda_rad", 0);
    elem.frame = FrameType::ECLIPTIC_J2000;
    elem.number = (int)get("number", 433);
    elem.designation = "433";
    return elem.a > 0;
}

// Confronto asteroide: propagazione vs riferimento JPL (Ecliptic)
static void testAsteroid(const std::string& refDir) {
    report("\n========== 3. ASTEROIDE (posizione astrometrica eliocentrica) ==========\n");

    std::map<std::string, double> ref;
    std::string refPath = refDir + "/jpl_horizons_asteroid_2026mar10.txt";
    if (!parseRefFile(refPath, ref)) {
        report("  [SKIP] File jpl_horizons_asteroid_2026mar10.txt non trovato.\n");
        report("  Formato: JD=..., X=, Y=, Z=, VX=, VY=, VZ= (Ecliptic J2000, AU, AU/day)\n");
        return;
    }

    auto get = [&ref](const std::string& k, double def) -> double {
        auto it = ref.find(k);
        return it != ref.end() ? it->second : def;
    };
    double jdRef = get("JD", JD_TEST);
    Vector3D posRef(get("X", 0), get("Y", 0), get("Z", 0));
    Vector3D velRef(get("VX", 0), get("VY", 0), get("VZ", 0));
    if (posRef.x == 0 && posRef.y == 0 && posRef.z == 0) {
        report("  [SKIP] jpl_horizons_asteroid_2026mar10.txt ha X,Y,Z=0 (dati non inseriti).\n");
        return;
    }

    // Cerca elementi: asteroid_elements_2026mar10.txt o simile
    std::string elemPath = refDir + "/asteroid_elements_2026mar10.txt";
    AstDynEquinoctialElements elem;
    if (!loadAsteroidElements(elemPath, elem)) {
        report("  [SKIP] File elementi " + elemPath + " non trovato o invalido.\n");
        report("  Formato: epoch_jd=, a_au=, h=, k=, p=, q=, lambda_rad=, number=\n");
        return;
    }

    PropagatorOptions opts;
    opts.integrator = IntegratorType::RKF78;
    opts.usePlanetaryPerturbations = true;
    opts.tolerance = 1e-12;
    OrbitPropagator prop(opts);

    OrbitState state = prop.propagate(elem, JulianDate(jdRef));
    // state.position/velocity sono nel frame degli elementi (eclittico)
    Vector3D posOurs = state.position;
    Vector3D velOurs = state.velocity;

    Vector3D dPos(posOurs.x - posRef.x, posOurs.y - posRef.y, posOurs.z - posRef.z);
    Vector3D dVel(velOurs.x - velRef.x, velOurs.y - velRef.y, velOurs.z - velRef.z);
    double dPosAu = std::sqrt(dPos.x*dPos.x + dPos.y*dPos.y + dPos.z*dPos.z);
    double dVelAud = std::sqrt(dVel.x*dVel.x + dVel.y*dVel.y + dVel.z*dVel.z);

    report("  JPL (Ecliptic) Pos [AU]: " + std::to_string(posRef.x) + " " + std::to_string(posRef.y) + " " + std::to_string(posRef.z) + "\n");
    report("  Nostro (prop.) Pos [AU]: " + std::to_string(posOurs.x) + " " + std::to_string(posOurs.y) + " " + std::to_string(posOurs.z) + "\n");
    report("  |Delta pos| [AU]: " + std::to_string(dPosAu) + "  (tolleranza " + std::to_string(g_tol.asteroid_pos_au) + ")\n");
    report("  JPL Vel [AU/day]: " + std::to_string(velRef.x) + " " + std::to_string(velRef.y) + " " + std::to_string(velRef.z) + "\n");
    report("  Nostro Vel [AU/day]: " + std::to_string(velOurs.x) + " " + std::to_string(velOurs.y) + " " + std::to_string(velOurs.z) + "\n");
    report("  |Delta vel| [AU/day]: " + std::to_string(dVelAud) + "  (tolleranza " + std::to_string(g_tol.asteroid_vel_au_per_day) + ")\n");

    bool okPos = dPosAu <= g_tol.asteroid_pos_au;
    bool okVel = dVelAud <= g_tol.asteroid_vel_au_per_day;
    if (okPos && okVel) {
        report("  [PASS] Asteroide: posizione e velocità entro tolleranza.\n");
    } else {
        if (!okPos) { report("  [FAIL] Asteroide: posizione fuori tolleranza.\n"); g_fail_count++; }
        if (!okVel) { report("  [FAIL] Asteroide: velocità fuori tolleranza.\n"); g_fail_count++; }
    }
}

// Trova directory reference: da argv, o fallback se eseguito da build/
static std::string findReferenceDir(const std::string& fromArg) {
    if (!fromArg.empty()) {
        std::ifstream f(fromArg + "/jpl_horizons_earth_2026mar10.txt");
        if (f) return fromArg;
    }
    const char* candidates[] = {
        "tests/reference_data",
        "reference_data",
        "../tests/reference_data",
        "../../tests/reference_data"
    };
    for (const char* c : candidates) {
        std::ifstream f(std::string(c) + "/jpl_horizons_earth_2026mar10.txt");
        if (f) return c;
    }
    return fromArg.empty() ? "tests/reference_data" : fromArg;
}

int main(int argc, char** argv) {
    std::string refDir = findReferenceDir(argc > 1 ? argv[1] : "");

    g_report.str("");
    g_report.clear();
    g_fail_count = 0;

    report("============================================\n");
    report("  Test JPL consistency (Terra, Stella, Asteroide)\n");
    report("  JD " + std::to_string(JD_TEST) + " (2026-Mar-10 00:00 TDB)\n");
    report("  Reference dir: " + refDir + "\n");
    report("============================================\n");

    auto reader = std::make_shared<SPICESPKReader>();
    std::string spkPath = "de440.bsp";
    if (argc > 2) spkPath = argv[2];
    if (!reader->ensureFileLoaded(spkPath)) {
        report("ERRORE: Impossibile caricare SPK " + spkPath + ". Verificare path e kernel.\n");
        return 1;
    }
    initializeSpiceProvider(reader);

    testEarth(refDir);
    testStar(refDir);
    testAsteroid(refDir);

    report("\n============================================\n");
    if (g_fail_count == 0) {
        report("  RISULTATO: Tutti i confronti eseguiti sono PASS.\n");
    } else {
        report("  RISULTATO: " + std::to_string(g_fail_count) + " FAIL. Intervenire sulle discrepanze.\n");
    }
    report("============================================\n");

    std::string reportPath = refDir + "/jpl_consistency_report.txt";
    std::ofstream f(reportPath);
    if (f) f << g_report.str();

    return g_fail_count > 0 ? 1 : 0;
}
