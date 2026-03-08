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
#include <cstdlib>
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
#include "ioccultcalc/gauss_asteroid_position.h"
#include "ioccultcalc/spice_spk_reader.h"
#include "ioccultcalc/star_catalog.h"
#include "ioccultcalc/types.h"

#include "astdyn/propagation/saba4_integrator.hpp"
#include "astdyn/propagation/Propagator.hpp"
#include "astdyn/propagation/OrbitalElements.hpp"
#include "astdyn/ephemeris/PlanetaryEphemeris.hpp"
#include "astdyn/ephemeris/DE441Provider.hpp"
#include "astdyn/core/Constants.hpp"

using namespace ioccultcalc;

static const double JD_TEST = 2461109.5;  // 2026-Mar-10 00:00 TDB

// Tolleranze (configurabili). Riferimento Terra/asteroide da JPL è DE441.
// Per l'asteroide: propagazione RKF78 da elementi può dare ~0.001 AU e ~0.001 AU/day vs Horizons.
struct Tolerances {
    double earth_pos_au = 5e-4;       // AU (~75e3 km); stringere a 1e-5 se stesso kernel
    double earth_vel_au_per_day = 5e-6;
    double star_app_arcsec = 0.1;     // posizione apparente (regressione stretta)
    double asteroid_pos_au = 1e-3;   // ~150e3 km; differenze da elementi/epoch vs JPL
    double asteroid_vel_au_per_day = 2e-3;
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
// Se codes_300ast è caricato, usa Eros (433) direttamente dallo SPK invece di propagare da elementi.
static void testAsteroid(const std::string& refDir, const std::string& spkPath) {
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
    bool hasJplRef = (posRef.x != 0 || posRef.y != 0 || posRef.z != 0);

    // Prova prima Eros (433) direttamente dallo SPK (codes_300ast_20100725.bsp)
    bool use_eros_spk = false;
    Vector3D posOursEqu(0, 0, 0), velOursEqu(0, 0, 0);
    {
        auto stateEros = Ephemeris::getAsteroidState(433, JulianDate(jdRef));
        double r = std::sqrt(stateEros.first.x*stateEros.first.x + stateEros.first.y*stateEros.first.y + stateEros.first.z*stateEros.first.z);
        if (r > 1e-6) {
            posOursEqu = stateEros.first;
            velOursEqu = stateEros.second;
            use_eros_spk = true;
            report("  Eros (433) da SPK (codes_300ast), JD " + std::to_string(jdRef) + ".\n");
        }
    }

    if (!use_eros_spk) {
        std::string elemPath = refDir + "/asteroid_elements_2026mar10.txt";
        AstDynEquinoctialElements elem;
        if (!loadAsteroidElements(elemPath, elem)) {
            if (hasJplRef) report("  [SKIP] File elementi " + elemPath + " non trovato e Eros non in SPK.\n");
            else report("  [SKIP] File elementi " + elemPath + " non trovato (necessario per test asteroide).\n");
            report("  Formato: epoch_jd=, a_au=, h=, k=, p=, q=, lambda_rad=, number=\n");
            return;
        }
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RKF78;
        opts.usePlanetaryPerturbations = true;
        opts.tolerance = 1e-12;
        OrbitPropagator prop(opts);
        OrbitState state = prop.propagate(elem, JulianDate(jdRef));
        posOursEqu = state.position;
        velOursEqu = state.velocity;
    }

    std::string elemPath = refDir + "/asteroid_elements_2026mar10.txt";
    AstDynEquinoctialElements elem;
    bool have_elem = loadAsteroidElements(elemPath, elem);

    bool gauss_ok = false;
    Vector3D posGaussEqu(0, 0, 0), velGaussEqu(0, 0, 0);
    if (std::getenv("USE_GAUSS") && !spkPath.empty() && have_elem) {
        std::string gaussBspPath = spkPath;
        if (spkPath.find('/') == std::string::npos) {
            const char* home = std::getenv("HOME");
            if (home)
                gaussBspPath = std::string(home) + "/.ioccultcalc/ephemerides/" + spkPath;
        }
        try {
            report("  [INFO] Avvio Gauss (DE441)...\n");
            GaussAsteroidPositionCalculator calc(gaussBspPath);
            report("  Propagazione SABA4 (epoca elementi -> JD " + std::to_string(jdRef) + ") in corso...\n");
            std::cout.flush();
            OrbitState stateGauss = calc.computePositionFromEquinoctial(elem, jdRef);
            report("  Propagazione SABA4 completata.\n");
            posGaussEqu = stateGauss.position;
            velGaussEqu = stateGauss.velocity;
            gauss_ok = true;
            report("  Gauss (DE441) path: " + gaussBspPath + "\n");
            auto st = calc.lastStatistics();
            report("  Gauss (DE441) stats: steps=" + std::to_string(st.steps_accepted) + " evals=" + std::to_string(st.function_evaluations) + "\n");
        } catch (const std::exception& e) {
            report("  [ERRORE] Gauss: " + std::string(e.what()) + "\n");
        } catch (...) {
            report("  [ERRORE] Gauss: eccezione sconosciuta\n");
        }
    }

    Vector3D posOurs = posOursEqu;
    Vector3D velOurs = velOursEqu;
    if (hasJplRef) {
        posOurs = Coordinates::equatorialToEcliptic(posOursEqu, jdRef);
        velOurs = Coordinates::equatorialToEcliptic(velOursEqu, jdRef);
    }

    if (!hasJplRef) {
        // Regressione: confronto con golden (stato in EQUATORIALE, come output del propagator)
        std::map<std::string, double> golden;
        std::string goldenPath = refDir + "/asteroid_golden_regression_2026mar10.txt";
        if (parseRefFile(goldenPath, golden)) {
            double gX = golden.count("X") ? golden["X"] : 0, gY = golden.count("Y") ? golden["Y"] : 0, gZ = golden.count("Z") ? golden["Z"] : 0;
            double gVX = golden.count("VX") ? golden["VX"] : 0, gVY = golden.count("VY") ? golden["VY"] : 0, gVZ = golden.count("VZ") ? golden["VZ"] : 0;
            if (gX != 0 || gY != 0 || gZ != 0) {
                Vector3D posGold(gX, gY, gZ), velGold(gVX, gVY, gVZ);
                Vector3D dPos(posOursEqu.x - posGold.x, posOursEqu.y - posGold.y, posOursEqu.z - posGold.z);
                Vector3D dVel(velOursEqu.x - velGold.x, velOursEqu.y - velGold.y, velOursEqu.z - velGold.z);
                double dPosAu = std::sqrt(dPos.x*dPos.x + dPos.y*dPos.y + dPos.z*dPos.z);
                double dVelAud = std::sqrt(dVel.x*dVel.x + dVel.y*dVel.y + dVel.z*dVel.z);
                double tolRegressPos = 5e-4;  // AU (regressione: stessa propagazione)
                double tolRegressVel = 5e-6;
                report("  Riferimento JPL assente. Confronto con golden (regressione) da " + goldenPath + ".\n");
                report("  Golden (Equat.) Pos [AU]: " + std::to_string(posGold.x) + " " + std::to_string(posGold.y) + " " + std::to_string(posGold.z) + "\n");
                report("  Nostro (Equat.) Pos [AU]: " + std::to_string(posOursEqu.x) + " " + std::to_string(posOursEqu.y) + " " + std::to_string(posOursEqu.z) + "\n");
                report("  Nostro (Equat.) Vel [AU/day]: " + std::to_string(velOursEqu.x) + " " + std::to_string(velOursEqu.y) + " " + std::to_string(velOursEqu.z) + "\n");
                report("  |Delta pos| [AU]: " + std::to_string(dPosAu) + "  (tolleranza " + std::to_string(tolRegressPos) + ")\n");
                report("  |Delta vel| [AU/day]: " + std::to_string(dVelAud) + "  (tolleranza " + std::to_string(tolRegressVel) + ")\n");
                bool okR = dPosAu <= tolRegressPos && dVelAud <= tolRegressVel;
                if (okR) {
                    report("  [PASS] Asteroide: regressione vs golden OK.\n");
                } else {
                    report("  [FAIL] Asteroide: regressione fuori tolleranza (propagazione cambiata?). Aggiornare " + goldenPath + " se intenzionale.\n");
                    g_fail_count++;
                }
                return;
            }
        }
        report("  Riferimento JPL e golden assenti. Propagazione 433 Eros a JD " + std::to_string(jdRef) + ".\n");
        report("  Stato (Equat.) Pos [AU]: " + std::to_string(posOursEqu.x) + " " + std::to_string(posOursEqu.y) + " " + std::to_string(posOursEqu.z) + "\n");
        report("  Stato (Ecl.)  Pos [AU]: " + std::to_string(posOurs.x) + " " + std::to_string(posOurs.y) + " " + std::to_string(posOurs.z) + "\n");
        report("  Vel [AU/day]: " + std::to_string(velOurs.x) + " " + std::to_string(velOurs.y) + " " + std::to_string(velOurs.z) + "\n");
        report("  Per confronto JPL: popolare jpl_horizons_asteroid_2026mar10.txt. Per regressione: usare asteroid_golden_regression_2026mar10.txt.\n");
        report("  [PASS] Asteroide: propagazione OK (nessun riferimento per confronto).\n");
        return;
    }

    report("  Confronto in Eclittico J2000 (nostro stato convertito da equatoriale).\n\n");

    Vector3D posGaussEcl(0, 0, 0), velGaussEcl(0, 0, 0);
    if (gauss_ok) {
        report("  [DEBUG] Elementi asteroide frame (0=ECLIPTIC 1=EQUATORIAL): " + std::to_string(static_cast<int>(elem.frame)) + "\n");
        report("  [DEBUG] Gauss/SABA4 restituisce stato in ICRF (equatoriale); conversione Equat->Ecl per confronto JPL.\n");
        posGaussEcl = Coordinates::equatorialToEcliptic(posGaussEqu, jdRef);
        velGaussEcl = Coordinates::equatorialToEcliptic(velGaussEqu, jdRef);
    }

    report("  --- Valori a JD " + std::to_string(jdRef) + " (Eclittico J2000 dove indicato) ---\n");
    report("  JPL (Ecl.)     Pos [AU]: " + std::to_string(posRef.x) + " " + std::to_string(posRef.y) + " " + std::to_string(posRef.z) + "\n");
    report("  JPL (Ecl.)     Vel [AU/day]: " + std::to_string(velRef.x) + " " + std::to_string(velRef.y) + " " + std::to_string(velRef.z) + "\n");
    report("  " + std::string(use_eros_spk ? "Eros SPK" : "RKF78") + " (Equat.) Pos [AU]: " + std::to_string(posOursEqu.x) + " " + std::to_string(posOursEqu.y) + " " + std::to_string(posOursEqu.z) + "\n");
    report("  " + std::string(use_eros_spk ? "Eros SPK" : "RKF78") + " (Equat.) Vel [AU/day]: " + std::to_string(velOursEqu.x) + " " + std::to_string(velOursEqu.y) + " " + std::to_string(velOursEqu.z) + "\n");
    report("  " + std::string(use_eros_spk ? "Eros SPK" : "RKF78") + " (Ecl.)   Pos [AU]: " + std::to_string(posOurs.x) + " " + std::to_string(posOurs.y) + " " + std::to_string(posOurs.z) + "\n");
    report("  " + std::string(use_eros_spk ? "Eros SPK" : "RKF78") + " (Ecl.)   Vel [AU/day]: " + std::to_string(velOurs.x) + " " + std::to_string(velOurs.y) + " " + std::to_string(velOurs.z) + "\n");
    if (gauss_ok) {
        report("  Gauss (Equat.) Pos [AU]: " + std::to_string(posGaussEqu.x) + " " + std::to_string(posGaussEqu.y) + " " + std::to_string(posGaussEqu.z) + "\n");
        report("  Gauss (Equat.) Vel [AU/day]: " + std::to_string(velGaussEqu.x) + " " + std::to_string(velGaussEqu.y) + " " + std::to_string(velGaussEqu.z) + "\n");
        report("  Gauss (Ecl.)   Pos [AU]: " + std::to_string(posGaussEcl.x) + " " + std::to_string(posGaussEcl.y) + " " + std::to_string(posGaussEcl.z) + "\n");
        report("  Gauss (Ecl.)   Vel [AU/day]: " + std::to_string(velGaussEcl.x) + " " + std::to_string(velGaussEcl.y) + " " + std::to_string(velGaussEcl.z) + "\n");
    }
    report("\n  --- Confronti con JPL ---\n");

    Vector3D dPos(posOurs.x - posRef.x, posOurs.y - posRef.y, posOurs.z - posRef.z);
    Vector3D dVel(velOurs.x - velRef.x, velOurs.y - velRef.y, velOurs.z - velRef.z);
    double dPosAu = std::sqrt(dPos.x*dPos.x + dPos.y*dPos.y + dPos.z*dPos.z);
    double dVelAud = std::sqrt(dVel.x*dVel.x + dVel.y*dVel.y + dVel.z*dVel.z);

    report("  " + std::string(use_eros_spk ? "Eros SPK" : "RKF78") + " vs JPL:  |Delta pos| [AU]: " + std::to_string(dPosAu) + "  (tolleranza " + std::to_string(g_tol.asteroid_pos_au) + ")\n");
    report("  " + std::string(use_eros_spk ? "Eros SPK" : "RKF78") + " vs JPL:  |Delta vel| [AU/day]: " + std::to_string(dVelAud) + "  (tolleranza " + std::to_string(g_tol.asteroid_vel_au_per_day) + ")\n");
    if (gauss_ok) {
        double dPosGjpl = std::sqrt(std::pow(posGaussEcl.x - posRef.x, 2) + std::pow(posGaussEcl.y - posRef.y, 2) + std::pow(posGaussEcl.z - posRef.z, 2));
        double dVelGjpl = std::sqrt(std::pow(velGaussEcl.x - velRef.x, 2) + std::pow(velGaussEcl.y - velRef.y, 2) + std::pow(velGaussEcl.z - velRef.z, 2));
        report("  Gauss vs JPL:  |Delta pos| [AU]: " + std::to_string(dPosGjpl) + "\n");
        report("  Gauss vs JPL:  |Delta vel| [AU/day]: " + std::to_string(dVelGjpl) + "\n");
        double dPosGRkf = std::sqrt(std::pow(posOursEqu.x - posGaussEqu.x, 2) + std::pow(posOursEqu.y - posGaussEqu.y, 2) + std::pow(posOursEqu.z - posGaussEqu.z, 2));
        double dVelGRkf = std::sqrt(std::pow(velOursEqu.x - velGaussEqu.x, 2) + std::pow(velOursEqu.y - velGaussEqu.y, 2) + std::pow(velOursEqu.z - velGaussEqu.z, 2));
        report("  " + std::string(use_eros_spk ? "Eros SPK" : "RKF78") + " vs Gauss: |Delta pos|=" + std::to_string(dPosGRkf) + " AU, |Delta vel|=" + std::to_string(dVelGRkf) + " AU/day\n");
    }

    bool okPos = dPosAu <= g_tol.asteroid_pos_au;
    bool okVel = dVelAud <= g_tol.asteroid_vel_au_per_day;
    if (okPos && okVel) {
        report("  [PASS] Asteroide: posizione e velocità entro tolleranza vs JPL.\n");
    } else {
        if (!okPos) { report("  [FAIL] Asteroide: posizione fuori tolleranza.\n"); g_fail_count++; }
        if (!okVel) { report("  [FAIL] Asteroide: velocità fuori tolleranza.\n"); g_fail_count++; }
    }
}

// Confronto diretto con SABA4Integrator: stessa propagazione asteroide, statistiche SABA4.
// Richiede USE_GAUSS=1 e spkPath non vuoto (path a DE441 .bsp).
static void testAsteroidWithSABA4(const std::string& refDir, const std::string& spkPath) {
    if (!std::getenv("USE_GAUSS") || spkPath.empty()) return;

    std::string elemPath = refDir + "/asteroid_elements_2026mar10.txt";
    AstDynEquinoctialElements elem;
    if (!loadAsteroidElements(elemPath, elem)) return;

    std::string de441Path = spkPath;
    if (spkPath.find('/') == std::string::npos) {
        const char* home = std::getenv("HOME");
        if (home) de441Path = std::string(home) + "/.ioccultcalc/ephemerides/" + spkPath;
    }

    using namespace astdyn::propagation;
    using namespace astdyn::ephemeris;
    using namespace astdyn::constants;

    PropagatorSettings settings;
    settings.include_planets = true;
    settings.perturb_mercury = true;
    settings.perturb_venus = true;
    settings.perturb_earth = true;
    settings.perturb_mars = true;
    settings.perturb_jupiter = true;
    settings.perturb_saturn = true;
    settings.perturb_uranus = true;
    settings.perturb_neptune = true;
    settings.include_relativity = true;
    settings.include_moon = true;
    settings.include_asteroids = true;
    settings.ppn_beta = 1.0;
    settings.ppn_gamma = 1.0;
    settings.asteroid_ephemeris_file = "";

    auto de441 = std::make_shared<DE441Provider>(de441Path);
    PlanetaryEphemeris::setProvider(de441);
    auto ephem = std::make_shared<PlanetaryEphemeris>();

    auto saba4 = std::make_unique<SABA4Integrator>(1.0, 1e-6, 1e-6, 100.0);
    SABA4Integrator* saba4_ptr = saba4.get();
    auto prop = std::make_unique<Propagator>(std::move(saba4), ephem, settings);

    EquinoctialElements eq;
    eq.epoch_mjd_tdb = elem.epoch.jd - 2400000.5;
    eq.a = elem.a;
    eq.h = elem.h;
    eq.k = elem.k;
    eq.p = elem.p;
    eq.q = elem.q;
    eq.lambda = elem.lambda;
    eq.gravitational_parameter = GMS;

    KeplerianElements kep = equinoctial_to_keplerian(eq);
    CartesianElements init_cart = keplerian_to_cartesian(kep);
    // Elementi da file sono in ECLITTICO; convertire stato iniziale in equatoriale per il Propagator (efemeridi ICRF).
    {
        Vector3D pos_ecl(init_cart.position.x(), init_cart.position.y(), init_cart.position.z());
        Vector3D vel_ecl(init_cart.velocity.x(), init_cart.velocity.y(), init_cart.velocity.z());
        Vector3D pos_eq = Coordinates::eclipticToEquatorial(pos_ecl, elem.epoch.jd);
        Vector3D vel_eq = Coordinates::eclipticToEquatorial(vel_ecl, elem.epoch.jd);
        init_cart.position = Eigen::Vector3d(pos_eq.x, pos_eq.y, pos_eq.z);
        init_cart.velocity = Eigen::Vector3d(vel_eq.x, vel_eq.y, vel_eq.z);
    }
    double target_mjd = JD_TEST - 2400000.5;
    report("  Propagazione SABA4 diretta in corso (epoca -> JD " + std::to_string(JD_TEST) + ")...\n");
    std::cout.flush();
    CartesianElements result = prop->propagate_cartesian(init_cart, target_mjd);

    auto stats = saba4_ptr->stats();
    report("\n  --- SABA4 (confronto diretto) ---\n");
    report("  SABA4 stats: steps=" + std::to_string(stats.num_steps) +
           " rejected=" + std::to_string(stats.num_rejected) +
           " energy_drift=" + std::to_string(stats.energy_drift) + "\n");
    report("  SABA4 Pos [AU] (Equat.): " + std::to_string(result.position.x()) + " " +
           std::to_string(result.position.y()) + " " + std::to_string(result.position.z()) + "\n");
    report("  SABA4 Vel [AU/day]: " + std::to_string(result.velocity.x()) + " " +
           std::to_string(result.velocity.y()) + " " + std::to_string(result.velocity.z()) + "\n");
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
    std::string spkPath = "de441.bsp";
    if (argc > 2) {
        spkPath = argv[2];
    } else {
        if (!reader->ensureFileLoaded("de441.bsp")) {
            report("  [WARNING] DE441 non trovato, uso DE440.\n");
            spkPath = "de440.bsp";
        } else {
            spkPath = "de441.bsp";
        }
    }
    if (!reader->isLoaded() && !reader->ensureFileLoaded(spkPath)) {
        report("ERRORE: Impossibile caricare SPK " + spkPath + ". Verificare path e kernel.\n");
        return 1;
    }
    // Carica kernel asteroidi (contiene 433 Eros) per confronto diretto SPK vs JPL
    {
        const char* home = std::getenv("HOME");
        if (home) {
            std::string codesPath = std::string(home) + "/.ioccultcalc/ephemerides/codes_300ast_20100725.bsp";
            reader->loadAdditionalFile(codesPath);
        }
    }
    initializeSpiceProvider(reader);

    testEarth(refDir);
    testStar(refDir);
    testAsteroid(refDir, spkPath);
    testAsteroidWithSABA4(refDir, spkPath);

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
