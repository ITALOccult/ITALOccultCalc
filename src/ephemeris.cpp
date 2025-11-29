#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/coordinates.h"
#include "ioccultcalc/spice_spk_reader.h"
#include <cmath>
#include <stdexcept>

namespace ioccultcalc {

// Costante gravitazionale gaussiana
constexpr double GAUSS_K = 0.01720209895; // AU^(3/2) / day

Ephemeris::Ephemeris() {}

Ephemeris::Ephemeris(const EquinoctialElements& elements) 
    : elements_(elements) {}

void Ephemeris::setElements(const EquinoctialElements& elements) {
    elements_ = elements;
}

EphemerisData Ephemeris::compute(const JulianDate& jd) {
    EphemerisData data;
    data.jd = jd;
    
    // Propaga l'orbita
    propagateOrbit(jd, data.heliocentricPos, data.heliocentricVel);
    
    // Posizione della Terra
    Vector3D earthPos = getEarthPosition(jd);
    Vector3D earthVel = getEarthVelocity(jd);
    
    // Posizione geocentrica dell'asteroide
    Vector3D geocentricVec = data.heliocentricPos - earthPos;
    data.distance = geocentricVec.magnitude();
    data.geocentricPos = Coordinates::cartesianToEquatorial(geocentricVec);
    
    // Posizione del Sole geocentrica
    Vector3D sunPos = getSunPosition(jd);
    
    // Elongazione solare
    Vector3D asteroidDir = geocentricVec.normalize();
    Vector3D sunDir = sunPos.normalize();
    data.elongation = acos(asteroidDir.dot(sunDir)) * RAD_TO_DEG;
    
    // Angolo di fase
    Vector3D toEarth = geocentricVec * -1.0;
    double cosPhase = data.heliocentricPos.normalize().dot(toEarth.normalize());
    data.phase = acos(cosPhase) * RAD_TO_DEG;
    
    // Magnitudine
    double r = data.heliocentricPos.magnitude(); // AU
    data.magnitude = calculateMagnitude(r, data.distance, data.phase);
    
    return data;
}

std::vector<EphemerisData> Ephemeris::computeRange(const JulianDate& startJD,
                                                   const JulianDate& endJD,
                                                   double stepDays) {
    std::vector<EphemerisData> results;
    
    for (double jd = startJD.jd; jd <= endJD.jd; jd += stepDays) {
        results.push_back(compute(JulianDate(jd)));
    }
    
    return results;
}

void Ephemeris::propagateOrbit(const JulianDate& targetJD,
                               Vector3D& helioPos, Vector3D& helioVel) {
    // Propaga usando elementi equinoziali
    
    // Tempo dall'epoca in giorni
    double dt = targetJD.jd - elements_.epoch.jd;
    
    // Mean motion
    double n = GAUSS_K / sqrt(elements_.a * elements_.a * elements_.a);
    
    // Mean longitude al tempo target
    double lambda_t = elements_.lambda + n * dt;
    while (lambda_t < 0) lambda_t += 2.0 * M_PI;
    while (lambda_t >= 2.0 * M_PI) lambda_t -= 2.0 * M_PI;
    
    // Calcola anomalia media
    double omega_plus_Omega = atan2(elements_.h, elements_.k);
    double M = lambda_t - omega_plus_Omega;
    
    // Eccentricità
    double e = sqrt(elements_.h * elements_.h + elements_.k * elements_.k);
    
    // Risolvi equazione di Keplero
    double E = solveKeplerEquation(M, e);
    
    // Anomalia vera
    double cosNu = (cos(E) - e) / (1.0 - e * cos(E));
    double sinNu = sqrt(1.0 - e * e) * sin(E) / (1.0 - e * cos(E));
    double nu = atan2(sinNu, cosNu);
    
    // Raggio vettore
    double r = elements_.a * (1.0 - e * cos(E));
    
    // Coordinate nel piano orbitale
    double x_orb = r * cos(nu);
    double y_orb = r * sin(nu);
    
    // Velocità nel piano orbitale
    double v_factor = GAUSS_K * sqrt(elements_.a) / r;
    double vx_orb = -v_factor * sin(E);
    double vy_orb = v_factor * sqrt(1.0 - e * e) * cos(E);
    
    // Trasforma dal piano orbitale al sistema equatoriale
    // Usando elementi equinoziali per la rotazione
    
    double f = 1.0 + elements_.p * elements_.p + elements_.q * elements_.q;
    
    // Matrice di trasformazione (elementi equinoziali -> equatoriale)
    double m11 = (1.0 - elements_.p * elements_.p + elements_.q * elements_.q) / f;
    double m12 = 2.0 * elements_.p * elements_.q / f;
    double m13 = -2.0 * elements_.p / f;
    
    double m21 = 2.0 * elements_.p * elements_.q / f;
    double m22 = (1.0 + elements_.p * elements_.p - elements_.q * elements_.q) / f;
    double m23 = 2.0 * elements_.q / f;
    
    double m31 = 2.0 * elements_.p / f;
    double m32 = -2.0 * elements_.q / f;
    double m33 = (1.0 - elements_.p * elements_.p - elements_.q * elements_.q) / f;
    
    // Argomento del periapside + longitudine del nodo
    double cos_wp = cos(omega_plus_Omega);
    double sin_wp = sin(omega_plus_Omega);
    
    // Ruota dal piano orbitale al sistema di riferimento
    double x_ref = x_orb * cos_wp - y_orb * sin_wp;
    double y_ref = x_orb * sin_wp + y_orb * cos_wp;
    double z_ref = 0;
    
    double vx_ref = vx_orb * cos_wp - vy_orb * sin_wp;
    double vy_ref = vx_orb * sin_wp + vy_orb * cos_wp;
    double vz_ref = 0;
    
    // Applica trasformazione al frame degli elementi equinoziali
    // NOTA: Elementi equinoziali da AstDyS sono in frame eclittico J2000 (ECLM)
    double x_ecl = m11 * x_ref + m12 * y_ref + m13 * z_ref;
    double y_ecl = m21 * x_ref + m22 * y_ref + m23 * z_ref;
    double z_ecl = m31 * x_ref + m32 * y_ref + m33 * z_ref;
    
    double vx_ecl = m11 * vx_ref + m12 * vy_ref + m13 * vz_ref;
    double vy_ecl = m21 * vx_ref + m22 * vy_ref + m23 * vz_ref;
    double vz_ecl = m31 * vx_ref + m32 * vy_ref + m33 * vz_ref;
    
    // Converti da eclittico a equatoriale (J2000)
    // Rotazione attorno asse X di ε = 23.4392811° (obliquità eclittica J2000)
    constexpr double OBLIQUITY_J2000 = 23.4392911 * M_PI / 180.0;  // rad
    double cos_eps = std::cos(OBLIQUITY_J2000);
    double sin_eps = std::sin(OBLIQUITY_J2000);
    
    helioPos.x = x_ecl;
    helioPos.y = y_ecl * cos_eps - z_ecl * sin_eps;
    helioPos.z = y_ecl * sin_eps + z_ecl * cos_eps;
    
    helioVel.x = vx_ecl;
    helioVel.y = vy_ecl * cos_eps - vz_ecl * sin_eps;
    helioVel.z = vy_ecl * sin_eps + vz_ecl * cos_eps;
}

double Ephemeris::solveKeplerEquation(double M, double e, double tolerance) {
    // Risolve E - e*sin(E) = M usando Newton-Raphson
    
    double E = M; // Prima approssimazione
    if (e > 0.8) {
        E = M_PI; // Migliore starting point per alte eccentricità
    }
    
    for (int i = 0; i < 100; i++) {
        double f = E - e * sin(E) - M;
        double fp = 1.0 - e * cos(E);
        double delta = f / fp;
        E -= delta;
        
        if (fabs(delta) < tolerance) {
            break;
        }
    }
    
    return E;
}

double Ephemeris::calculateMagnitude(double r, double delta, double phaseAngle) {
    // Formula HG per la magnitudine
    double phi1 = exp(-3.33 * pow(tan(phaseAngle * DEG_TO_RAD / 2.0), 0.63));
    double phi2 = exp(-1.87 * pow(tan(phaseAngle * DEG_TO_RAD / 2.0), 1.22));
    
    double H = elements_.H;
    double G = elements_.G;
    
    double mag = H + 5.0 * log10(r * delta) - 2.5 * log10((1.0 - G) * phi1 + G * phi2);
    
    return mag;
}

Vector3D Ephemeris::getSunPosition(const JulianDate& jd) {
    // Restituisce posizione Sole geocentrica (opposto della Terra eliocentrica)
    return getEarthPosition(jd) * -1.0;
}

Vector3D Ephemeris::getEarthPosition(const JulianDate& jd) {
    // Prova a usare SPK (JPL DE441/DE440) per massima precisione
    static SPICESPKReader spkReader;
    static bool spkInitialized = false;
    
    if (!spkInitialized) {
        // Prova a caricare DE441, DE440s o DE440
        if (spkReader.ensureFileLoaded("de441.bsp") || 
            spkReader.ensureFileLoaded("de440s.bsp") ||
            spkReader.ensureFileLoaded("de440.bsp")) {
            spkInitialized = true;
        }
    }
    
    if (spkReader.isLoaded()) {
        try {
            // NAIF ID: 399 = Terra, 10 = Sole, 0 = SSB (barycenter)
            // Prova prima con Sole come centro (heliocentric)
            try {
                auto [pos, vel] = spkReader.getState(399, jd.jd, 10);
                return pos;
            } catch (...) {
                // Se fallisce, prova SSB e sottrai posizione Sole
                auto [earthPos, earthVel] = spkReader.getState(399, jd.jd, 0);
                auto [sunPos, sunVel] = spkReader.getState(10, jd.jd, 0);
                return earthPos - sunPos;
            }
        } catch (...) {
            // Fallback a formula analitica
        }
    }
    
    // FALLBACK: Formula analitica semplificata
    // NOTA: Questa ha precisione limitata (~0.4 AU di errore!)
    // Per occultazioni asteroidali serve JPL DE
    
    double T = (jd.jd - 2451545.0) / 36525.0;
    
    // Elementi orbitali medi Terra
    double L = 280.46646 + 36000.76983 * T + 0.0003032 * T * T;
    double M = 357.52911 + 35999.05029 * T - 0.0001537 * T * T;
    
    L = fmod(L, 360.0);
    if (L < 0) L += 360.0;
    M = fmod(M, 360.0);
    if (M < 0) M += 360.0;
    
    double M_rad = M * DEG_TO_RAD;
    double e = 0.016708634 - 0.000042037 * T - 0.0000001267 * T * T;
    
    // Equazione del centro
    double C = (1.914602 - 0.004817 * T - 0.000014 * T * T) * sin(M_rad) +
               (0.019993 - 0.000101 * T) * sin(2.0 * M_rad) +
               0.000289 * sin(3.0 * M_rad);
    
    double sunLon = L + C;
    double R = (1.000001018 * (1.0 - e * e)) / (1.0 + e * cos(M_rad + C * DEG_TO_RAD));
    
    // ATTENZIONE: Questa formula è imprecisa!
    // Serve solo come fallback se SPK non disponibile
    double earthLon = sunLon + 180.0;
    if (earthLon >= 360.0) earthLon -= 360.0;
    double earthLon_rad = earthLon * DEG_TO_RAD;
    
    double x_ecl = R * cos(earthLon_rad);
    double y_ecl = R * sin(earthLon_rad);
    double z_ecl = 0.0;
    
    double eps = 23.4392911 * DEG_TO_RAD;
    
    Vector3D earthPos;
    earthPos.x = x_ecl;
    earthPos.y = y_ecl * cos(eps) - z_ecl * sin(eps);
    earthPos.z = y_ecl * sin(eps) + z_ecl * cos(eps);
    
    return earthPos;
}

Vector3D Ephemeris::getEarthVelocity(const JulianDate& jd) {
    // Prova prima SPK per velocità diretta
    static SPICESPKReader spkReader;
    static bool spkInitialized = false;
    
    if (!spkInitialized) {
        if (spkReader.ensureFileLoaded("de441.bsp") || 
            spkReader.ensureFileLoaded("de440s.bsp") ||
            spkReader.ensureFileLoaded("de440.bsp")) {
            spkInitialized = true;
        }
    }
    
    if (spkReader.isLoaded()) {
        try {
            auto [pos, vel] = spkReader.getState(399, jd.jd, 10);
            return vel;
        } catch (...) {
            // Fallback a differenze finite
        }
    }
    
    // FALLBACK: Calcola velocità con differenze finite
    double dt = 0.1; // 0.1 giorni
    
    Vector3D pos1 = getEarthPosition(JulianDate(jd.jd - dt / 2.0));
    Vector3D pos2 = getEarthPosition(JulianDate(jd.jd + dt / 2.0));
    
    return (pos2 - pos1) * (1.0 / dt);
}

Vector3D Ephemeris::getEarthPositionWithCorrections(const JulianDate& jd, 
                                                     const Vector3D& observerPos) {
    // Ottiene posizione base da SPK con interpolazione
    Vector3D earthPos = getEarthPosition(jd);
    Vector3D earthVel = getEarthVelocity(jd);
    
    // Costanti fisiche
    constexpr double C_AU_PER_DAY = 173.1446326846693;  // Velocità luce in AU/day
    constexpr double GM_SUN = 0.000295912208286;        // GM☉ in AU³/day²
    constexpr double C_AU_PER_DAY_SQ = C_AU_PER_DAY * C_AU_PER_DAY;
    
    // === 1. ABERRAZIONE DELLA LUCE ===
    // Correzione per il tempo di propagazione della luce
    Vector3D toEarth = earthPos - observerPos;
    double distance = toEarth.magnitude();
    double lightTime = distance / C_AU_PER_DAY;
    
    // Iterazione per tempo luce (miglior accuratezza)
    Vector3D earthPosIterative = earthPos;
    for (int iter = 0; iter < 2; ++iter) {
        JulianDate jdRetarded(jd.jd - lightTime);
        earthPosIterative = getEarthPosition(jdRetarded);
        Vector3D toEarthIter = earthPosIterative - observerPos;
        lightTime = toEarthIter.magnitude() / C_AU_PER_DAY;
    }
    
    // Aberrazione stellare (velocità osservatore)
    Vector3D aberrationCorr = earthVel * (-lightTime);
    earthPos = earthPos + aberrationCorr;
    
    // === 2. CORREZIONI RELATIVISTICHE ===
    
    // 2a. Effetto Shapiro (ritardo gravitazionale)
    // Δt = (2*GM/c³) * ln[(r_e + r_o + d)/(r_e + r_o - d)]
    // dove r_e = distanza Terra-Sole, r_o = distanza Osservatore-Sole
    double r_earth = earthPos.magnitude();  // Distanza Terra-Sole
    double r_obs = observerPos.magnitude(); // Distanza Osservatore-Sole
    
    if (r_earth > 0.01 && distance > 0.001) {  // Evita divisioni per zero
        // Geometria del percorso luce
        double sum_distances = r_earth + r_obs;
        double arg1 = sum_distances + distance;
        double arg2 = std::abs(sum_distances - distance);
        
        if (arg1 > 0 && arg2 > 0) {
            // Ritardo Shapiro in giorni
            double shapiroDelay = (2.0 * GM_SUN / (C_AU_PER_DAY * C_AU_PER_DAY_SQ)) * 
                                 std::log(arg1 / arg2);
            
            // Correzione posizione: Δr = v * Δt
            Vector3D shapiroCorr = earthVel * shapiroDelay;
            earthPos = earthPos + shapiroCorr;
        }
    }
    
    // 2b. Deflexione gravitazionale (light bending)
    // Δθ = (4*GM/c²) / b, dove b = parametro impatto
    // Approssimazione: correzione proporzionale alla massa solare
    Vector3D sunToEarth = earthPos;  // Terra rispetto al Sole
    Vector3D sunToObs = observerPos; // Osservatore rispetto al Sole
    
    // Parametro impatto (distanza minima raggio luce dal Sole)
    Vector3D lightDir = (sunToObs - sunToEarth);
    double lightDist = lightDir.magnitude();
    
    if (lightDist > 0.001) {
        lightDir = lightDir * (1.0 / lightDist);
        
        // Proiezione della posizione Terra sulla direzione luce
        double projection = sunToEarth.x * lightDir.x + 
                          sunToEarth.y * lightDir.y + 
                          sunToEarth.z * lightDir.z;
        
        Vector3D closestPoint = lightDir * projection;
        Vector3D impactVector = sunToEarth - closestPoint;
        double impactParam = impactVector.magnitude();
        
        if (impactParam > 0.01) {  // Solo se non passa troppo vicino al Sole
            // Angolo di deflessione (radianti)
            double deflectionAngle = (4.0 * GM_SUN / C_AU_PER_DAY_SQ) / impactParam;
            
            // Correzione posizione (perpendicolare alla direzione luce)
            Vector3D deflectionDir = impactVector * (1.0 / impactParam);
            Vector3D deflectionCorr = deflectionDir * (deflectionAngle * distance);
            
            earthPos = earthPos + deflectionCorr;
        }
    }
    
    // === 3. INTERPOLAZIONE CHEBYSHEV (cache per query multiple) ===
    // Implementata direttamente in SPK reader per migliore efficienza
    
    return earthPos;
}

} // namespace ioccultcalc
