/**
 * @file vsop87.h
 * @brief VSOP87D completo per calcolo posizione Terra ad alta precisione
 * 
 * Implementa teoria VSOP87D (heliocentric ecliptic coordinates J2000.0)
 * con TUTTI i termini periodici per precisione sub-km sulla posizione Terra.
 * 
 * Riferimento: Bretagnon & Francou (1988), A&A 202, 309
 * Precisione: < 1 km per Terra, < 10 km per altri pianeti
 * Validità: -4000 a +8000 anni da J2000
 */

#ifndef IOCCULTCALC_VSOP87_H
#define IOCCULTCALC_VSOP87_H

#include "ioccultcalc/types.h"
#include <vector>

namespace ioccultcalc {

/**
 * @struct VSOP87Term
 * @brief Singolo termine periodico VSOP87
 * 
 * Forma: A * cos(B + C*t)
 * dove t = (JD - 2451545.0) / 365250 (millenni giuliani da J2000)
 */
struct VSOP87Term {
    double A;  // Ampiezza
    double B;  // Fase
    double C;  // Frequenza
};

/**
 * @struct VSOP87Series
 * @brief Serie VSOP87 per una coordinata
 * 
 * La coordinata è calcolata come:
 * coord = Σ(i=0 to 5) t^i * Σ(terms_i) A*cos(B + C*t)
 */
struct VSOP87Series {
    std::vector<std::vector<VSOP87Term>> terms; // terms[power][term_index]
};

/**
 * @struct PlanetaryElements
 * @brief Elementi orbitali medi di un pianeta
 */
struct PlanetaryElements {
    double a;       // Semi-asse maggiore (AU)
    double lambda;  // Longitudine media (rad)
    double k;       // e*cos(ϖ)
    double h;       // e*sin(ϖ)
    double q;       // sin(i/2)*cos(Ω)
    double p;       // sin(i/2)*sin(Ω)
};

/**
 * @class VSOP87
 * @brief Calcolo posizioni planetarie con teoria VSOP87D completa
 */
class VSOP87 {
public:
    VSOP87();
    ~VSOP87();
    
    /**
     * @brief Calcola posizione Terra eliocentric ecliptica J2000.0
     * @param jd Julian Date
     * @return Posizione (L, B, R) in coordinate eclittiche
     *         L = longitudine eclittica (rad)
     *         B = latitudine eclittica (rad)
     *         R = distanza eliocentric (AU)
     */
    Vector3D computeEarth(const JulianDate& jd) const;
    
    /**
     * @brief Calcola velocità Terra eliocentric ecliptica J2000.0
     * @param jd Julian Date
     * @return Velocità (dL/dt, dB/dt, dR/dt) in AU/giorno
     */
    Vector3D computeEarthVelocity(const JulianDate& jd) const;
    
    /**
     * @brief Calcola posizione di un pianeta
     * @param planet 1=Mercurio, 2=Venere, 3=Terra, 4=Marte, 5=Giove,
     *               6=Saturno, 7=Urano, 8=Nettuno
     * @param jd Julian Date
     * @return Posizione eliocentric eclittica (L, B, R)
     */
    Vector3D computePlanet(int planet, const JulianDate& jd) const;
    
    /**
     * @brief Calcola posizione e velocità Luna geocentric
     * @param jd Julian Date
     * @param position Output: posizione geocentric equatoriale (AU)
     * @param velocity Output: velocità (AU/giorno)
     */
    void computeMoon(const JulianDate& jd, Vector3D& position, 
                     Vector3D& velocity) const;
    
    /**
     * @brief Converte coordinate eclittiche in equatoriali J2000
     * @param ecliptic Coordinate eclittiche (λ, β, r)
     * @return Coordinate equatoriali (x, y, z)
     */
    static Vector3D eclipticToEquatorial(const Vector3D& ecliptic);
    
    /**
     * @brief Obliquità eclittica media J2000.0
     */
    static constexpr double OBLIQUITY_J2000 = 0.40909280422232891; // 23.4392911° in radianti
    
private:
    class Impl;
    Impl* pImpl;
    
    // Carica serie VSOP87D dalla tabella interna
    void loadVSOP87Data();
    
    // Valuta serie VSOP87 per una coordinata
    double evaluateSeries(const VSOP87Series& series, double t) const;
};

/**
 * @class ELP2000
 * @brief Teoria ELP2000-82B per la Luna (semplificata)
 * 
 * Versione ridotta con ~200 termini per precisione ~1 km
 * Riferimento: Chapront-Touzé & Chapront (1983)
 */
class ELP2000 {
public:
    /**
     * @brief Calcola posizione Luna geocentric equatoriale J2000
     * @param jd Julian Date
     * @return Posizione (x, y, z) in AU
     */
    static Vector3D compute(const JulianDate& jd);
    
    /**
     * @brief Calcola velocità Luna
     * @param jd Julian Date
     * @return Velocità (vx, vy, vz) in AU/giorno
     */
    static Vector3D computeVelocity(const JulianDate& jd);
    
private:
    struct LunarTerm {
        int D, M, Mp, F;  // Argomenti lunari
        double sinA, cosA; // Coefficienti
    };
    
    static const std::vector<LunarTerm> longitudeTerms;
    static const std::vector<LunarTerm> latitudeTerms;
    static const std::vector<LunarTerm> distanceTerms;
};

/**
 * @class PlanetaryPerturbations
 * @brief Calcolo perturbazioni planetarie su orbita asteroide
 * 
 * Include accelerazioni gravitazionali di tutti i pianeti
 */
class PlanetaryPerturbations {
public:
    PlanetaryPerturbations();
    
    /**
     * @brief Calcola accelerazione totale su asteroide
     * @param jd Julian Date
     * @param asteroidPos Posizione asteroide eliocentric (AU)
     * @param asteroidVel Velocità asteroide (AU/giorno)
     * @return Accelerazione (AU/giorno²)
     */
    Vector3D computeAcceleration(const JulianDate& jd,
                                 const Vector3D& asteroidPos,
                                 const Vector3D& asteroidVel) const;
    
    /**
     * @brief Abilita/disabilita singoli pianeti
     */
    void enablePlanet(int planet, bool enable);
    
    /**
     * @brief Abilita correzioni relativistiche (Shapiro, deflessione luce)
     */
    void enableRelativisticCorrections(bool enable);
    
private:
    VSOP87 vsop;
    bool enabledPlanets[9]; // 1-8 = pianeti, 0 = Luna
    bool relativisticCorrections;
    
    // Masse planetarie in unità massa solare
    static constexpr double PLANET_MASSES[9] = {
        0.0,                    // [0] non usato
        1.660136795271931e-07,  // [1] Mercurio
        2.447838287784771e-06,  // [2] Venere  
        3.003489596331057e-06,  // [3] Terra+Luna
        3.227151448635193e-07,  // [4] Marte
        9.547919384243266e-04,  // [5] Giove
        2.858859806661029e-04,  // [6] Saturno
        4.366244043351564e-05,  // [7] Urano
        5.151389020535497e-05   // [8] Nettuno
    };
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_VSOP87_H
