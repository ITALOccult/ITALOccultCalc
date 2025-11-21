/**
 * @file relativistic_corrections.h
 * @brief Correzioni relativistiche per astrometria di precisione
 * 
 * Implementa:
 * - Light-time correction (tempo luce iterativo)
 * - Aberrazione stellare (annuale + diurna)
 * - Deflessione gravitazionale (Sole, pianeti)
 * - Ritardo Shapiro
 * - Correzione Rømer
 * 
 * Riferimenti:
 * - IERS Conventions 2010
 * - Explanatory Supplement to the Astronomical Almanac (2013)
 * - Soffel et al. (2003) - The IAU 2000 resolutions
 */

#ifndef IOCCULTCALC_RELATIVISTIC_CORRECTIONS_H
#define IOCCULTCALC_RELATIVISTIC_CORRECTIONS_H

#include "ioccultcalc/types.h"

namespace ioccultcalc {

/**
 * @class RelativisticCorrections
 * @brief Gestione correzioni relativistiche complete
 */
class RelativisticCorrections {
public:
    /**
     * @brief Correzione light-time iterativa
     * 
     * Risolve equazione: t_obs = t_em + |r(t_em)|/c
     * dove t_em è il tempo di emissione della luce
     * 
     * @param observerPos Posizione osservatore al tempo t_obs (AU)
     * @param targetPos Posizione target al tempo t_guess (AU)
     * @param lightTime Output: tempo luce in giorni
     * @return Posizione corretta del target al tempo emissione
     */
    static Vector3D lightTimeCorrection(
        const Vector3D& observerPos,
        const Vector3D& targetPos,
        double& lightTime);
    
    /**
     * @brief Aberrazione stellare annuale
     * 
     * Correzione dovuta al moto orbitale della Terra
     * Formula: Δα ≈ -κ * (v_earth · e_perp) / c
     * κ = costante aberrazione = 20.49552 arcsec
     * 
     * @param starDir Direzione stella (unitaria)
     * @param earthVel Velocità Terra (AU/giorno)
     * @return Correzione alla direzione (rad)
     */
    static Vector3D annualAberration(
        const Vector3D& starDir,
        const Vector3D& earthVel);
    
    /**
     * @brief Aberrazione diurna
     * 
     * Correzione dovuta alla rotazione terrestre
     * Formula: Δα ≈ (ω × r_obs) / c
     * 
     * @param starDir Direzione stella (unitaria)
     * @param observerVel Velocità osservatore per rotazione (km/s)
     * @return Correzione alla direzione (rad)
     */
    static Vector3D diurnalAberration(
        const Vector3D& starDir,
        const Vector3D& observerVel);
    
    /**
     * @brief Deflessione gravitazionale della luce
     * 
     * Secondo la relatività generale, la luce viene deflessa
     * passando vicino a masse. Effetto principale: Sole.
     * 
     * Formula di Schwarzschild: Δθ = (4GM/c²) / d
     * dove d è la distanza minima dal Sole
     * 
     * @param starDir Direzione apparente stella (unitaria)
     * @param sunPos Posizione Sole geocentrico (AU)
     * @param sunMass Massa Sole (default: 1 M☉)
     * @return Correzione alla direzione (rad)
     */
    static Vector3D gravitationalDeflection(
        const Vector3D& starDir,
        const Vector3D& sunPos,
        double sunMass = 1.0);
    
    /**
     * @brief Deflessione per tutti i corpi del sistema solare
     * 
     * Include contributi di Sole, Luna, pianeti
     * 
     * @param starDir Direzione stella
     * @param jd Julian Date
     * @param planetaryPositions Posizioni pianeti geocentriche
     * @return Correzione totale (rad)
     */
    static Vector3D totalGravitationalDeflection(
        const Vector3D& starDir,
        const JulianDate& jd,
        const std::vector<Vector3D>& planetaryPositions);
    
    /**
     * @brief Ritardo Shapiro
     * 
     * Ritardo aggiuntivo del segnale dovuto alla curvatura
     * spazio-tempo vicino al Sole
     * 
     * Δt = (2GM/c³) * ln[(r_E + r_A + d) / (r_E + r_A - d)]
     * 
     * @param earthPos Posizione Terra eliocentric (AU)
     * @param asteroidPos Posizione asteroide eliocentric (AU)
     * @return Ritardo in giorni
     */
    static double shapiroDelay(
        const Vector3D& earthPos,
        const Vector3D& asteroidPos);
    
    /**
     * @brief Correzione Rømer
     * 
     * Correzione classica per tempo luce finito
     * (già inclusa in lightTimeCorrection ma utile separatamente)
     * 
     * @param distance Distanza in AU
     * @return Tempo luce in giorni
     */
    static double roemerDelay(double distance);
    
    /**
     * @brief Calcola parallasse
     * 
     * Spostamento apparente dovuto al cambio di posizione osservatore
     * 
     * @param objectPos Posizione oggetto geocentrico (AU)
     * @param observerPos Posizione osservatore rispetto centro Terra (AU)
     * @return Correzione coordinate equatoriali (rad)
     */
    static EquatorialCoordinates parallaxCorrection(
        const Vector3D& objectPos,
        const Vector3D& observerPos);
    
    /**
     * @brief Rifrazione atmosferica
     * 
     * Modello Bennett migliorato con temperatura e pressione
     * Valido per elevazioni > 15°
     * 
     * @param apparentAltitude Altezza apparente (rad)
     * @param temperature Temperatura (°C)
     * @param pressure Pressione (mbar)
     * @param humidity Umidità relativa (0-1)
     * @return Correzione refrazione (rad, positiva verso zenith)
     */
    static double atmosphericRefraction(
        double apparentAltitude,
        double temperature = 10.0,
        double pressure = 1010.0,
        double humidity = 0.5);
    
    /**
     * @brief Rifrazione per raytracing preciso
     * 
     * Integra equazione del raggio attraverso atmosfera stratificata
     * Più preciso di Bennett per basse elevazioni
     * 
     * @param altitude Altezza osservatore (m)
     * @param apparentElevation Elevazione apparente (rad)
     * @param wavelength Lunghezza d'onda (nm, default: 550 = visibile)
     * @return Correzione refrazione (rad)
     */
    static double preciseatmosphericRefraction(
        double altitude,
        double apparentElevation,
        double wavelength = 550.0);
    
    // Costanti fisiche (CODATA 2018)
    static constexpr double C_LIGHT = 299792.458;        // km/s
    static constexpr double C_LIGHT_AU_DAY = 173.1446326846693; // AU/giorno
    static constexpr double AU_KM = 149597870.7;         // km
    static constexpr double GM_SUN = 1.32712440042e20;   // m³/s²
    static constexpr double ABERRATION_CONST = 20.49552; // arcsec
    static constexpr double ABERRATION_CONST_RAD = 9.93650754909556e-05; // rad
    
private:
    // Helper: calcola distanza minima stella-corpo per deflessione
    static double computeImpactParameter(
        const Vector3D& starDir,
        const Vector3D& bodyPos);
};

/**
 * @class PrecessionNutation
 * @brief Precessione e nutazione IAU 2000A/B
 * 
 * Implementa modello IAU 2000A (106 termini nutazione)
 * o IAU 2000B (77 termini, più veloce)
 */
class PrecessionNutation {
public:
    enum Model {
        IAU2000A,  // Precisione 0.2 mas
        IAU2000B   // Precisione 1 mas (più veloce)
    };
    
    PrecessionNutation(Model model = IAU2000A);
    
    /**
     * @brief Matrice di precessione da J2000 a data
     * @param jd Julian Date
     * @return Matrice 3x3 di rotazione
     */
    std::vector<std::vector<double>> precessionMatrix(
        const JulianDate& jd) const;
    
    /**
     * @brief Matrice di nutazione
     * @param jd Julian Date
     * @return Matrice 3x3 di rotazione
     */
    std::vector<std::vector<double>> nutationMatrix(
        const JulianDate& jd) const;
    
    /**
     * @brief Matrice completa precessione+nutazione
     * @param jd Julian Date
     * @return Matrice 3x3 di rotazione
     */
    std::vector<std::vector<double>> precessionNutationMatrix(
        const JulianDate& jd) const;
    
    /**
     * @brief Applica precessione e nutazione a vettore
     * @param pos Posizione in J2000
     * @param jd Julian Date target
     * @return Posizione precessa e nutata
     */
    Vector3D apply(const Vector3D& pos, const JulianDate& jd) const;
    
    /**
     * @brief Calcola nutazione in longitudine e obliquità
     * @param jd Julian Date
     * @param deltaPsi Output: nutazione in longitudine (rad)
     * @param deltaEps Output: nutazione in obliquità (rad)
     */
    void computeNutation(const JulianDate& jd,
                        double& deltaPsi,
                        double& deltaEps) const;
    
private:
    Model currentModel;
    
    struct NutationTerm {
        int nl, nlp, nF, nD, nOm;  // Moltiplicatori argomenti lunari
        double sinPsi, cosPsi;      // Coefficienti sin per Δψ
        double sinEps, cosEps;      // Coefficienti sin per Δε
    };
    
    static const std::vector<NutationTerm> IAU2000A_terms;
    static const std::vector<NutationTerm> IAU2000B_terms;
};

/**
 * @class EarthRotation
 * @brief Parametri rotazione Terra (EOP - Earth Orientation Parameters)
 * 
 * Gestisce:
 * - UT1-UTC (LOD - Length of Day)
 * - Polar motion (x_p, y_p)
 * - Equazione degli equinozi
 */
class EarthRotation {
public:
    /**
     * @brief Calcola GMST (Greenwich Mean Sidereal Time)
     * @param jd Julian Date (UT1)
     * @return GMST in radianti
     */
    static double computeGMST(const JulianDate& jd);
    
    /**
     * @brief Calcola GAST (Greenwich Apparent Sidereal Time)
     * Include equazione degli equinozi (nutazione)
     * @param jd Julian Date (UT1)
     * @return GAST in radianti
     */
    static double computeGAST(const JulianDate& jd);
    
    /**
     * @brief Calcola ERA (Earth Rotation Angle) per CIO-based
     * @param jd Julian Date (UT1)
     * @return ERA in radianti
     */
    static double computeERA(const JulianDate& jd);
    
    /**
     * @brief Matrice polar motion
     * @param xp Coordinate polare x (arcsec)
     * @param yp Coordinate polare y (arcsec)
     * @return Matrice 3x3 di rotazione
     */
    static std::vector<std::vector<double>> polarMotionMatrix(
        double xp, double yp);
    
    /**
     * @brief Stima UT1-UTC da modello predittivo
     * (in assenza di dati IERS aggiornati)
     * @param jd Julian Date (UTC)
     * @return UT1-UTC in secondi
     */
    static double estimateUT1_UTC(const JulianDate& jd);
    
    /**
     * @brief Stima polar motion da modello predittivo
     * @param jd Julian Date
     * @param xp Output: x_pole (arcsec)
     * @param yp Output: y_pole (arcsec)
     */
    static void estimatePolarMotion(const JulianDate& jd,
                                    double& xp, double& yp);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_RELATIVISTIC_CORRECTIONS_H
