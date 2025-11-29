/**
 * @file refraction.h
 * @brief Atmospheric refraction corrections for occultation predictions
 * 
 * This module implements various atmospheric refraction models to correct
 * the observed altitude of celestial objects. Critical for events near
 * the horizon where refraction can reach up to 35 arcminutes.
 * 
 * Models implemented:
 * - Bennett (1982): Simple formula, accuracy ~0.5'
 * - Saemundsson (1986): Improved for low altitudes
 * - Hohenkerk & Sinclair (1985): High precision with meteorological data
 * 
 * @author IOccultCalc Development Team
 * @date 2025-11-23
 */

#ifndef REFRACTION_H
#define REFRACTION_H

#include <cmath>

namespace ioccultcalc {

/**
 * @brief Atmospheric conditions for refraction calculation
 */
struct AtmosphericConditions {
    double temperature_celsius = 10.0;  ///< Temperature in °C (default: 10°C)
    double pressure_mbar = 1013.25;     ///< Pressure in mbar (default: standard)
    double relative_humidity = 0.0;     ///< Relative humidity 0-1 (default: 0)
    double wavelength_um = 0.55;        ///< Wavelength in micrometers (default: V band)
};

/**
 * @brief Refraction calculation method
 */
enum class RefractionModel {
    BENNETT,           ///< Bennett (1982) - simple, fast
    SAEMUNDSSON,       ///< Saemundsson (1986) - improved low altitudes
    HOHENKERK_SINCLAIR ///< Hohenkerk & Sinclair (1985) - high precision
};

/**
 * @class RefractionCalculator
 * @brief Calculate atmospheric refraction corrections
 */
class RefractionCalculator {
public:
    /**
     * @brief Constructor with atmospheric conditions
     * @param conditions Atmospheric conditions (temperature, pressure, humidity)
     */
    explicit RefractionCalculator(const AtmosphericConditions& conditions = AtmosphericConditions());
    
    /**
     * @brief Calculate refraction correction using Bennett (1982) formula
     * 
     * Simple formula: R = cot(h_app + 7.31/(h_app + 4.4)) arcminutes
     * Accuracy: ~0.5 arcminutes for h > 15°
     * Fast computation, suitable for most applications
     * 
     * @param apparent_altitude_deg Apparent altitude in degrees (observed)
     * @return Refraction correction in degrees (always positive, to subtract from apparent)
     */
    double calculateBennett(double apparent_altitude_deg) const;
    
    /**
     * @brief Calculate refraction using Saemundsson (1986) formula
     * 
     * Improved Bennett formula with better performance at low altitudes
     * R = 1.02 / tan(h_app + 10.3/(h_app + 5.11)) arcminutes
     * Accuracy: ~0.1 arcminutes for h > 5°
     * 
     * @param apparent_altitude_deg Apparent altitude in degrees
     * @return Refraction correction in degrees
     */
    double calculateSaemundsson(double apparent_altitude_deg) const;
    
    /**
     * @brief Calculate refraction using Hohenkerk & Sinclair (1985) method
     * 
     * High-precision formula including temperature, pressure, humidity effects
     * Based on Garfinkel (1967) with updates from Stone (1996)
     * Accuracy: ~0.01 arcminutes (0.6 arcseconds) for h > 3°
     * 
     * Includes:
     * - Temperature and pressure corrections
     * - Humidity effects (water vapor)
     * - Wavelength dependence (dispersion)
     * 
     * @param apparent_altitude_deg Apparent altitude in degrees
     * @return Refraction correction in degrees
     */
    double calculateHohenkerkSinclair(double apparent_altitude_deg) const;
    
    /**
     * @brief Calculate refraction using selected model
     * 
     * @param apparent_altitude_deg Apparent altitude in degrees
     * @param model Refraction model to use (default: Saemundsson)
     * @return Refraction correction in degrees
     */
    double calculate(double apparent_altitude_deg, 
                    RefractionModel model = RefractionModel::SAEMUNDSSON) const;
    
    /**
     * @brief Convert true altitude to apparent (observed) altitude
     * 
     * Iterative solution since refraction depends on apparent altitude
     * Apparent = True + Refraction(Apparent)
     * 
     * @param true_altitude_deg True (geometric) altitude in degrees
     * @param model Refraction model to use
     * @param max_iterations Maximum iterations (default: 3)
     * @param tolerance Convergence tolerance in degrees (default: 1e-6)
     * @return Apparent altitude in degrees
     */
    double trueToApparent(double true_altitude_deg,
                         RefractionModel model = RefractionModel::SAEMUNDSSON,
                         int max_iterations = 3,
                         double tolerance = 1e-6) const;
    
    /**
     * @brief Convert apparent altitude to true altitude
     * 
     * Simple subtraction: True = Apparent - Refraction(Apparent)
     * 
     * @param apparent_altitude_deg Apparent altitude in degrees
     * @param model Refraction model to use
     * @return True altitude in degrees
     */
    double apparentToTrue(double apparent_altitude_deg,
                         RefractionModel model = RefractionModel::SAEMUNDSSON) const;
    
    /**
     * @brief Check if refraction is negligible
     * 
     * @param altitude_deg Altitude in degrees
     * @param threshold_arcsec Threshold in arcseconds (default: 0.1")
     * @return true if refraction < threshold
     */
    static bool isNegligible(double altitude_deg, double threshold_arcsec = 0.1);
    
    /**
     * @brief Get atmospheric conditions
     * @return Current atmospheric conditions
     */
    const AtmosphericConditions& getConditions() const { return conditions_; }
    
    /**
     * @brief Set atmospheric conditions
     * @param conditions New atmospheric conditions
     */
    void setConditions(const AtmosphericConditions& conditions) { 
        conditions_ = conditions;
        updateRefractionConstants();
    }

private:
    AtmosphericConditions conditions_;  ///< Current atmospheric conditions
    
    // Cached refraction constants (updated when conditions change)
    double pressure_factor_;     ///< Pressure correction factor
    double temperature_factor_;  ///< Temperature correction factor
    double humidity_factor_;     ///< Humidity correction factor
    
    /**
     * @brief Update refraction constants based on current conditions
     */
    void updateRefractionConstants();
    
    /**
     * @brief Calculate pressure/temperature correction factor
     * 
     * Based on ideal gas law and standard atmosphere
     * Factor = (P/1013.25) * (283/(273+T))
     * 
     * @return Correction factor (dimensionless)
     */
    double calculatePressureTemperatureFactor() const;
    
    /**
     * @brief Calculate humidity correction
     * 
     * Water vapor reduces refractive index slightly
     * Based on Owens (1967) formula
     * 
     * @return Humidity correction in degrees
     */
    double calculateHumidityCorrection(double refraction_dry) const;
};

/**
 * @brief Global convenience function for quick refraction calculation
 * 
 * Uses default atmospheric conditions and Saemundsson model
 * 
 * @param apparent_altitude_deg Apparent altitude in degrees
 * @return Refraction correction in degrees
 */
double calculateRefraction(double apparent_altitude_deg);

/**
 * @brief Calculate refraction with custom atmospheric conditions
 * 
 * @param apparent_altitude_deg Apparent altitude in degrees
 * @param temperature_celsius Temperature in °C
 * @param pressure_mbar Pressure in mbar
 * @return Refraction correction in degrees
 */
double calculateRefractionWithConditions(double apparent_altitude_deg,
                                         double temperature_celsius,
                                         double pressure_mbar);

} // namespace ioccultcalc

#endif // REFRACTION_H
