/**
 * @file refraction.cpp
 * @brief Implementation of atmospheric refraction corrections
 */

#include "refraction.h"
#include "ioccultcalc/types.h"
#include <cmath>
#include <algorithm>
#include <stdexcept>

// Assuming these are now defined globally, e.g., in types.h
// If not, they would need to be defined here or included from another header.
// For this change, we assume they are available globally.
// M_PI is from <cmath> or <math.h>
// DEG_TO_RAD and RAD_TO_DEG are assumed to be global constants.
// ARCSEC_TO_DEG is assumed to be a global constant.
// STANDARD_PRESSURE_MBAR, STANDARD_TEMP_KELVIN, ZERO_CELSIUS_KELVIN are assumed to be global constants.

namespace ioccultcalc {

// Constants (avoid using from types.h to prevent ambiguity)
// Removed DEG_TO_RAD and RAD_TO_DEG as per instruction.
// ARCSEC_TO_DEG, STANDARD_PRESSURE_MBAR, STANDARD_TEMP_KELVIN, ZERO_CELSIUS_KELVIN
// are assumed to be global and thus removed from here.

RefractionCalculator::RefractionCalculator(const AtmosphericConditions& conditions)
    : conditions_(conditions),
      pressure_factor_(1.0),
      temperature_factor_(1.0),
      humidity_factor_(1.0)
{
    updateRefractionConstants();
}

void RefractionCalculator::updateRefractionConstants() {
    pressure_factor_ = calculatePressureTemperatureFactor();
    // Humidity factor will be calculated per-call since it depends on refraction value
}

double RefractionCalculator::calculatePressureTemperatureFactor() const {
    // Combined pressure and temperature correction
    // Based on ideal gas law: n-1 proportional to P/T
    double temp_kelvin = conditions_.temperature_celsius + ZERO_CELSIUS_KELVIN;
    return (conditions_.pressure_mbar / STANDARD_PRESSURE_MBAR) * 
           (STANDARD_TEMP_KELVIN / temp_kelvin);
}

double RefractionCalculator::calculateBennett(double apparent_altitude_deg) const {
    // Bennett (1982) formula
    // R = cot(h + 7.31/(h + 4.4)) arcminutes
    // Valid for h > 15°, accuracy ~0.5'
    
    if (apparent_altitude_deg < -1.0) {
        // Below horizon - refraction lifts objects up to ~35' at h=0
        // Extend formula with caution
        apparent_altitude_deg = -1.0;
    }
    
    if (apparent_altitude_deg > 85.0) {
        // Near zenith - refraction negligible
        return 0.0;
    }
    
    // Calculate refraction in arcminutes
    double h = apparent_altitude_deg;
    double refraction_arcmin = 1.0 / std::tan((h + 7.31 / (h + 4.4)) * DEG_TO_RAD);
    
    // Apply pressure/temperature correction
    refraction_arcmin *= pressure_factor_;
    
    // Convert to degrees
    return refraction_arcmin * ARCMIN_TO_DEG;
}

double RefractionCalculator::calculateSaemundsson(double apparent_altitude_deg) const {
    // Saemundsson (1986) formula - improved Bennett
    // R = 1.02 / tan(h + 10.3/(h + 5.11)) arcminutes
    // Valid for h > 5°, accuracy ~0.1'
    
    if (apparent_altitude_deg < -0.5) {
        apparent_altitude_deg = -0.5;
    }
    
    if (apparent_altitude_deg > 85.0) {
        return 0.0;
    }
    
    // Calculate refraction in arcminutes
    double h = apparent_altitude_deg;
    double refraction_arcmin = 1.02 / std::tan((h + 10.3 / (h + 5.11)) * DEG_TO_RAD);
    
    // Apply pressure/temperature correction
    refraction_arcmin *= pressure_factor_;
    
    // Convert to degrees
    return refraction_arcmin * ARCMIN_TO_DEG;
}

double RefractionCalculator::calculateHohenkerkSinclair(double apparent_altitude_deg) const {
    // Hohenkerk & Sinclair (1985) - high precision
    // Based on Garfinkel (1967) with Stone (1996) updates
    // Includes temperature, pressure, humidity, wavelength effects
    
    if (apparent_altitude_deg < 0.0) {
        // Below horizon - use empirical extrapolation
        // At h=0, R ≈ 34' (standard conditions)
        if (apparent_altitude_deg < -1.0) {
            return 35.0 * ARCMIN_TO_DEG;  // Clamp to maximum
        }
        // Linear extrapolation from h=0 to h=-1
        double r0 = 34.0 * ARCMIN_TO_DEG;  // Refraction at h=0
        return r0 * (1.0 + apparent_altitude_deg);  // Decreases linearly
    }
    
    if (apparent_altitude_deg > 85.0) {
        return 0.0;
    }
    
    // Convert to radians
    double h_rad = apparent_altitude_deg * DEG_TO_RAD;
    
    // Tangent of altitude
    double tan_h = std::tan(h_rad);
    
    // Basic refraction formula (Garfinkel 1967)
    // R₀ = 16.271" × tan(z) / (1 + 0.00347 × tan(z))
    // where z = 90° - h (zenith distance)
    
    double cot_h = 1.0 / tan_h;  // cot(h) = tan(z)
    
    // Refraction in arcseconds (dry air, 0°C, 1013.25 mbar)
    double R0_arcsec = 16.271 * cot_h / (1.0 + 0.00347 * cot_h);
    
    // For better accuracy at low altitudes, use series expansion
    if (apparent_altitude_deg < 15.0) {
        // Add correction terms for low altitudes
        double tan_h2 = tan_h * tan_h;
        double tan_h3 = tan_h2 * tan_h;
        
        // Second-order correction
        R0_arcsec += 0.061 * cot_h * cot_h;
        
        // Third-order correction (small)
        if (apparent_altitude_deg < 5.0) {
            R0_arcsec += 0.0003 * cot_h * cot_h * cot_h;
        }
    }
    
    // Apply pressure and temperature correction
    // R = R₀ × (P/P₀) × (T₀/T)
    double refraction_arcsec = R0_arcsec * pressure_factor_;
    
    // Humidity correction (Owens 1967)
    // Water vapor reduces refraction slightly
    if (conditions_.relative_humidity > 0.01) {
        double temp_kelvin = conditions_.temperature_celsius + ZERO_CELSIUS_KELVIN;
        
        // Water vapor partial pressure (approximate)
        // e_s = 6.1078 × exp(17.27 × T/(T + 237.3)) mbar (Magnus formula)
        double temp_c = conditions_.temperature_celsius;
        double e_sat = 6.1078 * std::exp(17.27 * temp_c / (temp_c + 237.3));
        double e_vapor = e_sat * conditions_.relative_humidity;
        
        // Humidity correction factor (reduces refraction)
        // Δn ≈ -0.0624 × e/T
        double humidity_correction = -0.0624 * e_vapor / temp_kelvin;
        refraction_arcsec *= (1.0 + humidity_correction);
    }
    
    // Wavelength correction (chromatic dispersion)
    // Refraction varies with wavelength: R(λ) = R(λ₀) × f(λ)
    // For λ = 0.55 μm (V band), f = 1.0 (reference)
    // This is a small effect (~0.1% variation across visible)
    if (std::abs(conditions_.wavelength_um - 0.55) > 0.01) {
        // Simplified Cauchy formula for air dispersion
        // n(λ) - 1 ≈ A × (1 + B/λ²)
        double lambda = conditions_.wavelength_um;
        double lambda_ref = 0.55;
        
        // Approximate dispersion factor
        double dispersion_factor = (1.0 + 0.013 / (lambda * lambda)) / 
                                  (1.0 + 0.013 / (lambda_ref * lambda_ref));
        refraction_arcsec *= dispersion_factor;
    }
    
    // Convert to degrees
    return refraction_arcsec * ARCSEC_TO_DEG;
}

double RefractionCalculator::calculate(double apparent_altitude_deg, 
                                      RefractionModel model) const {
    switch (model) {
        case RefractionModel::BENNETT:
            return calculateBennett(apparent_altitude_deg);
        case RefractionModel::SAEMUNDSSON:
            return calculateSaemundsson(apparent_altitude_deg);
        case RefractionModel::HOHENKERK_SINCLAIR:
            return calculateHohenkerkSinclair(apparent_altitude_deg);
        default:
            return calculateSaemundsson(apparent_altitude_deg);
    }
}

double RefractionCalculator::trueToApparent(double true_altitude_deg,
                                           RefractionModel model,
                                           int max_iterations,
                                           double tolerance) const {
    // Iterative solution: h_app = h_true + R(h_app)
    // Start with first approximation: h_app ≈ h_true + R(h_true)
    
    double h_apparent = true_altitude_deg;
    
    for (int i = 0; i < max_iterations; ++i) {
        double refraction = calculate(h_apparent, model);
        double h_apparent_new = true_altitude_deg + refraction;
        
        // Check convergence
        if (std::abs(h_apparent_new - h_apparent) < tolerance) {
            return h_apparent_new;
        }
        
        h_apparent = h_apparent_new;
    }
    
    return h_apparent;
}

double RefractionCalculator::apparentToTrue(double apparent_altitude_deg,
                                           RefractionModel model) const {
    // Simple: h_true = h_apparent - R(h_apparent)
    double refraction = calculate(apparent_altitude_deg, model);
    return apparent_altitude_deg - refraction;
}

bool RefractionCalculator::isNegligible(double altitude_deg, double threshold_arcsec) {
    // Quick check without creating calculator object
    // Use simple approximation: R ≈ cot(h) arcminutes
    
    if (altitude_deg > 85.0) {
        return true;  // Always negligible near zenith
    }
    
    if (altitude_deg < 5.0) {
        return false;  // Never negligible near horizon
    }
    
    // Approximate refraction in arcseconds
    double h_rad = altitude_deg * DEG_TO_RAD;
    double refraction_arcsec = 60.0 / std::tan(h_rad);  // Rough approximation
    
    return refraction_arcsec < threshold_arcsec;
}

// Global convenience functions

double calculateRefraction(double apparent_altitude_deg) {
    static RefractionCalculator default_calculator;
    return default_calculator.calculate(apparent_altitude_deg);
}

double calculateRefractionWithConditions(double apparent_altitude_deg,
                                         double temperature_celsius,
                                         double pressure_mbar) {
    AtmosphericConditions conditions;
    conditions.temperature_celsius = temperature_celsius;
    conditions.pressure_mbar = pressure_mbar;
    
    RefractionCalculator calculator(conditions);
    return calculator.calculate(apparent_altitude_deg);
}

} // namespace ioccultcalc
