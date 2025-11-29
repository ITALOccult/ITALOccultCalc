/**
 * @file compare_earth_methods.cpp
 * @brief Confronta metodi diversi per calcolare posizione Terra
 */

#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/time_utils.h"
#include "ioccultcalc/types.h"

using namespace ioccultcalc;

int main() {
    std::cout << std::fixed << std::setprecision(6);
    
    std::cout << "\n╔════════════════════════════════════════════════════════╗\n";
    std::cout << "║   CONFRONTO METODI CALCOLO POSIZIONE TERRA            ║\n";
    std::cout << "╚════════════════════════════════════════════════════════╝\n\n";
    
    // Data test
    std::string testDate = "2024-12-10T02:30:00";
    JulianDate jd = TimeUtils::isoToJD(testDate);
    
    std::cout << "Data test: " << testDate << "\n";
    std::cout << "JD: " << jd.jd << "\n\n";
    
    JPLHorizonsClient horizons;
    
    // 1. Horizons (riferimento)
    std::cout << "1. JPL Horizons (@sun, ICRF)\n";
    std::cout << "   -------------------------------\n";
    
    Vector3D horizonsPos, horizonsVel;
    try {
        auto [pos, vel] = horizons.getStateVectors("399", jd, "@sun");
        horizonsPos = pos;
        horizonsVel = vel;
        
        std::cout << "   Position: [" << pos.x << ", " << pos.y << ", " << pos.z << "] AU\n";
        std::cout << "   Velocity: [" << vel.x << ", " << vel.y << ", " << vel.z << "] AU/day\n";
        std::cout << "   Magnitude: " << pos.magnitude() << " AU\n\n";
    } catch (const std::exception& e) {
        std::cerr << "   ERRORE: " << e.what() << "\n\n";
        return 1;
    }
    
    // 2. SPK DE440s con ECLIPJ2000 (attuale)
    std::cout << "2. SPK DE440s (ECLIPJ2000 @ Sun)\n";
    std::cout << "   -------------------------------\n";
    
    Vector3D spkPos = Ephemeris::getEarthPosition(jd);
    Vector3D spkVel = Ephemeris::getEarthVelocity(jd);
    
    std::cout << "   Position: [" << spkPos.x << ", " << spkPos.y << ", " << spkPos.z << "] AU\n";
    std::cout << "   Velocity: [" << spkVel.x << ", " << spkVel.y << ", " << spkVel.z << "] AU/day\n";
    std::cout << "   Magnitude: " << spkPos.magnitude() << " AU\n\n";
    
    // Differenza
    Vector3D diffPos = spkPos - horizonsPos;
    Vector3D diffVel = spkVel - horizonsVel;
    
    constexpr double AU_TO_KM = 149597870.7;
    double errPos = diffPos.magnitude() * AU_TO_KM;
    double errVel = diffVel.magnitude() * AU_TO_KM; // km/day
    
    std::cout << "═══════════════════════════════════════════════════════════\n";
    std::cout << "                    CONFRONTO\n";
    std::cout << "═══════════════════════════════════════════════════════════\n\n";
    
    std::cout << "Differenza Posizione:\n";
    std::cout << "  ΔX: " << std::setw(12) << (diffPos.x * AU_TO_KM) << " km\n";
    std::cout << "  ΔY: " << std::setw(12) << (diffPos.y * AU_TO_KM) << " km\n";
    std::cout << "  ΔZ: " << std::setw(12) << (diffPos.z * AU_TO_KM) << " km\n";
    std::cout << "  |Δ|: " << std::setw(11) << errPos << " km\n\n";
    
    std::cout << "Differenza Velocità:\n";
    std::cout << "  ΔVX: " << std::setw(11) << (diffVel.x * AU_TO_KM) << " km/day\n";
    std::cout << "  ΔVY: " << std::setw(11) << (diffVel.y * AU_TO_KM) << " km/day\n";
    std::cout << "  ΔVZ: " << std::setw(11) << (diffVel.z * AU_TO_KM) << " km/day\n";
    std::cout << "  |ΔV|: " << std::setw(10) << errVel << " km/day\n\n";
    
    // Componente dominante
    std::cout << "Componente errore dominante in posizione: ";
    double maxComp = std::max({std::abs(diffPos.x), std::abs(diffPos.y), std::abs(diffPos.z)});
    if (std::abs(diffPos.x) == maxComp) {
        std::cout << "X (" << int(diffPos.x * AU_TO_KM) << " km)\n";
    } else if (std::abs(diffPos.y) == maxComp) {
        std::cout << "Y (" << int(diffPos.y * AU_TO_KM) << " km)\n";
    } else {
        std::cout << "Z (" << int(diffPos.z * AU_TO_KM) << " km)\n";
    }
    
    // Analisi geometrica
    std::cout << "\n═══════════════════════════════════════════════════════════\n";
    std::cout << "              ANALISI GEOMETRICA ERRORE\n";
    std::cout << "═══════════════════════════════════════════════════════════\n\n";
    
    // Errore radiale vs tangenziale
    Vector3D radialDir = horizonsPos.normalize();
    double errRadial = diffPos.dot(radialDir);
    Vector3D tangentialErr = diffPos - (radialDir * errRadial);
    double errTangential = tangentialErr.magnitude();
    
    std::cout << "Errore radiale:       " << std::setw(10) << int(errRadial * AU_TO_KM) << " km\n";
    std::cout << "Errore tangenziale:   " << std::setw(10) << int(errTangential * AU_TO_KM) << " km\n\n";
    
    // Impatto su osservazioni geocentriche
    std::cout << "Impatto su coordinate geocentriche:\n";
    std::cout << "(per asteroide a 3 AU dalla Terra)\n\n";
    
    double asteroidDist = 3.0; // AU
    double angularError = std::atan2(errTangential * AU_TO_KM, asteroidDist * AU_TO_KM);
    double angularErrorArcsec = angularError * 206265.0; // radianti -> arcsec
    
    std::cout << "  Errore angolare: " << std::setw(8) << std::setprecision(2) << angularErrorArcsec << " arcsec\n";
    std::cout << "  Errore angolare: " << std::setw(8) << std::setprecision(4) << (angularErrorArcsec / 3600.0) << " deg\n\n";
    
    // Valutazione finale
    std::cout << "═══════════════════════════════════════════════════════════\n";
    std::cout << "                    VALUTAZIONE\n";
    std::cout << "═══════════════════════════════════════════════════════════\n\n";
    
    if (errPos < 100000) {
        std::cout << "✓✓✓ Eccellente (<100,000 km)\n";
    } else if (errPos < 500000) {
        std::cout << "✓✓ Buono (<500,000 km)\n";
    } else if (errPos < 1000000) {
        std::cout << "✓ Accettabile (<1,000,000 km)\n";
    } else {
        std::cout << "✗ Troppo grande (>" << int(errPos / 1000000.0) << "M km)\n";
    }
    
    std::cout << "\nPer occultazioni asteroidali:\n";
    if (angularErrorArcsec < 1.0) {
        std::cout << "✓✓✓ Errore angolare eccellente (<1\")\n";
    } else if (angularErrorArcsec < 10.0) {
        std::cout << "✓✓ Errore angolare buono (<10\")\n";
    } else if (angularErrorArcsec < 60.0) {
        std::cout << "✓ Errore angolare accettabile (<1')\n";
    } else {
        std::cout << "⚠ Errore angolare potrebbe influenzare predizioni\n";
    }
    
    std::cout << "\n";
    
    return 0;
}
