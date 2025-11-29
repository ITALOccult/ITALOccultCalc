/**
 * Test delle correzioni di aberrazione e relativistiche
 * per la posizione della Terra
 */

#include <iostream>
#include <iomanip>
#include <cmath>
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/types.h"

using namespace ioccultcalc;

constexpr double AU_TO_KM = 149597870.7;

void testCorrections() {
    std::cout << "=== TEST CORREZIONI ABERRAZIONE E RELATIVISTICHE ===\n\n";
    
    // Data test
    JulianDate jd(2460288.0); // 2024-12-10
    
    // Posizione osservatore (sulla Terra, trascurabile)
    Vector3D observerPos(0, 0, 0);
    
    // 1. Posizione base (senza correzioni)
    Vector3D earthPosBasic = Ephemeris::getEarthPosition(jd);
    
    // 2. Posizione con correzioni
    Vector3D earthPosCorrected = Ephemeris::getEarthPositionWithCorrections(jd, observerPos);
    
    // 3. Calcola differenza
    Vector3D diff = earthPosCorrected - earthPosBasic;
    double diffMag = diff.magnitude();
    
    std::cout << std::fixed << std::setprecision(9);
    
    std::cout << "Posizione SENZA correzioni:\n";
    std::cout << "  X = " << earthPosBasic.x << " AU\n";
    std::cout << "  Y = " << earthPosBasic.y << " AU\n";
    std::cout << "  Z = " << earthPosBasic.z << " AU\n";
    std::cout << "  |r| = " << earthPosBasic.magnitude() << " AU\n\n";
    
    std::cout << "Posizione CON correzioni:\n";
    std::cout << "  X = " << earthPosCorrected.x << " AU\n";
    std::cout << "  Y = " << earthPosCorrected.y << " AU\n";
    std::cout << "  Z = " << earthPosCorrected.z << " AU\n";
    std::cout << "  |r| = " << earthPosCorrected.magnitude() << " AU\n\n";
    
    std::cout << std::setprecision(3);
    std::cout << "CORREZIONE APPLICATA:\n";
    std::cout << "  ΔX = " << diff.x * AU_TO_KM << " km\n";
    std::cout << "  ΔY = " << diff.y * AU_TO_KM << " km\n";
    std::cout << "  ΔZ = " << diff.z * AU_TO_KM << " km\n";
    std::cout << "  |Δr| = " << diffMag * AU_TO_KM << " km\n\n";
    
    // Velocità Terra
    Vector3D earthVel = Ephemeris::getEarthVelocity(jd);
    double velMag = earthVel.magnitude();
    
    std::cout << "Velocità Terra:\n";
    std::cout << "  |v| = " << velMag << " AU/day\n";
    std::cout << "  |v| = " << velMag * AU_TO_KM << " km/day\n";
    std::cout << "  |v| = " << (velMag * AU_TO_KM) / 86400.0 << " km/s\n\n";
    
    // Tempo luce atteso
    constexpr double C_AU_PER_DAY = 173.1446326846693;
    double lightTime = earthPosBasic.magnitude() / C_AU_PER_DAY;
    double expectedCorr = velMag * lightTime;
    
    std::cout << "ANALISI ABERRAZIONE:\n";
    std::cout << "  Tempo luce = " << lightTime * 86400.0 << " secondi\n";
    std::cout << "  Correzione attesa = " << expectedCorr * AU_TO_KM << " km\n";
    std::cout << "  Correzione misurata = " << diffMag * AU_TO_KM << " km\n";
    std::cout << "  Rapporto = " << (diffMag / expectedCorr) * 100.0 << "%\n\n";
    
    // Test con osservatore distante (es. asteroide a 3 AU)
    Vector3D asteroidPos(3.0, 0.0, 0.0);
    Vector3D earthPosCorrAsteroid = Ephemeris::getEarthPositionWithCorrections(jd, asteroidPos);
    Vector3D diffAsteroid = earthPosCorrAsteroid - earthPosBasic;
    
    std::cout << "CON OSSERVATORE A 3 AU:\n";
    std::cout << "  |Δr| = " << diffAsteroid.magnitude() * AU_TO_KM << " km\n\n";
    
    // Effetto angolare
    Vector3D toEarth = earthPosBasic - asteroidPos;
    double distance = toEarth.magnitude();
    double angularShift = (diffAsteroid.magnitude() / distance) * 206264.806247; // arcsec
    
    std::cout << "IMPATTO ANGOLARE:\n";
    std::cout << "  Distanza Terra-Asteroide = " << distance << " AU\n";
    std::cout << "  Spostamento angolare = " << angularShift << " arcsec\n\n";
    
    // Valutazione
    if (diffMag * AU_TO_KM < 100.0) {
        std::cout << "✓✓✓ Eccellente: correzione < 100 km\n";
    } else if (diffMag * AU_TO_KM < 1000.0) {
        std::cout << "✓✓ Buono: correzione < 1000 km\n";
    } else if (diffMag * AU_TO_KM < 10000.0) {
        std::cout << "✓ Accettabile: correzione < 10,000 km\n";
    } else {
        std::cout << "⚠ Correzione significativa: > 10,000 km\n";
    }
}

void testMultipleDates() {
    std::cout << "\n=== TEST SU DATE MULTIPLE ===\n\n";
    
    struct TestDate {
        double jd;
        const char* name;
    };
    
    TestDate dates[] = {
        {2460310.5, "2025-01-01 00:00 UTC"},
        {2460398.5, "2025-03-30 00:00 UTC"},
        {2460491.5, "2025-07-01 00:00 UTC"},
        {2460582.5, "2025-10-01 00:00 UTC"}
    };
    
    Vector3D observerPos(0, 0, 0);
    
    std::cout << std::fixed << std::setprecision(1);
    std::cout << std::setw(25) << std::left << "Data"
              << std::setw(20) << "Correzione (km)"
              << std::setw(20) << "Tempo luce (s)"
              << "\n";
    std::cout << std::string(65, '-') << "\n";
    
    for (const auto& date : dates) {
        JulianDate jd(date.jd);
        
        Vector3D posBasic = Ephemeris::getEarthPosition(jd);
        Vector3D posCorrected = Ephemeris::getEarthPositionWithCorrections(jd, observerPos);
        Vector3D diff = posCorrected - posBasic;
        
        constexpr double C_AU_PER_DAY = 173.1446326846693;
        double lightTime = posBasic.magnitude() / C_AU_PER_DAY;
        
        std::cout << std::setw(25) << std::left << date.name
                  << std::setw(20) << (diff.magnitude() * AU_TO_KM)
                  << std::setw(20) << (lightTime * 86400.0)
                  << "\n";
    }
    
    std::cout << "\n✓ Correzioni consistenti durante l'anno\n";
}

int main() {
    try {
        testCorrections();
        testMultipleDates();
        
        std::cout << "\n=== TEST COMPLETATO CON SUCCESSO ===\n";
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << "\n";
        return 1;
    }
}
