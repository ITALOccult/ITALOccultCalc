// Test per confrontare posizione calcolata vs Horizons diretta
#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/time_utils.h"
#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/ephemeris.h"
#include <iostream>
#include <iomanip>
#include <cmath>

using namespace ioccultcalc;

int main() {
    try {
        // Data dell'evento Preston
        JulianDate eventDate = TimeUtils::calendarToJD(2024, 12, 10, 2, 30, 0);  // Momento predetto da Preston
        
        std::cout << "\n═══════════════════════════════════════════════════════\n";
        std::cout << "  CONFRONTO POSIZIONE ASTEROIDE (704) Interamnia\n";
        std::cout << "  Data: 2024-12-10 02:30:00 UTC (Preston prediction)\n";
        std::cout << "═══════════════════════════════════════════════════════\n\n";
        
        // 1. Scarica state vectors da Horizons (heliocentric)
        JPLHorizonsClient horizons;
        auto [position, velocity] = horizons.getStateVectors("704", eventDate, "@sun");
        
        std::cout << "STEP 1: State vectors da Horizons (heliocentric @sun)\n";
        std::cout << "  Position: [" << position.x << ", " << position.y << ", " << position.z << "] AU\n";
        std::cout << "  Velocity: [" << velocity.x << ", " << velocity.y << ", " << velocity.z << "] AU/day\n\n";
        
        // 2a. Scarica posizione Terra da Horizons (per confronto)
        auto [earthPosHorizons, earthVelHorizons] = horizons.getStateVectors("399", eventDate, "@sun");
        
        std::cout << "STEP 2a: Posizione Terra da Horizons @sun\n";
        std::cout << "  Earth (Horizons): [" << earthPosHorizons.x << ", " << earthPosHorizons.y << ", " << earthPosHorizons.z << "] AU\n\n";
        
        // 2b. Confronta con Ephemeris::getEarthPosition()
        Vector3D earthPosLocal = Ephemeris::getEarthPosition(eventDate);
        std::cout << "STEP 2b: Posizione Terra da Ephemeris (locale)\n";
        std::cout << "  Earth (locale): [" << earthPosLocal.x << ", " << earthPosLocal.y << ", " << earthPosLocal.z << "] AU\n";
        
        Vector3D earthDiff = earthPosHorizons - earthPosLocal;
        std::cout << "  Differenza: [" << earthDiff.x << ", " << earthDiff.y << ", " << earthDiff.z << "] AU\n";
        std::cout << "  Mag differenza: " << earthDiff.magnitude() << " AU\n\n";
        
        // 2c. Converti in geocentrico (usando Horizons)
        Vector3D geocentricPos = position - earthPosHorizons;
        
        std::cout << "STEP 3: Calcolo geocentrico (asteroid - earth)\n";
        std::cout << "  Geocentric: [" << geocentricPos.x << ", " << geocentricPos.y << ", " << geocentricPos.z << "] AU\n\n";
        
        // 3. Converti in RA/Dec
        double r_mag = geocentricPos.magnitude();
        double ra_rad = std::atan2(geocentricPos.y, geocentricPos.x);
        if (ra_rad < 0) ra_rad += 2.0 * M_PI;
        double dec_rad = std::asin(geocentricPos.z / r_mag);
        
        double ra_deg = ra_rad * 180.0 / M_PI;
        double dec_deg = dec_rad * 180.0 / M_PI;
        
        std::cout << "STEP 4: Conversione RA/Dec (dal vettore geocentrico)\n";
        std::cout << std::fixed << std::setprecision(6);
        std::cout << "  RA:  " << ra_deg << "°\n";
        std::cout << "  Dec: " << dec_deg << "°\n";
        std::cout << "  Distanza: " << r_mag << " AU\n\n";
        
        // Confronto con stella
        double star_ra = 51.077083;  // TYC 5857-01303-1
        double star_dec = 23.278333;
        
        double delta_ra = ra_deg - star_ra;
        double delta_dec = dec_deg - star_dec;
        
        std::cout << "═══════════════════════════════════════════════════════\n";
        std::cout << "                CONFRONTO CON STELLA\n";
        std::cout << "═══════════════════════════════════════════════════════\n\n";
        std::cout << "Stella TYC 5857-01303-1:\n";
        std::cout << "  RA:  " << star_ra << "°\n";
        std::cout << "  Dec: " << star_dec << "°\n\n";
        
        std::cout << "Differenza:\n";
        std::cout << "  ΔRA:  " << delta_ra << "° (" << delta_ra * 3600 << " arcsec)\n";
        std::cout << "  ΔDec: " << delta_dec << "° (" << delta_dec * 3600 << " arcsec)\n\n";
        
        // Calcola separazione angolare
        double cos_sep = std::sin(dec_rad) * std::sin(star_dec * M_PI / 180.0) +
                        std::cos(dec_rad) * std::cos(star_dec * M_PI / 180.0) * 
                        std::cos((ra_deg - star_ra) * M_PI / 180.0);
        double sep_rad = std::acos(std::max(-1.0, std::min(1.0, cos_sep)));
        double sep_arcsec = sep_rad * 180.0 * 3600.0 / M_PI;
        
        std::cout << "Separazione angolare: " << sep_arcsec << " arcsec (" 
                  << sep_arcsec / 3600.0 << "°)\n\n";
        
        // Dimensione ombra
        double diameter_km = 306.0;
        double distance_km = r_mag * 149597870.7;  // AU to km
        double shadow_arcsec = (diameter_km / distance_km) * 206265.0;
        
        std::cout << "Ombra asteroide: ~" << shadow_arcsec << " arcsec\n";
        
        if (sep_arcsec < shadow_arcsec) {
            std::cout << "\n✓ OCCULTAZIONE POSSIBILE!\n";
        } else {
            std::cout << "\n✗ Miss (separazione > ombra)\n";
        }
        
        std::cout << "\n═══════════════════════════════════════════════════════\n\n";
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "✗ Errore: " << e.what() << std::endl;
        return 1;
    }
}
