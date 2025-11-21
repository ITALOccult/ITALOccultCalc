#include <iostream>
#include <iomanip>
#include <cmath>

extern "C" {
    #include "SpiceUsr.h"
}

int main() {
    std::cout << std::setprecision(15);
    
    // Carica DE441
    std::string home = getenv("HOME");
    std::string de441Path = home + "/.ioccultcalc/ephemerides/linux_m13000p17000.441";
    
    furnsh_c(de441Path.c_str());
    
    if (failed_c()) {
        char msg[1841];
        getmsg_c("SHORT", 1840, msg);
        std::cerr << "Errore caricamento DE441: " << msg << std::endl;
        return 1;
    }
    
    std::cout << "DE441 caricato con successo\n\n";
    
    // Costanti di conversione
    const double AU_TO_KM = 149597870.7;
    const double DAY_TO_SEC = 86400.0;
    const double KM3S2_TO_AU3D2 = 1.0 / (AU_TO_KM * AU_TO_KM * AU_TO_KM) * (DAY_TO_SEC * DAY_TO_SEC);
    
    // Lista di corpi
    struct Body {
        int naifId;
        const char* name;
        const char* spiceName;
    };
    
    Body bodies[] = {
        {10, "Sole", "SUN"},
        {1, "Mercurio", "MERCURY"},
        {2, "Venere", "VENUS"},
        {399, "Terra", "EARTH"},
        {4, "Marte", "MARS"},
        {5, "Giove", "JUPITER BARYCENTER"},
        {6, "Saturno", "SATURN BARYCENTER"},
        {7, "Urano", "URANUS BARYCENTER"},
        {8, "Nettuno", "NEPTUNE BARYCENTER"},
        {301, "Luna", "MOON"}
    };
    
    std::cout << "Costanti GM da DE441:\n";
    std::cout << "==========================================\n\n";
    
    for (const auto& body : bodies) {
        SpiceInt n;
        SpiceDouble gm_km3s2;
        
        bodvrd_c(body.spiceName, "GM", 1, &n, &gm_km3s2);
        
        if (failed_c()) {
            char msg[1841];
            getmsg_c("SHORT", 1840, msg);
            reset_c();
            std::cout << body.name << " (" << body.spiceName << "): NON DISPONIBILE\n";
            std::cout << "  Errore: " << msg << "\n\n";
            continue;
        }
        
        double gm_au3d2 = gm_km3s2 * KM3S2_TO_AU3D2;
        
        std::cout << body.name << " (NAIF " << body.naifId << "):\n";
        std::cout << "  SPICE name: " << body.spiceName << "\n";
        std::cout << "  GM = " << gm_km3s2 << " km³/s²\n";
        std::cout << "  GM = " << gm_au3d2 << " AU³/day²\n";
        std::cout << "  GM = " << (gm_km3s2 / 1e3) << "e3 km³/s²\n\n";
    }
    
    // Calcola anche AU da DE441
    SpiceInt n;
    SpiceDouble au_km;
    bodvrd_c("EARTH", "LONG_AXIS", 1, &n, &au_km);
    
    if (!failed_c()) {
        std::cout << "AU da DE441: " << au_km << " km\n";
        std::cout << "AU codice:   " << AU_TO_KM << " km\n";
        std::cout << "Differenza:  " << (au_km - AU_TO_KM) << " km\n\n";
    }
    
    unload_c(de441Path.c_str());
    
    return 0;
}
