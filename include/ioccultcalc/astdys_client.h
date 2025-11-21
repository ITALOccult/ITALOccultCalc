#ifndef IOCCULTCALC_ASTDYS_CLIENT_H
#define IOCCULTCALC_ASTDYS_CLIENT_H

#include "orbital_elements.h"
#include <string>
#include <memory>
#include <vector>

namespace ioccultcalc {

class AstDysClient {
public:
    AstDysClient();
    ~AstDysClient();
    
    // Scarica elementi orbitali equinoziali per un asteroide specifico
    // designation può essere numero (es. "433") o designazione (es. "2024 AA")
    EquinoctialElements getElements(const std::string& designation);
    
    // Scarica elementi per una lista di asteroidi
    std::vector<EquinoctialElements> getElementsBatch(const std::vector<std::string>& designations);
    
    // Cerca asteroidi per nome (restituisce lista di possibili match)
    std::vector<std::string> searchByName(const std::string& name);
    
    // Imposta l'URL base di AstDyS (default: https://newton.spacedys.com/astdys2/)
    void setBaseURL(const std::string& url);
    
    // Imposta timeout per le richieste HTTP (secondi)
    void setTimeout(int seconds);
    
private:
    class Impl;
    std::unique_ptr<Impl> pImpl;
    
    // Parsing del file .eq (equinoctial elements)
    EquinoctialElements parseEquinoctialFile(const std::string& content, 
                                            const std::string& designation);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_ASTDYS_CLIENT_H
