#ifndef IOCCULTCALC_MPC_CLIENT_H
#define IOCCULTCALC_MPC_CLIENT_H

#include "observation.h"
#include <string>
#include <memory>

namespace ioccultcalc {

class MPCClient {
public:
    MPCClient();
    ~MPCClient();
    
    // Scarica osservazioni per un oggetto dal Minor Planet Center
    ObservationSet getObservations(const std::string& designation);
    
    // Scarica osservazioni in un intervallo temporale
    ObservationSet getObservations(const std::string& designation,
                                   const JulianDate& startDate,
                                   const JulianDate& endDate);
    
    // Scarica dal servizio MPC WebService
    ObservationSet getObservationsFromWebService(const std::string& designation);
    
    // Scarica da file locale MPC format
    ObservationSet loadFromFile(const std::string& filename);
    
    // Carica da file .rwo (formato AstDyS)
    ObservationSet loadFromRWOFile(const std::string& filename);
    
    // Salva osservazioni in formato MPC
    bool saveToFile(const ObservationSet& observations, const std::string& filename);
    
    // Imposta URL base del servizio MPC
    void setBaseURL(const std::string& url);
    
    // Imposta timeout
    void setTimeout(int seconds);
    
    // Scarica file ObsCodes.html (catalogo osservatori)
    bool downloadObservatoryCodes(const std::string& outputFile = "ObsCodes.txt");
    
private:
    class Impl;
    std::unique_ptr<Impl> pImpl;
    
    // Parsing formato MPC 80 colonne
    AstrometricObservation parseMPC80Line(const std::string& line);
    
    // Parsing formato RWO (AstDyS custom format)
    AstrometricObservation parseRWOLine(const std::string& line);
    
    // Parsing formato ADES (XML/JSON)
    std::vector<AstrometricObservation> parseADES(const std::string& content);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_MPC_CLIENT_H
