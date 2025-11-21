#ifndef IOCCULTCALC_KML_EXPORTER_H
#define IOCCULTCALC_KML_EXPORTER_H

#include "occultation_predictor.h"
#include <string>

namespace ioccultcalc {

class KMLExporter {
public:
    KMLExporter();
    ~KMLExporter();
    
    // Esporta un evento di occultazione in formato KML
    bool exportToKML(const OccultationEvent& event, const std::string& filename);
    
    // Esporta in formato KMZ (KML compresso)
    bool exportToKMZ(const OccultationEvent& event, const std::string& filename);
    
    // Esporta multipli eventi in un unico file KML
    bool exportMultipleToKML(const std::vector<OccultationEvent>& events, 
                            const std::string& filename);
    
    // Opzioni di visualizzazione
    struct ExportOptions {
        bool showUncertaintyBands;   // Mostra bande di incertezza
        bool showTimestamps;         // Mostra timestamp lungo il path
        bool showCenterline;         // Evidenzia linea centrale
        bool showObserverPoints;     // Mostra punti osservatore
        double pathWidthKm;          // Larghezza del path (km)
        std::string centerlineColor; // Colore linea centrale (AABBGGRR)
        std::string pathColor;       // Colore path (AABBGGRR)
        std::string uncertaintyColor; // Colore incertezze (AABBGGRR)
        
        ExportOptions() 
            : showUncertaintyBands(true),
              showTimestamps(true),
              showCenterline(true),
              showObserverPoints(false),
              pathWidthKm(100.0),
              centerlineColor("ff0000ff"),    // Rosso
              pathColor("7f00ffff"),          // Giallo semitrasparente
              uncertaintyColor("7fff0000") {} // Blu semitrasparente
    };
    
    void setExportOptions(const ExportOptions& options);
    ExportOptions getExportOptions() const;
    
private:
    ExportOptions options_;
    
    // Genera il documento KML
    std::string generateKML(const OccultationEvent& event);
    std::string generateKML(const std::vector<OccultationEvent>& events);
    
    // Genera elementi KML specifici
    std::string generatePlacemark(const std::string& name, 
                                 const GeographicCoordinates& coord,
                                 const std::string& description);
    
    std::string generatePath(const std::vector<ShadowPathPoint>& points,
                            const std::string& name,
                            const std::string& color,
                            double width);
    
    std::string generateTimeSpan(const JulianDate& begin, const JulianDate& end);
    
    // Comprimi KML in KMZ
    bool compressToKMZ(const std::string& kmlContent, const std::string& filename);
    
    // Utility per formattazione XML
    std::string escapeXML(const std::string& text);
    std::string formatCoordinates(const GeographicCoordinates& coord);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_KML_EXPORTER_H
