#include "ioccultcalc/kml_exporter.h"
#include "ioccultcalc/time_utils.h"
#include <fstream>
#include <sstream>
#include <iomanip>
#include <stdexcept>

namespace ioccultcalc {

KMLExporter::KMLExporter() {}

KMLExporter::~KMLExporter() {}

void KMLExporter::setExportOptions(const ExportOptions& options) {
    options_ = options;
}

KMLExporter::ExportOptions KMLExporter::getExportOptions() const {
    return options_;
}

bool KMLExporter::exportToKML(const OccultationEvent& event, const std::string& filename) {
    try {
        std::string kmlContent = generateKML(event);
        
        std::ofstream file(filename);
        if (!file.is_open()) {
            return false;
        }
        
        file << kmlContent;
        file.close();
        
        return true;
    } catch (...) {
        return false;
    }
}

bool KMLExporter::exportToKMZ(const OccultationEvent& event, const std::string& filename) {
    // Per semplicità, esportiamo solo KML
    // KMZ richiede compressione ZIP che necessita di libreria aggiuntiva (zlib)
    std::string kmlContent = generateKML(event);
    return compressToKMZ(kmlContent, filename);
}

bool KMLExporter::exportMultipleToKML(const std::vector<OccultationEvent>& events,
                                      const std::string& filename) {
    try {
        std::string kmlContent = generateKML(events);
        
        std::ofstream file(filename);
        if (!file.is_open()) {
            return false;
        }
        
        file << kmlContent;
        file.close();
        
        return true;
    } catch (...) {
        return false;
    }
}

std::string KMLExporter::generateKML(const OccultationEvent& event) {
    std::ostringstream kml;
    
    // Header KML
    kml << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    kml << "<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n";
    kml << "<Document>\n";
    kml << "  <name>Occultazione " << escapeXML(event.asteroid.designation) << "</name>\n";
    kml << "  <description>\n";
    kml << "    <![CDATA[\n";
    kml << "    Asteroide: " << escapeXML(event.asteroid.name) << " (" 
        << escapeXML(event.asteroid.designation) << ")<br/>\n";
    kml << "    Stella Gaia: " << event.star.sourceId << "<br/>\n";
    kml << "    Magnitudine stella: " << std::fixed << std::setprecision(2) 
        << event.star.phot_g_mean_mag << "<br/>\n";
    kml << "    Tempo (UT): " << TimeUtils::jdToISO(event.timeCA) << "<br/>\n";
    kml << "    Closest Approach: " << std::setprecision(3) 
        << event.closeApproachDistance << " arcsec<br/>\n";
    kml << "    Probabilità: " << std::setprecision(1) << (event.probability * 100) << "%<br/>\n";
    kml << "    Durata massima: " << std::setprecision(2) << event.maxDuration << " s<br/>\n";
    kml << "    ]]>\n";
    kml << "  </description>\n\n";
    
    // Stili
    kml << "  <Style id=\"centerlineStyle\">\n";
    kml << "    <LineStyle>\n";
    kml << "      <color>" << options_.centerlineColor << "</color>\n";
    kml << "      <width>3</width>\n";
    kml << "    </LineStyle>\n";
    kml << "  </Style>\n";
    
    kml << "  <Style id=\"pathStyle\">\n";
    kml << "    <LineStyle>\n";
    kml << "      <color>" << options_.pathColor << "</color>\n";
    kml << "      <width>8</width>\n";
    kml << "    </LineStyle>\n";
    kml << "  </Style>\n";
    
    kml << "  <Style id=\"uncertaintyStyle\">\n";
    kml << "    <LineStyle>\n";
    kml << "      <color>" << options_.uncertaintyColor << "</color>\n";
    kml << "      <width>2</width>\n";
    kml << "    </LineStyle>\n";
    kml << "  </Style>\n\n";
    
    // Folder per l'evento
    kml << "  <Folder>\n";
    kml << "    <name>" << event.eventId << "</name>\n";
    
    // Traccia centrale
    if (options_.showCenterline && !event.shadowPath.empty()) {
        kml << generatePath(event.shadowPath, "Central Path", 
                          options_.centerlineColor, options_.pathWidthKm);
    }
    
    // Bande di incertezza
    if (options_.showUncertaintyBands && !event.shadowPath.empty()) {
        // Banda nord
        std::vector<ShadowPathPoint> northPath = event.shadowPath;
        for (auto& point : northPath) {
            // Offset verso nord (approssimazione semplice)
            point.location.latitude += event.uncertaintyNorth / 111.0; // ~111 km per grado
        }
        kml << generatePath(northPath, "Northern Limit (1-sigma)",
                          options_.uncertaintyColor, 1.0);
        
        // Banda sud
        std::vector<ShadowPathPoint> southPath = event.shadowPath;
        for (auto& point : southPath) {
            point.location.latitude -= event.uncertaintySouth / 111.0;
        }
        kml << generatePath(southPath, "Southern Limit (1-sigma)",
                          options_.uncertaintyColor, 1.0);
    }
    
    // Timestamp markers
    if (options_.showTimestamps && !event.shadowPath.empty()) {
        // Mostra un marker ogni 10 punti
        for (size_t i = 0; i < event.shadowPath.size(); i += 10) {
            const auto& point = event.shadowPath[i];
            std::string timeStr = TimeUtils::jdToISO(point.time);
            kml << generatePlacemark("T: " + timeStr, point.location,
                                   "Durata: " + std::to_string(point.duration) + " s");
        }
    }
    
    kml << "  </Folder>\n";
    kml << "</Document>\n";
    kml << "</kml>\n";
    
    return kml.str();
}

std::string KMLExporter::generateKML(const std::vector<OccultationEvent>& events) {
    std::ostringstream kml;
    
    kml << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    kml << "<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n";
    kml << "<Document>\n";
    kml << "  <name>Occultazioni Asteroidali</name>\n";
    kml << "  <description>Collezione di " << events.size() << " eventi</description>\n\n";
    
    // Stili (come sopra)
    kml << "  <Style id=\"centerlineStyle\">\n";
    kml << "    <LineStyle><color>" << options_.centerlineColor 
        << "</color><width>3</width></LineStyle>\n";
    kml << "  </Style>\n";
    
    // Per ogni evento, crea un folder
    for (const auto& event : events) {
        kml << "  <Folder>\n";
        kml << "    <name>" << event.eventId << "</name>\n";
        
        if (!event.shadowPath.empty()) {
            kml << generatePath(event.shadowPath, "Path", 
                              options_.centerlineColor, options_.pathWidthKm);
        }
        
        kml << "  </Folder>\n";
    }
    
    kml << "</Document>\n";
    kml << "</kml>\n";
    
    return kml.str();
}

std::string KMLExporter::generatePlacemark(const std::string& name,
                                          const GeographicCoordinates& coord,
                                          const std::string& description) {
    std::ostringstream kml;
    
    kml << "    <Placemark>\n";
    kml << "      <name>" << escapeXML(name) << "</name>\n";
    if (!description.empty()) {
        kml << "      <description>" << escapeXML(description) << "</description>\n";
    }
    kml << "      <Point>\n";
    kml << "        <coordinates>" << formatCoordinates(coord) << "</coordinates>\n";
    kml << "      </Point>\n";
    kml << "    </Placemark>\n";
    
    return kml.str();
}

std::string KMLExporter::generatePath(const std::vector<ShadowPathPoint>& points,
                                     const std::string& name,
                                     const std::string& color,
                                     double width) {
    if (points.empty()) return "";
    
    std::ostringstream kml;
    
    kml << "    <Placemark>\n";
    kml << "      <name>" << escapeXML(name) << "</name>\n";
    kml << "      <Style>\n";
    kml << "        <LineStyle>\n";
    kml << "          <color>" << color << "</color>\n";
    kml << "          <width>" << (int)(width / 10.0) << "</width>\n";
    kml << "        </LineStyle>\n";
    kml << "      </Style>\n";
    kml << "      <LineString>\n";
    kml << "        <tessellate>1</tessellate>\n";
    kml << "        <coordinates>\n";
    
    for (const auto& point : points) {
        kml << "          " << formatCoordinates(point.location) << "\n";
    }
    
    kml << "        </coordinates>\n";
    kml << "      </LineString>\n";
    kml << "    </Placemark>\n";
    
    return kml.str();
}

std::string KMLExporter::generateTimeSpan(const JulianDate& begin, const JulianDate& end) {
    std::ostringstream kml;
    
    kml << "      <TimeSpan>\n";
    kml << "        <begin>" << TimeUtils::jdToISO(begin) << "</begin>\n";
    kml << "        <end>" << TimeUtils::jdToISO(end) << "</end>\n";
    kml << "      </TimeSpan>\n";
    
    return kml.str();
}

bool KMLExporter::compressToKMZ(const std::string& kmlContent, const std::string& filename) {
    // KMZ è un file ZIP contenente doc.kml
    // Richiede zlib o altra libreria di compressione
    // Per ora, salviamo solo il KML non compresso
    
    std::ofstream file(filename);
    if (!file.is_open()) {
        return false;
    }
    
    file << kmlContent;
    file.close();
    
    return true;
}

std::string KMLExporter::escapeXML(const std::string& text) {
    std::string escaped;
    escaped.reserve(text.size());
    
    for (char c : text) {
        switch (c) {
            case '&':  escaped += "&amp;"; break;
            case '<':  escaped += "&lt;"; break;
            case '>':  escaped += "&gt;"; break;
            case '"':  escaped += "&quot;"; break;
            case '\'': escaped += "&apos;"; break;
            default:   escaped += c; break;
        }
    }
    
    return escaped;
}

std::string KMLExporter::formatCoordinates(const GeographicCoordinates& coord) {
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(6)
        << coord.longitude << "," << coord.latitude << "," << coord.altitude;
    return oss.str();
}

} // namespace ioccultcalc
