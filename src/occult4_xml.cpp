#include "ioccultcalc/occult4_xml.h"
#include "ioccultcalc/time_utils.h"
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <ctime>
#include <algorithm>
#include <cctype>

namespace ioccultcalc {

// Helper: split string by comma
static std::vector<std::string> splitCsv(const std::string& s) {
    std::vector<std::string> tokens;
    std::string token;
    std::istringstream tokenStream(s);
    while (std::getline(tokenStream, token, ',')) {
        tokens.push_back(token);
    }
    return tokens;
}

// ============================================================================
// Constructor/Destructor
// ============================================================================

Occult4XMLHandler::Occult4XMLHandler() {
    // Inizializza libxml2
    xmlInitParser();
}

Occult4XMLHandler::~Occult4XMLHandler() {
    // Cleanup libxml2
    xmlCleanupParser();
}

// Helper: Converti JD in data calendario
void jdToCalendar(double jd, int& year, int& month, int& day, double& ut) {
    int a = (int)(jd + 0.5);
    int b = a + 1537;
    int c = (int)((b - 122.1) / 365.25);
    int d = (int)(365.25 * c);
    int e = (int)((b - d) / 30.6001);
    
    day = b - d - (int)(30.6001 * e);
    month = e - (e < 14 ? 1 : 13);
    year = c - (month > 2 ? 4716 : 4715);
    
    double frac = jd + 0.5 - (int)(jd + 0.5);
    ut = frac * 24.0;  // UT in decimal hours
}

// ============================================================================
// IMPORT da XML
// ============================================================================

std::vector<Occult4XMLHandler::Occult4Event> 
Occult4XMLHandler::loadFromXML(const std::string& filename) {
    // Leggi file
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open XML file: " + filename);
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string xmlContent = buffer.str();
    file.close();
    
    return parseXML(xmlContent);
}

std::vector<Occult4XMLHandler::Occult4Event>
Occult4XMLHandler::parseXML(const std::string& xmlContent) {
    std::vector<Occult4Event> events;
    
    // Parse XML
    xmlDocPtr doc = xmlReadMemory(xmlContent.c_str(), xmlContent.length(),
                                  "noname.xml", nullptr, 0);
    if (doc == nullptr) {
        throw std::runtime_error("Failed to parse XML content");
    }
    
    // Get root element
    xmlNode* root = xmlDocGetRootElement(doc);
    if (root == nullptr) {
        xmlFreeDoc(doc);
        throw std::runtime_error("Empty XML document");
    }
    
    // Find all <Event> or <Occultation> nodes
    for (xmlNode* node = root->children; node; node = node->next) {
        if (node->type != XML_ELEMENT_NODE) continue;
        
        std::string nodeName = (const char*)node->name;
        if (nodeName == "Event" || nodeName == "Occultation") {
            Occult4Event event = parseEventNode(node);
            events.push_back(event);
        }
    }
    
    xmlFreeDoc(doc);
    return events;
}

Occult4XMLHandler::Occult4Event 
Occult4XMLHandler::parseEventNode(void* nodePtr) {
    xmlNode* node = (xmlNode*)nodePtr;
    Occult4Event event;
    
    // Parse child elements
    for (xmlNode* child = node->children; child; child = child->next) {
        if (child->type != XML_ELEMENT_NODE) continue;
        
        std::string name = (const char*)child->name;
        
        // Asteroid info
        if (name == "AsteroidNumber") {
            event.asteroidNumber = extractTextContent(child);
        } else if (name == "AsteroidName") {
            event.asteroidName = extractTextContent(child);
        } else if (name == "AsteroidDesignation") {
            event.asteroidDesignation = extractTextContent(child);
        } else if (name == "AsteroidMag") {
            event.asteroidMag = extractDoubleContent(child);
        }
        
        // Star info
        else if (name == "StarCatalog") {
            event.starCatalog = extractTextContent(child);
        } else if (name == "StarID") {
            event.starId = extractTextContent(child);
        } else if (name == "StarRA") {
            event.starRA = extractDoubleContent(child);
        } else if (name == "StarDec") {
            event.starDec = extractDoubleContent(child);
        } else if (name == "StarMag") {
            event.starMag = extractDoubleContent(child);
        } else if (name == "StarDistance") {
            event.starDistance = extractDoubleContent(child);
        }
        
        // Event time
        else if (name == "JulianDate" || name == "JD") {
            event.jdEvent = extractDoubleContent(child);
        } else if (name == "DateTime" || name == "EventTime") {
            event.dateTimeUTC = extractTextContent(child);
        }
        
        // Geometry
        else if (name == "CloseApproach" || name == "CA") {
            event.closeApproachDist = extractDoubleContent(child);
        } else if (name == "PositionAngle" || name == "PA") {
            event.posAngle = extractDoubleContent(child);
        } else if (name == "PathWidth") {
            event.pathWidth = extractDoubleContent(child);
        } else if (name == "MaxDuration") {
            event.maxDuration = extractDoubleContent(child);
        } else if (name == "Uncertainty") {
            event.uncertainty = extractDoubleContent(child);
        } else if (name == "Probability") {
            event.probability = extractDoubleContent(child);
        } else if (name == "MagDrop") {
            event.dropMag = extractDoubleContent(child);
        } else if (name == "EventID") {
            event.eventId = extractTextContent(child);
        }
        
        // Compact CSV-style tags (Standard Occult4 / IOTA)
        else if (name == "Star") {
            auto tokens = splitCsv(extractTextContent(child));
            if (tokens.size() >= 5) {
                event.starId = tokens[0];
                event.starRA = std::atof(tokens[1].c_str()) * 15.0; // Hours to Deg
                event.starDec = std::atof(tokens[2].c_str());
                event.starMag = std::atof(tokens[4].c_str()); // Mv
            }
            if (tokens.size() >= 13) {
                event.dropMag = std::atof(tokens[12].c_str());
            }
        } else if (name == "Object") {
            auto tokens = splitCsv(extractTextContent(child));
            if (tokens.size() >= 2) {
                event.asteroidNumber = tokens[0];
                event.asteroidName = tokens[1];
            }
            if (tokens.size() >= 4) {
                // Diameter is field 3
            }
        } else if (name == "Orbit") {
            auto tokens = splitCsv(extractTextContent(child));
            // Currently not mapped to Occult4Event but could be
        } else if (name == "ID") {
            auto tokens = splitCsv(extractTextContent(child));
            if (tokens.size() >= 2) {
                event.eventId = tokens[0];
                // JD is field 1, but usually we prefer JulianDate tag
            }
        }
        
        // Path points
        else if (name == "CenterLine" || name == "CentralPath") {
            event.centerLine = parsePathPoints(child);
        } else if (name == "NorthLimit") {
            event.northLimit = parsePathPoints(child);
        } else if (name == "SouthLimit") {
            event.southLimit = parsePathPoints(child);
        }
    }
    
    return event;
}

std::vector<Occult4XMLHandler::Occult4Event::PathPoint>
Occult4XMLHandler::parsePathPoints(void* nodePtr) {
    xmlNode* node = (xmlNode*)nodePtr;
    std::vector<Occult4Event::PathPoint> points;
    
    for (xmlNode* child = node->children; child; child = child->next) {
        if (child->type != XML_ELEMENT_NODE) continue;
        
        std::string name = (const char*)child->name;
        if (name == "Point" || name == "PathPoint") {
            Occult4Event::PathPoint point;
            
            for (xmlNode* attr = child->children; attr; attr = attr->next) {
                if (attr->type != XML_ELEMENT_NODE) continue;
                
                std::string attrName = (const char*)attr->name;
                if (attrName == "Latitude" || attrName == "Lat") {
                    point.latitude = extractDoubleContent(attr);
                } else if (attrName == "Longitude" || attrName == "Lon") {
                    point.longitude = extractDoubleContent(attr);
                } else if (attrName == "JD") {
                    point.jd = extractDoubleContent(attr);
                } else if (attrName == "DateTime") {
                    point.dateTime = extractTextContent(attr);
                } else if (attrName == "Altitude" || attrName == "StarAlt") {
                    point.altitude = extractDoubleContent(attr);
                } else if (attrName == "SunAltitude" || attrName == "SunAlt") {
                    point.sunAltitude = extractDoubleContent(attr);
                }
            }
            
            points.push_back(point);
        }
    }
    
    return points;
}

OccultationEvent Occult4XMLHandler::toIOccultCalcEvent(const Occult4Event& o4) {
    OccultationEvent event;
    
    // Asteroid data
    event.asteroid.name = o4.asteroidName;
    event.asteroid.designation = o4.asteroidDesignation.empty() ? 
                                 o4.asteroidNumber : o4.asteroidDesignation;
    
    // Star data
    event.star.sourceId = o4.starId;
    event.star.pos.ra = o4.starRA * DEG_TO_RAD;
    event.star.pos.dec = o4.starDec * DEG_TO_RAD;
    event.star.phot_g_mean_mag = o4.starMag;
    
    // Event timing
    event.timeCA.jd = o4.jdEvent;
    
    // Geometry
    event.closeApproachDistance = o4.closeApproachDist;
    event.positionAngle = o4.posAngle;
    event.maxDuration = o4.maxDuration;
    event.probability = o4.probability;
    
    // Uncertainty
    event.uncertaintyNorth = o4.uncertainty;
    event.uncertaintySouth = o4.uncertainty;
    
    // Shadow path
    event.shadowPath.clear();
    for (const auto& pt : o4.centerLine) {
        ShadowPathPoint ioPoint;
        ioPoint.location.latitude = pt.latitude * DEG_TO_RAD;
        ioPoint.location.longitude = pt.longitude * DEG_TO_RAD;
        ioPoint.location.altitude = 0.0;
        ioPoint.time.jd = pt.jd;
        ioPoint.duration = 0.0; // non disponibile da XML
        ioPoint.centerlineDistance = 0.0;
        event.shadowPath.push_back(ioPoint);
    }
    
    event.eventId = o4.eventId;
    
    return event;
}

// ============================================================================
// EXPORT a XML
// ============================================================================

bool Occult4XMLHandler::exportToXML(const OccultationEvent& event, 
                                    const std::string& filename) {
    std::string xml = generateXML(event);
    
    std::ofstream file(filename);
    if (!file.is_open()) {
        return false;
    }
    
    file << xml;
    file.close();
    return true;
}

bool Occult4XMLHandler::exportMultipleToXML(
    const std::vector<OccultationEvent>& events,
    const std::string& filename) {
    
    std::string xml = generateXML(events);
    
    std::ofstream file(filename);
    if (!file.is_open()) {
        return false;
    }
    
    file << xml;
    file.close();
    return true;
}

std::string Occult4XMLHandler::generateXML(const OccultationEvent& event) {
    std::vector<OccultationEvent> events = {event};
    return generateXML(events);
}

std::string Occult4XMLHandler::generateXML(const std::vector<OccultationEvent>& events) {
    std::ostringstream xml;
    // No XML declaration as per test_output_occult4.xml
    xml << "<Occultations>\n";
    for (const auto& event : events) {
        xml << generateOccult4EventXML(event);
    }
    xml << "</Occultations>\n";
    return xml.str();
}

std::string Occult4XMLHandler::generateOccult4EventXML(const OccultationEvent& event) {
    std::ostringstream xml;
    xml << std::fixed << std::setprecision(8);
    const double rad2deg = 180.0 / M_PI;
    
    // Time conversion (TDB -> UTC)
    // Delta T for 2026 is approx 69.184 seconds. 
    // TDB is ahead of UTC. UTC = TDB - DeltaT
    double deltaT_days = 69.184 / 86400.0;
    
    JulianDate jd_tdb = event.timeCA;
    JulianDate jd_utc(jd_tdb.jd - deltaT_days);
    
    int year, month, day;
    double ut_hours;
    jdToCalendar(jd_utc.jd, year, month, day, ut_hours);
    
    // For ID and Epoch we use the UTC date
    int epoch_year, epoch_month, epoch_day;
    double epoch_ut;
    jdToCalendar(jd_utc.jd, epoch_year, epoch_month, epoch_day, epoch_ut);
    
    xml << "  <Event>\n";
    
    // 1. <Elements> - 14 fields
    xml << "    <Elements>";
    const char* months[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
    char sourceDate[64];
    snprintf(sourceDate, sizeof(sourceDate), "ITALOCC:%d-%s-%02d", year, months[month-1], day);
    xml << sourceDate; 
    xml << "," << std::setprecision(2) << event.maxDuration;
    xml << "," << year << "," << month << "," << day;
    xml << "," << std::setprecision(6) << ut_hours;
    xml << "," << std::setprecision(8) << event.besselianX;
    xml << "," << event.besselianY;
    xml << "," << event.besselianDX;
    xml << "," << event.besselianDY;
    xml << ",0.0,0.0,0.0,0.0"; // d2X, d2Y, d3X, d3Y
    xml << "</Elements>\n";
    
    // 2. <Earth> - 5 fields (Substellar/Subsolar)
    xml << "    <Earth>";
    xml << std::setprecision(6) << event.substellarLon << "," << event.substellarLat;
    xml << "," << event.subsolarLon << "," << event.subsolarLat;
    xml << ",False"; // JWST
    xml << "</Earth>\n";
    
    // 3. <Star> - 16 fields
    xml << "    <Star>";
    std::string sId = event.star.sourceId;
    if (sId.find("GAIA") == std::string::npos && sId.length() > 10) {
        xml << "GAIA DR3 " << sId;
    } else {
        xml << (sId.empty() ? "Unknown" : escapeXML(sId));
    }
    
    double raHours = event.star.pos.ra * (180.0 / M_PI) / 15.0;
    double decDeg = event.star.pos.dec * (180.0 / M_PI);
    
    double appRaHours = event.starAppRA / 15.0;
    double appDecDeg = event.starAppDec;
    
    xml << "," << std::setprecision(8) << raHours << "," << decDeg;
    xml << "," << std::setprecision(2) << event.star.phot_g_mean_mag; // Mb
    xml << "," << event.star.phot_g_mean_mag; // Mv
    xml << "," << event.star.phot_g_mean_mag; // Mr
    xml << ",0.0,0,," << std::setprecision(8) << appRaHours << "," << appDecDeg; // dia, double, K2, AppRA, AppDec
    xml << "," << std::setprecision(2) << (event.magnitudeDrop > 0 ? event.magnitudeDrop : 0.01); // MdropV
    xml << "," << std::setprecision(2) << (event.magnitudeDrop > 0 ? event.magnitudeDrop : 0.01); // MdropR
    xml << ",0,0,0"; // Adjusted, NearbyCounts
    xml << "</Star>\n";
    
    // 4. <Object> - 15 fields (trailing comma)
    xml << "    <Object>";
    if (event.asteroid.number > 0) xml << event.asteroid.number;
    else xml << escapeXML(event.asteroid.designation);
    
    xml << "," << escapeXML(event.asteroid.name);
    xml << "," << std::setprecision(2) << event.asteroid.H;
    xml << "," << std::setprecision(1) << event.asteroid.diameter;
    xml << "," << std::setprecision(6) << (event.asteroidDistanceAu > 0 ? event.asteroidDistanceAu : 1.0);
    xml << ",0,0,0.0,0.0,,0.0,0"; // #rings, #moons, dRA, dDec, Tax, DiamUnc, ShadowFlag
    xml << "," << std::setprecision(2) << event.asteroid.H << "," << event.asteroid.H << ","; // MagV, MagR, trailing comma
    xml << "</Object>\n";
    
    // 5. <Orbit> - 14 fields
    xml << "    <Orbit>";
    xml << "2000.0"; // Equinox
    ioccultcalc::OrbitalElements kep = event.asteroid.toKeplerian();
    xml << "," << std::setprecision(4) << (kep.M * rad2deg);
    xml << "," << year << "," << month << "," << day; // Epoch
    xml << "," << std::setprecision(4) << (kep.omega * rad2deg);
    xml << "," << (kep.Omega * rad2deg);
    xml << "," << (kep.i * rad2deg);
    xml << "," << std::setprecision(6) << kep.e;
    xml << "," << kep.a;
    xml << "," << (kep.a * (1.0 - kep.e)); // q
    xml << "," << std::setprecision(2) << event.asteroid.H;
    xml << ",0.0," << event.asteroid.G; // Coeff, G
    xml << "</Orbit>\n";
    
    // 6. <Errors> - 10 fields
    xml << "    <Errors>";
    double pathWidthKm = event.pathWidth > 0 ? event.pathWidth : 10.0;
    double uncertKm = (event.uncertaintyNorth + event.uncertaintySouth) / 2.0;
    xml << std::setprecision(3) << (uncertKm / pathWidthKm); // in PathWidths
    xml << ",0.0,0.0,0.0,0.0"; // Major, Minor, PA, 1SigmaPos
    xml << ",Known errors,-1,0,0,0"; // Basis, Reliability, Duplicate, Non-Gaia, GaiaPM
    xml << "</Errors>\n";
    
    // 7. <ID> - 2 fields
    xml << "    <ID>";
    char dateID[32];
    snprintf(dateID, sizeof(dateID), "%04d%02d%02d", year, month, day);
    std::string cleanId = event.star.sourceId;
    // Remove any non-digit characters if it's a Gaia ID
    cleanId.erase(std::remove_if(cleanId.begin(), cleanId.end(), [](char c) { return !std::isdigit(c); }), cleanId.end());
    xml << dateID << "_" << cleanId.substr(std::max(0, (int)cleanId.length() - 8));
    xml << "," << std::setprecision(4) << (jd_utc.jd - 2400000.5);
    xml << "</ID>\n";
    
    // 8. Path points (CenterLine, NorthLimit, SouthLimit)
    if (!event.shadowPath.empty()) {
        xml << "    <CenterLine>\n";
        for (const auto& pt : event.shadowPath) {
            xml << "      <Point>\n";
            xml << "        <Latitude>" << std::setprecision(6) << (pt.location.latitude * rad2deg) << "</Latitude>\n";
            xml << "        <Longitude>" << (pt.location.longitude * rad2deg) << "</Longitude>\n";
            xml << "        <JD>" << std::setprecision(8) << pt.time.jd << "</JD>\n";
            xml << "        <DateTime>" << TimeUtils::jdToISO(pt.time) << "</DateTime>\n";
            xml << "      </Point>\n";
        }
        xml << "    </CenterLine>\n";
    }

    if (!event.north_limit.empty()) {
        xml << "    <NorthLimit>\n";
        for (const auto& pt : event.north_limit) {
            xml << "      <Point>\n";
            xml << "        <Latitude>" << std::setprecision(6) << (pt.latitude * rad2deg) << "</Latitude>\n";
            xml << "        <Longitude>" << (pt.longitude * rad2deg) << "</Longitude>\n";
            xml << "      </Point>\n";
        }
        xml << "    </NorthLimit>\n";
    }

    if (!event.south_limit.empty()) {
        xml << "    <SouthLimit>\n";
        for (const auto& pt : event.south_limit) {
            xml << "      <Point>\n";
            xml << "        <Latitude>" << std::setprecision(6) << (pt.latitude * rad2deg) << "</Latitude>\n";
            xml << "        <Longitude>" << (pt.longitude * rad2deg) << "</Longitude>\n";
            xml << "      </Point>\n";
        }
        xml << "    </SouthLimit>\n";
    }
    
    xml << "  </Event>\n";
    return xml.str();
}


std::string Occult4XMLHandler::generateEventXML(const Occult4Event& event) {
    std::ostringstream xml;
    xml << std::fixed << std::setprecision(6);
    
    xml << "  <Event>\n";
    xml << "    <EventID>" << escapeXML(event.eventId) << "</EventID>\n";
    
    // Asteroid
    if (options_.includeAsteroidData) {
        xml << "    <Asteroid>\n";
        if (!event.asteroidNumber.empty()) {
            xml << "      <Number>" << escapeXML(event.asteroidNumber) << "</Number>\n";
        }
        if (!event.asteroidName.empty()) {
            xml << "      <Name>" << escapeXML(event.asteroidName) << "</Name>\n";
        }
        if (!event.asteroidDesignation.empty()) {
            xml << "      <Designation>" << escapeXML(event.asteroidDesignation) << "</Designation>\n";
        }
        if (event.asteroidMag > 0) {
            xml << "      <Magnitude>" << formatDouble(event.asteroidMag, 2) << "</Magnitude>\n";
        }
        xml << "    </Asteroid>\n";
    }
    
    // Star
    if (options_.includeStarData) {
        xml << "    <Star>\n";
        xml << "      <Catalog>" << escapeXML(event.starCatalog) << "</Catalog>\n";
        xml << "      <ID>" << escapeXML(event.starId) << "</ID>\n";
        xml << "      <RA unit=\"degrees\">" << formatDouble(event.starRA, 8) << "</RA>\n";
        xml << "      <Dec unit=\"degrees\">" << formatDouble(event.starDec, 8) << "</Dec>\n";
        xml << "      <RAFormatted>" << formatRA(event.starRA) << "</RAFormatted>\n";
        xml << "      <DecFormatted>" << formatDec(event.starDec) << "</DecFormatted>\n";
        xml << "      <Magnitude>" << formatDouble(event.starMag, 2) << "</Magnitude>\n";
        if (event.starDistance > 0) {
            xml << "      <Distance unit=\"parsec\">" << formatDouble(event.starDistance, 2) << "</Distance>\n";
        }
        xml << "    </Star>\n";
    }
    
    // Event timing
    xml << "    <Time>\n";
    xml << "      <JulianDate>" << formatDouble(event.jdEvent, 8) << "</JulianDate>\n";
    xml << "      <UTC>" << event.dateTimeUTC << "</UTC>\n";
    xml << "    </Time>\n";
    
    // Geometry
    xml << "    <Geometry>\n";
    xml << "      <CloseApproach unit=\"arcsec\">" << formatDouble(event.closeApproachDist, 4) << "</CloseApproach>\n";
    xml << "      <PositionAngle unit=\"degrees\">" << formatDouble(event.posAngle, 2) << "</PositionAngle>\n";
    xml << "      <PathWidth unit=\"km\">" << formatDouble(event.pathWidth, 2) << "</PathWidth>\n";
    xml << "      <MaxDuration unit=\"seconds\">" << formatDouble(event.maxDuration, 2) << "</MaxDuration>\n";
    if (options_.includeUncertainty && event.uncertainty > 0) {
        xml << "      <Uncertainty unit=\"km\">" << formatDouble(event.uncertainty, 2) << "</Uncertainty>\n";
    }
    xml << "      <Probability>" << formatDouble(event.probability, 4) << "</Probability>\n";
    if (event.dropMag > 0) {
        xml << "      <MagnitudeDrop>" << formatDouble(event.dropMag, 2) << "</MagnitudeDrop>\n";
    }
    xml << "    </Geometry>\n";
    
    // Path points
    if (options_.includePathPoints && !event.centerLine.empty()) {
        xml << "    <CenterLine>\n";
        for (const auto& pt : event.centerLine) {
            xml << "      <Point>\n";
            xml << "        <Latitude>" << formatDouble(pt.latitude, 6) << "</Latitude>\n";
            xml << "        <Longitude>" << formatDouble(pt.longitude, 6) << "</Longitude>\n";
            xml << "        <JD>" << formatDouble(pt.jd, 8) << "</JD>\n";
            xml << "        <DateTime>" << pt.dateTime << "</DateTime>\n";
            xml << "        <StarAltitude>" << formatDouble(pt.altitude, 2) << "</StarAltitude>\n";
            xml << "        <SunAltitude>" << formatDouble(pt.sunAltitude, 2) << "</SunAltitude>\n";
            xml << "      </Point>\n";
        }
        xml << "    </CenterLine>\n";
        
        if (options_.includeUncertainty) {
            if (!event.northLimit.empty()) {
                xml << "    <NorthLimit>\n";
                for (const auto& pt : event.northLimit) {
                    xml << "      <Point>\n";
                    xml << "        <Latitude>" << formatDouble(pt.latitude, 6) << "</Latitude>\n";
                    xml << "        <Longitude>" << formatDouble(pt.longitude, 6) << "</Longitude>\n";
                    xml << "      </Point>\n";
                }
                xml << "    </NorthLimit>\n";
            }
            
            if (!event.southLimit.empty()) {
                xml << "    <SouthLimit>\n";
                for (const auto& pt : event.southLimit) {
                    xml << "      <Point>\n";
                    xml << "        <Latitude>" << formatDouble(pt.latitude, 6) << "</Latitude>\n";
                    xml << "        <Longitude>" << formatDouble(pt.longitude, 6) << "</Longitude>\n";
                    xml << "      </Point>\n";
                }
                xml << "    </SouthLimit>\n";
            }
        }
    }
    
    xml << "  </Event>\n\n";
    
    return xml.str();
}

// ... 

Occult4XMLHandler::Occult4Event 
Occult4XMLHandler::toOccult4Event(const OccultationEvent& event) {
    Occult4Event o4;
    
    // Asteroid
    o4.asteroidName = event.asteroid.name;
    o4.asteroidDesignation = event.asteroid.designation;
    
    // Extract number if present (e.g., "(433) Eros" -> "433")
    if (!o4.asteroidDesignation.empty() && o4.asteroidDesignation[0] == '(') {
        size_t end = o4.asteroidDesignation.find(')');
        if (end != std::string::npos) {
            o4.asteroidNumber = o4.asteroidDesignation.substr(1, end - 1);
        }
    }
    
    // Star
    o4.starCatalog = options_.useGaiaIds ? "Gaia DR3" : "UCAC4";
    o4.starId = event.star.sourceId;
    o4.starRA = event.star.pos.ra * RAD_TO_DEG;
    o4.starDec = event.star.pos.dec * RAD_TO_DEG;
    o4.starMag = event.star.phot_g_mean_mag;
    
    // Time
    o4.jdEvent = event.timeCA.jd;
    o4.dateTimeUTC = TimeUtils::jdToISO(event.timeCA);
    
    // Geometry
    o4.closeApproachDist = event.closeApproachDistance;
    o4.posAngle = event.positionAngle;
    o4.maxDuration = event.maxDuration;
    o4.probability = event.probability;
    o4.uncertainty = (event.uncertaintyNorth + event.uncertaintySouth) / 2.0;
    
    // Path width
    o4.pathWidth = event.pathWidth;
    o4.dropMag = event.magnitudeDrop;
    
    // Event ID
    o4.eventId = event.eventId;
    
    // Convert shadow path points
    o4.centerLine = generatePathPoints(event.shadowPath);
    
    return o4;
}

std::vector<Occult4XMLHandler::Occult4Event::PathPoint>
Occult4XMLHandler::generatePathPoints(const std::vector<ShadowPathPoint>& ioPoints) {
    std::vector<Occult4Event::PathPoint> points;
    
    for (const auto& ioPt : ioPoints) {
        Occult4Event::PathPoint pt;
        pt.latitude = ioPt.location.latitude * RAD_TO_DEG;
        pt.longitude = ioPt.location.longitude * RAD_TO_DEG;
        pt.jd = ioPt.time.jd;
        pt.dateTime = TimeUtils::jdToISO(ioPt.time);
        pt.altitude = 0.0; // non disponibile in ShadowPathPoint
        pt.sunAltitude = 0.0; // non disponibile in ShadowPathPoint
        points.push_back(pt);
    }
    
    return points;
}

// ============================================================================
// CONFIGURATION
// ============================================================================

void Occult4XMLHandler::setOptions(const XMLOptions& options) {
    options_ = options;
}

Occult4XMLHandler::XMLOptions Occult4XMLHandler::getOptions() const {
    return options_;
}

bool Occult4XMLHandler::validateXML(const std::string& filename) {
    try {
        loadFromXML(filename);
        return true;
    } catch (...) {
        return false;
    }
}

std::string Occult4XMLHandler::detectXMLVersion(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        return "";
    }
    
    std::string line;
    while (std::getline(file, line)) {
        size_t pos = line.find("version=");
        if (pos != std::string::npos) {
            size_t start = line.find('"', pos);
            size_t end = line.find('"', start + 1);
            if (start != std::string::npos && end != std::string::npos) {
                return line.substr(start + 1, end - start - 1);
            }
        }
    }
    
    return "unknown";
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

std::string Occult4XMLHandler::extractTextContent(void* nodePtr) {
    xmlNode* node = (xmlNode*)nodePtr;
    xmlChar* content = xmlNodeGetContent(node);
    if (content == nullptr) {
        return "";
    }
    std::string result = (const char*)content;
    xmlFree(content);
    return result;
}

double Occult4XMLHandler::extractDoubleContent(void* nodePtr, double defaultValue) {
    std::string content = extractTextContent(nodePtr);
    if (content.empty()) {
        return defaultValue;
    }
    try {
        return std::stod(content);
    } catch (...) {
        return defaultValue;
    }
}

std::string Occult4XMLHandler::escapeXML(const std::string& text) {
    std::string result;
    for (char c : text) {
        switch (c) {
            case '<':  result += "&lt;"; break;
            case '>':  result += "&gt;"; break;
            case '&':  result += "&amp;"; break;
            case '\'': result += "&apos;"; break;
            case '"':  result += "&quot;"; break;
            default:   result += c; break;
        }
    }
    return result;
}

std::string Occult4XMLHandler::formatDouble(double value, int precision) {
    std::ostringstream ss;
    ss << std::fixed << std::setprecision(precision) << value;
    return ss.str();
}

std::string Occult4XMLHandler::formatRA(double raDeg) {
    double raHours = raDeg / 15.0;
    int h = (int)raHours;
    double mFrac = (raHours - h) * 60.0;
    int m = (int)mFrac;
    double s = (mFrac - m) * 60.0;
    
    std::ostringstream ss;
    ss << std::setfill('0') << std::setw(2) << h << ":"
       << std::setw(2) << m << ":"
       << std::fixed << std::setprecision(3) << std::setw(6) << s;
    return ss.str();
}

std::string Occult4XMLHandler::formatDec(double decDeg) {
    char sign = decDeg >= 0 ? '+' : '-';
    decDeg = std::abs(decDeg);
    int d = (int)decDeg;
    double mFrac = (decDeg - d) * 60.0;
    int m = (int)mFrac;
    double s = (mFrac - m) * 60.0;
    
    std::ostringstream ss;
    ss << sign << std::setfill('0') << std::setw(2) << d << ":"
       << std::setw(2) << m << ":"
       << std::fixed << std::setprecision(2) << std::setw(5) << s;
    return ss.str();
}

std::string Occult4XMLHandler::formatDateTime(double jd) {
    JulianDate julianDate;
    julianDate.jd = jd;
    return TimeUtils::jdToISO(julianDate);
}

} // namespace ioccultcalc
