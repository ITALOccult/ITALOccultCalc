/**
 * @file preston_parser.cpp
 * @brief Implementazione del parser per previsioni Preston
 * 
 * @author Michele Bigi
 * @date 2025
 */

#include "ioccultcalc/preston_parser.h"
#include <fstream>
#include <sstream>
#include <regex>
#include <cmath>
#include <algorithm>

namespace ioccultcalc {

// ========== PrestonParser ==========

PrestonParser::PrestonParser() {}

PrestonParser::~PrestonParser() {}

PrestonPrediction PrestonParser::parseFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        errors_.push_back("Cannot open file: " + filename);
        return PrestonPrediction();
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    file.close();
    
    PrestonPrediction pred = parseString(buffer.str());
    pred.sourceFile = filename;
    return pred;
}

PrestonPrediction PrestonParser::parseString(const std::string& content) {
    errors_.clear();
    
    PrestonPrediction pred;
    pred.rawText = content;
    
    try {
        parseHeader(content, pred);
        parseEventInfo(content, pred);
        parseStarInfo(content, pred);
        parsePathInfo(content, pred);
        parseUncertainty(content, pred);
    } catch (const std::exception& e) {
        errors_.push_back(std::string("Parse error: ") + e.what());
    }
    
    return pred;
}

PrestonPrediction PrestonParser::parseURL(const std::string& url) {
    // TODO: Implementare con libcurl
    errors_.push_back("URL parsing not yet implemented (requires libcurl)");
    return PrestonPrediction();
}

void PrestonParser::parseHeader(const std::string& text, PrestonPrediction& pred) {
    // Pattern: (number) Name occults CATALOG starId
    std::regex headerPattern(R"(\((\d+)\)\s+([A-Za-z0-9\s]+)\s+occults\s+([A-Za-z0-9\-]+)\s+([A-Za-z0-9\-]+))");
    std::smatch match;
    
    if (std::regex_search(text, match, headerPattern)) {
        pred.asteroidNumber = std::stoi(match[1]);
        pred.asteroidName = trim(match[2]);
        pred.starCatalog = match[3];
        pred.starId = match[4];
    }
}

void PrestonParser::parseEventInfo(const std::string& text, PrestonPrediction& pred) {
    // Pattern data/ora evento: YYYY MMM DD HH:MM:SS UT
    std::regex datePattern(R"((\d{4})\s+([A-Za-z]{3})\s+(\d{1,2})\s+(\d{2}):(\d{2}):(\d{2})\s+UT)");
    std::smatch match;
    
    if (std::regex_search(text, match, datePattern)) {
        pred.eventTimeString = match[0];
        pred.eventTime = parseDateTime(pred.eventTimeString);
    }
    
    // Path width
    std::regex widthPattern(R"(Path width:\s*([\d.]+)\s*km)");
    if (std::regex_search(text, match, widthPattern)) {
        pred.pathWidth = std::stod(match[1]);
    }
    
    // Duration
    std::regex durationPattern(R"(Duration:\s*([\d.]+)\s*sec)");
    if (std::regex_search(text, match, durationPattern)) {
        pred.maxDuration = std::stod(match[1]);
    }
    
    // Close approach
    std::regex caPattern(R"(C/A:\s*([\d.]+)\s*\")");
    if (std::regex_search(text, match, caPattern)) {
        pred.closeApproachDist = std::stod(match[1]);
    }
    
    // Position angle
    std::regex paPattern(R"(PA:\s*([\d.]+)\s*°)");
    if (std::regex_search(text, match, paPattern)) {
        pred.positionAngle = std::stod(match[1]);
    }
    
    // Velocity
    std::regex velPattern(R"(Vel:\s*([\d.]+)\s*km/s)");
    if (std::regex_search(text, match, velPattern)) {
        pred.shadowVelocity = std::stod(match[1]);
    }
}

void PrestonParser::parseStarInfo(const std::string& text, PrestonPrediction& pred) {
    // Pattern RA: HH MM SS.sss
    std::regex raPattern(R"(RA\s+(\d{1,2})\s+(\d{2})\s+([\d.]+))");
    std::smatch match;
    
    if (std::regex_search(text, match, raPattern)) {
        double hours = std::stod(match[1]);
        double minutes = std::stod(match[2]);
        double seconds = std::stod(match[3]);
        pred.starRA = hours + minutes/60.0 + seconds/3600.0;
    }
    
    // Pattern Dec: +DD MM SS.ss
    std::regex decPattern(R"(Dec\s+([+-])(\d{1,2})\s+(\d{2})\s+([\d.]+))");
    if (std::regex_search(text, match, decPattern)) {
        double degrees = std::stod(match[2]);
        double arcminutes = std::stod(match[3]);
        double arcseconds = std::stod(match[4]);
        pred.starDec = degrees + arcminutes/60.0 + arcseconds/3600.0;
        if (match[1] == "-") {
            pred.starDec = -pred.starDec;
        }
    }
    
    // Magnitudine
    std::regex magPattern(R"(Mag\s+([\d.]+))");
    if (std::regex_search(text, match, magPattern)) {
        pred.starMagnitude = std::stod(match[1]);
    }
}

void PrestonParser::parsePathInfo(const std::string& text, PrestonPrediction& pred) {
    // Pattern coordinate: DD°MM'SS"N/S  DDD°MM'SS"E/W or DD MM SS N/S DDD MM SS E/W
    // Supporta sia formato con simboli che con spazi
    std::regex coordPattern(R"((\d{1,2})[°\s]+(\d{2})['\s]+(\d{2})["\s]+([NS])\s+(\d{1,3})[°\s]+(\d{2})['\s]+(\d{2})["\s]+([EW]))");
    
    auto lines = splitLines(text);
    bool inPathSection = false;
    
    for (const auto& line : lines) {
        if (line.find("Center Line") != std::string::npos || 
            line.find("Central Line") != std::string::npos) {
            inPathSection = true;
            continue;
        }
        
        if (inPathSection) {
            std::smatch match;
            if (std::regex_search(line, match, coordPattern)) {
                PrestonPrediction::PathPoint pt;
                
                // Latitudine
                double latDeg = std::stod(match[1]);
                double latMin = std::stod(match[2]);
                double latSec = std::stod(match[3]);
                pt.latitude = latDeg + latMin/60.0 + latSec/3600.0;
                if (match[4] == "S") pt.latitude = -pt.latitude;
                
                // Longitudine
                double lonDeg = std::stod(match[5]);
                double lonMin = std::stod(match[6]);
                double lonSec = std::stod(match[7]);
                pt.longitude = lonDeg + lonMin/60.0 + lonSec/3600.0;
                if (match[8] == "W") pt.longitude = -pt.longitude;
                
                // Nome località (se presente)
                size_t coordEnd = line.find(match[8]) + 1;
                if (coordEnd < line.length()) {
                    pt.locationName = trim(line.substr(coordEnd));
                }
                
                pred.centerLinePath.push_back(pt);
            }
        }
        
        // Fine sezione path
        if (inPathSection && (line.find("North Limit") != std::string::npos || 
                              line.find("South Limit") != std::string::npos ||
                              line.find("Uncertainty") != std::string::npos)) {
            inPathSection = false;
        }
    }
}

void PrestonParser::parseUncertainty(const std::string& text, PrestonPrediction& pred) {
    // Pattern incertezza cross-track
    std::regex crossPattern(R"(cross[- ]track[:\s]*([\d.]+)\s*km)", std::regex::icase);
    std::smatch match;
    
    if (std::regex_search(text, match, crossPattern)) {
        pred.uncertaintyCrossTrack = std::stod(match[1]);
    }
    
    // Pattern incertezza along-track
    std::regex alongPattern(R"(along[- ]track[:\s]*([\d.]+)\s*km)", std::regex::icase);
    if (std::regex_search(text, match, alongPattern)) {
        pred.uncertaintyAlongTrack = std::stod(match[1]);
    }
}

double PrestonParser::parseRA(const std::string& raString) {
    // Formato: HH MM SS.sss o HH:MM:SS.sss
    std::regex raPattern(R"((\d{1,2})[\s:](\d{2})[\s:]([\d.]+))");
    std::smatch match;
    
    if (std::regex_search(raString, match, raPattern)) {
        double hours = std::stod(match[1]);
        double minutes = std::stod(match[2]);
        double seconds = std::stod(match[3]);
        return hours + minutes/60.0 + seconds/3600.0;
    }
    
    return 0.0;
}

double PrestonParser::parseDec(const std::string& decString) {
    // Formato: +DD MM SS.ss o +DD:MM:SS.ss
    std::regex decPattern(R"(([+-]?)(\d{1,2})[\s:](\d{2})[\s:]([\d.]+))");
    std::smatch match;
    
    if (std::regex_search(decString, match, decPattern)) {
        double degrees = std::stod(match[2]);
        double arcminutes = std::stod(match[3]);
        double arcseconds = std::stod(match[4]);
        double dec = degrees + arcminutes/60.0 + arcseconds/3600.0;
        if (match[1] == "-") dec = -dec;
        return dec;
    }
    
    return 0.0;
}

JulianDate PrestonParser::parseDateTime(const std::string& dateTimeString) {
    // Formato: YYYY MMM DD HH:MM:SS
    std::regex dtPattern(R"((\d{4})\s+([A-Za-z]{3})\s+(\d{1,2})\s+(\d{2}):(\d{2}):(\d{2}))");
    std::smatch match;
    
    JulianDate jd;
    jd.jd = 0.0;
    
    if (std::regex_search(dateTimeString, match, dtPattern)) {
        int year = std::stoi(match[1]);
        std::string monthStr = match[2];
        int day = std::stoi(match[3]);
        int hour = std::stoi(match[4]);
        int minute = std::stoi(match[5]);
        int second = std::stoi(match[6]);
        
        // Converti mese
        int month = 1;
        if (monthStr == "Jan") month = 1;
        else if (monthStr == "Feb") month = 2;
        else if (monthStr == "Mar") month = 3;
        else if (monthStr == "Apr") month = 4;
        else if (monthStr == "May") month = 5;
        else if (monthStr == "Jun") month = 6;
        else if (monthStr == "Jul") month = 7;
        else if (monthStr == "Aug") month = 8;
        else if (monthStr == "Sep") month = 9;
        else if (monthStr == "Oct") month = 10;
        else if (monthStr == "Nov") month = 11;
        else if (monthStr == "Dec") month = 12;
        
        // Calcolo JD semplificato
        // Formula: JD = 367*Y - INT(7*(Y + INT((M+9)/12))/4) + INT(275*M/9) + D + 1721013.5 + UT/24
        double UT = hour + minute/60.0 + second/3600.0;
        jd.jd = 367.0*year - std::floor(7.0*(year + std::floor((month+9.0)/12.0))/4.0) + 
                std::floor(275.0*month/9.0) + day + 1721013.5 + UT/24.0;
    }
    
    return jd;
}

std::vector<std::string> PrestonParser::splitLines(const std::string& text) {
    std::vector<std::string> lines;
    std::istringstream stream(text);
    std::string line;
    
    while (std::getline(stream, line)) {
        lines.push_back(line);
    }
    
    return lines;
}

std::string PrestonParser::trim(const std::string& str) {
    size_t first = str.find_first_not_of(" \t\n\r");
    if (first == std::string::npos) return "";
    size_t last = str.find_last_not_of(" \t\n\r");
    return str.substr(first, last - first + 1);
}

PredictionData PrestonParser::toIOccultCalcData(const PrestonPrediction& preston) {
    PredictionData data;
    
    // Asteroide
    data.asteroidNumber = preston.asteroidNumber;
    data.asteroidName = preston.asteroidName;
    data.asteroidDiameter = preston.pathWidth; // Approssimazione
    data.asteroidUncertainty = preston.uncertaintyCrossTrack;
    
    // Stella
    data.starCatalog = preston.starCatalog;
    data.starId = preston.starId;
    data.star.pos.ra = preston.starRA;
    data.star.pos.dec = preston.starDec;
    data.starMagnitude = preston.starMagnitude;
    
    // Evento
    data.eventTime = preston.eventTime;
    data.closeApproachDistance = preston.closeApproachDist;
    data.positionAngle = preston.positionAngle;
    data.shadowVelocity = preston.shadowVelocity;
    data.pathWidth = preston.pathWidth;
    data.maxDuration = preston.maxDuration;
    
    // Path centrale
    for (const auto& pt : preston.centerLinePath) {
        ShadowPathPoint ioPoint;
        ioPoint.location.latitude = pt.latitude;
        ioPoint.location.longitude = pt.longitude;
        ioPoint.time = preston.eventTime; // Approssimazione
        data.centerLine.push_back(ioPoint);
    }
    
    data.ephemerisSource = preston.ephemerisSource;
    
    return data;
}

bool PrestonParser::validate(const PrestonPrediction& preston) {
    if (preston.asteroidNumber <= 0) return false;
    if (preston.asteroidName.empty()) return false;
    if (preston.eventTime.jd == 0.0) return false;
    if (preston.starRA < 0 || preston.starRA >= 24) return false;
    if (preston.starDec < -90 || preston.starDec > 90) return false;
    return true;
}

// ========== PredictionComparator ==========

PredictionComparator::PredictionComparator() {}

PredictionComparator::~PredictionComparator() {}

PredictionComparison PredictionComparator::compare(const PredictionData& ioccult,
                                                   const PrestonPrediction& preston) {
    PredictionComparison comp;
    
    // Differenze temporali (in secondi)
    comp.timeDifference = (ioccult.eventTime.jd - preston.eventTime.jd) * 86400.0;
    
    // Differenze geometriche
    comp.pathWidthDifference = ioccult.pathWidth - preston.pathWidth;
    comp.durationDifference = ioccult.maxDuration - preston.maxDuration;
    comp.closeApproachDiff = ioccult.closeApproachDistance - preston.closeApproachDist;
    comp.positionAngleDiff = ioccult.positionAngle - preston.positionAngle;
    
    // Normalizza differenza PA a [-180, 180]
    while (comp.positionAngleDiff > 180) comp.positionAngleDiff -= 360;
    while (comp.positionAngleDiff < -180) comp.positionAngleDiff += 360;
    
    // Differenze coordinate stella (in arcsec)
    comp.starRADiff = (ioccult.star.pos.ra - preston.starRA) * 3600.0 * 15.0 * 
                      std::cos(ioccult.star.pos.dec * M_PI / 180.0);
    comp.starDecDiff = (ioccult.star.pos.dec - preston.starDec) * 3600.0;
    
    // Differenze path
    comp.pathRMSError = calculatePathRMS(ioccult.centerLine, preston.centerLinePath);
    comp.maxPathError = 0.0;
    
    // Calcolo overall agreement (0-1)
    double timeScore = 1.0 - std::min(1.0, std::abs(comp.timeDifference) / 60.0);
    double pathScore = 1.0 - std::min(1.0, comp.pathRMSError / 100.0);
    double starScore = 1.0 - std::min(1.0, std::sqrt(comp.starRADiff*comp.starRADiff + 
                                                     comp.starDecDiff*comp.starDecDiff) / 1.0);
    
    comp.overallAgreement = (timeScore + pathScore + starScore) / 3.0;
    comp.assessment = assessAgreement(comp.overallAgreement);
    
    // Note
    if (std::abs(comp.timeDifference) > 10.0) {
        comp.notes.push_back("Significant time difference (> 10 seconds)");
    }
    if (comp.pathRMSError > 50.0) {
        comp.notes.push_back("Large path deviation (> 50 km)");
    }
    if (std::abs(comp.pathWidthDifference) > ioccult.pathWidth * 0.2) {
        comp.notes.push_back("Path width differs by > 20%");
    }
    
    return comp;
}

std::string PredictionComparator::generateComparisonReport(const PredictionComparison& comparison,
                                                          bool includeDetails) {
    std::ostringstream report;
    
    report << std::string(80, '=') << "\n";
    report << "                     PREDICTION COMPARISON REPORT\n";
    report << std::string(80, '=') << "\n\n";
    
    report << "Overall Agreement: " << std::fixed << std::setprecision(1) 
           << (comparison.overallAgreement * 100) << "% - " 
           << comparison.assessment << "\n\n";
    
    if (includeDetails) {
        report << std::string(80, '-') << "\n";
        report << "DETAILED DIFFERENCES (IOccultCalc - Preston)\n";
        report << std::string(80, '-') << "\n\n";
        
        report << "Time:              " << std::showpos << std::fixed << std::setprecision(2)
               << comparison.timeDifference << " seconds\n";
        
        report << "Path Width:        " << std::showpos << std::fixed << std::setprecision(1)
               << comparison.pathWidthDifference << " km\n";
        
        report << "Duration:          " << std::showpos << std::fixed << std::setprecision(2)
               << comparison.durationDifference << " seconds\n";
        
        report << "Close Approach:    " << std::showpos << std::fixed << std::setprecision(3)
               << comparison.closeApproachDiff << " arcsec\n";
        
        report << "Position Angle:    " << std::showpos << std::fixed << std::setprecision(1)
               << comparison.positionAngleDiff << "°\n";
        
        report << "\nStar Coordinates:\n";
        report << "  RA difference:   " << std::showpos << std::fixed << std::setprecision(3)
               << comparison.starRADiff << " arcsec\n";
        report << "  Dec difference:  " << std::showpos << std::fixed << std::setprecision(3)
               << comparison.starDecDiff << " arcsec\n";
        
        report << "\nPath Deviation:\n";
        report << "  RMS error:       " << std::noshowpos << std::fixed << std::setprecision(1)
               << comparison.pathRMSError << " km\n";
        report << "  Max error:       " << std::fixed << std::setprecision(1)
               << comparison.maxPathError << " km\n";
    }
    
    if (!comparison.notes.empty()) {
        report << "\n" << std::string(80, '-') << "\n";
        report << "NOTES:\n";
        for (const auto& note : comparison.notes) {
            report << "  • " << note << "\n";
        }
    }
    
    report << "\n" << std::string(80, '=') << "\n";
    
    return report.str();
}

std::string PredictionComparator::generateComparisonTable(const PredictionData& ioccult,
                                                         const PrestonPrediction& preston,
                                                         const PredictionComparison& comparison) {
    std::ostringstream table;
    
    table << std::string(80, '=') << "\n";
    table << "                      PREDICTION COMPARISON TABLE\n";
    table << std::string(80, '=') << "\n\n";
    
    table << std::left << std::setw(30) << "Parameter" 
          << std::setw(20) << "IOccultCalc" 
          << std::setw(20) << "Preston"
          << std::setw(10) << "Difference" << "\n";
    table << std::string(80, '-') << "\n";
    
    // Event time
    table << std::left << std::setw(30) << "Event Time (JD)";
    table << std::fixed << std::setprecision(6) << std::setw(20) << ioccult.eventTime.jd;
    table << std::fixed << std::setprecision(6) << std::setw(20) << preston.eventTime.jd;
    table << std::showpos << std::fixed << std::setprecision(2) << comparison.timeDifference << " s\n";
    
    // Path width
    table << std::left << std::setw(30) << "Path Width (km)";
    table << std::noshowpos << std::fixed << std::setprecision(1) << std::setw(20) << ioccult.pathWidth;
    table << std::fixed << std::setprecision(1) << std::setw(20) << preston.pathWidth;
    table << std::showpos << std::fixed << std::setprecision(1) << comparison.pathWidthDifference << "\n";
    
    // Duration
    table << std::left << std::setw(30) << "Max Duration (s)";
    table << std::noshowpos << std::fixed << std::setprecision(1) << std::setw(20) << ioccult.maxDuration;
    table << std::fixed << std::setprecision(1) << std::setw(20) << preston.maxDuration;
    table << std::showpos << std::fixed << std::setprecision(2) << comparison.durationDifference << "\n";
    
    // Close approach
    table << std::left << std::setw(30) << "Close Approach (arcsec)";
    table << std::noshowpos << std::fixed << std::setprecision(3) << std::setw(20) << ioccult.closeApproachDistance;
    table << std::fixed << std::setprecision(3) << std::setw(20) << preston.closeApproachDist;
    table << std::showpos << std::fixed << std::setprecision(3) << comparison.closeApproachDiff << "\n";
    
    // Position angle
    table << std::left << std::setw(30) << "Position Angle (°)";
    table << std::noshowpos << std::fixed << std::setprecision(1) << std::setw(20) << ioccult.positionAngle;
    table << std::fixed << std::setprecision(1) << std::setw(20) << preston.positionAngle;
    table << std::showpos << std::fixed << std::setprecision(1) << comparison.positionAngleDiff << "\n";
    
    table << std::string(80, '=') << "\n";
    
    return table.str();
}

double PredictionComparator::calculatePathRMS(const std::vector<ShadowPathPoint>& path1,
                                             const std::vector<PrestonPrediction::PathPoint>& path2) {
    if (path1.empty() || path2.empty()) return 0.0;
    
    // Calcola RMS delle distanze tra punti corrispondenti
    double sumSquares = 0.0;
    size_t minSize = std::min(path1.size(), path2.size());
    
    for (size_t i = 0; i < minSize; ++i) {
        double lat1 = path1[i].location.latitude * M_PI / 180.0;
        double lon1 = path1[i].location.longitude * M_PI / 180.0;
        double lat2 = path2[i].latitude * M_PI / 180.0;
        double lon2 = path2[i].longitude * M_PI / 180.0;
        
        // Haversine formula
        double dlat = lat2 - lat1;
        double dlon = lon2 - lon1;
        double a = std::sin(dlat/2)*std::sin(dlat/2) + 
                   std::cos(lat1)*std::cos(lat2)*std::sin(dlon/2)*std::sin(dlon/2);
        double c = 2 * std::atan2(std::sqrt(a), std::sqrt(1-a));
        double distance = 6371.0 * c; // km
        
        sumSquares += distance * distance;
    }
    
    return std::sqrt(sumSquares / minSize);
}

std::string PredictionComparator::assessAgreement(double agreementScore) {
    if (agreementScore >= 0.95) return "Excellent";
    if (agreementScore >= 0.85) return "Very Good";
    if (agreementScore >= 0.70) return "Good";
    if (agreementScore >= 0.50) return "Fair";
    return "Poor";
}

} // namespace ioccultcalc
