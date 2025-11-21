/**
 * @file prediction_report.cpp
 * @brief Implementazione del generatore di schede di previsione
 * 
 * @author Michele Bigi
 * @date 2025
 */

#include "ioccultcalc/prediction_report.h"
#include <sstream>
#include <iomanip>
#include <fstream>
#include <cmath>
#include <algorithm>

namespace ioccultcalc {

PredictionReportGenerator::PredictionReportGenerator() {}

PredictionReportGenerator::~PredictionReportGenerator() {}

std::string PredictionReportGenerator::generateIOTAReport(const PredictionData& data, 
                                                          const ReportOptions& options) {
    std::ostringstream report;
    
    // Header
    report << generateHeader(data, options);
    report << "\n";
    
    // Sezione evento
    report << generateEventSection(data, options);
    report << "\n";
    
    // Sezione stella
    report << generateStarSection(data, options);
    report << "\n";
    
    // Sezione path
    if (options.includePathCoordinates) {
        report << generatePathSection(data, options);
        report << "\n";
    }
    
    // Sezione incertezza
    if (options.includeUncertainty) {
        report << generateUncertaintySection(data, options);
        report << "\n";
    }
    
    // Footer
    report << std::string(80, '=') << "\n";
    report << "Calculated by IOccultCalc using " << data.ephemerisSource << "\n";
    report << "Calculation date: " << data.calculationDate << "\n";
    if (!data.observerInfo.empty()) {
        report << "Observer: " << data.observerInfo << "\n";
    }
    report << std::string(80, '=') << "\n";
    
    return report.str();
}

std::string PredictionReportGenerator::generatePrestonReport(const PredictionData& data) {
    std::ostringstream report;
    
    // Formato compatto stile Preston
    report << "(" << data.asteroidNumber << ") " << data.asteroidName << "  ";
    report << "occults " << data.starCatalog << " " << data.starId << "  ";
    report << formatDateTime(data.eventTime, "UTC") << " UT\n\n";
    
    // Coordinate stella
    report << "Star: RA " << formatRA(data.star.pos.ra) << "  ";
    report << "Dec " << formatDec(data.star.pos.dec) << "  ";
    report << "Mag " << std::fixed << std::setprecision(1) << data.starMagnitude << "\n";
    
    // Geometria
    report << "C/A: " << std::fixed << std::setprecision(3) << data.closeApproachDistance << "\"  ";
    report << "PA: " << std::fixed << std::setprecision(1) << data.positionAngle << "°  ";
    report << "Vel: " << std::fixed << std::setprecision(1) << data.shadowVelocity << " km/s\n";
    
    report << "Path width: " << std::fixed << std::setprecision(1) << data.pathWidth << " km  ";
    report << "Duration: " << std::fixed << std::setprecision(1) << data.maxDuration << " sec  ";
    report << "Prob: " << std::fixed << std::setprecision(0) << (data.probability * 100) << "%\n";
    
    // Path centrale (sample di 5 punti)
    if (!data.centerLine.empty()) {
        report << "\nCenter Line:\n";
        int step = std::max<int>(1, static_cast<int>(data.centerLine.size()) / 5);
        for (size_t i = 0; i < data.centerLine.size(); i += step) {
            const auto& pt = data.centerLine[i];
            report << "  " << formatCoordinate(pt.location.latitude, pt.location.longitude, true) << "\n";
        }
    }
    
    return report.str();
}

std::string PredictionReportGenerator::generateHeader(const PredictionData& data, 
                                                     const ReportOptions& options) {
    std::ostringstream header;
    
    header << std::string(80, '=') << "\n";
    
    std::string title = "ASTEROID OCCULTATION PREDICTION";
    if (options.language == "it") {
        title = "PREVISIONE OCCULTAZIONE ASTEROIDALE";
    }
    header << centerText(title, 80) << "\n";
    header << std::string(80, '=') << "\n\n";
    
    // Titolo evento
    std::ostringstream eventTitle;
    eventTitle << "(" << data.asteroidNumber << ") " << data.asteroidName 
               << " occults " << data.starCatalog << " " << data.starId;
    header << centerText(eventTitle.str(), 80) << "\n";
    header << centerText(formatDateTime(data.eventTime, options.timezone), 80) << "\n";
    
    return header.str();
}

std::string PredictionReportGenerator::generateEventSection(const PredictionData& data,
                                                           const ReportOptions& options) {
    std::ostringstream section;
    
    section << std::string(80, '-') << "\n";
    section << "EVENT DETAILS\n";
    if (options.language == "it") {
        section.str("");
        section << std::string(80, '-') << "\n";
        section << "DETTAGLI EVENTO\n";
    }
    section << std::string(80, '-') << "\n";
    
    // Data e ora
    section << padRight("Event time (" + options.timezone + "):", 35);
    section << formatDateTime(data.eventTime, options.timezone) << "\n";
    
    section << padRight("Julian Date:", 35);
    section << std::fixed << std::setprecision(6) << data.eventTime.jd << "\n";
    
    // Asteroide
    section << "\n";
    section << padRight("Asteroid:", 35) << "(" << data.asteroidNumber << ") " 
            << data.asteroidName << "\n";
    section << padRight("Estimated diameter:", 35);
    section << std::fixed << std::setprecision(1) << data.asteroidDiameter << " km\n";
    
    // Geometria
    section << "\n";
    section << padRight("Close approach distance:", 35);
    section << std::fixed << std::setprecision(3) << data.closeApproachDistance << " arcsec\n";
    
    section << padRight("Position angle:", 35);
    section << std::fixed << std::setprecision(1) << data.positionAngle << "° (from N to E)\n";
    
    section << padRight("Shadow velocity:", 35);
    section << std::fixed << std::setprecision(2) << data.shadowVelocity << " km/s\n";
    
    section << padRight("Path width:", 35);
    section << std::fixed << std::setprecision(1) << data.pathWidth << " km\n";
    
    section << padRight("Maximum duration:", 35);
    section << std::fixed << std::setprecision(1) << data.maxDuration << " seconds\n";
    
    section << padRight("Probability:", 35);
    section << std::fixed << std::setprecision(0) << (data.probability * 100) << "%\n";
    
    return section.str();
}

std::string PredictionReportGenerator::generateStarSection(const PredictionData& data,
                                                          const ReportOptions& options) {
    std::ostringstream section;
    
    section << std::string(80, '-') << "\n";
    section << "STAR DATA\n";
    if (options.language == "it") {
        section.str("");
        section << std::string(80, '-') << "\n";
        section << "DATI STELLA\n";
    }
    section << std::string(80, '-') << "\n";
    
    section << padRight("Catalog:", 35) << data.starCatalog << " " << data.starId << "\n";
    
    section << padRight("Right Ascension (J2000):", 35);
    section << formatRA(data.star.pos.ra) << "\n";
    
    section << padRight("Declination (J2000):", 35);
    section << formatDec(data.star.pos.dec) << "\n";
    
    section << padRight("Magnitude:", 35);
    section << std::fixed << std::setprecision(2) << data.starMagnitude;
    if (data.starCatalog.find("Gaia") != std::string::npos) {
        section << " (Gaia G)";
    }
    section << "\n";
    
    // Proper motion se disponibile
    if (std::abs(data.star.pmra) > 0.001 || std::abs(data.star.pmdec) > 0.001) {
        section << padRight("Proper motion (RA):", 35);
        section << std::fixed << std::setprecision(2) << data.star.pmra << " mas/yr\n";
        
        section << padRight("Proper motion (Dec):", 35);
        section << std::fixed << std::setprecision(2) << data.star.pmdec << " mas/yr\n";
    }
    
    // Parallasse se disponibile
    if (data.star.parallax > 0.001) {
        section << padRight("Parallax:", 35);
        section << std::fixed << std::setprecision(2) << data.star.parallax << " mas\n";
    }
    
    return section.str();
}

std::string PredictionReportGenerator::generatePathSection(const PredictionData& data,
                                                          const ReportOptions& options) {
    std::ostringstream section;
    
    section << std::string(80, '-') << "\n";
    section << "SHADOW PATH\n";
    if (options.language == "it") {
        section.str("");
        section << std::string(80, '-') << "\n";
        section << "PERCORSO OMBRA\n";
    }
    section << std::string(80, '-') << "\n";
    
    // Center line
    section << "\nCenter Line:\n";
    section << "  Latitude    Longitude   Time (UTC)        Duration\n";
    section << "  " << std::string(70, '-') << "\n";
    
    int step = std::max<int>(1, static_cast<int>(data.centerLine.size()) / options.pathPointsCount);
    for (size_t i = 0; i < data.centerLine.size(); i += step) {
        const auto& pt = data.centerLine[i];
        section << "  " << formatCoordinate(pt.location.latitude, pt.location.longitude, true);
        section << "  " << formatDateTime(pt.time, "UTC");
        section << "  " << std::fixed << std::setprecision(1) << pt.duration << "s\n";
    }
    
    // North limit
    if (!data.northLimit.empty()) {
        section << "\nNorth Limit:\n";
        section << "  Latitude    Longitude\n";
        section << "  " << std::string(30, '-') << "\n";
        
        step = std::max<int>(1, static_cast<int>(data.northLimit.size()) / (options.pathPointsCount / 2));
        for (size_t i = 0; i < data.northLimit.size(); i += step) {
            const auto& pt = data.northLimit[i];
            section << "  " << formatCoordinate(pt.location.latitude, pt.location.longitude, true) << "\n";
        }
    }
    
    // South limit
    if (!data.southLimit.empty()) {
        section << "\nSouth Limit:\n";
        section << "  Latitude    Longitude\n";
        section << "  " << std::string(30, '-') << "\n";
        
        step = std::max<int>(1, static_cast<int>(data.southLimit.size()) / (options.pathPointsCount / 2));
        for (size_t i = 0; i < data.southLimit.size(); i += step) {
            const auto& pt = data.southLimit[i];
            section << "  " << formatCoordinate(pt.location.latitude, pt.location.longitude, true) << "\n";
        }
    }
    
    return section.str();
}

std::string PredictionReportGenerator::generateUncertaintySection(const PredictionData& data,
                                                                 const ReportOptions& options) {
    std::ostringstream section;
    
    section << std::string(80, '-') << "\n";
    section << "UNCERTAINTY ANALYSIS\n";
    if (options.language == "it") {
        section.str("");
        section << std::string(80, '-') << "\n";
        section << "ANALISI INCERTEZZA\n";
    }
    section << std::string(80, '-') << "\n";
    
    section << padRight("Cross-track uncertainty:", 35);
    section << std::fixed << std::setprecision(1) << data.asteroidUncertainty << " km\n";
    
    section << padRight("Uncertainty ellipse (1-sigma):", 35) << "\n";
    section << padRight("  Semi-major axis:", 35);
    section << std::fixed << std::setprecision(3) << data.uncertaintyEllipseA << " arcsec\n";
    
    section << padRight("  Semi-minor axis:", 35);
    section << std::fixed << std::setprecision(3) << data.uncertaintyEllipseB << " arcsec\n";
    
    section << padRight("  Position angle:", 35);
    section << std::fixed << std::setprecision(1) << data.uncertaintyAngle << "°\n";
    
    section << "\n";
    section << "Note: The predicted path may shift by up to the uncertainty amount.\n";
    section << "      Always observe several path widths to the north and south.\n";
    
    return section.str();
}

bool PredictionReportGenerator::saveToFile(const std::string& report, const std::string& filename) {
    std::ofstream file(filename);
    if (!file.is_open()) {
        return false;
    }
    
    file << report;
    file.close();
    return true;
}

std::string PredictionReportGenerator::formatCoordinate(double lat, double lon, bool isDMS) {
    std::ostringstream coord;
    
    if (isDMS) {
        // Formato DMS
        char latDir = (lat >= 0) ? 'N' : 'S';
        char lonDir = (lon >= 0) ? 'E' : 'W';
        lat = std::abs(lat);
        lon = std::abs(lon);
        
        int latDeg = (int)lat;
        int latMin = (int)((lat - latDeg) * 60);
        int latSec = (int)((lat - latDeg - latMin/60.0) * 3600);
        
        int lonDeg = (int)lon;
        int lonMin = (int)((lon - lonDeg) * 60);
        int lonSec = (int)((lon - lonDeg - lonMin/60.0) * 3600);
        
        coord << std::setfill('0');
        coord << std::setw(2) << latDeg << "°" << std::setw(2) << latMin << "'" << std::setw(2) << latSec << "\"" << latDir << "  ";
        coord << std::setw(3) << lonDeg << "°" << std::setw(2) << lonMin << "'" << std::setw(2) << lonSec << "\"" << lonDir;
    } else {
        // Formato decimale
        coord << std::fixed << std::setprecision(4);
        coord << std::setw(8) << lat << "  " << std::setw(9) << lon;
    }
    
    return coord.str();
}

std::string PredictionReportGenerator::formatRA(double ra_hours) {
    int hours = (int)ra_hours;
    int minutes = (int)((ra_hours - hours) * 60);
    double seconds = ((ra_hours - hours) * 60 - minutes) * 60;
    
    std::ostringstream ra;
    ra << std::setfill('0');
    ra << std::setw(2) << hours << "h ";
    ra << std::setw(2) << minutes << "m ";
    ra << std::fixed << std::setprecision(3) << std::setw(6) << seconds << "s";
    
    return ra.str();
}

std::string PredictionReportGenerator::formatDec(double dec_degrees) {
    char sign = (dec_degrees >= 0) ? '+' : '-';
    dec_degrees = std::abs(dec_degrees);
    
    int degrees = (int)dec_degrees;
    int arcminutes = (int)((dec_degrees - degrees) * 60);
    double arcseconds = ((dec_degrees - degrees) * 60 - arcminutes) * 60;
    
    std::ostringstream dec;
    dec << sign;
    dec << std::setfill('0');
    dec << std::setw(2) << degrees << "° ";
    dec << std::setw(2) << arcminutes << "' ";
    dec << std::fixed << std::setprecision(2) << std::setw(5) << arcseconds << "\"";
    
    return dec.str();
}

std::string PredictionReportGenerator::formatDateTime(const JulianDate& jd, const std::string& timezone) {
    // Conversione JD -> ISO datetime
    std::string isoDate = TimeUtils::jdToISO(jd);
    
    // Formato: YYYY-MM-DD HH:MM:SS
    std::ostringstream formatted;
    formatted << isoDate << " " << timezone;
    
    return formatted.str();
}

std::string PredictionReportGenerator::formatDuration(double seconds) {
    std::ostringstream duration;
    duration << std::fixed << std::setprecision(1) << seconds << " sec";
    return duration.str();
}

std::string PredictionReportGenerator::centerText(const std::string& text, int width) {
    int padding = (width - text.length()) / 2;
    return std::string(padding, ' ') + text;
}

std::string PredictionReportGenerator::padRight(const std::string& text, int width) {
    if (text.length() >= width) return text;
    return text + std::string(width - text.length(), ' ');
}

std::string PredictionReportGenerator::padLeft(const std::string& text, int width) {
    if (text.length() >= width) return text;
    return std::string(width - text.length(), ' ') + text;
}

} // namespace ioccultcalc
