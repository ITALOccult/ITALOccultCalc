/**
 * @file prediction_report.h
 * @brief Generatore di schede di previsione in formato IOTA/Occult4 classico
 * 
 * Genera schede di previsione testuale nel formato standard utilizzato
 * da IOTA (International Occultation Timing Association) e Occult4,
 * compatibile con le previsioni di Steve Preston.
 * 
 * @author Michele Bigi
 * @date 2025
 */

#ifndef IOCCULTCALC_PREDICTION_REPORT_H
#define IOCCULTCALC_PREDICTION_REPORT_H

#include "types.h"
#include "time_utils.h"
#include "star_catalog.h"
#include "gaia_client.h"
#include "occultation_predictor.h"
#include <string>
#include <vector>
#include <memory>

namespace ioccultcalc {

/**
 * @brief Dati completi per una scheda di previsione
 */
struct PredictionData {
    // Informazioni asteroide
    int asteroidNumber;
    std::string asteroidName;
    double asteroidDiameter;      // km
    double asteroidUncertainty;   // km (cross-track)
    
    // Informazioni stella
    std::string starCatalog;
    std::string starId;
    GaiaStar star;                // Dati completi stella
    double starMagnitude;         // mag (visuale o Gaia G)
    
    // Geometria evento
    JulianDate eventTime;
    double closeApproachDistance; // arcsec
    double positionAngle;         // gradi (da Nord verso Est)
    double shadowVelocity;        // km/s
    double pathWidth;             // km
    double maxDuration;           // secondi
    double probability;           // 0-1
    
    // Incertezza
    double uncertaintyEllipseA;   // arcsec (semi-asse maggiore)
    double uncertaintyEllipseB;   // arcsec (semi-asse minore)
    double uncertaintyAngle;      // gradi (orientamento ellisse)
    
    // Linea centrale e limiti
    std::vector<ShadowPathPoint> centerLine;
    std::vector<ShadowPathPoint> northLimit;
    std::vector<ShadowPathPoint> southLimit;
    
    // Informazioni di calcolo
    std::string ephemerisSource;  // "JPL DE441", "VSOP87", etc.
    std::string calculationDate;
    std::string observerInfo;
};

/**
 * @brief Opzioni per la generazione della scheda
 */
struct ReportOptions {
    bool includePathCoordinates;  // Include coordinate path
    int pathPointsCount;          // Numero punti path (default: 10)
    bool includeUncertainty;      // Include info incertezza
    bool includeFinder;           // Include carta di ricerca ASCII
    bool includeTimingDetails;    // Include dettagli timing
    std::string timezone;         // "UTC", "Local", etc.
    std::string language;         // "en", "it"
    
    ReportOptions() :
        includePathCoordinates(true),
        pathPointsCount(10),
        includeUncertainty(true),
        includeFinder(false),
        includeTimingDetails(true),
        timezone("UTC"),
        language("en") {}
};

/**
 * @brief Generatore di schede di previsione in formato testo
 */
class PredictionReportGenerator {
public:
    PredictionReportGenerator();
    ~PredictionReportGenerator();
    
    /**
     * @brief Genera scheda completa in formato IOTA/Occult4
     * @param data Dati della previsione
     * @param options Opzioni di formattazione
     * @return Stringa con scheda formattata
     */
    std::string generateIOTAReport(const PredictionData& data, 
                                   const ReportOptions& options = ReportOptions());
    
    /**
     * @brief Genera scheda compatta in formato Preston
     * @param data Dati della previsione
     * @return Stringa con scheda in formato Preston
     */
    std::string generatePrestonReport(const PredictionData& data);
    
    /**
     * @brief Genera sezione header della scheda
     * @param data Dati della previsione
     * @param options Opzioni
     * @return Header formattato
     */
    std::string generateHeader(const PredictionData& data, const ReportOptions& options);
    
    /**
     * @brief Genera sezione evento
     * @param data Dati della previsione
     * @param options Opzioni
     * @return Sezione evento formattata
     */
    std::string generateEventSection(const PredictionData& data, const ReportOptions& options);
    
    /**
     * @brief Genera sezione stella
     * @param data Dati della previsione
     * @param options Opzioni
     * @return Sezione stella formattata
     */
    std::string generateStarSection(const PredictionData& data, const ReportOptions& options);
    
    /**
     * @brief Genera sezione path (centerline + limiti)
     * @param data Dati della previsione
     * @param options Opzioni
     * @return Sezione path formattata
     */
    std::string generatePathSection(const PredictionData& data, const ReportOptions& options);
    
    /**
     * @brief Genera sezione incertezza
     * @param data Dati della previsione
     * @param options Opzioni
     * @return Sezione incertezza formattata
     */
    std::string generateUncertaintySection(const PredictionData& data, const ReportOptions& options);
    
    /**
     * @brief Salva scheda su file
     * @param report Contenuto scheda
     * @param filename Nome file output
     * @return true se successo
     */
    bool saveToFile(const std::string& report, const std::string& filename);

private:
    std::string formatCoordinate(double lat, double lon, bool isDMS = true);
    std::string formatRA(double ra_hours);
    std::string formatDec(double dec_degrees);
    std::string formatDateTime(const JulianDate& jd, const std::string& timezone);
    std::string formatDuration(double seconds);
    std::string centerText(const std::string& text, int width);
    std::string padRight(const std::string& text, int width);
    std::string padLeft(const std::string& text, int width);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_PREDICTION_REPORT_H
