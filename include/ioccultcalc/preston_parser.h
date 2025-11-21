/**
 * @file preston_parser.h
 * @brief Parser per le previsioni di occultazione di Steve Preston
 * 
 * Legge e interpreta i file di previsione nel formato utilizzato
 * da Steve Preston (www.asteroidoccultation.com).
 * 
 * @author Michele Bigi
 * @date 2025
 */

#ifndef IOCCULTCALC_PRESTON_PARSER_H
#define IOCCULTCALC_PRESTON_PARSER_H

#include "types.h"
#include "time_utils.h"
#include "star_catalog.h"
#include "prediction_report.h"
#include <string>
#include <vector>
#include <memory>

namespace ioccultcalc {

/**
 * @brief Previsione di Steve Preston
 */
struct PrestonPrediction {
    // Identificatori
    int asteroidNumber;
    std::string asteroidName;
    std::string starCatalog;
    std::string starId;
    
    // Coordinate stella (J2000)
    double starRA;        // ore
    double starDec;       // gradi
    double starMagnitude;
    
    // Tempo evento
    JulianDate eventTime;
    std::string eventTimeString; // Come nel file originale
    
    // Geometria
    double pathWidth;            // km
    double maxDuration;          // secondi
    double closeApproachDist;    // arcsec
    double positionAngle;        // gradi
    double shadowVelocity;       // km/s
    
    // Incertezza
    double uncertaintyCrossTrack; // km
    double uncertaintyAlongTrack; // km
    
    // Path centrale (sample di coordinate)
    struct PathPoint {
        double latitude;
        double longitude;
        std::string locationName; // Se presente
    };
    std::vector<PathPoint> centerLinePath;
    
    // Metadati
    std::string predictionDate;  // Data calcolo previsione
    std::string ephemerisSource;
    std::string notes;           // Note aggiuntive
    
    // File originale
    std::string sourceFile;
    std::string rawText;         // Testo completo originale
};

/**
 * @brief Confronto tra due previsioni
 */
struct PredictionComparison {
    // Differenze temporali
    double timeDifference;        // secondi (IOccultCalc - Preston)
    
    // Differenze geometriche
    double pathWidthDifference;   // km
    double durationDifference;    // secondi
    double closeApproachDiff;     // arcsec
    double positionAngleDiff;     // gradi
    
    // Differenze coordinate stella
    double starRADiff;            // arcsec
    double starDecDiff;           // arcsec
    
    // Differenze path (RMS delle distanze punto-punto)
    double pathRMSError;          // km
    double maxPathError;          // km
    
    // Qualità confronto
    double overallAgreement;      // 0-1 (1 = perfetto accordo)
    std::string assessment;       // "Excellent", "Good", "Fair", "Poor"
    std::vector<std::string> notes;
};

/**
 * @brief Parser per file di previsione Preston
 */
class PrestonParser {
public:
    PrestonParser();
    ~PrestonParser();
    
    /**
     * @brief Carica previsione da file testo Preston
     * @param filename Path del file
     * @return Previsione parsata
     */
    PrestonPrediction parseFile(const std::string& filename);
    
    /**
     * @brief Carica previsione da stringa
     * @param content Contenuto testuale
     * @return Previsione parsata
     */
    PrestonPrediction parseString(const std::string& content);
    
    /**
     * @brief Carica previsione da URL (richiede libcurl)
     * @param url URL della previsione
     * @return Previsione parsata
     */
    PrestonPrediction parseURL(const std::string& url);
    
    /**
     * @brief Converte previsione Preston in formato IOccultCalc
     * @param preston Previsione Preston
     * @return Dati in formato IOccultCalc
     */
    PredictionData toIOccultCalcData(const PrestonPrediction& preston);
    
    /**
     * @brief Verifica validità della previsione parsata
     * @param preston Previsione da validare
     * @return true se valida
     */
    bool validate(const PrestonPrediction& preston);
    
    /**
     * @brief Ottiene lista errori di parsing
     * @return Vettore di messaggi di errore
     */
    std::vector<std::string> getErrors() const { return errors_; }

private:
    std::vector<std::string> errors_;
    
    // Metodi di parsing interni
    void parseHeader(const std::string& text, PrestonPrediction& pred);
    void parseEventInfo(const std::string& text, PrestonPrediction& pred);
    void parseStarInfo(const std::string& text, PrestonPrediction& pred);
    void parsePathInfo(const std::string& text, PrestonPrediction& pred);
    void parseUncertainty(const std::string& text, PrestonPrediction& pred);
    
    // Utility
    double parseRA(const std::string& raString);
    double parseDec(const std::string& decString);
    JulianDate parseDateTime(const std::string& dateTimeString);
    std::vector<std::string> splitLines(const std::string& text);
    std::string trim(const std::string& str);
};

/**
 * @brief Strumento di confronto tra previsioni
 */
class PredictionComparator {
public:
    PredictionComparator();
    ~PredictionComparator();
    
    /**
     * @brief Confronta previsione IOccultCalc con Preston
     * @param ioccult Dati IOccultCalc
     * @param preston Dati Preston
     * @return Risultato del confronto
     */
    PredictionComparison compare(const PredictionData& ioccult, 
                                 const PrestonPrediction& preston);
    
    /**
     * @brief Genera report testuale del confronto
     * @param comparison Risultato confronto
     * @param includeDetails Includi dettagli differenze
     * @return Report formattato
     */
    std::string generateComparisonReport(const PredictionComparison& comparison,
                                        bool includeDetails = true);
    
    /**
     * @brief Genera tabella comparativa
     * @param ioccult Dati IOccultCalc
     * @param preston Dati Preston
     * @param comparison Risultato confronto
     * @return Tabella formattata
     */
    std::string generateComparisonTable(const PredictionData& ioccult,
                                       const PrestonPrediction& preston,
                                       const PredictionComparison& comparison);

private:
    double calculatePathRMS(const std::vector<ShadowPathPoint>& path1,
                           const std::vector<PrestonPrediction::PathPoint>& path2);
    std::string assessAgreement(double agreementScore);
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_PRESTON_PARSER_H
