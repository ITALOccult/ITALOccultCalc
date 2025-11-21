/**
 * @file example_prediction_comparison.cpp
 * @brief Esempio di generazione schede IOTA e confronto con previsioni Preston
 * 
 * Questo esempio dimostra come:
 * 1. Generare una scheda di previsione in formato IOTA classico
 * 2. Caricare una previsione di Steve Preston
 * 3. Confrontare le due previsioni e generare un report delle differenze
 * 
 * Uso:
 *   ./example_prediction_comparison generate <asteroid_number>
 *   ./example_prediction_comparison compare <preston_file> <asteroid_number>
 *   ./example_prediction_comparison demo
 * 
 * @author Michele Bigi
 * @date 2025
 */

#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/occultation_predictor.h"
#include "ioccultcalc/prediction_report.h"
#include "ioccultcalc/preston_parser.h"
#include "ioccultcalc/gaia_client.h"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <ctime>

using namespace ioccultcalc;

/**
 * @brief Crea dati di esempio per test
 */
PredictionData createExamplePrediction() {
    PredictionData data;
    
    // Asteroide (433) Eros
    data.asteroidNumber = 433;
    data.asteroidName = "Eros";
    data.asteroidDiameter = 16.8; // km
    data.asteroidUncertainty = 5.2; // km
    
    // Stella Gaia DR3 (esempio)
    data.starCatalog = "Gaia DR3";
    data.starId = "1234567890123456";
    data.star.pos.ra = 12.5;  // ore
    data.star.pos.dec = 15.3; // gradi
    data.star.pmra = 5.2;
    data.star.pmdec = -3.1;
    data.star.parallax = 2.5;
    data.starMagnitude = 11.2;
    
    // Evento (2026-03-15 23:45:30 UTC)
    data.eventTime.jd = 2460749.48994;
    data.closeApproachDistance = 0.035; // arcsec
    data.positionAngle = 125.5; // gradi
    data.shadowVelocity = 18.2; // km/s
    data.pathWidth = 16.8; // km
    data.maxDuration = 0.92; // secondi
    data.probability = 0.95;
    
    // Incertezza
    data.uncertaintyEllipseA = 0.045; // arcsec
    data.uncertaintyEllipseB = 0.015; // arcsec
    data.uncertaintyAngle = 132.0; // gradi
    
    // Path centrale (esempio con 10 punti)
    for (int i = 0; i < 10; ++i) {
        ShadowPathPoint pt;
        pt.location.latitude = 40.0 + i * 0.5;  // Da 40°N a 44.5°N
        pt.location.longitude = -5.0 + i * 0.3; // Da 5°W a 2.7°W
        pt.location.altitude = 0.0;
        pt.time.jd = data.eventTime.jd + (i - 5) * 0.00001; // ±0.86 secondi
        pt.duration = data.maxDuration * (1.0 - std::abs(i - 5) * 0.05);
        pt.centerlineDistance = 0.0;
        // pt.sunAltitude = -15.0; // Field non disponibile in ShadowPathPoint
        data.centerLine.push_back(pt);
    }
    
    // Limiti (semplificati, 5 punti ciascuno)
    for (int i = 0; i < 5; ++i) {
        ShadowPathPoint north, south;
        double baseLat = 40.0 + i * 1.0;
        double baseLon = -5.0 + i * 0.6;
        
        north.location.latitude = baseLat + 0.1;
        north.location.longitude = baseLon;
        north.location.altitude = 0.0;
        data.northLimit.push_back(north);
        
        south.location.latitude = baseLat - 0.1;
        south.location.longitude = baseLon;
        south.location.altitude = 0.0;
        data.southLimit.push_back(south);
    }
    
    // Metadati
    data.ephemerisSource = "JPL DE441";
    data.calculationDate = "2025-11-21";
    data.observerInfo = "Michele Bigi - Gruppo Astrofili Massesi";
    
    return data;
}

/**
 * @brief Crea previsione Preston di esempio per confronto
 */
PrestonPrediction createExamplePrestonPrediction() {
    PrestonPrediction preston;
    
    preston.asteroidNumber = 433;
    preston.asteroidName = "Eros";
    preston.starCatalog = "Gaia DR3";
    preston.starId = "1234567890123456";
    
    preston.starRA = 12.5 + 0.0001;  // Piccola differenza
    preston.starDec = 15.3 - 0.0002;
    preston.starMagnitude = 11.2;
    
    preston.eventTime.jd = 2460749.48994 + 0.0001; // ~8.6 secondi differenza
    preston.eventTimeString = "2026 Mar 15 23:45:38 UT";
    
    preston.pathWidth = 17.2; // km (leggera differenza)
    preston.maxDuration = 0.95; // secondi
    preston.closeApproachDist = 0.033; // arcsec
    preston.positionAngle = 126.0; // gradi
    preston.shadowVelocity = 18.3; // km/s
    
    preston.uncertaintyCrossTrack = 5.5; // km
    preston.uncertaintyAlongTrack = 12.0; // km
    
    // Path centrale (leggermente diverso)
    for (int i = 0; i < 10; ++i) {
        PrestonPrediction::PathPoint pt;
        pt.latitude = 40.0 + i * 0.5 + 0.02;  // Offset di ~2 km
        pt.longitude = -5.0 + i * 0.3 + 0.01;
        pt.locationName = "";
        preston.centerLinePath.push_back(pt);
    }
    
    preston.ephemerisSource = "JPL #48";
    preston.predictionDate = "2025-11-15";
    preston.sourceFile = "preston_example.txt";
    
    return preston;
}

/**
 * @brief Modalità 1: Genera scheda IOTA
 */
void modeGenerate(int asteroidNumber) {
    std::cout << "=== GENERATION MODE: Creating IOTA prediction sheet ===" << std::endl;
    std::cout << "Asteroid: (" << asteroidNumber << ")" << std::endl << std::endl;
    
    // Crea dati di esempio (in un caso reale, calcolarli con OccultationPredictor)
    PredictionData data = createExamplePrediction();
    data.asteroidNumber = asteroidNumber;
    
    // Genera scheda formato IOTA completo
    PredictionReportGenerator generator;
    
    ReportOptions options;
    options.includePathCoordinates = true;
    options.pathPointsCount = 10;
    options.includeUncertainty = true;
    options.includeFinder = false;
    options.includeTimingDetails = true;
    options.timezone = "UTC";
    options.language = "en";
    
    std::cout << "Generating full IOTA format report..." << std::endl << std::endl;
    std::string iotaReport = generator.generateIOTAReport(data, options);
    std::cout << iotaReport << std::endl;
    
    // Salva su file
    std::string filename = "prediction_" + std::to_string(asteroidNumber) + "_iota.txt";
    if (generator.saveToFile(iotaReport, filename)) {
        std::cout << "\n✓ IOTA report saved to: " << filename << std::endl;
    }
    
    // Genera anche formato Preston compatto
    std::cout << "\n" << std::string(80, '=') << std::endl;
    std::cout << "Generating compact Preston format report..." << std::endl << std::endl;
    std::string prestonReport = generator.generatePrestonReport(data);
    std::cout << prestonReport << std::endl;
    
    filename = "prediction_" + std::to_string(asteroidNumber) + "_preston.txt";
    if (generator.saveToFile(prestonReport, filename)) {
        std::cout << "✓ Preston report saved to: " << filename << std::endl;
    }
}

/**
 * @brief Modalità 2: Confronta con previsione Preston
 */
void modeCompare(const std::string& prestonFile, int asteroidNumber) {
    std::cout << "=== COMPARISON MODE: IOccultCalc vs Preston ===" << std::endl;
    std::cout << "Preston file: " << prestonFile << std::endl;
    std::cout << "Asteroid: (" << asteroidNumber << ")" << std::endl << std::endl;
    
    // Carica previsione Preston
    PrestonParser parser;
    PrestonPrediction preston = parser.parseFile(prestonFile);
    
    if (!parser.validate(preston)) {
        std::cerr << "ERROR: Invalid Preston prediction file!" << std::endl;
        std::cerr << "Errors:" << std::endl;
        for (const auto& error : parser.getErrors()) {
            std::cerr << "  • " << error << std::endl;
        }
        return;
    }
    
    std::cout << "✓ Preston prediction loaded successfully" << std::endl;
    std::cout << "  Event: (" << preston.asteroidNumber << ") " << preston.asteroidName 
              << " occults " << preston.starCatalog << " " << preston.starId << std::endl;
    std::cout << "  Time: " << preston.eventTimeString << std::endl << std::endl;
    
    // Genera previsione IOccultCalc (in questo esempio usiamo dati di test)
    PredictionData ioccult = createExamplePrediction();
    ioccult.asteroidNumber = asteroidNumber;
    
    std::cout << "✓ IOccultCalc prediction generated" << std::endl;
    std::cout << "  Ephemeris: " << ioccult.ephemerisSource << std::endl << std::endl;
    
    // Confronta le previsioni
    PredictionComparator comparator;
    PredictionComparison comparison = comparator.compare(ioccult, preston);
    
    // Mostra tabella comparativa
    std::string comparisonTable = comparator.generateComparisonTable(ioccult, preston, comparison);
    std::cout << comparisonTable << std::endl;
    
    // Mostra report dettagliato
    std::string comparisonReport = comparator.generateComparisonReport(comparison, true);
    std::cout << comparisonReport << std::endl;
    
    // Salva report su file
    std::string filename = "comparison_" + std::to_string(asteroidNumber) + ".txt";
    std::ofstream outFile(filename);
    if (outFile.is_open()) {
        outFile << comparisonTable << std::endl;
        outFile << comparisonReport << std::endl;
        outFile.close();
        std::cout << "✓ Comparison report saved to: " << filename << std::endl;
    }
    
    // Valutazione finale
    std::cout << "\n" << std::string(80, '=') << std::endl;
    std::cout << "ASSESSMENT: " << comparison.assessment << std::endl;
    std::cout << "Agreement Score: " << std::fixed << std::setprecision(1) 
              << (comparison.overallAgreement * 100) << "%" << std::endl;
    std::cout << std::string(80, '=') << std::endl;
}

/**
 * @brief Modalità 3: Demo completa con dati di esempio
 */
void modeDemo() {
    std::cout << "\n";
    std::cout << "╔════════════════════════════════════════════════════════════════════════════╗\n";
    std::cout << "║                    IOccultCalc PREDICTION COMPARISON DEMO                  ║\n";
    std::cout << "║                                                                            ║\n";
    std::cout << "║  Questo esempio dimostra la generazione di schede di previsione IOTA      ║\n";
    std::cout << "║  e il confronto con le previsioni di Steve Preston                        ║\n";
    std::cout << "╚════════════════════════════════════════════════════════════════════════════╝\n";
    std::cout << std::endl;
    
    // Parte 1: Generazione schede
    std::cout << "\n┌─ STEP 1: Generating IOTA Prediction Sheet ─────────────────────────────┐\n\n";
    
    PredictionData ioccult = createExamplePrediction();
    PredictionReportGenerator generator;
    
    ReportOptions options;
    options.includePathCoordinates = true;
    options.pathPointsCount = 5; // Ridotto per demo
    options.includeUncertainty = true;
    options.timezone = "UTC";
    options.language = "en";
    
    std::string iotaReport = generator.generateIOTAReport(ioccult, options);
    std::cout << iotaReport << std::endl;
    
    // Parte 2: Previsione Preston
    std::cout << "\n┌─ STEP 2: Preston Prediction Format ────────────────────────────────────┐\n\n";
    
    PrestonPrediction preston = createExamplePrestonPrediction();
    std::string prestonReport = generator.generatePrestonReport(ioccult);
    std::cout << prestonReport << std::endl;
    
    // Parte 3: Confronto
    std::cout << "\n┌─ STEP 3: Comparing Predictions ────────────────────────────────────────┐\n\n";
    
    PredictionComparator comparator;
    PredictionComparison comparison = comparator.compare(ioccult, preston);
    
    std::string comparisonTable = comparator.generateComparisonTable(ioccult, preston, comparison);
    std::cout << comparisonTable << std::endl;
    
    std::string comparisonReport = comparator.generateComparisonReport(comparison, true);
    std::cout << comparisonReport << std::endl;
    
    // Conclusioni
    std::cout << "\n┌─ INTERPRETATION ────────────────────────────────────────────────────────┐\n\n";
    std::cout << "Time Difference: " << std::fixed << std::setprecision(1) 
              << comparison.timeDifference << " seconds\n";
    std::cout << "  → " << (std::abs(comparison.timeDifference) < 5.0 ? 
                           "Excellent agreement (< 5s)" : 
                           "Check timing accuracy") << "\n\n";
    
    std::cout << "Path RMS Error: " << std::fixed << std::setprecision(1) 
              << comparison.pathRMSError << " km\n";
    std::cout << "  → " << (comparison.pathRMSError < 30.0 ? 
                           "Path location consistent" : 
                           "Significant path shift detected") << "\n\n";
    
    std::cout << "Star Position Difference: " 
              << std::fixed << std::setprecision(3)
              << std::sqrt(comparison.starRADiff*comparison.starRADiff + 
                          comparison.starDecDiff*comparison.starDecDiff) 
              << " arcsec\n";
    std::cout << "  → " << (std::abs(comparison.starRADiff) < 0.1 && 
                           std::abs(comparison.starDecDiff) < 0.1 ? 
                           "Using same star catalog" : 
                           "Different catalog epochs or proper motion") << "\n\n";
    
    std::cout << "Overall Assessment: " << comparison.assessment << " ("
              << std::fixed << std::setprecision(0) << (comparison.overallAgreement * 100) 
              << "% agreement)\n\n";
    
    if (comparison.overallAgreement > 0.90) {
        std::cout << "✓ Both predictions are highly consistent. IOccultCalc validates\n";
        std::cout << "  against Steve Preston's established methodology.\n";
    } else if (comparison.overallAgreement > 0.70) {
        std::cout << "⚠ Good agreement overall. Minor differences may be due to:\n";
        std::cout << "  - Different ephemerides (JPL DE441 vs JPL #48)\n";
        std::cout << "  - Different integration methods\n";
        std::cout << "  - Different star catalog epochs\n";
    } else {
        std::cout << "⚠ Significant differences detected. Further investigation needed:\n";
        std::cout << "  - Verify asteroid orbital elements\n";
        std::cout << "  - Check star coordinates and proper motion\n";
        std::cout << "  - Compare ephemeris sources\n";
    }
    
    std::cout << "\n└─────────────────────────────────────────────────────────────────────────┘\n";
    
    // Salva i file
    std::cout << "\n📁 Saving demo files...\n";
    generator.saveToFile(iotaReport, "demo_iota_prediction.txt");
    std::cout << "   ✓ demo_iota_prediction.txt\n";
    
    generator.saveToFile(prestonReport, "demo_preston_format.txt");
    std::cout << "   ✓ demo_preston_format.txt\n";
    
    std::ofstream compFile("demo_comparison.txt");
    if (compFile.is_open()) {
        compFile << comparisonTable << std::endl;
        compFile << comparisonReport << std::endl;
        compFile.close();
        std::cout << "   ✓ demo_comparison.txt\n";
    }
    
    std::cout << "\n✓ Demo completed successfully!\n\n";
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cout << "IOccultCalc - Prediction Report Generator & Comparison Tool\n";
        std::cout << "==========================================================\n\n";
        std::cout << "Usage:\n";
        std::cout << "  " << argv[0] << " generate <asteroid_number>\n";
        std::cout << "      Generate IOTA and Preston format prediction sheets\n\n";
        std::cout << "  " << argv[0] << " compare <preston_file> <asteroid_number>\n";
        std::cout << "      Compare IOccultCalc prediction with Preston file\n\n";
        std::cout << "  " << argv[0] << " demo\n";
        std::cout << "      Run complete demonstration with example data\n\n";
        std::cout << "Examples:\n";
        std::cout << "  " << argv[0] << " generate 433\n";
        std::cout << "  " << argv[0] << " compare preston_433.txt 433\n";
        std::cout << "  " << argv[0] << " demo\n";
        return 0;
    }
    
    std::string mode = argv[1];
    
    try {
        if (mode == "generate" && argc >= 3) {
            int asteroidNumber = std::atoi(argv[2]);
            modeGenerate(asteroidNumber);
        } else if (mode == "compare" && argc >= 4) {
            std::string prestonFile = argv[2];
            int asteroidNumber = std::atoi(argv[3]);
            modeCompare(prestonFile, asteroidNumber);
        } else if (mode == "demo") {
            modeDemo();
        } else {
            std::cerr << "Invalid arguments. Use '" << argv[0] << "' without arguments for help." << std::endl;
            return 1;
        }
    } catch (const std::exception& e) {
        std::cerr << "ERROR: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
