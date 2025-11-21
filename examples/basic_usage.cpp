#include <iostream>
#include <ioccultcalc/occultation_predictor.h>
#include <ioccultcalc/kml_exporter.h>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

int main(int argc, char* argv[]) {
    try {
        std::cout << "IOccultCalc - Esempio Uso Base\n";
        std::cout << "================================\n\n";
        
        // 1. Crea il predittore
        OccultationPredictor predictor;
        
        // 2. Carica asteroide da AstDyS2
        std::string asteroidDesignation = "433"; // Eros
        if (argc > 1) {
            asteroidDesignation = argv[1];
        }
        
        std::cout << "Caricamento elementi orbitali per " << asteroidDesignation << "...\n";
        predictor.loadAsteroidFromAstDyS(asteroidDesignation);
        
        // 3. Imposta parametri (opzionale)
        predictor.setAsteroidDiameter(16.8); // km (Eros)
        predictor.setOrbitalUncertainty(50.0); // km (1-sigma)
        
        // 4. Definisci intervallo temporale
        JulianDate startDate = TimeUtils::isoToJD("2025-12-01");
        JulianDate endDate = TimeUtils::isoToJD("2025-12-31");
        
        std::cout << "Ricerca occultazioni dal " << TimeUtils::jdToISO(startDate)
                  << " al " << TimeUtils::jdToISO(endDate) << "\n";
        std::cout << "Magnitudine limite stelle: 12.0\n";
        std::cout << "Attendere, la ricerca può richiedere alcuni minuti...\n\n";
        
        // 5. Cerca occultazioni
        auto occultations = predictor.findOccultations(
            startDate,
            endDate,
            12.0,  // magnitudine limite
            0.05,  // raggio ricerca in gradi
            0.01   // probabilità minima
        );
        
        // 6. Mostra risultati
        std::cout << "Trovate " << occultations.size() << " occultazioni\n\n";
        
        for (size_t i = 0; i < occultations.size(); ++i) {
            const auto& occ = occultations[i];
            
            std::cout << "Evento " << (i + 1) << ":\n";
            std::cout << "  Tempo: " << TimeUtils::jdToISO(occ.timeCA) << " UT\n";
            std::cout << "  Stella Gaia: " << occ.star.sourceId << "\n";
            std::cout << "  Magnitudine: " << occ.star.phot_g_mean_mag << "\n";
            std::cout << "  Closest Approach: " << occ.closeApproachDistance << " arcsec\n";
            std::cout << "  Probabilità: " << (occ.probability * 100) << "%\n";
            std::cout << "  Durata max: " << occ.maxDuration << " secondi\n";
            std::cout << "  Angolo posizione: " << occ.positionAngle << " gradi\n";
            std::cout << "  Punti shadow path: " << occ.shadowPath.size() << "\n";
            std::cout << "\n";
            
            // 7. Esporta in KML
            KMLExporter exporter;
            
            // Configura opzioni export
            KMLExporter::ExportOptions opts;
            opts.showUncertaintyBands = true;
            opts.showTimestamps = true;
            opts.showCenterline = true;
            exporter.setExportOptions(opts);
            
            std::string filename = "occultation_" + std::to_string(i + 1) + ".kml";
            if (exporter.exportToKML(occ, filename)) {
                std::cout << "  Esportato in: " << filename << "\n\n";
            } else {
                std::cout << "  Errore nell'esportazione KML\n\n";
            }
        }
        
        // 8. Esporta tutti gli eventi in un unico file
        if (!occultations.empty()) {
            KMLExporter exporter;
            if (exporter.exportMultipleToKML(occultations, "all_occultations.kml")) {
                std::cout << "Tutti gli eventi esportati in: all_occultations.kml\n";
            }
        }
        
        std::cout << "\nCompletato!\n";
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
