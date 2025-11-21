#include <iostream>
#include <fstream>
#include <vector>
#include <ioccultcalc/occultation_predictor.h>
#include <ioccultcalc/kml_exporter.h>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

// Legge una lista di asteroidi da un file
std::vector<std::string> readAsteroidList(const std::string& filename) {
    std::vector<std::string> asteroids;
    std::ifstream file(filename);
    
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + filename);
    }
    
    std::string line;
    while (std::getline(file, line)) {
        // Ignora linee vuote e commenti
        if (!line.empty() && line[0] != '#') {
            asteroids.push_back(line);
        }
    }
    
    return asteroids;
}

int main(int argc, char* argv[]) {
    try {
        std::cout << "IOccultCalc - Ricerca Occultazioni Multiple\n";
        std::cout << "============================================\n\n";
        
        // Parametri da linea di comando
        std::string asteroidListFile = "asteroids.txt";
        std::string startDateStr = "2026-01-01";
        std::string endDateStr = "2026-12-31";
        double maxMagnitude = 14.0;
        
        if (argc > 1) asteroidListFile = argv[1];
        if (argc > 2) startDateStr = argv[2];
        if (argc > 3) endDateStr = argv[3];
        if (argc > 4) maxMagnitude = std::stod(argv[4]);
        
        std::cout << "Parametri:\n";
        std::cout << "  File asteroidi: " << asteroidListFile << "\n";
        std::cout << "  Periodo: " << startDateStr << " - " << endDateStr << "\n";
        std::cout << "  Magnitudine limite: " << maxMagnitude << "\n\n";
        
        // Carica lista asteroidi
        std::vector<std::string> asteroids;
        
        try {
            asteroids = readAsteroidList(asteroidListFile);
            std::cout << "Caricati " << asteroids.size() << " asteroidi\n\n";
        } catch (const std::exception& e) {
            // Se il file non esiste, usa alcuni asteroidi di default
            std::cout << "File non trovato, uso asteroidi di default\n\n";
            asteroids = {"433", "6", "7", "15", "16"}; // Eros, Hebe, Iris, Eunomia, Psyche
        }
        
        // Date
        JulianDate startDate = TimeUtils::isoToJD(startDateStr);
        JulianDate endDate = TimeUtils::isoToJD(endDateStr);
        
        // Vettore per tutti gli eventi
        std::vector<OccultationEvent> allEvents;
        
        // Cerca occultazioni per ogni asteroide
        for (const auto& asteroidDesig : asteroids) {
            std::cout << "Elaborazione asteroide " << asteroidDesig << "...\n";
            
            try {
                OccultationPredictor predictor;
                predictor.loadAsteroidFromAstDyS(asteroidDesig);
                
                // Ricerca
                auto events = predictor.findOccultations(
                    startDate,
                    endDate,
                    maxMagnitude,
                    0.1,   // raggio ricerca più ampio
                    0.001  // probabilità minima più bassa
                );
                
                std::cout << "  Trovate " << events.size() << " occultazioni\n";
                
                // Aggiungi agli eventi totali
                allEvents.insert(allEvents.end(), events.begin(), events.end());
                
            } catch (const std::exception& e) {
                std::cout << "  Errore: " << e.what() << "\n";
            }
        }
        
        std::cout << "\n========================================\n";
        std::cout << "TOTALE: " << allEvents.size() << " occultazioni trovate\n\n";
        
        // Ordina per tempo
        std::sort(allEvents.begin(), allEvents.end(),
                 [](const OccultationEvent& a, const OccultationEvent& b) {
                     return a.timeCA.jd < b.timeCA.jd;
                 });
        
        // Stampa sommario
        std::cout << "Sommario eventi:\n";
        std::cout << "----------------\n";
        
        for (size_t i = 0; i < allEvents.size() && i < 20; ++i) {
            const auto& ev = allEvents[i];
            std::cout << TimeUtils::jdToISO(ev.timeCA) << " - "
                      << ev.asteroid.designation << " - "
                      << "mag " << ev.star.phot_g_mean_mag << " - "
                      << "P=" << (int)(ev.probability * 100) << "%\n";
        }
        
        if (allEvents.size() > 20) {
            std::cout << "... e altri " << (allEvents.size() - 20) << " eventi\n";
        }
        
        // Esporta tutti gli eventi
        if (!allEvents.empty()) {
            std::cout << "\nEsportazione in KML...\n";
            
            KMLExporter exporter;
            
            if (exporter.exportMultipleToKML(allEvents, "search_results.kml")) {
                std::cout << "Esportato: search_results.kml\n";
            }
            
            // Esporta anche eventi singoli con alta probabilità
            int exportCount = 0;
            for (const auto& ev : allEvents) {
                if (ev.probability > 0.1) { // > 10%
                    std::string filename = "event_" + ev.eventId + ".kml";
                    if (exporter.exportToKML(ev, filename)) {
                        exportCount++;
                    }
                }
            }
            
            std::cout << "Esportati " << exportCount << " eventi individuali (P>10%)\n";
        }
        
        std::cout << "\nCompletato!\n";
        
    } catch (const std::exception& e) {
        std::cerr << "ERRORE: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
