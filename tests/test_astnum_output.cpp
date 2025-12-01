/**
 * @file test_astnum_output.cpp
 * @brief Test del nuovo formato output ASTNUM_LIST
 */

#include <iostream>
#include <fstream>
#include <ioccultcalc/output_manager.h>
#include <ioccultcalc/time_utils.h>

using namespace ioccultcalc;

int main() {
    std::cout << "\n";
    std::cout << "╔════════════════════════════════════════════════════╗\n";
    std::cout << "║  Test ASTNUM_LIST Output Format                   ║\n";
    std::cout << "╚════════════════════════════════════════════════════╝\n\n";
    
    // Crea alcuni eventi di test
    std::vector<OccultationEvent> events;
    
    // Evento 1: Ceres
    OccultationEvent evt1;
    evt1.asteroid_number = 1;
    evt1.asteroid_name = "Ceres";
    evt1.diameter_km = 939.4;
    evt1.star_mag = 10.2;
    evt1.computation_date = "2025-11-29 12:00:00";
    evt1.software_version = "IOccultCalc v2.1.0-rkf78";
    events.push_back(evt1);
    
    // Evento 2: altro evento Ceres (stesso asteroide)
    OccultationEvent evt2 = evt1;
    evt2.star_mag = 11.5;
    events.push_back(evt2);
    
    // Evento 3: Vesta
    OccultationEvent evt3;
    evt3.asteroid_number = 4;
    evt3.asteroid_name = "Vesta";
    evt3.diameter_km = 525.4;
    evt3.star_mag = 9.8;
    evt3.computation_date = "2025-11-29 12:00:00";
    evt3.software_version = "IOccultCalc v2.1.0-rkf78";
    events.push_back(evt3);
    
    // Evento 4: Hygiea
    OccultationEvent evt4;
    evt4.asteroid_number = 10;
    evt4.asteroid_name = "Hygiea";
    evt4.diameter_km = 434.0;
    evt4.star_mag = 12.1;
    evt4.computation_date = "2025-11-29 12:00:00";
    evt4.software_version = "IOccultCalc v2.1.0-rkf78";
    events.push_back(evt4);
    
    // Evento 5: Eros
    OccultationEvent evt5;
    evt5.asteroid_number = 433;
    evt5.asteroid_name = "Eros";
    evt5.diameter_km = 16.8;
    evt5.star_mag = 8.5;
    evt5.computation_date = "2025-11-29 12:00:00";
    evt5.software_version = "IOccultCalc v2.1.0-rkf78";
    events.push_back(evt5);
    
    // Evento 6: altro Vesta
    events.push_back(evt3);
    
    std::cout << "📝 Created " << events.size() << " test events:\n";
    std::cout << "   - Ceres (1): 2 events\n";
    std::cout << "   - Vesta (4): 2 events\n";
    std::cout << "   - Hygiea (10): 1 event\n";
    std::cout << "   - Eros (433): 1 event\n\n";
    
    // Configura OutputManager per formato ASTNUM_LIST
    OutputManager manager;
    OutputOptions opts;
    opts.format = OutputFormat::ASTNUM_LIST;
    opts.output_file = "test_asteroids_list.txt";
    opts.append_mode = false;
    manager.setOptions(opts);
    
    std::cout << "💾 Writing ASTNUM_LIST format to: " << opts.output_file << "\n";
    
    bool success = manager.writeEvents(events);
    
    if (success) {
        std::cout << "✅ ASTNUM_LIST file written successfully!\n\n";
        
        // Leggi e mostra il contenuto
        std::cout << "📄 File content:\n";
        std::cout << "════════════════════════════════════════════════════\n";
        std::ifstream file(opts.output_file);
        std::string line;
        while (std::getline(file, line)) {
            std::cout << line << "\n";
        }
        file.close();
        std::cout << "════════════════════════════════════════════════════\n\n";
        
        std::cout << "✓ Test completed successfully!\n\n";
        return 0;
    } else {
        std::cerr << "❌ Failed to write ASTNUM_LIST file\n\n";
        return 1;
    }
}
