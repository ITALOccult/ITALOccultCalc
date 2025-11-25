/**
 * @file gaia_cache_downloader.cpp
 * @brief Tool per scaricare e gestire cache locale GAIA DR3
 * 
 * Utilizzo:
 *   gaia_cache_downloader --ecliptic <width_deg> <mag_limit>
 *   gaia_cache_downloader --mainbelt <mag_limit>
 *   gaia_cache_downloader --region <ra_min> <ra_max> <dec_min> <dec_max> <mag>
 *   gaia_cache_downloader --stats
 *   gaia_cache_downloader --clear
 * 
 * Opzioni:
 *   --cache-dir <path>  : Directory cache (default: ~/.ioccultcalc/catalogs)
 *   --verbose           : Output dettagliato
 */

#include "ioccultcalc/gaia_cache.h"
#include "ioccultcalc/gaia_client.h"
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>
#include <memory>

using namespace ioccultcalc;

// ==================== Utility Functions ====================

void printHeader(const std::string& title) {
    std::cout << "\n╔═══════════════════════════════════════════════════════════════╗\n";
    std::cout << "║ " << std::left << std::setw(61) << title << " ║\n";
    std::cout << "╚═══════════════════════════════════════════════════════════════╝\n\n";
}

void printUsage(const char* progname) {
    std::cout << "Utilizzo:\n\n";
    std::cout << "  Download regioni predefinite:\n";
    std::cout << "    " << progname << " --ecliptic <width_deg> <mag_limit>\n";
    std::cout << "    " << progname << " --mainbelt <mag_limit>\n\n";
    std::cout << "  Download regione personalizzata:\n";
    std::cout << "    " << progname << " --region <ra_min> <ra_max> <dec_min> <dec_max> <mag>\n\n";
    std::cout << "  Gestione cache:\n";
    std::cout << "    " << progname << " --stats\n";
    std::cout << "    " << progname << " --clear\n\n";
    std::cout << "  Opzioni:\n";
    std::cout << "    --cache-dir <path>  Directory cache (default: ~/.ioccultcalc/catalogs)\n";
    std::cout << "    --verbose           Output dettagliato\n";
    std::cout << "    --help              Mostra questo aiuto\n\n";
    std::cout << "Esempi:\n";
    std::cout << "  # Download fascia principale (±25° eclittica, mag < 15)\n";
    std::cout << "  " << progname << " --mainbelt 15.0\n\n";
    std::cout << "  # Download banda eclittica larga 30°, mag < 16\n";
    std::cout << "  " << progname << " --ecliptic 30.0 16.0\n\n";
    std::cout << "  # Download regione personalizzata\n";
    std::cout << "  " << progname << " --region 0 360 -30 30 15.0\n\n";
    std::cout << "  # Statistiche cache\n";
    std::cout << "  " << progname << " --stats\n\n";
}

void printStats(const GaiaCacheStats& stats) {
    std::cout << "Statistiche Cache:\n";
    std::cout << "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
    std::cout << "  Tiles totali:      " << stats.total_tiles << "\n";
    std::cout << "  Stelle totali:     " << stats.total_stars << "\n";
    std::cout << "  Dimensione:        " << std::fixed << std::setprecision(1) 
              << stats.total_size_mb << " MB\n";
    std::cout << "  Copertura cielo:   " << std::fixed << std::setprecision(1) 
              << stats.sky_coverage << " deg²\n";
    std::cout << "  Magnitudine:       " << std::fixed << std::setprecision(2)
              << stats.min_magnitude << " - " << stats.max_magnitude << "\n";
    std::cout << "  Ultimo aggiorn.:   " << stats.last_update << "\n";
    std::cout << "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n";
}

void progressCallback(int current, int total) {
    int percentage = (current * 100) / total;
    int barWidth = 50;
    int progress = (current * barWidth) / total;
    
    std::cout << "\r[";
    for (int i = 0; i < barWidth; i++) {
        if (i < progress) std::cout << "█";
        else std::cout << "░";
    }
    std::cout << "] " << std::setw(3) << percentage << "% (" 
              << current << "/" << total << " tiles)" << std::flush;
    
    if (current == total) {
        std::cout << "\n";
    }
}

// ==================== Main ====================

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printUsage(argv[0]);
        return 1;
    }
    
    // Parse options
    std::string cacheDir = "";
    bool verbose = false;
    
    std::vector<std::string> args;
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg == "--cache-dir" && i + 1 < argc) {
            cacheDir = argv[++i];
        } else if (arg == "--verbose") {
            verbose = true;
        } else if (arg == "--help" || arg == "-h") {
            printUsage(argv[0]);
            return 0;
        } else {
            args.push_back(arg);
        }
    }
    
    printHeader("GAIA Cache Downloader v1.0");
    
    // Initialize cache
    auto cache = std::make_unique<GaiaCache>(cacheDir);
    cache->setVerbose(verbose);
    
    // Setup Gaia client for downloads
    auto gaiaClient = std::make_shared<GaiaClient>();
    cache->setGaiaClient(gaiaClient);
    cache->setAutoDownload(true);
    
    // Load existing index
    if (cache->loadIndex()) {
        if (verbose) {
            std::cout << "✓ Cache esistente caricata\n\n";
        }
    } else {
        if (verbose) {
            std::cout << "○ Cache non trovata, verrà creata\n\n";
        }
    }
    
    try {
        // Parse command
        if (args.empty()) {
            std::cerr << "Errore: Nessun comando specificato\n";
            printUsage(argv[0]);
            return 1;
        }
        
        std::string command = args[0];
        
        // ==================== STATS ====================
        if (command == "--stats") {
            auto stats = cache->getStats();
            printStats(stats);
            return 0;
        }
        
        // ==================== CLEAR ====================
        else if (command == "--clear") {
            std::cout << "⚠️  ATTENZIONE: Questa operazione eliminerà TUTTA la cache!\n";
            std::cout << "Confermi? (yes/no): ";
            std::string confirm;
            std::cin >> confirm;
            
            if (confirm == "yes" || confirm == "y") {
                cache->clearCache();
                std::cout << "\n✓ Cache eliminata\n\n";
            } else {
                std::cout << "\n○ Operazione annullata\n\n";
            }
            return 0;
        }
        
        // ==================== MAINBELT ====================
        else if (command == "--mainbelt") {
            if (args.size() < 2) {
                std::cerr << "Errore: --mainbelt richiede <mag_limit>\n";
                return 1;
            }
            
            double magLimit = std::stod(args[1]);
            
            std::cout << "Download Main Belt Region:\n";
            std::cout << "  Fascia eclittica: ±25°\n";
            std::cout << "  RA: 0° - 360°\n";
            std::cout << "  Dec: -25° - +25°\n";
            std::cout << "  Magnitudine: < " << magLimit << "\n\n";
            
            std::cout << "Inizio download...\n";
            int downloaded = cache->downloadRegion(180.0, 0.0, 25.0, magLimit, progressCallback);
            
            std::cout << "\n✓ Download completato!\n";
            std::cout << "  Tiles scaricate: " << downloaded << "\n\n";
            
            auto stats = cache->getStats();
            printStats(stats);
            
            return 0;
        }
        
        // ==================== ECLIPTIC ====================
        else if (command == "--ecliptic") {
            if (args.size() < 3) {
                std::cerr << "Errore: --ecliptic richiede <width_deg> <mag_limit>\n";
                return 1;
            }
            
            double widthDeg = std::stod(args[1]);
            double magLimit = std::stod(args[2]);
            
            std::cout << "Download Ecliptic Band:\n";
            std::cout << "  Larghezza: ±" << widthDeg << "°\n";
            std::cout << "  RA: 0° - 360°\n";
            std::cout << "  Dec: " << -widthDeg << "° - +" << widthDeg << "°\n";
            std::cout << "  Magnitudine: < " << magLimit << "\n\n";
            
            std::cout << "Inizio download...\n";
            int downloaded = cache->downloadRegion(180.0, 0.0, widthDeg, magLimit, progressCallback);
            
            std::cout << "\n✓ Download completato!\n";
            std::cout << "  Tiles scaricate: " << downloaded << "\n\n";
            
            auto stats = cache->getStats();
            printStats(stats);
            
            return 0;
        }
        
        // ==================== REGION ====================
        else if (command == "--region") {
            if (args.size() < 6) {
                std::cerr << "Errore: --region richiede <ra_min> <ra_max> <dec_min> <dec_max> <mag>\n";
                return 1;
            }
            
            double raMin = std::stod(args[1]);
            double raMax = std::stod(args[2]);
            double decMin = std::stod(args[3]);
            double decMax = std::stod(args[4]);
            double magLimit = std::stod(args[5]);
            
            double raCenterDeg = (raMin + raMax) / 2.0;
            double decCenterDeg = (decMin + decMax) / 2.0;
            double raRange = raMax - raMin;
            double decRange = decMax - decMin;
            double radiusDeg = std::max(raRange, decRange) / 2.0;
            
            std::cout << "Download Custom Region:\n";
            std::cout << "  RA: " << raMin << "° - " << raMax << "°\n";
            std::cout << "  Dec: " << decMin << "° - " << decMax << "°\n";
            std::cout << "  Centro: RA=" << raCenterDeg << "° Dec=" << decCenterDeg << "°\n";
            std::cout << "  Raggio: " << radiusDeg << "°\n";
            std::cout << "  Magnitudine: < " << magLimit << "\n\n";
            
            std::cout << "Inizio download...\n";
            int downloaded = cache->downloadRegion(raCenterDeg, decCenterDeg, radiusDeg, 
                                                   magLimit, progressCallback);
            
            std::cout << "\n✓ Download completato!\n";
            std::cout << "  Tiles scaricate: " << downloaded << "\n\n";
            
            auto stats = cache->getStats();
            printStats(stats);
            
            return 0;
        }
        
        // ==================== UNKNOWN ====================
        else {
            std::cerr << "Errore: Comando sconosciuto '" << command << "'\n";
            printUsage(argv[0]);
            return 1;
        }
        
    } catch (const std::exception& e) {
        std::cerr << "\n✗ Errore: " << e.what() << "\n\n";
        return 1;
    }
    
    return 0;
}
