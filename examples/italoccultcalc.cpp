#include "ioccultcalc/config_manager.h"
#include "ioc_gaialib/unified_gaia_catalog.h"
#include "starmap/StarMap.h"
#include "ioccultcalc/occultation_predictor.h"
#include "ioccultcalc/occultation_engine.h"
#include "ioccultcalc/spice_spk_reader.h"
#include "ioccultcalc/occult4_xml.h"
#include <iostream>
#include <iomanip>

using namespace ioccultcalc;

int main(int argc, char* argv[]) {
    // 1. Initialize StarMap (Catalog System)
    starmap::config::LibraryConfig::CatalogPaths paths;
    paths.gaiaSaoDatabase = std::string(getenv("HOME")) + "/.catalog/crossreference/gaia_sao_xmatch.db";
    paths.iauCatalog = "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/external/IOC_GaiaLib/data/IAU-CSN.json";
    paths.starNamesDatabase = "/Users/michelebigi/Documents/Develop/ASTDYN/IOccultCalc/external/IOC_GaiaLib/data/common_star_names.csv";
    starmap::initialize(paths);

    // 2. Initialize UnifiedGaiaCatalog
    std::string homeDir = getenv("HOME");
    nlohmann::json gaiaConfJson;
    gaiaConfJson["catalog_type"] = "sqlite_dr3";
    gaiaConfJson["sqlite_file_path"] = "/Users/michelebigi/.catalog/crossreference/gaia_dr3_occult_pro.db";
    gaiaConfJson["log_level"] = "warning";
    
    ioc::gaia::UnifiedGaiaCatalog::initialize(gaiaConfJson.dump());
    
    std::cout << "ITALOccultCalc v1.0 [Native API Workflow]" << std::endl;

    try {
        std::cout << "\n=== ITALOccultCalc DEBUG BUILD ===\n" << std::endl;
        
    // 3. Setup Configuration
        Phase1Config p1Config;
        int asteroidNumber = 0;
        bool useHorizons = false;
        
        // Default values
        p1Config.max_magnitude = 14.0;
        p1Config.corridor_width_deg = 5.0;
        p1Config.threshold_arcsec = 300.0;
        
        // Parse arguments for config
        std::string configFile;
        if (argc > 1) {
            configFile = argv[1];
        }
        
        // Output filenames
        std::string a4CardFile;
        std::string ioccultCardFile;
        
        if (!configFile.empty()) {
            std::cout << "📄 Loading configuration from: " << configFile << std::endl;
            ConfigManager config;
            config.loadFromJson(configFile);
            
            // OUTPUT Params
            auto outParams = config.getSection(ConfigSection::OUTPUT);
            if (outParams) {
                if (outParams->hasParameter("event_card_file")) 
                    a4CardFile = outParams->getParameter("event_card_file")->asString();
                if (outParams->hasParameter("ioccult_card_file"))
                    ioccultCardFile = outParams->getParameter("ioccult_card_file")->asString();
            }
            std::cout << "DEBUG: output config read - A4='" << a4CardFile << "', IOccult='" << ioccultCardFile << "'" << std::endl;
            
            // OBJECT Section
            auto objParams = config.getSection(ConfigSection::OBJECT);
            if (objParams) {
                auto idParam = objParams->getParameter("id");
                if (idParam) {
                    p1Config.asteroid_name = idParam->asString();
                    try {
                        asteroidNumber = std::stoi(p1Config.asteroid_name);
                    } catch (...) {
                         std::cerr << "Warning: Asteroid ID is not a number: " << p1Config.asteroid_name << std::endl;
                    }
                }
            }
            
            // SEARCH Section
            auto searchParams = config.getSection(ConfigSection::SEARCH);
            if (searchParams) {
                auto startParam = searchParams->getParameter("start_mjd");
                if (startParam) p1Config.start_mjd_tdb = startParam->asDouble();
                
                auto endParam = searchParams->getParameter("end_mjd");
                if (endParam) p1Config.end_mjd_tdb = endParam->asDouble();
                
                auto magParam = searchParams->getParameter("mag_limit");
                if (magParam) p1Config.max_magnitude = magParam->asDouble();
                
                auto widthParam = searchParams->getParameter("corridor_width");
                if (widthParam) p1Config.corridor_width_deg = widthParam->asDouble();
            }
            
            // PROPAGATION Section (Optional flags)
            auto propParams = config.getSection(ConfigSection::PROPAGATION);
            if (propParams) {
                auto horizonsParam = propParams->getParameter("use_horizons");
                if (horizonsParam) useHorizons = horizonsParam->asBool();
            }

        } else {
            // Fallback Legacy Default
            std::cout << "⚠️ No config file provided. Using hardcoded defaults (1272 Gefion)." << std::endl;
            p1Config.asteroid_name = "1272";
            asteroidNumber = 1272;
            p1Config.start_mjd_tdb = 61050.0;
            p1Config.end_mjd_tdb = 61055.0;
        }

        std::cout << "Target: " << p1Config.asteroid_name << " (" << asteroidNumber << ")" << std::endl;
        std::cout << "Window: " << p1Config.start_mjd_tdb << " -> " << p1Config.end_mjd_tdb << std::endl;
        
        // 4. Initialize SPK Reader for Earth positions
        auto spk = std::make_shared<SPICESPKReader>();
        std::string kernelsDir = homeDir + "/.ioccultcalc/ephemerides/";
        
        if (!spk->loadFile(kernelsDir + "de440.bsp")) {
             // Fallback
             if (!spk->loadFile(kernelsDir + "de441_part-2.bsp")) {
                 std::cerr << "Critical: Failed to load DE ephemeris." << std::endl;
                 return 1;
             }
        }
        std::cout << "Ephemeris loaded." << std::endl;
        
        // 5. Initialize OccultationEngine
        OccultationEngine engine;
        
        // Load Asteroid (Try DB then JSON)
        std::cout << "Loading asteroid " << asteroidNumber << "..." << std::endl;
        bool loaded = engine.loadAsteroidFromDB(asteroidNumber);
        if (!loaded) {
             std::cout << "DB load failed, trying JSON/Allnum fallback..." << std::endl;
             loaded = engine.loadAsteroidFromJSON(asteroidNumber);
        }

        if (!loaded) {
            std::cerr << "Failed to load asteroid elements." << std::endl;
            if (asteroidNumber == 13477) {
                 std::cerr << "Attempting manual injection for 13477..." << std::endl;
                 AstDynEquinoctialElements elem;
                 // Set manually... (Simplified for brevity as Engine handles set)
                 // Just exit for cleaner test
                 return 1;
            } else {
                return 1;
            }
        }
        
        // Pass SPK Reader to sub-components (Engine should probably handle this in future)
        // For now, access phases via Engine getters
        engine.getPhase1().setSPKReader(spk);
        engine.getPhase2().setSPKReader(spk);

        // 6. Run Phase 1
        std::cout << "Phase 1: Screening stars in " << p1Config.corridor_width_deg << " deg corridor...\n";
        Phase1Results p1Results = engine.runPhase1(p1Config);
        std::cout << "Found " << p1Results.candidates.size() << " candidates (" 
                  << p1Results.num_stars_in_corridor << " stars initially in corridor).\n";

        // 7. Run Phase 2
        if (!p1Results.candidates.empty()) {
            std::cout << "\nPhase 2: Calculating precise geometry...\n";
            Phase2Config p2Config;
            p2Config.refine_orbit = true;  // ENABLE REFINEMENT (Correction enabled)
            p2Config.last_n_obs = 100;     // Use last 100 observations
            p2Config.use_horizons = useHorizons;
            
            // Engine handles refinement internally now!
            std::vector<Phase2OccultationEvent> events = engine.runPhase2(p2Config, p1Results.candidates);
            std::cout << "Processed Phase 2 for all candidates. Found " << events.size() << " confirmed events.\n";
            
            for (const auto& evt : events) {
                std::cout << "--------------------------------------------------\n";
                // Print results
                std::cout << "CANDIDATE RESULT (Phase 2):\n";
                std::cout << "  Star ID:  " << evt.star_id << "\n";
                std::cout << "  Star Pos: RA=" << std::fixed << std::setprecision(6) << evt.star_ra_deg 
                          << " Dec=" << evt.star_dec_deg << "\n";
                std::cout << "  Date:     " << std::fixed << std::setprecision(5) << evt.mjd_tdb << "\n";
                std::cout << "  CA Dist:  " << std::fixed << std::setprecision(3) << (evt.min_dist_mas / 1000.0) << "\"\n";
                std::cout << "  Duration: " << std::fixed << std::setprecision(2) << evt.duration_sec << " s\n";
                
                if (!evt.shadow_path.empty()) {
                    std::cout << "  Shadow Path (" << evt.shadow_path.size() << " pts):\n";
                    size_t center_idx = evt.shadow_path.size() / 2;
                    const auto& pt = evt.shadow_path[center_idx];
                    std::cout << "    [CENTER] Lat: " << pt.lat_deg << " Lon: " << pt.lon_deg << " MJD: " << pt.mjd_tdb << "\n";
                }

                if (evt.is_valid) {
                    std::cout << "  *** VALID OCCULTATION (<1\") ***\n";
                }
                std::cout << "--------------------------------------------------\n";
            }
            
            // XML Export
            std::cout << "\nExporting results to XML...\n";
            std::vector<OccultationEvent> exportEvents;
            const auto& currentElements = engine.getCurrentElements();
            
            for (const auto& p2Evt : events) {
                if (!p2Evt.is_valid && p2Evt.min_dist_mas > 300000.0) continue; // Export only interesting events (< 300")
                
                OccultationEvent outEvt;
                // Bind Asteroid
                outEvt.asteroid = currentElements;
                outEvt.asteroidDistanceAu = p2Evt.asteroid_distance_au;
                
                // Besselian elements
                outEvt.besselianX = p2Evt.besselian_x;
                outEvt.besselianY = p2Evt.besselian_y;
                outEvt.besselianDX = p2Evt.besselian_dx;
                outEvt.besselianDY = p2Evt.besselian_dy;
                
                // Substellar/Subsolar
                outEvt.substellarLon = p2Evt.substellar_lon_deg;
                outEvt.substellarLat = p2Evt.substellar_lat_deg;
                outEvt.subsolarLon = p2Evt.subsolar_lon_deg;
                outEvt.subsolarLat = p2Evt.subsolar_lat_deg;
                
                // Apparent star
                outEvt.starAppRA = p2Evt.star_app_ra_deg;
                outEvt.starAppDec = p2Evt.star_app_dec_deg;
                
                // Bind Star (Find mag from candidates)
                outEvt.star.sourceId = std::to_string(p2Evt.star_id);
                outEvt.star.pos.ra = p2Evt.star_ra_deg * M_PI / 180.0;
                outEvt.star.pos.dec = p2Evt.star_dec_deg * M_PI / 180.0;
                
                // Find mag
                auto it = std::find_if(p1Results.candidates.begin(), p1Results.candidates.end(),
                    [&](const CandidateStar& c) { return c.source_id == p2Evt.star_id; });
                if (it != p1Results.candidates.end()) {
                    outEvt.star.phot_g_mean_mag = it->phot_g_mean_mag;
                }
                
                // Bind Geometry
                outEvt.timeCA = ioccultcalc::JulianDate::fromMJD(p2Evt.t_ca_mjd);
                outEvt.closeApproachDistance = p2Evt.min_dist_mas / 1000.0;
                outEvt.maxDuration = p2Evt.duration_sec;
                
                double sMag = outEvt.star.phot_g_mean_mag;
                double aMag = outEvt.asteroid.H; // Simplified for G-mag
                if (aMag > 0 && sMag > 0) {
                    double fTotal = std::pow(10.0, -0.4 * sMag) + std::pow(10.0, -0.4 * aMag);
                    double mTotal = -2.5 * std::log10(fTotal);
                    outEvt.magnitudeDrop = aMag - mTotal;
                }
                
                outEvt.eventId = "OCC_" + std::to_string(p2Evt.star_id) + "_" + std::to_string((long)p2Evt.t_ca_mjd);
                outEvt.pathWidth = currentElements.diameter; 
                if (outEvt.pathWidth <= 0) outEvt.pathWidth = 0.0; // Unknown
                
                // Ensure designation is set
                if (outEvt.asteroid.designation.empty()) {
                    outEvt.asteroid.designation = std::to_string(currentElements.number);
                }

                // Bind Shadow Path
                std::vector<ioccultcalc::ShadowPathPoint> xmlShadowPath;
                for (const auto& pt : p2Evt.shadow_path) {
                    ioccultcalc::ShadowPathPoint xmlPt;
                    xmlPt.time = ioccultcalc::JulianDate::fromMJD(pt.mjd_tdb);
                    xmlPt.location.latitude = pt.lat_deg * M_PI / 180.0;
                    xmlPt.location.longitude = pt.lon_deg * M_PI / 180.0;
                    xmlPt.location.altitude = 0.0;
                    xmlShadowPath.push_back(xmlPt);
                }
                outEvt.shadowPath = xmlShadowPath;
                
                exportEvents.push_back(outEvt);
            }
            
            if (!exportEvents.empty()) {
                ioccultcalc::Occult4XMLHandler xmlHandler;
                // Set options...
                ioccultcalc::Occult4XMLHandler::XMLOptions opts;
                opts.observerName = "ITALOccultCalc User";
                xmlHandler.setOptions(opts);
                
                std::string xmlFilename = "italoccultcalc_output.xml";
                bool exportSuccess = xmlHandler.exportMultipleToXML(exportEvents, xmlFilename);
                if (exportSuccess) {
                    std::cout << "✅ Results exported to XML: " << xmlFilename << std::endl;
                } else {
                    std::cerr << "❌ Failed to export XML." << std::endl;
                }
            }

            // --- Card Generation ---
            if (!exportEvents.empty() && (!a4CardFile.empty() || !ioccultCardFile.empty())) {
                std::cout << "\nGenerating Event Cards..." << std::endl;
                
                // Initialize OutputManager
                ConfigManager cfg;
                if (!configFile.empty()) cfg.loadFromJson(configFile);
                OutputManager outMgr(cfg);
                
                // Convert OccultationEvent to OutputManager::OutputEvent
                std::vector<OutputEvent> outEvents;
                for (const auto& ev : exportEvents) {
                    OutputEvent outEv;
                    outEv.asteroid_number = ev.asteroid.number;
                    outEv.asteroid_name = ev.asteroid.name;
                    outEv.star_id = ev.star.sourceId;
                    outEv.star_ra_deg = ev.star.pos.ra * 180.0/M_PI;
                    outEv.star_dec_deg = ev.star.pos.dec * 180.0/M_PI;
                    outEv.star_mag = ev.star.phot_g_mean_mag;
                    outEv.jd_event = ev.timeCA.jd;
                    
                    // Format time string (placeholder)
                    outEv.utc_string = "2026-01-XX"; 

                    outEv.duration_seconds = ev.maxDuration;
                    outEv.mag_drop = ev.star.phot_g_mean_mag - ev.asteroid.H; // Simplified
                    
                    // Path and Limits
                    for (const auto& p : ev.shadowPath) {
                        outEv.central_path.push_back({p.location.latitude, p.location.longitude});
                        outEv.north_limit.push_back({p.north_limit.latitude, p.north_limit.longitude});
                        outEv.south_limit.push_back({p.south_limit.latitude, p.south_limit.longitude});
                        outEv.north_margin.push_back({p.north_margin.latitude, p.north_margin.longitude});
                        outEv.south_margin.push_back({p.south_margin.latitude, p.south_margin.longitude});
                    }
                    
                    outEv.star_apparent_ra_deg = ev.starAppRA * 180.0/M_PI;
                    outEv.star_apparent_dec_deg = ev.starAppDec * 180.0/M_PI;
                    
                    outEvents.push_back(outEv);
                }

                if (!outEvents.empty()) {
                   if (!a4CardFile.empty()) {
                       auto opts = outMgr.getOptions();
                       opts.format = OutputFormat::A4_VERTICAL_CARD;
                       outMgr.setOptions(opts);
                       outMgr.writeEvents(outEvents, a4CardFile);
                   }
                   if (!ioccultCardFile.empty()) {
                       auto opts = outMgr.getOptions();
                       opts.format = OutputFormat::IOCCULT_CARD;
                       outMgr.setOptions(opts);
                       outMgr.writeEvents(outEvents, ioccultCardFile);
                   }
                }
            }
            
        } else {
             std::cout << "No candidates found in Phase 1.\n";
        }
        


        std::cout << "\nIOccultCalc Engine Workflow completed.\n";
        return 0;

    } catch (const std::exception& e) {
        std::cerr << "\n✗ CRITICAL ERROR: " << e.what() << "\n\n";
        return 1;
    }
}
