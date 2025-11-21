#include "ioccultcalc/observation.h"
#include <cmath>
#include <algorithm>
#include <numeric>
#include <set>
#include <map>
#include <fstream>
#include <iomanip>
#include <stdexcept>
#include <sstream>
#include <regex>
#include <cctype>

namespace ioccultcalc {

void ObservationSet::computeStatistics() {
    if (observations.empty()) return;
    
    numberOfObservations = observations.size();
    
    // Trova prima e ultima osservazione
    auto minmax = std::minmax_element(observations.begin(), observations.end(),
        [](const AstrometricObservation& a, const AstrometricObservation& b) {
            return a.epoch.jd < b.epoch.jd;
        });
    
    firstObservation = minmax.first->epoch;
    lastObservation = minmax.second->epoch;
    arcLength = lastObservation.jd - firstObservation.jd;
    
    // Conta osservatori unici
    std::set<std::string> uniqueObs;
    for (const auto& obs : observations) {
        uniqueObs.insert(obs.observatoryCode);
    }
    numberOfObservatories = uniqueObs.size();
}

void ObservationSet::filterOutliers(double sigmaThreshold) {
    if (observations.size() < 3) return;
    
    // Calcola RMS attuale
    double rms = getRMSResidual();
    
    // Marca outliers
    for (auto& obs : observations) {
        if (obs.totalResidual > sigmaThreshold * rms) {
            obs.outlier = true;
        }
    }
}

std::vector<AstrometricObservation> ObservationSet::getObservationsInRange(
    const JulianDate& start, const JulianDate& end) const {
    
    std::vector<AstrometricObservation> result;
    
    for (const auto& obs : observations) {
        if (obs.epoch.jd >= start.jd && obs.epoch.jd <= end.jd && !obs.outlier) {
            result.push_back(obs);
        }
    }
    
    return result;
}

double ObservationSet::getRMSResidual() const {
    if (observations.empty()) return 0.0;
    
    double sumSq = 0.0;
    int count = 0;
    
    for (const auto& obs : observations) {
        if (!obs.outlier) {
            sumSq += obs.totalResidual * obs.totalResidual;
            count++;
        }
    }
    
    return count > 0 ? sqrt(sumSq / count) : 0.0;
}

double ObservationSet::getRAResidualRMS() const {
    if (observations.empty()) return 0.0;
    
    double sumSq = 0.0;
    int count = 0;
    
    for (const auto& obs : observations) {
        if (!obs.outlier) {
            sumSq += obs.raResidual * obs.raResidual;
            count++;
        }
    }
    
    return count > 0 ? sqrt(sumSq / count) : 0.0;
}

double ObservationSet::getDecResidualRMS() const {
    if (observations.empty()) return 0.0;
    
    double sumSq = 0.0;
    int count = 0;
    
    for (const auto& obs : observations) {
        if (!obs.outlier) {
            sumSq += obs.decResidual * obs.decResidual;
            count++;
        }
    }
    
    return count > 0 ? sqrt(sumSq / count) : 0.0;
}

// Cache statica per evitare download ripetuti
static std::map<std::string, Observatory> observatoryCache;
static bool observatoryCacheLoaded = false;

// Parse un file ObsCodes.html dal MPC
static bool parseObsCodesHTML(const std::string& html) {
    std::istringstream stream(html);
    std::string line;
    
    // Cerca la sezione con i codici (dopo "<pre>")
    bool inPreSection = false;
    int parsed = 0;
    
    while (std::getline(stream, line)) {
        if (!inPreSection) {
            if (line.find("<pre>") != std::string::npos || 
                line.find("<PRE>") != std::string::npos) {
                inPreSection = true;
            }
            continue;
        }
        
        if (line.find("</pre>") != std::string::npos ||
            line.find("</PRE>") != std::string::npos) {
            break;
        }
        
        // Parse formato: "CODE LON RHO_COS_PHI RHO_SIN_PHI NAME"
        // Ma attenzione: i numeri possono essere attaccati!
        // Es: "C4  10.5417 0.71938 +0.69251 Nome"
        
        if (line.length() < 4) continue;
        
        std::string code = line.substr(0, 3);
        // Rimuovi spazi
        code.erase(std::remove(code.begin(), code.end(), ' '), code.end());
        
        if (code.empty() || !std::isalnum(code[0])) continue;
        
        // Parse numeri: cerca pattern "digit.digit"
        std::regex numPattern(R"(([\d\-\+\.]+)\s+([\d\-\+\.]+)\s+([\d\-\+\.]+))");
        std::smatch match;
        std::string restOfLine = line.substr(3);
        
        if (std::regex_search(restOfLine, match, numPattern)) {
            try {
                double lon = std::stod(match[1].str());
                double rho_cos = std::stod(match[2].str());
                double rho_sin = std::stod(match[3].str());
                
                Observatory obs;
                obs.code = code;
                obs.name = "MPC " + code;  // Nome estratto dopo
                obs.isSpacecraft = false;
                obs.rho_cos_phi = rho_cos;
                obs.rho_sin_phi = rho_sin;
                
                // Coordinate geografiche approssimate
                double lat = atan2(rho_sin, rho_cos) * 180.0 / M_PI;
                double alt = sqrt(rho_cos*rho_cos + rho_sin*rho_sin) * 6378.137 - 6378.137;
                obs.location = GeographicCoordinates(lon, lat, alt * 1000.0);
                
                observatoryCache[code] = obs;
                parsed++;
                
            } catch (...) {
                // Ignora errori di parsing
            }
        }
    }
    
    return parsed > 0;
}

// Carica la lista completa di osservatori MPC
static void loadObservatoryCodes() {
    if (observatoryCacheLoaded) return;
    
    // Prova a scaricare da MPC
    // TODO: Implementare download HTTP (per ora usa cache minimale)
    
    auto addObs = [](const std::string& code, const std::string& name,
                     double lon, double rho_cos_phi, double rho_sin_phi) {
        Observatory obs;
        obs.code = code;
        obs.name = name;
        obs.isSpacecraft = false;
        
        // Parametri MPC: coordinate geocentriche equatoriali
        // ρ·cos(φ') = componente nel piano equatoriale (in raggi terrestri)
        // ρ·sin(φ') = componente lungo asse polare (in raggi terrestri)
        // Riferimento: raggio equatoriale = 6378.137 km
        obs.rho_cos_phi = rho_cos_phi;
        obs.rho_sin_phi = rho_sin_phi;
        
        // Stima coordinate geografiche approssimate (per informazione)
        double lat = atan2(rho_sin_phi, rho_cos_phi) * 180.0 / M_PI;
        double alt = sqrt(rho_cos_phi * rho_cos_phi + rho_sin_phi * rho_sin_phi) * 6378.137 - 6378.137;
        
        obs.location = GeographicCoordinates(lon, lat, alt * 1000.0); // convert km to m
        observatoryCache[code] = obs;
    };
    
    // Geocentro
    addObs("500", "Geocenter", 0.0, 0.0, 0.0);
    
    // Osservatori principali (da ObsCodes.html MPC)
    // Codici storici e moderni più usati
    addObs("000", "Greenwich", 0.0000, 0.62411, 0.77873);
    addObs("008", "Alger", 3.0361, 0.79889, 0.59960);
    addObs("016", "Vienna", 16.3390, 0.66739, 0.74227);
    addObs("027", "Uccle", 4.3575, 0.65531, 0.75316);
    addObs("070", "Cerro Tololo", -70.8150, 0.86682, -0.49665);
    addObs("083", "Kiev", 30.5246, 0.64007, 0.76575);
    addObs("095", "Crimea-Nauchnyi", 34.0160, 0.71172, 0.70024);
    addObs("250", "Haleakala-NEAT/GEODSS", -156.2569, 0.82923, 0.55654);
    addObs("568", "Mauna Kea", -155.4681, 0.82967, 0.55662);
    addObs("644", "Palomar Mountain/NEAT", -116.8622, 0.73779, 0.67352);
    addObs("691", "Kitt Peak-Spacewatch", -111.6003, 0.73898, 0.67168);
    addObs("703", "Catalina Sky Survey", -110.7317, 0.73839, 0.67204);
    addObs("704", "Lincoln ETS New Mexico", -106.3089, 0.74427, 0.66606);
    addObs("802", "Heidelberg", 8.7216, 0.65211, 0.75570);
    
    // Codici recenti alphanumerici (post-2009)
    addObs("B7", "Osservatorio La Dehesilla", -16.5105, 0.80325, 0.59371);
    addObs("C4", "Xingming Mt. Nanshan", 87.1778, 0.72711, 0.68469);
    addObs("C5", "XJLTP Nanshan Stn", 87.1732, 0.72710, 0.68470);
    addObs("C9", "Mt. Lemmon Survey Ext", -110.7889, 0.73771, 0.67293);
    addObs("D0", "MASTER II Tunka", 103.0670, 0.61962, 0.78241);
    addObs("D4", "Winer Observatory", -110.5992, 0.85211, 0.52205);
    addObs("D6", "Camarillo", -119.0392, 0.82775, 0.55922);
    addObs("D7", "San Clemente", -117.6093, 0.83553, 0.54762);
    addObs("G0", "Observatorio Carmelita", 2.2636, 0.75026, 0.65897);
    addObs("G1", "Cerro Paranal", -70.4044, 0.90994, -0.41434);
    addObs("G3", "San Emigdio Peak", -119.1762, 0.82540, 0.56279);
    addObs("G9", "Catalina-Mt Bigelow", -110.7322, 0.84531, 0.53321);
    addObs("J3", "OAI Kryoneri", 23.8628, 0.78943, 0.61203);
    addObs("J5", "Observatoire Haute Provence", 5.7136, 0.72157, 0.69014);
    addObs("J6", "Liverpool Telescope La Palma", -17.8792, 0.87770, 0.47838);
    addObs("J8", "Observatorio Valinhos", -46.9656, 0.92108, -0.38842);
    addObs("K1", "Piszkesteto", 19.8937, 0.67154, 0.73869);
    addObs("K7", "Xingming Ussuriysk", 132.1656, 0.72418, 0.68737);
    addObs("K8", "ISON Blagoveschensk", 127.4820, 0.63981, 0.76600);
    addObs("L0", "St.Andrews", -2.8303, 0.55559, 0.82867);
    addObs("L6", "Melbourne", 144.9758, 0.79082, -0.61001);
    addObs("L7", "Lemmon SkyCenter", -110.7888, 0.84640, 0.53095);
    addObs("L9", "Cerro Tololo LCO", -70.8046, 0.86558, -0.49976);
    addObs("M2", "Mayhill NM Skies", -105.5288, 0.84071, 0.54031);
    addObs("M4", "Dauban Observatory", 5.6469, 0.72058, 0.69119);
    addObs("M5", "BOOTES-1 Mazagon", -6.7348, 0.79853, 0.59997);
    addObs("M6", "Stardreams Valenii", 26.0456, 0.70582, 0.70610);
    addObs("O5", "WFST Lenghu", 93.8952, 0.78297, 0.62102);
    addObs("R1", "La Palma Roque Muchachos", -17.8792, 0.87770, 0.47838);
    addObs("R6", "Auckland Mt John", 170.4650, 0.72077, -0.69108);
    addObs("T0", "Haleakala LCO OGG", -156.2569, 0.93624, 0.35154);
    addObs("U5", "iTelescope Auberry", -119.4128, 0.79904, 0.59962);
    addObs("U7", "Table Mountain Wrightwood", -117.6817, 0.82647, 0.56172);
    addObs("V7", "McDonald Observatory LCO ELP", -104.0153, 0.86105, 0.50743);
    addObs("V9", "Starfront Rockwood", -99.3827, 0.85305, 0.52021);
    addObs("W3", "Transit Dreams Campobello", -82.1655, 0.81932, 0.57150);
    addObs("W6", "Atacama SPECULOOS Paranal", -70.4032, 0.91001, -0.41415);
    addObs("X0", "Atacama CAO San Pedro", -68.1800, 0.92165, -0.38770);
    addObs("X1", "Atacama VLT Paranal", -70.4042, 0.90995, -0.41432);
    addObs("X3", "Atacama MAPS San Pedro", -68.1801, 0.92165, -0.38771);
    addObs("Y0", "SONEAR 2 Belo Horizonte", -43.9842, 0.94089, -0.33799);
    addObs("Y8", "Coimbra OAI Frengual", -6.6285, 0.78677, 0.61533);
    addObs("Y9", "Atacama SON San Pedro", -68.1801, 0.92164, -0.38771);
    addObs("Z5", "Kitt Peak SARA", -111.5997, 0.84950, 0.52649);
    addObs("Z8", "Northolt Branch London", -0.3683, 0.62380, 0.77957);
    
    // Survey telescopes
    addObs("F51", "Pan-STARRS 1 Haleakala", -156.2569, 0.93624, 0.35154);
    addObs("F52", "Pan-STARRS 2 Haleakala", -156.2569, 0.93624, 0.35154);
    addObs("G96", "Mt Lemmon Survey", -110.7889, 0.73771, 0.67293);
    addObs("J04", "ESA OGS Tenerife", -16.5114, 0.78474, 0.61766);
    
    // Spacecraft (nessuna correzione topocentrica)
    Observatory wise;
    wise.code = "C51";
    wise.name = "WISE";
    wise.isSpacecraft = true;
    wise.rho_cos_phi = 0.0;
    wise.rho_sin_phi = 0.0;
    wise.location = GeographicCoordinates(0, 0, 0);
    observatoryCache["C51"] = wise;
    
    observatoryCacheLoaded = true;
}

Observatory Observatory::fromMPCCode(const std::string& code) {
    loadObservatoryCodes();
    
    Observatory obs;
    obs.code = code;
    obs.isSpacecraft = false;
    obs.rho_cos_phi = 0.0;
    obs.rho_sin_phi = 0.0;
    
    // Cerca nella cache
    auto it = observatoryCache.find(code);
    if (it != observatoryCache.end()) {
        return it->second;
    }
    
    // Non trovato - crea placeholder (usa geocentro)
    obs.name = "Unknown Observatory (" + code + ")";
    obs.location = GeographicCoordinates(0, 0, 0);
    
    return obs;
}

void ObservationSet::saveToFile(const std::string& filename) const {
    std::ofstream out(filename);
    if (!out) {
        throw std::runtime_error("Cannot open file: " + filename);
    }
    
    out << "# Observation Set\n";
    out << "# Number of observations: " << numberOfObservations << "\n";
    out << "# Arc length: " << arcLength << " days\n";
    out << "# First obs: " << firstObservation.jd << "\n";
    out << "# Last obs:  " << lastObservation.jd << "\n";
    out << "# RMS residual: " << getRMSResidual() << " arcsec\n";
    out << "#\n";
    out << "# Format: Date(JD) RA(deg) Dec(deg) RA_res(as) Dec_res(as) Total_res(as) Outlier Observatory\n";
    out << "#\n";
    
    out << std::fixed << std::setprecision(6);
    for (const auto& obs : observations) {
        out << obs.epoch.jd << " "
            << obs.obs.ra * RAD_TO_DEG << " "
            << obs.obs.dec * RAD_TO_DEG << " "
            << obs.raResidual << " "
            << obs.decResidual << " "
            << obs.totalResidual << " "
            << (obs.outlier ? "1" : "0") << " "
            << obs.observatoryCode << "\n";
    }
    
    out.close();
}

} // namespace ioccultcalc
