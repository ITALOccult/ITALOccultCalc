#include "ioccultcalc/mpc_client.h"
#include "ioccultcalc/time_utils.h"
#include <curl/curl.h>
#include <sstream>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <regex>
#include "astdyn/io/parsers/AstDysRWOParser.hpp"

namespace ioccultcalc {

static size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* output) {
    size_t totalSize = size * nmemb;
    output->append(static_cast<char*>(contents), totalSize);
    return totalSize;
}

class MPCClient::Impl {
public:
    std::string baseURL;
    int timeout;
    CURL* curl;
    
    Impl() : baseURL("https://www.minorplanetcenter.net/"), timeout(60) {
        curl_global_init(CURL_GLOBAL_DEFAULT);
        curl = curl_easy_init();
    }
    
    ~Impl() {
        if (curl) {
            curl_easy_cleanup(curl);
        }
        curl_global_cleanup();
    }
    
    std::string httpGet(const std::string& url) {
        if (!curl) {
            throw std::runtime_error("CURL not initialized");
        }
        
        std::string response;
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, timeout);
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
        
        CURLcode res = curl_easy_perform(curl);
        
        if (res != CURLE_OK) {
            throw std::runtime_error(std::string("HTTP request failed: ") + 
                                   curl_easy_strerror(res));
        }
        
        return response;
    }
};

MPCClient::MPCClient() : pImpl(new Impl()) {}

MPCClient::~MPCClient() = default;

void MPCClient::setBaseURL(const std::string& url) {
    pImpl->baseURL = url;
    if (pImpl->baseURL.back() != '/') {
        pImpl->baseURL += '/';
    }
}

void MPCClient::setTimeout(int seconds) {
    pImpl->timeout = seconds;
}

ObservationSet MPCClient::getObservations(const std::string& designation) {
    // Prova prima con AstDyS per asteroidi numerati
    // Struttura: https://newton.spacedys.com/~astdys2/mpcobs/numbered/<num/1000>/<num>.rwo
    
    ObservationSet obsSet;
    obsSet.objectDesignation = designation;
    
    // Se è un numero, prova AstDyS (RWO è molto più affidabile per il fit)
    if (std::all_of(designation.begin(), designation.end(), ::isdigit)) {
        try {
            int asteroidNumber = std::stoi(designation);
            int dirNumber = asteroidNumber / 1000;
            
            std::string astdysURL = "https://newton.spacedys.com/~astdys2/mpcobs/numbered/" 
                                   + std::to_string(dirNumber) + "/" + designation + ".rwo";
            
            std::cout << "[MPCClient] Downloading observations from AstDyS: " << astdysURL << std::endl;
            std::string content = pImpl->httpGet(astdysURL);
            
            if (content.empty() || content.find("404 Not Found") != std::string::npos) {
                throw std::runtime_error("RWO file not found on AstDyS");
            }

            // Salva su file temporaneo per usare il parser di AstDyn
            std::string tempFile = "/tmp/ioc_temp_" + designation + ".rwo";
            {
                std::ofstream ofs(tempFile);
                ofs << content;
            }
            
            std::cout << "[MPCClient] Parsing RWO with native library parser..." << std::endl;
            ObservationSet obsSetFromRWO = loadFromRWOFile(tempFile);
            std::remove(tempFile.c_str());
            
            if (!obsSetFromRWO.observations.empty()) {
                std::cout << "[MPCClient] Successfully loaded " << obsSetFromRWO.observations.size() 
                          << " observations from RWO." << std::endl;
                return obsSetFromRWO;
            }
        } catch (const std::exception& e) {
            std::cerr << "MPCClient: AstDyS RWO download/parse failed: " << e.what() << std::endl;
        }
    }
    
    // Fallback: Se RWO fallisce o non è numerato, prova MPC ma LOGGA ATTENZIONE
    std::cout << "[MPCClient] WARNING: Falling back to MPC HTML scraper (EXPERIMENTAL)" << std::endl;
    
    std::string url = pImpl->baseURL + "db_search/show_object?object_id=" + designation 
                     + "&obs_display=obs";
    
    std::string content = pImpl->httpGet(url);
    
    std::istringstream iss(content);
    std::string line;
    
    while (std::getline(iss, line)) {
        // Cerca linee che sembrano osservazioni MPC (80 colonne standard)
        // NOTA: Il parser MPC80 qui è fragile se il contenuto HTML è sporco.
        if (line.length() >= 80 && (line[14] == ' ' || line[14] == '*')) {
            try {
                auto obs = parseMPC80Line(line);
                obsSet.observations.push_back(obs);
            } catch (...) { }
        }
    }
    
    obsSet.computeStatistics();
    return obsSet;
}

ObservationSet MPCClient::getObservations(const std::string& designation,
                                         const JulianDate& startDate,
                                         const JulianDate& endDate) {
    // Scarica tutte le osservazioni e filtra
    ObservationSet allObs = getObservations(designation);
    
    ObservationSet filtered;
    filtered.objectDesignation = designation;
    
    for (const auto& obs : allObs.observations) {
        if (obs.epoch.jd >= startDate.jd && obs.epoch.jd <= endDate.jd) {
            filtered.observations.push_back(obs);
        }
    }
    
    filtered.computeStatistics();
    return filtered;
}

ObservationSet MPCClient::getRecentObservations(const std::string& designation, int n) {
    ObservationSet allObs = getObservations(designation);
    
    if (allObs.observations.size() <= (size_t)n) {
        return allObs;
    }
    
    // allObs.computeStatistics() ordina per tempo (solitamente) - verifichiamo observation.cpp
    // Assumiamo che siano ordinate per tempo crescente. Prendiamo le ultime n.
    ObservationSet filtered;
    filtered.objectDesignation = designation;
    
    size_t start = allObs.observations.size() - n;
    for (size_t i = start; i < allObs.observations.size(); ++i) {
        filtered.observations.push_back(allObs.observations[i]);
    }
    
    filtered.computeStatistics();
    return filtered;
}

ObservationSet MPCClient::getObservationsFromWebService(const std::string& designation) {
    // Servizio alternativo via API
    std::string url = pImpl->baseURL + "web_service/get_observations?name=" + designation;
    
    std::string content = pImpl->httpGet(url);
    
    ObservationSet obsSet;
    obsSet.objectDesignation = designation;
    
    // Parse response (potrebbe essere JSON o plain text)
    std::istringstream iss(content);
    std::string line;
    
    while (std::getline(iss, line)) {
        if (line.length() >= 80) {
            try {
                auto obs = parseMPC80Line(line);
                obsSet.observations.push_back(obs);
            } catch (...) {}
        }
    }
    
    obsSet.computeStatistics();
    return obsSet;
}

ObservationSet MPCClient::loadFromFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + filename);
    }
    
    ObservationSet obsSet;
    std::string line;
    
    while (std::getline(file, line)) {
        if (line.length() >= 80 && line[0] != '#') {
            try {
                auto obs = parseMPC80Line(line);
                
                if (obsSet.objectDesignation.empty()) {
                    // Estrai designation dalla prima osservazione
                    obsSet.objectDesignation = line.substr(0, 12);
                    // Trim spaces
                    obsSet.objectDesignation.erase(
                        obsSet.objectDesignation.find_last_not_of(" \t\n\r") + 1);
                }
                
                obsSet.observations.push_back(obs);
            } catch (...) {}
        }
    }
    
    obsSet.computeStatistics();
    return obsSet;
}

ObservationSet MPCClient::loadFromRWOFile(const std::string& filename) {
    try {
        astdyn::io::parsers::AstDysRWOParser parser;
        auto astdynObs = parser.parse(filename);
        
        ObservationSet obsSet;
        if (astdynObs.empty()) return obsSet;
        
        obsSet.objectDesignation = astdynObs[0].object_name;
        
        for (const auto& ao : astdynObs) {
            AstrometricObservation obs;
            obs.epoch = JulianDate::fromMJD(ao.mjd_utc);
            obs.obs.ra = ao.ra;
            obs.obs.dec = ao.dec;
            obs.magnitude = ao.mag;
            obs.observatoryCode = ao.obs_code;
            obs.raError = ao.sigma_ra;
            obs.decError = ao.sigma_dec;
            
            // Carica info osservatorio
            Observatory observatory = Observatory::fromMPCCode(obs.observatoryCode);
            obs.observerLocation = observatory.location;
            
            obsSet.observations.push_back(obs);
        }
        
        obsSet.computeStatistics();
        return obsSet;
        
    } catch (const std::exception& e) {
        std::cerr << "MPCClient: Errore caricamento file RWO (" << filename << "): " << e.what() << std::endl;
        throw;
    }
}

bool MPCClient::saveToFile(const ObservationSet& observations, const std::string& filename) {
    std::ofstream file(filename);
    if (!file.is_open()) {
        return false;
    }
    
    // Formato MPC 80 colonne
    for (const auto& obs : observations.observations) {
        // Implementazione semplificata - formato completo è complesso
        file << std::left << std::setw(12) << observations.objectDesignation;
        file << " "; // Note
        file << " "; // Discovery flag
        
        // Data
        int year, month, day, hour, minute;
        double second;
        TimeUtils::jdToCalendar(obs.epoch, year, month, day, hour, minute, second);
        
        file << std::setw(4) << year << " ";
        file << std::setfill('0') << std::setw(2) << month << " ";
        file << std::setw(5) << std::setprecision(2) << std::fixed 
             << (day + (hour + minute/60.0 + second/3600.0)/24.0);
        file << std::setfill(' ');
        
        // RA
        double raHours = obs.obs.ra * RAD_TO_DEG / 15.0;
        int raH = (int)raHours;
        int raM = (int)((raHours - raH) * 60);
        double raS = ((raHours - raH) * 60 - raM) * 60;
        
        file << std::setw(3) << raH << " ";
        file << std::setfill('0') << std::setw(2) << raM << " ";
        file << std::setw(5) << std::setprecision(2) << std::fixed << raS;
        file << std::setfill(' ');
        
        // Dec
        double decDeg = obs.obs.dec * RAD_TO_DEG;
        char sign = decDeg >= 0 ? '+' : '-';
        decDeg = fabs(decDeg);
        int decD = (int)decDeg;
        int decM = (int)((decDeg - decD) * 60);
        double decS = ((decDeg - decD) * 60 - decM) * 60;
        
        file << sign;
        file << std::setfill('0') << std::setw(2) << decD << " ";
        file << std::setw(2) << decM << " ";
        file << std::setw(4) << std::setprecision(1) << std::fixed << decS;
        file << std::setfill(' ');
        
        // Magnitudine e codice osservatorio
        file << "         "; // Spaces
        if (obs.magnitude < 99) {
            file << std::setw(4) << std::setprecision(1) << std::fixed << obs.magnitude;
        } else {
            file << "    ";
        }
        file << "      ";
        file << obs.observatoryCode;
        
        file << "\n";
    }
    
    return true;
}

bool MPCClient::downloadObservatoryCodes(const std::string& outputFile) {
    std::string url = pImpl->baseURL + "iau/lists/ObsCodes.html";
    
    try {
        std::string content = pImpl->httpGet(url);
        
        std::ofstream file(outputFile);
        if (!file.is_open()) {
            return false;
        }
        
        file << content;
        return true;
        
    } catch (...) {
        return false;
    }
}

AstrometricObservation MPCClient::parseRWOLine(const std::string& line) {
    // Formato .rwo di AstDyS (custom format, non MPC standard)
    // Lunghezza tipica: ~197 caratteri
    // ! Design   K T N YYYY MM DD.dddddddddd   Accuracy HH MM SS.sss  Accuracy      RMS  F     Bias    Resid sDD MM SS.ss  Accuracy      RMS  F     Bias    Resid Val  B   RMS  Resid Cat Cod       Chi A M
    //     433       O A   1893 10 29.4132        1.000E-04 06 08 59.320  1.500E-01    2.000 F    0.000   -0.493 +53 39 04.20  1.000E-01    2.000 F    0.000    1.114                         802      0.61 1 0
    
    if (line.length() < 150) {
        throw std::runtime_error("Line too short for RWO format");
    }
    
    AstrometricObservation obs;
    
    // Design (colonne 0-10 circa)
    // K T N (colonne 14-20)
    // Data: YYYY MM DD.dddddddddd (colonne ~22-42)
    // RA: HH MM SS.sss (colonne ~60-72)
    // Dec: sDD MM SS.ss (colonne ~100-112)
    // Cod: observatory code (colonne ~130-133)
    
    try {
        // Formato Fortran .rwo - colonne fisse basate su header:
        // ! Design   K T N YYYY MM DD.dddddddddd   Accuracy HH MM SS.sss...
        // Cols 1-10:   Design (numero asteroide con spazi)
        // Cols 11-20:  K T N (tipo osservazione)
        // Cols 21-40:  YYYY MM DD.dddddddddd (data)
        // Cols 51-62:  HH MM SS.sss (RA)
        // Cols 101-112: sDD MM SS.ss (Dec)
        // Cols 181-183: Cod (codice osservatorio)
        
        // Parsing data: colonne 18-38 (Fortran 1-based) = substr(17, 21) (C++ 0-based)
        // Esempio: "1893 10 29.4132    " oppure "2023 07 04.244976  "
        std::string dateStr = line.substr(17, 21);
        std::istringstream dateStream(dateStr);
        
        int year, month;
        double day;
        dateStream >> year >> month >> day;
        
        if (year < 1800 || year > 2100 || month < 1 || month > 12 || day <= 0 || day >= 32) {
            throw std::runtime_error("Invalid date values in RWO");
        }
        
        int dayInt = (int)day;
        double dayFrac = day - dayInt;
        obs.epoch = TimeUtils::calendarToJD(year, month, dayInt, 0, 0, dayFrac * 86400.0);
        
        // Parsing RA: colonne 51-63 (Fortran 1-based) = substr(50, 13) (C++ 0-based)
        // Esempio: "06 08 59.320"
        std::string raStr = line.substr(50, 13);
        std::istringstream raStream(raStr);
        
        int raH, raM;
        double raS;
        raStream >> raH >> raM >> raS;
        
        if (raH < 0 || raH >= 24 || raM < 0 || raM >= 60) {
            throw std::runtime_error("Invalid RA in RWO");
        }
        
        double raHours = raH + raM / 60.0 + raS / 3600.0;
        obs.obs.ra = raHours * 15.0 * DEG_TO_RAD;
        
        // Parsing Dec: robustly handle signs and leading spaces
        std::string decStrFull = line.substr(100, 20); // Get a slice that definitely contains Dec
        size_t first_non_space = decStrFull.find_first_not_of(" \t");
        if (first_non_space == std::string::npos) {
            throw std::runtime_error("Empty Dec field in RWO");
        }
        
        char sign = decStrFull[first_non_space];
        std::string numeric_dec = decStrFull.substr(first_non_space + 1);
        std::istringstream decStream(numeric_dec);
        int decD = 0, decM = 0;
        double decS = 0.0;
        
        if (!(decStream >> decD >> decM >> decS)) {
            // Fallback: try reading the sign as part of the number
            std::istringstream decStream2(decStrFull.substr(first_non_space));
            if (!(decStream2 >> decD >> decM >> decS)) {
                throw std::runtime_error("Invalid Dec numeric format in RWO");
            }
            // If decStream2 worked, decD already has the sign.
            double decDeg = std::abs(decD) + decM / 60.0 + decS / 3600.0;
            if (decD < 0 || sign == '-') decDeg = -decDeg;
            obs.obs.dec = decDeg * M_PI / 180.0;
        } else {
            double decDeg = decD + decM / 60.0 + decS / 3600.0;
            if (sign == '-') decDeg = -decDeg;
            obs.obs.dec = decDeg * M_PI / 180.0;
        }
        
        // Codice osservatorio: posizione 180-182 (1-based) = substr(179, 3) (0-based)
        // Esempio: "802" per Heidelberg
        if (line.length() >= 182) {
            obs.observatoryCode = line.substr(179, 3);
            // Trim spazi
            obs.observatoryCode.erase(0, obs.observatoryCode.find_first_not_of(" \t"));
            obs.observatoryCode.erase(obs.observatoryCode.find_last_not_of(" \t") + 1);
        }
        
        // Errori tipici per osservazioni moderne
        obs.raError = 0.5;  // arcsec
        obs.decError = 0.5; // arcsec
        
        // Carica info osservatorio se disponibile
        if (!obs.observatoryCode.empty()) {
            Observatory observatory = Observatory::fromMPCCode(obs.observatoryCode);
            obs.observerLocation = observatory.location;
        }
        
    } catch (const std::exception& e) {
        throw std::runtime_error(std::string("Error parsing RWO line: ") + e.what());
    }
    
    return obs;
}

AstrometricObservation MPCClient::parseMPC80Line(const std::string& line) {
    // Formato MPC 80 colonne standard
    // Columns 1-12: Object designation
    // Column 14: Discovery flag
    // Column 15: Note 1
    // Column 16: Note 2
    // Columns 16-32: Date (YYYY MM DD.dddddd)
    // Columns 33-44: RA (HH MM SS.sss)
    // Columns 45-56: Dec (sDD MM SS.ss)
    // Columns 66-70: Magnitude
    // Column 71: Band
    // Column 72: Catalog
    // Columns 78-80: Observatory code
    
    if (line.length() < 80) {
        throw std::runtime_error("Line too short for MPC format");
    }
    
    AstrometricObservation obs;
    
    // Discovery flag e note
    obs.discoveryFlag = line.substr(12, 1);
    obs.note1 = line.substr(13, 1);
    obs.note2 = line.substr(14, 1);
    
    // Data
    int year = std::stoi(line.substr(15, 4));
    int month = std::stoi(line.substr(20, 2));
    double day = std::stod(line.substr(23, 9));
    
    obs.epoch = TimeUtils::calendarToJD(year, month, (int)day, 0, 0, 
                                        (day - (int)day) * 86400.0);
    
    // RA (HH MM SS.sss)
    int raH = std::stoi(line.substr(32, 2));
    int raM = std::stoi(line.substr(35, 2));
    double raS = std::stod(line.substr(38, 5));
    double raHours = raH + raM / 60.0 + raS / 3600.0;
    obs.obs.ra = raHours * 15.0 * DEG_TO_RAD;
    
    // Dec (sDD MM SS.ss)
    // MPC format is strict: sign is at column 44 (0-indexed)
    char sign = line[44];
    int decD = std::stoi(line.substr(45, 2));
    int decM = std::stoi(line.substr(48, 2));
    double decS = std::stod(line.substr(51, 4));
    
    // Correctly handle negative declination including -00
    double decDeg = decD + decM / 60.0 + decS / 3600.0;
    if (sign == '-') decDeg = -decDeg;
    obs.obs.dec = decDeg * M_PI / 180.0;
    
    // Magnitudine (se presente)
    std::string magStr = line.substr(65, 5);
    if (magStr.find_first_not_of(" ") != std::string::npos) {
        obs.magnitude = std::stod(magStr);
    }
    
    // Catalogo
    if (line.length() > 71) {
        obs.catalogCode = line.substr(71, 1);
    }
    
    // Codice osservatorio
    obs.observatoryCode = line.substr(77, 3);
    
    // Carica info osservatorio
    Observatory observatory = Observatory::fromMPCCode(obs.observatoryCode);
    obs.observerLocation = observatory.location;
    
    // Stima errori tipici basati sul catalogo
    if (obs.catalogCode == "V" || obs.catalogCode == "W") {
        // Gaia - molto preciso
        obs.raError = 0.1;
        obs.decError = 0.1;
    } else if (obs.catalogCode == "t" || obs.catalogCode == "U") {
        // 2MASS, UCAC - buono
        obs.raError = 0.3;
        obs.decError = 0.3;
    } else {
        // Altri cataloghi - standard
        obs.raError = 1.0;
        obs.decError = 1.0;
    }
    
    return obs;
}

std::vector<AstrometricObservation> MPCClient::parseADES(const std::string& content) {
    // ADES (Astrometric Data Exchange Standard) - formato XML o JSON
    // Implementazione semplificata - formato completo è molto più complesso
    
    std::vector<AstrometricObservation> observations;
    
    // TODO: Implementare parser ADES completo
    // Per ora restituisce vettore vuoto
    
    return observations;
}

} // namespace ioccultcalc
