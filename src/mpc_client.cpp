#include "ioccultcalc/mpc_client.h"
#include "ioccultcalc/time_utils.h"
#include <curl/curl.h>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <regex>

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
    // URL per scaricare osservazioni dal MPC
    // Formato: https://www.minorplanetcenter.net/db_search/show_object?object_id=433
    
    std::string url = pImpl->baseURL + "db_search/show_object?object_id=" + designation 
                     + "&obs_display=obs";
    
    std::string content = pImpl->httpGet(url);
    
    // Parse HTML e estrai osservazioni in formato MPC
    ObservationSet obsSet;
    obsSet.objectDesignation = designation;
    
    // Parsing semplificato - in realtà il formato è più complesso
    std::istringstream iss(content);
    std::string line;
    
    while (std::getline(iss, line)) {
        // Cerca linee che sembrano osservazioni MPC (80 colonne)
        if (line.length() >= 80 && line[14] == ' ') {
            try {
                auto obs = parseMPC80Line(line);
                obsSet.observations.push_back(obs);
            } catch (...) {
                // Ignora linee non parsabili
            }
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
    char sign = line[44];
    int decD = std::stoi(line.substr(45, 2));
    int decM = std::stoi(line.substr(48, 2));
    double decS = std::stod(line.substr(51, 4));
    double decDeg = decD + decM / 60.0 + decS / 3600.0;
    if (sign == '-') decDeg = -decDeg;
    obs.obs.dec = decDeg * DEG_TO_RAD;
    
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
