/**
 * @file jpl_horizons_client.cpp
 * @brief Implementazione client JPL Horizons
 */

#include "ioccultcalc/jpl_horizons_client.h"
#include "ioccultcalc/orbital_elements.h"
#include <curl/curl.h>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <regex>
#include <iostream>
#include <fstream>

namespace ioccultcalc {

// Callback per CURL
static size_t WriteCallback(void* contents, size_t size, size_t nmemb, void* userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

class JPLHorizonsClient::Impl {
public:
    std::string baseURL;
    int timeout;
    std::string email;
    CURL* curl;
    
    Impl() : baseURL("https://ssd.jpl.nasa.gov/api/horizons.api"),
             timeout(30),
             email("") {
        curl_global_init(CURL_GLOBAL_DEFAULT);
        curl = curl_easy_init();
    }
    
    ~Impl() {
        if (curl) curl_easy_cleanup(curl);
        curl_global_cleanup();
    }
    
    std::string performRequest(const std::string& queryParams) {
        if (!curl) {
            throw std::runtime_error("CURL non inizializzato");
        }
        
        std::string url = baseURL + "?" + queryParams;
        std::string response;
        
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, timeout);
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
        
        // User agent
        std::string userAgent = "IOccultCalc/1.0";
        if (!email.empty()) {
            userAgent += " (" + email + ")";
        }
        curl_easy_setopt(curl, CURLOPT_USERAGENT, userAgent.c_str());
        
        CURLcode res = curl_easy_perform(curl);
        
        if (res != CURLE_OK) {
            throw std::runtime_error(std::string("CURL error: ") + 
                                   curl_easy_strerror(res));
        }
        
        return response;
    }
};

JPLHorizonsClient::JPLHorizonsClient() 
    : pImpl(new Impl()) {}

JPLHorizonsClient::~JPLHorizonsClient() = default;

void JPLHorizonsClient::setBaseURL(const std::string& url) {
    pImpl->baseURL = url;
}

void JPLHorizonsClient::setTimeout(int seconds) {
    pImpl->timeout = seconds;
}

void JPLHorizonsClient::setEmail(const std::string& email) {
    pImpl->email = email;
}

std::string JPLHorizonsClient::buildVectorQuery(const std::string& target,
                                               const JulianDate& start,
                                               const JulianDate& stop,
                                               const std::string& center) {
    std::ostringstream oss;
    
    // Converti JD in formato calendario
    // JPL Horizons vuole: "JD2460000.5" oppure "2023-Jan-01"
    // Usiamo il formato JD esplicito
    
    // Parametri query Horizons API
    oss << "format=text";
    oss << "&COMMAND='" << target << "'";
    oss << "&OBJ_DATA='YES'";
    oss << "&MAKE_EPHEM='YES'";
    oss << "&EPHEM_TYPE='VECTORS'";
    oss << "&CENTER='" << center << "'";
    oss << "&START_TIME='JD" << std::fixed << std::setprecision(6) << start.jd << "'";
    oss << "&STOP_TIME='JD" << stop.jd << "'";
    oss << "&STEP_SIZE='1%20d'";  // 1 giorno
    oss << "&OUT_UNITS='AU-D'";    // AU e giorni
    oss << "&REF_PLANE='ECLIPTIC'";
    oss << "&REF_SYSTEM='ICRF'";
    oss << "&VEC_CORR='NONE'";
    oss << "&VEC_DELTA_T='NO'";
    oss << "&CSV_FORMAT='YES'";
    
    return oss.str();
}

std::pair<Vector3D, Vector3D> JPLHorizonsClient::getStateVectors(
    const std::string& target,
    const JulianDate& epoch,
    const std::string& center) {
    
    // Query con piccolo intervallo intorno all'epoca
    JulianDate start(epoch.jd - 0.1);
    JulianDate stop(epoch.jd + 0.1);
    
    std::string queryParams = buildVectorQuery(target, start, stop, center);
    std::string response = pImpl->performRequest(queryParams);
    
    return parseVectors(response);
}

std::pair<Vector3D, Vector3D> JPLHorizonsClient::parseVectors(const std::string& response) {
    Vector3D position, velocity;
    
    // Cerca la sezione dei vettori
    // Formato: JDTDB, Calendar, X, Y, Z, VX, VY, VZ, ...
    
    std::regex dataLineRegex(R"(\$\$SOE\s*([\s\S]*?)\$\$EOE)");
    std::smatch match;
    
    if (!std::regex_search(response, match, dataLineRegex)) {
        throw std::runtime_error("Impossibile trovare dati effemeridi in risposta Horizons");
    }
    
    std::string dataSection = match[1];
    
    // Parse CSV line
    // Formato: 2459920.500000000, A.D. 2022-Dec-01 00:00:00.0000, X, Y, Z, VX, VY, VZ, ...
    std::istringstream iss(dataSection);
    std::string line;
    
    while (std::getline(iss, line)) {
        if (line.empty() || line[0] == ' ') continue;
        
        // Parse CSV
        std::istringstream lineStream(line);
        std::string token;
        std::vector<std::string> tokens;
        
        while (std::getline(lineStream, token, ',')) {
            // Trim whitespace
            token.erase(0, token.find_first_not_of(" \t\n\r"));
            token.erase(token.find_last_not_of(" \t\n\r") + 1);
            tokens.push_back(token);
        }
        
        if (tokens.size() >= 9) {
            // tokens[0] = JD
            // tokens[1] = Calendar date
            // tokens[2] = X, tokens[3] = Y, tokens[4] = Z (AU)
            // tokens[5] = VX, tokens[6] = VY, tokens[7] = VZ (AU/day)
            
            try {
                position.x = std::stod(tokens[2]);
                position.y = std::stod(tokens[3]);
                position.z = std::stod(tokens[4]);
                velocity.x = std::stod(tokens[5]);
                velocity.y = std::stod(tokens[6]);
                velocity.z = std::stod(tokens[7]);
                
                // Prendi solo la prima linea
                break;
            } catch (const std::exception& e) {
                continue;
            }
        }
    }
    
    if (position.magnitude() < 1e-10) {
        throw std::runtime_error("Nessun vettore di stato valido trovato in risposta Horizons");
    }
    
    return {position, velocity};
}

HorizonsEphemeris JPLHorizonsClient::getEphemeris(const std::string& target,
                                                 const JulianDate& epoch,
                                                 const std::string& center) {
    auto [pos, vel] = getStateVectors(target, epoch, center);
    
    HorizonsEphemeris eph;
    eph.epoch = epoch;
    eph.position = pos;
    eph.velocity = vel;
    eph.targetName = target;
    
    return eph;
}

std::vector<HorizonsEphemeris> JPLHorizonsClient::getEphemerides(const HorizonsQuery& query) {
    std::string queryParams = buildVectorQuery(query.target, 
                                              query.startTime, 
                                              query.stopTime, 
                                              query.center);
    
    std::string response = pImpl->performRequest(queryParams);
    
    return parseHorizonsOutput(response);
}

std::vector<HorizonsEphemeris> JPLHorizonsClient::parseHorizonsOutput(const std::string& response) {
    std::vector<HorizonsEphemeris> result;
    
    // Implementazione completa parsing simile a parseVectors
    // Per ora return vuoto, TODO se serve batch
    
    return result;
}

std::pair<double, double> JPLHorizonsClient::getApparentCoordinates(
    const std::string& target,
    const JulianDate& epoch,
    const std::string& observerCode) {
    
    std::ostringstream oss;
    oss << "format=text"
        << "&COMMAND='" << target << "'"
        << "&OBJ_DATA='NO'"
        << "&MAKE_EPHEM='YES'"
        << "&EPHEM_TYPE='OBSERVER'"
        << "&CENTER='" << observerCode << "'"
        << "&START_TIME='JD" << std::fixed << std::setprecision(6) << epoch.jd << "'"
        << "&STOP_TIME='JD" << epoch.jd + 0.01 << "'"
        << "&STEP_SIZE='1d'"
        << "&QUANTITIES='1'"
        << "&ANG_FORMAT='DEG'"
        << "&CSV_FORMAT='YES'";
        
    std::string response = pImpl->performRequest(oss.str());
    return parseRADec(response);
}

OrbitalElements JPLHorizonsClient::getOsculatingElements(
    const std::string& target,
    const JulianDate& epoch,
    const std::string& center) {
    
    // Costruisci query per elementi orbitali
    // TABLE_TYPE='ELEMENTS' richiede elementi orbitali invece di vettori
    std::string startDate = "JD" + std::to_string(epoch.jd);
    std::string stopDate = "JD" + std::to_string(epoch.jd + 0.1); // +2.4 ore
    
    std::string params = "format=text"
                        "&COMMAND='" + target + "'"
                        "&OBJ_DATA='YES'"
                        "&MAKE_EPHEM='YES'"
                        "&EPHEM_TYPE='ELEMENTS'"
                        "&CENTER='" + center + "'"
                        "&START_TIME='" + startDate + "'"
                        "&STOP_TIME='" + stopDate + "'"
                        "&STEP_SIZE='1d'"
                        "&REF_SYSTEM='ICRF'"
                        "&REF_PLANE='ECLIPTIC'"
                        "&OUT_UNITS='AU-D'"
                        "&CSV_FORMAT='NO'"
                        "&ELM_LABELS='YES'";
    
    std::string response = pImpl->performRequest(params);
    
    return parseOrbitalElements(response);
}

std::pair<double, double> JPLHorizonsClient::parseRADec(const std::string& response) {
    std::regex dataLineRegex(R"(\$\$SOE\s*([\s\S]*?)\$\$EOE)");
    std::smatch match;
    
    if (!std::regex_search(response, match, dataLineRegex)) {
        throw std::runtime_error("Impossibile trovare dati RA/Dec in risposta Horizons");
    }
    
    std::string dataSection = match[1];
    std::istringstream iss(dataSection);
    std::string line;
    
    while (std::getline(iss, line)) {
        if (line.empty() || line[0] == ' ') continue;
        
        std::istringstream lineStream(line);
        std::string token;
        std::vector<std::string> tokens;
        
        while (std::getline(lineStream, token, ',')) {
            token.erase(0, token.find_first_not_of(" \t\n\r"));
            token.erase(token.find_last_not_of(" \t\n\r") + 1);
            tokens.push_back(token);
        }
        
        if (tokens.size() >= 5) {
            try {
                double ra = std::stod(tokens[3]) * DEG_TO_RAD;
                double dec = std::stod(tokens[4]) * DEG_TO_RAD;
                return {ra, dec};
            } catch (...) {
                continue;
            }
        }
    }
    
    throw std::runtime_error("Nessun dato RA/Dec valido trovato in risposta Horizons");
}

OrbitalElements JPLHorizonsClient::parseOrbitalElements(const std::string& response) {
    OrbitalElements elem;
    
    size_t soePos = response.find("$$SOE");
    size_t eoePos = response.find("$$EOE");
    
    if (soePos == std::string::npos || eoePos == std::string::npos) {
        throw std::runtime_error("Impossibile trovare dati elementi orbitali in risposta Horizons");
    }
    
    std::string ephData = response.substr(soePos + 5, eoePos - soePos - 5);
    
    // Helper lambda to extract value after a key
    auto extractValue = [&](const std::string& key) -> double {
        size_t pos = ephData.find(key);
        if (pos == std::string::npos) return 0.0;
        
        size_t start = pos + key.length();
        // Skip = and spaces
        while (start < ephData.length() && (ephData[start] == '=' || std::isspace(ephData[start]))) {
            start++;
        }
        
        char* endptr;
        double val = std::strtod(&ephData[start], &endptr);
        if (endptr == &ephData[start]) return 0.0;
        return val;
    };

    // Extract elements using keywords
    elem.e = extractValue("EC=");
    elem.a = extractValue("A =");
    if (elem.a == 0.0) elem.a = extractValue("A="); // Try without space
    
    elem.i = extractValue("IN=") * DEG_TO_RAD;
    elem.Omega = extractValue("OM=") * DEG_TO_RAD;
    elem.omega = extractValue("W ="); 
    if (elem.omega == 0.0) elem.omega = extractValue("W=");
    elem.omega *= DEG_TO_RAD;
    
    elem.M = extractValue("MA=") * DEG_TO_RAD;
    
    // Extract JD
    std::istringstream iss(ephData);
    std::string firstWord;
    if (iss >> firstWord) {
        try {
            elem.epoch.jd = std::stod(firstWord);
        } catch (...) {
            throw std::runtime_error("Impossibile parsare JD dall'output Horizons: " + firstWord);
        }
    }

    if (elem.a == 0.0 || elem.e == 0.0) {
        // Fallback for compact table format if keywords failed
        std::istringstream iss2(ephData);
        std::string line;
        while (std::getline(iss2, line)) {
            if (line.find("TDB") != std::string::npos) {
                std::istringstream ls(line);
                std::string jdStr, tdbStr;
                double ec, qr, in, om, w, tp, n, ma, ta, a, ad, pr;
                if (ls >> jdStr) {
                    elem.epoch.jd = std::stod(jdStr);
                    // This format is very fragile, prefer keyword extraction
                }
                break;
            }
        }
    }
    
    return elem;
}

bool JPLHorizonsClient::isTargetAvailable(const std::string& target) {
    try {
        JulianDate testEpoch(2460000.0);
        getStateVectors(target, testEpoch, "@sun");
        return true;
    } catch (...) {
        return false;
    }
}

std::string JPLHorizonsClient::buildQuery(const HorizonsQuery& query) {
    return buildVectorQuery(query.target, query.startTime, query.stopTime, query.center);
}

std::pair<double, double> compareWithHorizons(
    const Vector3D& localPosition,
    const Vector3D& localVelocity,
    const Vector3D& horizonsPosition,
    const Vector3D& horizonsVelocity) {
    
    // Errore posizione (km)
    Vector3D posError = localPosition - horizonsPosition;
    double posErrorKm = posError.magnitude() * 149597870.7;
    
    // Errore velocità (mm/s)
    Vector3D velError = localVelocity - horizonsVelocity;
    double velErrorMms = velError.magnitude() * 149597870.7 / 86400.0 * 1000.0;
    
    return {posErrorKm, velErrorMms};
}

} // namespace ioccultcalc
