#include "ioccultcalc/astdys_client.h"
#include "ioccultcalc/time_utils.h"
#include <curl/curl.h>
#include <sstream>
#include <stdexcept>
#include <regex>

namespace ioccultcalc {

// Callback per ricevere dati da libcurl
static size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* output) {
    size_t totalSize = size * nmemb;
    output->append(static_cast<char*>(contents), totalSize);
    return totalSize;
}

class AstDysClient::Impl {
public:
    std::string baseURL;
    int timeout;
    CURL* curl;
    
    Impl() : baseURL("https://newton.spacedys.com/astdys2/"), timeout(30) {
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
        
        long responseCode;
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &responseCode);
        
        if (responseCode != 200) {
            throw std::runtime_error("HTTP error code: " + std::to_string(responseCode));
        }
        
        return response;
    }
};

AstDysClient::AstDysClient() : pImpl(new Impl()) {}

AstDysClient::~AstDysClient() = default;

void AstDysClient::setBaseURL(const std::string& url) {
    pImpl->baseURL = url;
    if (pImpl->baseURL.back() != '/') {
        pImpl->baseURL += '/';
    }
}

void AstDysClient::setTimeout(int seconds) {
    pImpl->timeout = seconds;
}

EquinoctialElements AstDysClient::getElements(const std::string& designation) {
    // Costruisci URL per scaricare file .eq
    // Esempio: https://newton.spacedys.com/astdys2/propsynth/(433).eq0
    
    std::string filename = designation;
    // Se è un numero, aggiungi parentesi
    if (std::all_of(designation.begin(), designation.end(), ::isdigit)) {
        filename = "(" + designation + ")";
    }
    
    std::string url = pImpl->baseURL + "propsynth/" + filename + ".eq0";
    
    try {
        std::string content = pImpl->httpGet(url);
        return parseEquinoctialFile(content, designation);
    } catch (const std::exception& e) {
        // Prova con altra convenzione
        url = pImpl->baseURL + "propsynth/" + designation + ".eq0";
        std::string content = pImpl->httpGet(url);
        return parseEquinoctialFile(content, designation);
    }
}

std::vector<EquinoctialElements> AstDysClient::getElementsBatch(
    const std::vector<std::string>& designations) {
    
    std::vector<EquinoctialElements> results;
    results.reserve(designations.size());
    
    for (const auto& desig : designations) {
        try {
            results.push_back(getElements(desig));
        } catch (const std::exception& e) {
            // Log error e continua
            // In una implementazione reale, si potrebbe usare un sistema di logging
        }
    }
    
    return results;
}

std::vector<std::string> AstDysClient::searchByName(const std::string& name) {
    // Query al servizio di ricerca di AstDyS
    std::string url = pImpl->baseURL + "search?name=" + name;
    
    std::string content = pImpl->httpGet(url);
    
    // Parsing del risultato (dipende dal formato della risposta)
    // Questo è un esempio semplificato
    std::vector<std::string> results;
    std::istringstream iss(content);
    std::string line;
    
    while (std::getline(iss, line)) {
        if (!line.empty()) {
            results.push_back(line);
        }
    }
    
    return results;
}

EquinoctialElements AstDysClient::parseEquinoctialFile(const std::string& content,
                                                       const std::string& designation) {
    EquinoctialElements elem;
    elem.designation = designation;
    
    // Parsing del formato .eq di AstDyS
    // Formato tipico (varia, questa è una approssimazione):
    // Nome
    // Epoca MJD
    // a h k p q lambda
    // H G
    
    std::istringstream iss(content);
    std::string line;
    int lineNum = 0;
    
    while (std::getline(iss, line)) {
        if (line.empty() || line[0] == '#') continue;
        
        lineNum++;
        
        if (lineNum == 1) {
            // Nome
            elem.name = line;
        } else if (lineNum == 2) {
            // Epoca (MJD)
            double mjd;
            std::istringstream(line) >> mjd;
            elem.epoch = JulianDate::fromMJD(mjd);
        } else if (lineNum == 3) {
            // Elementi equinoziali
            std::istringstream(line) >> elem.a >> elem.h >> elem.k 
                                     >> elem.p >> elem.q >> elem.lambda;
            // Converti lambda in radianti se necessario
            if (elem.lambda > 2.0 * M_PI) {
                elem.lambda *= DEG_TO_RAD;
            }
        } else if (lineNum == 4) {
            // Parametri fotometrici
            std::istringstream(line) >> elem.H >> elem.G;
        }
    }
    
    return elem;
}

} // namespace ioccultcalc
