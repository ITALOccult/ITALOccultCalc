#include "ioccultcalc/gaia_client.h"
#include "ioccultcalc/time_utils.h"
#include <curl/curl.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <sstream>
#include <stdexcept>
#include <iomanip>

namespace ioccultcalc {

static size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* output) {
    size_t totalSize = size * nmemb;
    output->append(static_cast<char*>(contents), totalSize);
    return totalSize;
}

EquatorialCoordinates GaiaStar::propagateToEpoch(const JulianDate& epoch) const {
    // Epoca di riferimento Gaia DR3: J2016.0
    const double gaiaEpochJD = 2457389.0; // 2016-01-01
    double deltaYears = (epoch.jd - gaiaEpochJD) / 365.25;
    
    // Proper motion in mas/yr -> radianti/yr
    double pmra_rad = pmra / 3600000.0 * DEG_TO_RAD;
    double pmdec_rad = pmdec / 3600000.0 * DEG_TO_RAD;
    
    // Applica proper motion
    double newRA = pos.ra + pmra_rad * deltaYears / cos(pos.dec);
    double newDec = pos.dec + pmdec_rad * deltaYears;
    
    // Normalizza RA
    while (newRA < 0) newRA += 2.0 * M_PI;
    while (newRA >= 2.0 * M_PI) newRA -= 2.0 * M_PI;
    
    return EquatorialCoordinates(newRA, newDec, pos.distance);
}

class GaiaClient::Impl {
public:
    std::string tapURL;
    int timeout;
    int maxRows;
    CURL* curl;
    
    Impl() : tapURL("https://gea.esac.esa.int/tap-server/tap/"), 
             timeout(60), maxRows(10000) {
        curl_global_init(CURL_GLOBAL_DEFAULT);
        curl = curl_easy_init();
    }
    
    ~Impl() {
        if (curl) {
            curl_easy_cleanup(curl);
        }
        curl_global_cleanup();
    }
    
    std::string httpPost(const std::string& url, const std::string& postData) {
        if (!curl) {
            throw std::runtime_error("CURL not initialized");
        }
        
        std::string response;
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, postData.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, timeout);
        
        CURLcode res = curl_easy_perform(curl);
        
        if (res != CURLE_OK) {
            throw std::runtime_error(std::string("HTTP request failed: ") + 
                                   curl_easy_strerror(res));
        }
        
        return response;
    }
};

GaiaClient::GaiaClient() : pImpl(new Impl()) {}

GaiaClient::~GaiaClient() = default;

void GaiaClient::setTAPURL(const std::string& url) {
    pImpl->tapURL = url;
    if (pImpl->tapURL.back() != '/') {
        pImpl->tapURL += '/';
    }
}

void GaiaClient::setTimeout(int seconds) {
    pImpl->timeout = seconds;
}

void GaiaClient::setMaxRows(int rows) {
    pImpl->maxRows = rows;
}

std::vector<GaiaStar> GaiaClient::queryRegion(double raCenterDeg, double decCenterDeg,
                                              double radiusDeg, double maxMagnitude) {
    return queryCone(raCenterDeg, decCenterDeg, radiusDeg, maxMagnitude);
}

std::vector<GaiaStar> GaiaClient::queryCone(double raCenterDeg, double decCenterDeg,
                                            double radiusDeg, double maxMagnitude) {
    // Costruisci query ADQL
    std::ostringstream adql;
    adql << "SELECT TOP " << pImpl->maxRows << " "
         << "source_id, ra, dec, parallax, pmra, pmdec, "
         << "phot_g_mean_mag, phot_bp_mean_mag, phot_rp_mean_mag "
         << "FROM gaiadr3.gaia_source "
         << "WHERE CONTAINS("
         << "POINT('ICRS', ra, dec), "
         << "CIRCLE('ICRS', " 
         << std::fixed << std::setprecision(8)
         << raCenterDeg << ", " << decCenterDeg << ", " << radiusDeg
         << ")) = 1 "
         << "AND phot_g_mean_mag < " << maxMagnitude << " "
         << "ORDER BY phot_g_mean_mag ASC";
    
    std::string votable = executeADQL(adql.str());
    return parseVOTable(votable);
}

GaiaStar GaiaClient::queryStar(const std::string& sourceId) {
    std::ostringstream adql;
    adql << "SELECT source_id, ra, dec, parallax, pmra, pmdec, "
         << "phot_g_mean_mag, phot_bp_mean_mag, phot_rp_mean_mag "
         << "FROM gaiadr3.gaia_source "
         << "WHERE source_id = " << sourceId;
    
    std::string votable = executeADQL(adql.str());
    std::vector<GaiaStar> stars = parseVOTable(votable);
    
    if (stars.empty()) {
        throw std::runtime_error("Star not found: " + sourceId);
    }
    
    return stars[0];
}

std::vector<GaiaStar> GaiaClient::queryAlongPath(
    const std::vector<EquatorialCoordinates>& path,
    double widthDeg, double maxMagnitude) {
    
    // Per semplicità, query multipli coni lungo il path
    std::vector<GaiaStar> allStars;
    double stepDeg = widthDeg; // Step size per i coni
    
    for (size_t i = 0; i < path.size(); i += std::max(1, static_cast<int>(path.size() / 20))) {
        double raDeg = path[i].ra * RAD_TO_DEG;
        double decDeg = path[i].dec * RAD_TO_DEG;
        
        auto stars = queryCone(raDeg, decDeg, widthDeg, maxMagnitude);
        allStars.insert(allStars.end(), stars.begin(), stars.end());
    }
    
    // Rimuovi duplicati (stesso source_id)
    std::sort(allStars.begin(), allStars.end(),
             [](const GaiaStar& a, const GaiaStar& b) {
                 return a.sourceId < b.sourceId;
             });
    
    allStars.erase(std::unique(allStars.begin(), allStars.end(),
                              [](const GaiaStar& a, const GaiaStar& b) {
                                  return a.sourceId == b.sourceId;
                              }), allStars.end());
    
    return allStars;
}

std::string GaiaClient::executeADQL(const std::string& adqlQuery) {
    std::string url = pImpl->tapURL + "sync";
    
    // Prepara i dati POST
    std::ostringstream postData;
    postData << "REQUEST=doQuery&LANG=ADQL&FORMAT=votable&QUERY=";
    
    // URL-encode della query
    CURL* curl = pImpl->curl;
    char* encodedQuery = curl_easy_escape(curl, adqlQuery.c_str(), adqlQuery.length());
    postData << encodedQuery;
    curl_free(encodedQuery);
    
    return pImpl->httpPost(url, postData.str());
}

std::vector<GaiaStar> GaiaClient::parseVOTable(const std::string& votable) {
    std::vector<GaiaStar> stars;
    
    // Parse XML usando libxml2
    xmlDocPtr doc = xmlReadMemory(votable.c_str(), votable.length(),
                                  "votable.xml", NULL, 0);
    if (!doc) {
        throw std::runtime_error("Failed to parse VOTable XML");
    }
    
    xmlNodePtr root = xmlDocGetRootElement(doc);
    
    // Trova la tabella e i dati
    // Questa è una implementazione semplificata
    // VOTable ha una struttura complessa con RESOURCE/TABLE/DATA/TABLEDATA
    
    xmlNodePtr node = root;
    xmlNodePtr tableData = nullptr;
    
    // Naviga l'albero XML per trovare TABLEDATA
    std::function<void(xmlNodePtr)> findTableData = [&](xmlNodePtr n) {
        if (!n) return;
        if (xmlStrcmp(n->name, (const xmlChar*)"TABLEDATA") == 0) {
            tableData = n;
            return;
        }
        for (xmlNodePtr child = n->children; child; child = child->next) {
            findTableData(child);
            if (tableData) return;
        }
    };
    
    findTableData(root);
    
    if (tableData) {
        // Parse ogni riga (TR)
        for (xmlNodePtr tr = tableData->children; tr; tr = tr->next) {
            if (xmlStrcmp(tr->name, (const xmlChar*)"TR") == 0) {
                GaiaStar star;
                int colIndex = 0;
                
                // Parse ogni cella (TD)
                for (xmlNodePtr td = tr->children; td; td = td->next) {
                    if (xmlStrcmp(td->name, (const xmlChar*)"TD") == 0) {
                        xmlChar* content = xmlNodeGetContent(td);
                        std::string value = content ? (char*)content : "";
                        xmlFree(content);
                        
                        // Mappa ai campi della stella (ordine della SELECT)
                        switch (colIndex) {
                            case 0: star.sourceId = value; break;
                            case 1: star.pos.ra = std::stod(value) * DEG_TO_RAD; break;
                            case 2: star.pos.dec = std::stod(value) * DEG_TO_RAD; break;
                            case 3: star.parallax = value.empty() ? 0 : std::stod(value); break;
                            case 4: star.pmra = value.empty() ? 0 : std::stod(value); break;
                            case 5: star.pmdec = value.empty() ? 0 : std::stod(value); break;
                            case 6: star.phot_g_mean_mag = value.empty() ? 99 : std::stod(value); break;
                            case 7: star.phot_bp_mean_mag = value.empty() ? 99 : std::stod(value); break;
                            case 8: star.phot_rp_mean_mag = value.empty() ? 99 : std::stod(value); break;
                        }
                        colIndex++;
                    }
                }
                
                stars.push_back(star);
            }
        }
    }
    
    xmlFreeDoc(doc);
    xmlCleanupParser();
    
    return stars;
}

} // namespace ioccultcalc
