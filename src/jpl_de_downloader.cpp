/**
 * @file jpl_de_downloader.cpp
 * @brief Implementazione downloader JPL DE
 */

#include "ioccultcalc/jpl_de_downloader.h"
#include <curl/curl.h>
#include <sys/stat.h>
#include <fstream>
#include <sstream>
#include <iostream>
#include <cstdlib>

#ifdef _WIN32
#include <direct.h>
#define mkdir(path, mode) _mkdir(path)
#else
#include <sys/stat.h>
#include <unistd.h>
#include <pwd.h>
#endif

namespace ioccultcalc {

// Callback per CURL download con progresso
static size_t WriteFileCallback(void* ptr, size_t size, size_t nmemb, FILE* stream) {
    return fwrite(ptr, size, nmemb, stream);
}

static int ProgressCallback(void* clientp, curl_off_t dltotal, curl_off_t dlnow,
                           curl_off_t ultotal, curl_off_t ulnow) {
    if (clientp) {
        auto callback = static_cast<std::function<void(size_t, size_t)>*>(clientp);
        if (dltotal > 0) {
            (*callback)(static_cast<size_t>(dlnow), static_cast<size_t>(dltotal));
        }
    }
    return 0;
}

class JPLDEDownloader::Impl {
public:
    std::string cacheDir;
    std::string baseURL;
    
    Impl() {
        cacheDir = getDefaultCacheDirectory();
        baseURL = "https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/";
    }
    
    std::vector<JPLDEFileInfo> getKnownFiles() {
        std::vector<JPLDEFileInfo> files;
        
        // DE441 - Most recent (2021) - LINUX FORMAT (più compatto)
        JPLDEFileInfo de441;
        de441.name = "linux_m13000p17000.441";
        de441.version = "DE441";
        de441.url = "https://ssd.jpl.nasa.gov/ftp/eph/planets/Linux/de441/linux_m13000p17000.441";
        de441.coverageStart = -3100015.5;  // 13200 BCE
        de441.coverageEnd = 8000016.5;     // 17191 CE  
        de441.sizeBytes = 2788676624;      // ~2.6 GB (Linux format)
        de441.description = "Most recent JPL ephemeris (2021), includes 343 asteroids + planets";
        files.push_back(de441);
        
        // DE440 - High precision (2020)
        JPLDEFileInfo de440;
        de440.name = "de440.bsp";
        de440.version = "DE440";
        de440.url = baseURL + "a_old_versions/de440.bsp";
        de440.coverageStart = -3100015.5;
        de440.coverageEnd = 8000016.5;
        de440.sizeBytes = 3462081536;      // ~3.2 GB
        de440.description = "High precision ephemeris (2020), spacecraft navigation";
        files.push_back(de440);
        
        // DE430 - Smaller, sufficient for most uses (2013) - LINUX FORMAT
        JPLDEFileInfo de430;
        de430.name = "linux_p1550p2650.430";
        de430.version = "DE430";
        de430.url = "https://ssd.jpl.nasa.gov/ftp/eph/planets/Linux/de430/linux_p1550p2650.430";
        de430.coverageStart = 2287184.5;   // 1550 CE
        de430.coverageEnd = 2688976.5;     // 2650 CE
        de430.sizeBytes = 102844080;       // ~98 MB (Linux format)
        de430.description = "Compact ephemeris (2013), planets only, 1550-2650 CE";
        files.push_back(de430);
        
        // DE421 - Legacy, small
        JPLDEFileInfo de421;
        de421.name = "de421.bsp";
        de421.version = "DE421";
        de421.url = baseURL + "de421.bsp";
        de421.coverageStart = 2414864.5;   // 1900 CE
        de421.coverageEnd = 2471184.5;     // 2050 CE
        de421.sizeBytes = 16441344;        // ~16 MB (very small!)
        de421.description = "Legacy ephemeris, 1900-2050 CE only";
        files.push_back(de421);
        
        return files;
    }
};

JPLDEDownloader::JPLDEDownloader() : pImpl(new Impl()) {}
JPLDEDownloader::~JPLDEDownloader() = default;

void JPLDEDownloader::setCacheDirectory(const std::string& path) {
    pImpl->cacheDir = path;
}

void JPLDEDownloader::setBaseURL(const std::string& url) {
    pImpl->baseURL = url;
}

std::vector<JPLDEFileInfo> JPLDEDownloader::listAvailableFiles() {
    return pImpl->getKnownFiles();
}

std::string JPLDEDownloader::findLocalFile(const std::string& version,
                                          const std::string& searchPath) {
    std::string dir = searchPath.empty() ? pImpl->cacheDir : searchPath;
    
    // Cerca il file nella cache
    auto files = pImpl->getKnownFiles();
    for (const auto& file : files) {
        if (file.version == version) {
            std::string fullPath = dir + "/" + file.name;
            
            // Verifica esistenza
            struct stat buffer;
            if (stat(fullPath.c_str(), &buffer) == 0) {
                return fullPath;
            }
        }
    }
    
    return "";
}

std::string JPLDEDownloader::downloadFile(const std::string& version,
                                         const std::string& outputPath,
                                         std::function<void(size_t, size_t)> progressCallback) {
    // Trova informazioni file
    auto files = pImpl->getKnownFiles();
    JPLDEFileInfo* fileInfo = nullptr;
    
    for (auto& file : files) {
        if (file.version == version) {
            fileInfo = &file;
            break;
        }
    }
    
    if (!fileInfo) {
        throw std::runtime_error("Versione JPL DE non supportata: " + version);
    }
    
    // Determina percorso output
    std::string outDir = outputPath.empty() ? pImpl->cacheDir : outputPath;
    std::string outFile = outDir + "/" + fileInfo->name;
    
    // Crea directory se non esiste
    struct stat st;
    if (stat(outDir.c_str(), &st) != 0) {
        #ifdef _WIN32
        mkdir(outDir.c_str());
        #else
        mkdir(outDir.c_str(), 0755);
        #endif
    }
    
    std::cout << "Scaricamento " << fileInfo->name << " da JPL/NAIF...\n";
    std::cout << "  URL: " << fileInfo->url << "\n";
    std::cout << "  Dimensione: " << (fileInfo->sizeBytes / 1024 / 1024) << " MB\n";
    std::cout << "  Output: " << outFile << "\n\n";
    
    // Download con CURL
    CURL* curl = curl_easy_init();
    if (!curl) {
        throw std::runtime_error("Impossibile inizializzare CURL");
    }
    
    FILE* fp = fopen(outFile.c_str(), "wb");
    if (!fp) {
        curl_easy_cleanup(curl);
        throw std::runtime_error("Impossibile creare file: " + outFile);
    }
    
    curl_easy_setopt(curl, CURLOPT_URL, fileInfo->url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteFileCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 0L);  // No timeout per file grandi
    
    if (progressCallback) {
        curl_easy_setopt(curl, CURLOPT_XFERINFOFUNCTION, ProgressCallback);
        curl_easy_setopt(curl, CURLOPT_XFERINFODATA, &progressCallback);
        curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0L);
    }
    
    CURLcode res = curl_easy_perform(curl);
    
    fclose(fp);
    curl_easy_cleanup(curl);
    
    if (res != CURLE_OK) {
        remove(outFile.c_str());
        throw std::runtime_error(std::string("Download fallito: ") + 
                               curl_easy_strerror(res));
    }
    
    std::cout << "\nDownload completato: " << outFile << "\n";
    
    return outFile;
}

std::string JPLDEDownloader::ensureFileAvailable(const std::string& version) {
    // Prima cerca localmente
    std::string localPath = findLocalFile(version);
    if (!localPath.empty()) {
        std::cout << "File " << version << " già presente: " << localPath << "\n";
        return localPath;
    }
    
    // Altrimenti scarica
    std::cout << "File " << version << " non trovato localmente, scaricamento in corso...\n";
    
    // Progress callback
    auto progress = [](size_t current, size_t total) {
        if (total > 0) {
            int percent = static_cast<int>(current * 100 / total);
            std::cout << "\r  Progresso: " << percent << "% (" 
                      << (current / 1024 / 1024) << "/" 
                      << (total / 1024 / 1024) << " MB)" << std::flush;
        }
    };
    
    return downloadFile(version, "", progress);
}

bool JPLDEDownloader::verifyFileIntegrity(const std::string& filePath) {
    // Verifica base: dimensione file
    struct stat st;
    if (stat(filePath.c_str(), &st) != 0) {
        return false;
    }
    
    // TODO: Verifica checksum se disponibile
    // Per ora solo verifica dimensione > 0
    return st.st_size > 0;
}

std::string getDefaultCacheDirectory() {
    std::string homeDir;
    
    #ifdef _WIN32
    const char* userProfile = std::getenv("USERPROFILE");
    if (userProfile) {
        homeDir = userProfile;
    }
    #else
    const char* home = std::getenv("HOME");
    if (home) {
        homeDir = home;
    } else {
        struct passwd* pw = getpwuid(getuid());
        if (pw) {
            homeDir = pw->pw_dir;
        }
    }
    #endif
    
    if (homeDir.empty()) {
        homeDir = ".";
    }
    
    std::string cacheDir = homeDir + "/.ioccultcalc/ephemerides";
    
    // Crea directory se non esiste
    struct stat st;
    if (stat(cacheDir.c_str(), &st) != 0) {
        // Crea prima .ioccultcalc
        std::string parentDir = homeDir + "/.ioccultcalc";
        #ifdef _WIN32
        mkdir(parentDir.c_str());
        mkdir(cacheDir.c_str());
        #else
        mkdir(parentDir.c_str(), 0755);
        mkdir(cacheDir.c_str(), 0755);
        #endif
    }
    
    return cacheDir;
}

size_t getFileSize(const std::string& filePath) {
    struct stat st;
    if (stat(filePath.c_str(), &st) == 0) {
        return st.st_size;
    }
    return 0;
}

} // namespace ioccultcalc
