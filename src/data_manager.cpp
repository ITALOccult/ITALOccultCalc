/**
 * @file data_manager.cpp
 * @brief Implementazione gestione directory dati centralizzata
 */

#include "ioccultcalc/data_manager.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <pwd.h>
#include <ctime>
#include <iostream>
#include <sstream>
#include <iomanip>

namespace ioccultcalc {

DataManager& DataManager::instance() {
    static DataManager instance;
    return instance;
}

DataManager::DataManager() : initialized_(false) {
    // Determina root directory
    const char* home = getenv("HOME");
    if (!home) {
        // Fallback to getpwuid
        struct passwd* pw = getpwuid(getuid());
        if (pw) {
            home = pw->pw_dir;
        }
    }
    
    if (home) {
        rootDir_ = std::string(home) + "/.ioccultcalc";
    } else {
        rootDir_ = ".ioccultcalc"; // Fallback to current dir
    }
}

bool DataManager::initialize() {
    if (initialized_) {
        return true;
    }
    
    // Crea directory principale
    if (!createDirectory(rootDir_)) {
        std::cerr << "Failed to create root directory: " << rootDir_ << std::endl;
        return false;
    }
    
    // Crea sottodirectory
    std::vector<std::string> subdirs = {
        "database",
        "ephemerides",
        "iers",
        "cache",
        "logs",
        "config"
    };
    
    for (const auto& subdir : subdirs) {
        std::string path = rootDir_ + "/" + subdir;
        if (!createDirectory(path)) {
            std::cerr << "Failed to create directory: " << path << std::endl;
            return false;
        }
    }
    
    initialized_ = true;
    return true;
}

std::string DataManager::getRootDir() const {
    return rootDir_;
}

std::string DataManager::getDatabaseDir() const {
    return rootDir_ + "/database";
}

std::string DataManager::getAsteroidDatabasePath() const {
    return rootDir_ + "/database/asteroids.db";
}

std::string DataManager::getEphemeridesDir() const {
    return rootDir_ + "/ephemerides";
}

std::string DataManager::getEphemerisPath(const std::string& name) const {
    return rootDir_ + "/ephemerides/" + name;
}

std::string DataManager::getIERSDir() const {
    return rootDir_ + "/iers";
}

std::string DataManager::getIERSPath(const std::string& name) const {
    return rootDir_ + "/iers/" + name;
}

std::string DataManager::getCacheDir() const {
    return rootDir_ + "/cache";
}

std::string DataManager::getLogsDir() const {
    return rootDir_ + "/logs";
}

std::string DataManager::getConfigDir() const {
    return rootDir_ + "/config";
}

bool DataManager::fileExists(const std::string& path) const {
    std::string fullPath = path;
    if (path[0] != '/') {
        fullPath = rootDir_ + "/" + path;
    }
    
    struct stat buffer;
    return (stat(fullPath.c_str(), &buffer) == 0);
}

size_t DataManager::getFileSize(const std::string& path) const {
    std::string fullPath = path;
    if (path[0] != '/') {
        fullPath = rootDir_ + "/" + path;
    }
    
    struct stat buffer;
    if (stat(fullPath.c_str(), &buffer) == 0) {
        return buffer.st_size;
    }
    return 0;
}

int DataManager::getFileAgeDays(const std::string& path) const {
    std::string fullPath = path;
    if (path[0] != '/') {
        fullPath = rootDir_ + "/" + path;
    }
    
    struct stat buffer;
    if (stat(fullPath.c_str(), &buffer) == 0) {
        time_t now = time(nullptr);
        double seconds = difftime(now, buffer.st_mtime);
        return static_cast<int>(seconds / 86400.0);
    }
    return -1;
}

bool DataManager::deleteFile(const std::string& path) {
    std::string fullPath = path;
    if (path[0] != '/') {
        fullPath = rootDir_ + "/" + path;
    }
    
    return (unlink(fullPath.c_str()) == 0);
}

int DataManager::cleanCache(int maxAgeDays) {
    std::string cacheDir = getCacheDir();
    DIR* dir = opendir(cacheDir.c_str());
    if (!dir) {
        return 0;
    }
    
    int deletedCount = 0;
    struct dirent* entry;
    
    while ((entry = readdir(dir)) != nullptr) {
        if (entry->d_name[0] == '.') {
            continue; // Skip . and ..
        }
        
        std::string filePath = cacheDir + "/" + entry->d_name;
        int age = getFileAgeDays(filePath);
        
        if (age > maxAgeDays) {
            if (deleteFile(filePath)) {
                deletedCount++;
            }
        }
    }
    
    closedir(dir);
    return deletedCount;
}

size_t DataManager::getTotalDiskUsage() const {
    size_t total = 0;
    
    // Recursive directory scan
    std::function<size_t(const std::string&)> scanDir = [&](const std::string& path) -> size_t {
        size_t size = 0;
        DIR* dir = opendir(path.c_str());
        if (!dir) return 0;
        
        struct dirent* entry;
        while ((entry = readdir(dir)) != nullptr) {
            if (entry->d_name[0] == '.') continue;
            
            std::string fullPath = path + "/" + entry->d_name;
            struct stat st;
            
            if (stat(fullPath.c_str(), &st) == 0) {
                if (S_ISDIR(st.st_mode)) {
                    size += scanDir(fullPath);
                } else {
                    size += st.st_size;
                }
            }
        }
        closedir(dir);
        return size;
    };
    
    return scanDir(rootDir_);
}

std::map<std::string, size_t> DataManager::getDiskUsageByCategory() const {
    std::map<std::string, size_t> usage;
    
    auto scanCategory = [&](const std::string& name, const std::string& path) {
        size_t size = 0;
        DIR* dir = opendir(path.c_str());
        if (!dir) {
            usage[name] = 0;
            return;
        }
        
        struct dirent* entry;
        while ((entry = readdir(dir)) != nullptr) {
            if (entry->d_name[0] == '.') continue;
            
            std::string fullPath = path + "/" + entry->d_name;
            struct stat st;
            
            if (stat(fullPath.c_str(), &st) == 0 && S_ISREG(st.st_mode)) {
                size += st.st_size;
            }
        }
        closedir(dir);
        usage[name] = size;
    };
    
    scanCategory("database", getDatabaseDir());
    scanCategory("ephemerides", getEphemeridesDir());
    scanCategory("iers", getIERSDir());
    scanCategory("cache", getCacheDir());
    scanCategory("logs", getLogsDir());
    scanCategory("config", getConfigDir());
    
    return usage;
}

std::vector<std::string> DataManager::listAllFiles() const {
    std::vector<std::string> files;
    
    std::function<void(const std::string&, const std::string&)> scanDir = 
        [&](const std::string& path, const std::string& relPath) {
        DIR* dir = opendir(path.c_str());
        if (!dir) return;
        
        struct dirent* entry;
        while ((entry = readdir(dir)) != nullptr) {
            if (entry->d_name[0] == '.') continue;
            
            std::string name = entry->d_name;
            std::string fullPath = path + "/" + name;
            std::string newRelPath = relPath.empty() ? name : relPath + "/" + name;
            
            struct stat st;
            if (stat(fullPath.c_str(), &st) == 0) {
                if (S_ISDIR(st.st_mode)) {
                    scanDir(fullPath, newRelPath);
                } else {
                    files.push_back(newRelPath);
                }
            }
        }
        closedir(dir);
    };
    
    scanDir(rootDir_, "");
    return files;
}

bool DataManager::verifyIntegrity() {
    // Verifica che tutte le directory esistano
    std::vector<std::string> requiredDirs = {
        rootDir_,
        getDatabaseDir(),
        getEphemeridesDir(),
        getIERSDir(),
        getCacheDir(),
        getLogsDir(),
        getConfigDir()
    };
    
    for (const auto& dir : requiredDirs) {
        struct stat st;
        if (stat(dir.c_str(), &st) != 0 || !S_ISDIR(st.st_mode)) {
            return false;
        }
    }
    
    return true;
}

bool DataManager::repairStructure() {
    return initialize(); // Ricrea tutte le directory
}

std::string DataManager::getSystemInfo() const {
    std::ostringstream oss;
    
    oss << "IOccultCalc Data Directory Information\n";
    oss << "======================================\n\n";
    
    oss << "Root Directory: " << rootDir_ << "\n";
    oss << "Initialized: " << (initialized_ ? "Yes" : "No") << "\n\n";
    
    // Disk usage
    auto usage = getDiskUsageByCategory();
    size_t total = 0;
    
    oss << "Disk Usage by Category:\n";
    for (const auto& pair : usage) {
        double mb = pair.second / (1024.0 * 1024.0);
        oss << "  " << std::setw(15) << std::left << pair.first << ": " 
            << std::fixed << std::setprecision(2) << mb << " MB\n";
        total += pair.second;
    }
    
    double totalMB = total / (1024.0 * 1024.0);
    oss << "  " << std::setw(15) << std::left << "TOTAL" << ": " 
        << std::fixed << std::setprecision(2) << totalMB << " MB\n\n";
    
    // Key files
    oss << "Key Files:\n";
    
    std::vector<std::pair<std::string, std::string>> keyFiles = {
        {"Asteroid DB", getAsteroidDatabasePath()},
        {"DE441 SPK", getEphemerisPath("de441.bsp")},
        {"IERS Finals", getIERSPath("finals2000A.all")}
    };
    
    for (const auto& file : keyFiles) {
        oss << "  " << std::setw(15) << std::left << file.first << ": ";
        if (fileExists(file.second)) {
            size_t size = getFileSize(file.second);
            double mb = size / (1024.0 * 1024.0);
            int age = getFileAgeDays(file.second);
            oss << std::fixed << std::setprecision(1) << mb << " MB, " 
                << age << " days old\n";
        } else {
            oss << "Not found\n";
        }
    }
    
    return oss.str();
}

bool DataManager::createDirectory(const std::string& path) {
    struct stat st;
    
    // Already exists?
    if (stat(path.c_str(), &st) == 0) {
        return S_ISDIR(st.st_mode);
    }
    
    // Create it
    if (mkdir(path.c_str(), 0755) == 0) {
        return true;
    }
    
    // Maybe parent doesn't exist, try recursive
    size_t pos = path.find_last_of('/');
    if (pos != std::string::npos) {
        std::string parent = path.substr(0, pos);
        if (createDirectory(parent)) {
            return mkdir(path.c_str(), 0755) == 0;
        }
    }
    
    return false;
}

std::string DataManager::expandHome(const std::string& path) const {
    if (path.empty() || path[0] != '~') {
        return path;
    }
    
    const char* home = getenv("HOME");
    if (!home) {
        struct passwd* pw = getpwuid(getuid());
        if (pw) {
            home = pw->pw_dir;
        }
    }
    
    if (home) {
        return std::string(home) + path.substr(1);
    }
    
    return path;
}

} // namespace ioccultcalc
