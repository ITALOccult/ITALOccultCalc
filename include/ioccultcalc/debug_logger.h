#ifndef IOCCULTCALC_DEBUG_LOGGER_H
#define IOCCULTCALC_DEBUG_LOGGER_H

#include <string>
#include <fstream>
#include <mutex>
#include <nlohmann/json.hpp>

namespace ioccultcalc {

class DebugLogger {
public:
    static DebugLogger& getInstance() {
        static DebugLogger instance;
        return instance;
    }

    void setFilename(const std::string& filename) {
        std::lock_guard<std::mutex> lock(mutex_);
        if (filename_ != filename) {
            filename_ = filename;
        }
    }

    std::string getFilename() const {
        return filename_;
    }

    void log(const std::string& section, const std::string& key, const nlohmann::json& value) {
        std::lock_guard<std::mutex> lock(mutex_);
        root_[section][key].push_back(value);
    }

    void logValue(const std::string& section, const std::string& key, const nlohmann::json& value) {
        std::lock_guard<std::mutex> lock(mutex_);
        root_[section][key] = value;
    }

    void save() {
        std::lock_guard<std::mutex> lock(mutex_);
        if (filename_.empty()) return;
        
        std::ofstream file(filename_);
        if (file.is_open()) {
            file << root_.dump(4);
            file.close();
        }
    }

private:
    DebugLogger() = default;
    ~DebugLogger() {
        save();
    }
    
    std::string filename_;
    nlohmann::json root_;
    std::mutex mutex_;
};

} // namespace ioccultcalc

#endif // IOCCULTCALC_DEBUG_LOGGER_H
