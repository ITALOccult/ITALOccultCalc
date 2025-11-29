/**
 * @file asteroid_list_reader.cpp
 * @brief Implementazione reader liste asteroidi
 */

#include "ioccultcalc/asteroid_list_reader.h"
#include <algorithm>
#include <cctype>
#include <regex>

namespace ioccultcalc {

// ============================================================================
// PUBLIC METHODS
// ============================================================================

std::vector<AsteroidListEntry> AsteroidListReader::readFromFile(
    const std::string& filename,
    AsteroidListFormat format) {
    
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + filename);
    }
    
    // Auto-detect formato se necessario
    if (format == AsteroidListFormat::AUTO_DETECT) {
        format = detectFormat(filename);
    }
    
    // Parsing basato sul formato
    std::vector<AsteroidListEntry> entries;
    
    switch (format) {
        case AsteroidListFormat::ASTNUM_LIST:
            entries = readAstnumList(file);
            break;
        
        case AsteroidListFormat::MPC_NUMBERED:
            entries = readMpcNumbered(file);
            break;
        
        case AsteroidListFormat::PLAIN_TEXT:
            entries = readPlainText(file);
            break;
        
        default:
            throw std::runtime_error("Unknown asteroid list format");
    }
    
    file.close();
    return entries;
}

std::vector<int> AsteroidListReader::readNumbersFromFile(
    const std::string& filename,
    AsteroidListFormat format) {
    
    auto entries = readFromFile(filename, format);
    
    std::vector<int> numbers;
    numbers.reserve(entries.size());
    
    for (const auto& entry : entries) {
        if (entry.hasNumber()) {
            numbers.push_back(entry.number);
        }
    }
    
    return numbers;
}

AsteroidListFormat AsteroidListReader::detectFormat(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + filename);
    }
    
    std::string line;
    int linesChecked = 0;
    int astnumListScore = 0;
    int mpcNumberedScore = 0;
    
    while (std::getline(file, line) && linesChecked < 20) {
        // Skip linee vuote
        if (line.empty() || line.find_first_not_of(" \t\r\n") == std::string::npos) {
            continue;
        }
        
        // Controlla header ASTNUM_LIST
        if (line.find("ITALOccultCalc") != std::string::npos ||
            line.find("ASTNUM") != std::string::npos ||
            line.find("Asteroid List with Occultations") != std::string::npos) {
            astnumListScore += 10;
        }
        
        // Controlla pattern ASTNUM_LIST: (numero) + commento + (N events)
        if (std::regex_search(line, std::regex(R"(\(\s*\d+\s*\)\s+#.*\(\d+\s+events?\))"))) {
            astnumListScore += 5;
        }
        
        // Controlla anche formato vecchio: numero + commento + (N events)
        if (std::regex_search(line, std::regex(R"(^\s*\d+\s+#.*\(\d+\s+events?\))"))) {
            astnumListScore += 3;
        }
        
        // Controlla se linea inizia con numero (MPC numbered)
        std::string cleaned = cleanLine(line);
        if (!cleaned.empty() && std::isdigit(cleaned[0])) {
            mpcNumberedScore += 1;
        }
        
        linesChecked++;
    }
    
    file.close();
    
    // Decisione basata su score
    if (astnumListScore > 0) {
        return AsteroidListFormat::ASTNUM_LIST;
    } else if (mpcNumberedScore > linesChecked / 2) {
        return AsteroidListFormat::MPC_NUMBERED;
    } else {
        return AsteroidListFormat::PLAIN_TEXT;
    }
}

bool AsteroidListReader::isCommentLine(const std::string& line) {
    std::string trimmed = line;
    // Trim leading whitespace
    trimmed.erase(0, trimmed.find_first_not_of(" \t\r\n"));
    
    return trimmed.empty() || trimmed[0] == '#';
}

int AsteroidListReader::extractAsteroidNumber(const std::string& line) {
    // Cerca numero tra parentesi: (numero) con spazi opzionali
    std::regex parenNumberRegex(R"(^\s*\(\s*(\d+)\s*\))");
    std::smatch match;
    
    if (std::regex_search(line, match, parenNumberRegex)) {
        try {
            return std::stoi(match[1].str());
        } catch (...) {
            return 0;
        }
    }
    
    // Fallback: cerca primo numero nella linea (formato vecchio)
    std::regex numberRegex(R"(^\s*(\d+))");
    if (std::regex_search(line, match, numberRegex)) {
        try {
            return std::stoi(match[1].str());
        } catch (...) {
            return 0;
        }
    }
    
    return 0;
}

std::string AsteroidListReader::extractAsteroidName(const std::string& line) {
    // Cerca pattern: # Nome
    std::regex nameRegex(R"(#\s+([A-Za-z][A-Za-z\s\-']+?)\s+\()");
    std::smatch match;
    
    if (std::regex_search(line, match, nameRegex)) {
        std::string name = match[1].str();
        // Trim trailing whitespace
        name.erase(name.find_last_not_of(" \t\r\n") + 1);
        return name;
    }
    
    return "";
}

int AsteroidListReader::extractEventCount(const std::string& line) {
    // Cerca pattern: (N events) o (N event)
    std::regex eventRegex(R"(\((\d+)\s+events?\))");
    std::smatch match;
    
    if (std::regex_search(line, match, eventRegex)) {
        try {
            return std::stoi(match[1].str());
        } catch (...) {
            return 0;
        }
    }
    
    return 0;
}

std::string AsteroidListReader::cleanLine(const std::string& line) {
    std::string cleaned = line;
    
    // Rimuovi commenti inline che iniziano con #
    size_t commentPos = cleaned.find('#');
    if (commentPos != std::string::npos) {
        // Mantieni il numero prima del #
        std::string beforeComment = cleaned.substr(0, commentPos);
        cleaned = beforeComment;
    }
    
    // Trim whitespace
    cleaned.erase(0, cleaned.find_first_not_of(" \t\r\n"));
    cleaned.erase(cleaned.find_last_not_of(" \t\r\n") + 1);
    
    return cleaned;
}

// ============================================================================
// PRIVATE FORMAT-SPECIFIC READERS
// ============================================================================

std::vector<AsteroidListEntry> AsteroidListReader::readAstnumList(std::ifstream& file) {
    std::vector<AsteroidListEntry> entries;
    std::string line;
    
    while (std::getline(file, line)) {
        // Skip commenti e linee vuote
        if (isCommentLine(line)) {
            continue;
        }
        
        // Estrai dati
        int number = extractAsteroidNumber(line);
        if (number == 0) {
            continue; // Skip linee senza numero valido
        }
        
        AsteroidListEntry entry;
        entry.number = number;
        entry.name = extractAsteroidName(line);
        entry.eventCount = extractEventCount(line);
        
        entries.push_back(entry);
    }
    
    return entries;
}

std::vector<AsteroidListEntry> AsteroidListReader::readMpcNumbered(std::ifstream& file) {
    std::vector<AsteroidListEntry> entries;
    std::string line;
    
    while (std::getline(file, line)) {
        // Skip commenti
        if (isCommentLine(line)) {
            continue;
        }
        
        // Pulisci linea
        std::string cleaned = cleanLine(line);
        if (cleaned.empty()) {
            continue;
        }
        
        // Prova a parsare come numero
        try {
            int number = std::stoi(cleaned);
            if (number > 0) {
                AsteroidListEntry entry;
                entry.number = number;
                entries.push_back(entry);
            }
        } catch (...) {
            // Skip linee che non sono numeri validi
            continue;
        }
    }
    
    return entries;
}

std::vector<AsteroidListEntry> AsteroidListReader::readPlainText(std::ifstream& file) {
    std::vector<AsteroidListEntry> entries;
    std::string line;
    
    while (std::getline(file, line)) {
        // Skip commenti
        if (isCommentLine(line)) {
            continue;
        }
        
        // Pulisci linea
        std::string cleaned = cleanLine(line);
        if (cleaned.empty()) {
            continue;
        }
        
        AsteroidListEntry entry;
        
        // Verifica se è SOLO un numero (no lettere)
        bool isOnlyNumber = !cleaned.empty() && 
                           cleaned.find_first_not_of("0123456789") == std::string::npos;
        
        if (isOnlyNumber) {
            // È solo un numero
            try {
                int number = std::stoi(cleaned);
                if (number > 0) {
                    entry.number = number;
                    entries.push_back(entry);
                    continue;
                }
            } catch (...) {
                // Skip
            }
        }
        
        // Tratta come designazione (contiene lettere o spazi)
        entry.designation = cleaned;
        entries.push_back(entry);
    }
    
    return entries;
}

} // namespace ioccultcalc
