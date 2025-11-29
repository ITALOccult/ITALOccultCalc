/**
 * @file test_oop_presets.cpp
 * @brief Test per validare i file preset .oop con liste combinate
 * @author IOccultCalc Team
 */

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <sstream>

// Simple INI-like parser for .oop files
class OOPParser {
public:
    struct Section {
        std::string name;
        std::map<std::string, std::string> values;
    };
    
    std::map<std::string, Section> sections;
    
    bool parse(const std::string& filename) {
        std::ifstream file(filename);
        if (!file.is_open()) {
            std::cerr << "ERROR: Cannot open file: " << filename << "\n";
            return false;
        }
        
        std::string line;
        std::string currentSection;
        int lineNum = 0;
        
        while (std::getline(file, line)) {
            lineNum++;
            line = trim(line);
            
            // Skip empty lines and comments
            if (line.empty() || line[0] == '#') {
                continue;
            }
            
            // Section header [section_name]
            if (line[0] == '[' && line[line.length()-1] == ']') {
                currentSection = line.substr(1, line.length()-2);
                sections[currentSection].name = currentSection;
                continue;
            }
            
            // Key = value
            size_t eqPos = line.find('=');
            if (eqPos != std::string::npos && !currentSection.empty()) {
                std::string key = trim(line.substr(0, eqPos));
                std::string value = trim(line.substr(eqPos + 1));
                
                // Remove quotes if present
                if (value.length() >= 2 && value[0] == '"' && value[value.length()-1] == '"') {
                    value = value.substr(1, value.length()-2);
                }
                
                sections[currentSection].values[key] = value;
            }
        }
        
        return true;
    }
    
    bool hasSection(const std::string& section) const {
        return sections.find(section) != sections.end();
    }
    
    bool hasKey(const std::string& section, const std::string& key) const {
        auto it = sections.find(section);
        if (it == sections.end()) return false;
        return it->second.values.find(key) != it->second.values.end();
    }
    
    std::string getValue(const std::string& section, const std::string& key, 
                        const std::string& defaultValue = "") const {
        auto secIt = sections.find(section);
        if (secIt == sections.end()) return defaultValue;
        
        auto valIt = secIt->second.values.find(key);
        if (valIt == secIt->second.values.end()) return defaultValue;
        
        return valIt->second;
    }
    
private:
    std::string trim(const std::string& str) {
        size_t start = str.find_first_not_of(" \t\r\n");
        if (start == std::string::npos) return "";
        size_t end = str.find_last_not_of(" \t\r\n");
        return str.substr(start, end - start + 1);
    }
};

// Test functions
bool testPresetFile(const std::string& filename, const std::string& description) {
    std::cout << "\n--- Testing: " << filename << " ---\n";
    std::cout << "Description: " << description << "\n\n";
    
    OOPParser parser;
    if (!parser.parse(filename)) {
        std::cout << "✗ FAIL: Cannot parse file\n";
        return false;
    }
    
    std::cout << "✓ File parsed successfully\n";
    
    // Check required sections
    std::vector<std::string> requiredSections = {
        "general", "asteroid_selection", "time_range", 
        "search_parameters", "observer", "output"
    };
    
    bool allSectionsPresent = true;
    for (const auto& section : requiredSections) {
        if (parser.hasSection(section)) {
            std::cout << "  ✓ Section [" << section << "] present\n";
        } else {
            std::cout << "  ✗ Section [" << section << "] MISSING\n";
            allSectionsPresent = false;
        }
    }
    
    if (!allSectionsPresent) {
        std::cout << "✗ FAIL: Missing required sections\n";
        return false;
    }
    
    // Check asteroid_selection configuration
    std::cout << "\nAsteroid Selection Configuration:\n";
    std::string inputType = parser.getValue("asteroid_selection", "input_type");
    std::cout << "  - input_type: " << inputType << "\n";
    
    if (inputType == "combined" || inputType == "file") {
        std::string files = parser.getValue("asteroid_selection", "additional_files");
        std::cout << "  - additional_files: " << files << "\n";
        
        if (files.empty()) {
            std::cout << "  ⚠ WARNING: No additional_files specified for input_type=" 
                     << inputType << "\n";
        }
    }
    
    if (inputType == "combined" || inputType == "range") {
        std::string from = parser.getValue("asteroid_selection", "range_from");
        std::string to = parser.getValue("asteroid_selection", "range_to");
        std::cout << "  - range: [" << from << " - " << to << "]\n";
    }
    
    // Check time range
    std::cout << "\nTime Range:\n";
    std::string startDate = parser.getValue("time_range", "start_date");
    std::string endDate = parser.getValue("time_range", "end_date");
    std::cout << "  - start_date: " << startDate << "\n";
    std::cout << "  - end_date: " << endDate << "\n";
    
    if (startDate.empty() || endDate.empty()) {
        std::cout << "✗ FAIL: Missing time range dates\n";
        return false;
    }
    
    // Check output configuration
    std::cout << "\nOutput Configuration:\n";
    std::string formats = parser.getValue("output", "formats");
    std::cout << "  - formats: " << formats << "\n";
    
    if (formats.find("ASTNUM_LIST") != std::string::npos) {
        std::cout << "  ✓ ASTNUM_LIST format enabled\n";
    }
    
    // Check observer location
    std::cout << "\nObserver Location:\n";
    std::string lat = parser.getValue("observer", "latitude");
    std::string lon = parser.getValue("observer", "longitude");
    std::string alt = parser.getValue("observer", "altitude");
    std::cout << "  - latitude: " << lat << "°\n";
    std::cout << "  - longitude: " << lon << "°\n";
    std::cout << "  - altitude: " << alt << " m\n";
    
    // Summary
    std::cout << "\n✓ PASS: Preset file is valid\n";
    std::cout << "Total sections: " << parser.sections.size() << "\n";
    
    return true;
}

int main() {
    std::cout << "========================================\n";
    std::cout << "  OOP Preset Files Validation Test\n";
    std::cout << "========================================\n";
    
    bool allPassed = true;
    
    // Test 1: Quick Combined
    if (!testPresetFile("../preset_quick_combined.oop", 
                       "Quick daily/weekly survey with range + localasteroid")) {
        allPassed = false;
    }
    
    // Test 2: Combined Lists Example
    if (!testPresetFile("../preset_combined_lists_example.oop",
                       "Full featured example with all options")) {
        allPassed = false;
    }
    
    // Test 3: Multifile Survey
    if (!testPresetFile("../preset_multifile_survey.oop",
                       "Multiple file lists without range")) {
        allPassed = false;
    }
    
    // Test 4: Verify example data files exist
    std::cout << "\n--- Testing Example Data Files ---\n";
    std::vector<std::string> exampleFiles = {
        "../example_priority_asteroids.txt",
        "../example_research_targets.txt",
        "../etc/localasteroid"
    };
    
    for (const auto& file : exampleFiles) {
        std::ifstream test(file);
        if (test.good()) {
            // Count lines
            int lines = 0;
            std::string line;
            while (std::getline(test, line)) {
                if (!line.empty() && line[0] != '#') {
                    lines++;
                }
            }
            std::cout << "  ✓ " << file << " (" << lines << " lines)\n";
        } else {
            std::cout << "  ⚠ " << file << " not found (optional)\n";
        }
    }
    
    // Final summary
    std::cout << "\n========================================\n";
    if (allPassed) {
        std::cout << "  ✓✓✓ ALL TESTS PASSED ✓✓✓\n";
        std::cout << "========================================\n";
        std::cout << "\nAll .oop preset files are valid and ready to use!\n";
        std::cout << "\nUsage:\n";
        std::cout << "  ./ioccultcalc --preset preset_quick_combined.oop\n";
        std::cout << "  ./ioccultcalc --preset preset_combined_lists_example.oop\n";
        std::cout << "  ./ioccultcalc --preset preset_multifile_survey.oop\n";
        return 0;
    } else {
        std::cout << "  ✗✗✗ SOME TESTS FAILED ✗✗✗\n";
        std::cout << "========================================\n";
        return 1;
    }
}
