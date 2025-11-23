/**
 * @file test_config_manager.cpp
 * @brief Test configuration management system
 */

#include <iostream>
#include <ioccultcalc/config_manager.h>

using namespace ioccultcalc;

void printSection(const std::string& title) {
    std::cout << "\n" << std::string(60, '=') << "\n";
    std::cout << title << "\n";
    std::cout << std::string(60, '=') << "\n";
}

int main() {
    try {
        printSection("Test 1: Create configuration using builder");
        
        ConfigManager config = ConfigBuilder()
            .setObject("433 Eros", "433")
            .setPropagator("RK4")
            .setStepSize(0.05)
            .setTimeSpan(2461041.0, 2461405.0)
            .enablePlanets(true)
            .enableAsteroids(17)
            .enableRelativity(true)
            .setJplEphemeris("DE441")
            .setAst17File("~/.ioccultcalc/ephemerides/codes_300ast_20100725.bsp")
            .setSearchInterval(2461041.0, 2461405.0, 0.5)
            .setMagnitudeLimit(14.0)
            .setOutputFormat("JSON")
            .setOutputFile("results.json")
            .setVerbosity(1)
            .build();
        
        config.setMetadata("created_by", "IOccultCalc");
        config.setMetadata("version", "1.0");
        config.setMetadata("date", "2025-11-22");
        
        std::cout << "\n✓ Configuration created successfully\n";
        
        // Print some parameters
        auto objSection = config.getSection(ConfigSection::OBJECT);
        if (objSection) {
            auto name = objSection->getParameter("name");
            if (name) {
                std::cout << "  Object: " << name->asString() << "\n";
            }
        }
        
        auto propagSection = config.getSection(ConfigSection::PROPAGATION);
        if (propagSection) {
            auto stepSize = propagSection->getParameter("step_size");
            if (stepSize) {
                std::cout << "  Step size: " << stepSize->asDouble() << " days\n";
            }
        }
        
        printSection("Test 2: Export to JSON");
        
        config.saveToJson("test_config.json");
        std::cout << "✓ Saved to test_config.json\n";
        
        // Display JSON
        auto jsonObj = config.toJson();
        std::cout << "\nJSON preview:\n" << jsonObj.dump(2).substr(0, 500) << "...\n";
        
        printSection("Test 3: Export to OOP format (OrbFit-style)");
        
        config.saveToOop("test_config.oop");
        std::cout << "✓ Saved to test_config.oop\n";
        
        printSection("Test 4: Load from JSON");
        
        ConfigManager loadedConfig;
        loadedConfig.loadFromJson("test_config.json");
        std::cout << "✓ Loaded from test_config.json\n";
        
        // Verify loaded data
        auto loadedObj = loadedConfig.getSection(ConfigSection::OBJECT);
        if (loadedObj) {
            auto name = loadedObj->getParameter("name");
            if (name) {
                std::cout << "  Verified object name: " << name->asString() << "\n";
            }
        }
        
        printSection("Test 5: Validate configuration");
        
        std::vector<std::string> errors;
        bool valid = config.validate(errors);
        
        if (valid) {
            std::cout << "✓ Configuration is valid\n";
        } else {
            std::cout << "✗ Configuration has errors:\n";
            for (const auto& error : errors) {
                std::cout << "  - " << error << "\n";
            }
        }
        
        printSection("Test 6: Preset configurations");
        
        auto defaultConfig = ConfigManager::createDefault();
        std::cout << "✓ Created default configuration\n";
        defaultConfig.saveToJson("preset_default.json");
        
        auto highPrecConfig = ConfigManager::createHighPrecision();
        std::cout << "✓ Created high-precision configuration\n";
        highPrecConfig.saveToJson("preset_high_precision.json");
        
        auto fastConfig = ConfigManager::createFastSearch();
        std::cout << "✓ Created fast-search configuration\n";
        fastConfig.saveToJson("preset_fast_search.json");
        
        printSection("Test 7: Parameter access and modification");
        
        // Find parameter across all sections
        auto stepSizeParam = config.findParameter("step_size");
        if (stepSizeParam) {
            std::cout << "Found step_size: " << stepSizeParam->asDouble() << "\n";
        }
        
        // Modify a section
        auto modifiedPropag = config.getSection(ConfigSection::PROPAGATION);
        if (modifiedPropag) {
            ConfigSectionData updated = *modifiedPropag;
            updated.setParameter("step_size", 0.01, "Reduced step size for higher precision");
            config.addSection(updated);
            std::cout << "✓ Modified step_size to 0.01\n";
        }
        
        printSection("Test 8: Section iteration");
        
        auto allSections = config.getAllSections();
        std::cout << "Configuration has " << allSections.size() << " sections:\n";
        for (const auto& section : allSections) {
            auto params = section.getAllParameters();
            std::cout << "  - " << section.getName() << " (" << params.size() << " parameters)\n";
        }
        
        printSection("Test 9: Type conversions");
        
        ConfigSectionData testSection(ConfigSection::CUSTOM, "test");
        testSection.setParameter("string_val", "Hello World");
        testSection.setParameter("int_val", 42);
        testSection.setParameter("double_val", 3.14159);
        testSection.setParameter("bool_val", true);
        testSection.setParameter("array_val", std::vector<std::string>{"a", "b", "c"});
        
        std::cout << "String: " << testSection.getParameter("string_val")->asString() << "\n";
        std::cout << "Int: " << testSection.getParameter("int_val")->asInt() << "\n";
        std::cout << "Double: " << testSection.getParameter("double_val")->asDouble() << "\n";
        std::cout << "Bool: " << (testSection.getParameter("bool_val")->asBool() ? "true" : "false") << "\n";
        
        auto arrayVals = testSection.getParameter("array_val")->asArray();
        std::cout << "Array: [";
        for (size_t i = 0; i < arrayVals.size(); ++i) {
            if (i > 0) std::cout << ", ";
            std::cout << arrayVals[i];
        }
        std::cout << "]\n";
        
        printSection("All tests completed successfully!");
        
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "\n❌ Error: " << e.what() << "\n";
        return 1;
    }
}
