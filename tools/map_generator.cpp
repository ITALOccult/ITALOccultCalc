#include "ioccultcalc/occultation_map_utility.h"
#include <iostream>

int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "Usage: map_generator <input_xml> <output_png> [data_dir]" << std::endl;
        return 1;
    }

    std::string inputXml = argv[1];
    std::string outputPng = argv[2];
    std::string dataDir = (argc > 3) ? argv[3] : "external/IOC_Earth/data/";

    std::cout << "Loading XML: " << inputXml << std::endl;
    std::cout << "VERIFICATION: Running map_generator NEW VERSION" << std::endl;
    ioccultcalc::OccultationMapUtility mapper;
    if (!mapper.loadFromXml(inputXml)) {
        std::cerr << "Failed to load XML: " << inputXml << std::endl;
        return 1;
    }

    std::cout << "Generating Map: " << outputPng << " using data from " << dataDir << std::endl;
    if (!mapper.generateMap(outputPng, dataDir)) {
        std::cerr << "Failed to generate map: " << outputPng << std::endl;
        return 1;
    }

    std::cout << "✅ Successfully generated map: " << outputPng << std::endl;
    return 0;
}
