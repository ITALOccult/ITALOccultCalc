#include "ioccultcalc/spice_spk_reader.h"
#include <stdexcept>
#include <iostream>

namespace ioccultcalc {

class SPICESPKReader::Impl {
};

SPICESPKReader::SPICESPKReader() : pImpl(std::make_unique<Impl>()) {}
SPICESPKReader::~SPICESPKReader() = default;

bool SPICESPKReader::loadFile(const std::string& filepath) {
    std::cerr << "Warning: CSPICE not available. Cannot load SPK file." << std::endl;
    return false;
}

bool SPICESPKReader::ensureFileLoaded(const std::string& name) {
    return false;
}

Vector3D SPICESPKReader::getPosition(int bodyId, double jd, int centerId) {
    throw std::runtime_error("CSPICE not available");
}

std::pair<Vector3D, Vector3D> SPICESPKReader::getState(int bodyId, double jd, int centerId) {
    throw std::runtime_error("CSPICE not available");
}

bool SPICESPKReader::isLoaded() const {
    return false;
}

std::vector<int> SPICESPKReader::getAvailableBodies() const {
    return {};
}

void SPICESPKReader::close() {}

} // namespace ioccultcalc
