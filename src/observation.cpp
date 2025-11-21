#include "ioccultcalc/observation.h"
#include <cmath>
#include <algorithm>
#include <numeric>
#include <set>
#include <fstream>
#include <iomanip>
#include <stdexcept>

namespace ioccultcalc {

void ObservationSet::computeStatistics() {
    if (observations.empty()) return;
    
    numberOfObservations = observations.size();
    
    // Trova prima e ultima osservazione
    auto minmax = std::minmax_element(observations.begin(), observations.end(),
        [](const AstrometricObservation& a, const AstrometricObservation& b) {
            return a.epoch.jd < b.epoch.jd;
        });
    
    firstObservation = minmax.first->epoch;
    lastObservation = minmax.second->epoch;
    arcLength = lastObservation.jd - firstObservation.jd;
    
    // Conta osservatori unici
    std::set<std::string> uniqueObs;
    for (const auto& obs : observations) {
        uniqueObs.insert(obs.observatoryCode);
    }
    numberOfObservatories = uniqueObs.size();
}

void ObservationSet::filterOutliers(double sigmaThreshold) {
    if (observations.size() < 3) return;
    
    // Calcola RMS attuale
    double rms = getRMSResidual();
    
    // Marca outliers
    for (auto& obs : observations) {
        if (obs.totalResidual > sigmaThreshold * rms) {
            obs.outlier = true;
        }
    }
}

std::vector<AstrometricObservation> ObservationSet::getObservationsInRange(
    const JulianDate& start, const JulianDate& end) const {
    
    std::vector<AstrometricObservation> result;
    
    for (const auto& obs : observations) {
        if (obs.epoch.jd >= start.jd && obs.epoch.jd <= end.jd && !obs.outlier) {
            result.push_back(obs);
        }
    }
    
    return result;
}

double ObservationSet::getRMSResidual() const {
    if (observations.empty()) return 0.0;
    
    double sumSq = 0.0;
    int count = 0;
    
    for (const auto& obs : observations) {
        if (!obs.outlier) {
            sumSq += obs.totalResidual * obs.totalResidual;
            count++;
        }
    }
    
    return count > 0 ? sqrt(sumSq / count) : 0.0;
}

double ObservationSet::getRAResidualRMS() const {
    if (observations.empty()) return 0.0;
    
    double sumSq = 0.0;
    int count = 0;
    
    for (const auto& obs : observations) {
        if (!obs.outlier) {
            sumSq += obs.raResidual * obs.raResidual;
            count++;
        }
    }
    
    return count > 0 ? sqrt(sumSq / count) : 0.0;
}

double ObservationSet::getDecResidualRMS() const {
    if (observations.empty()) return 0.0;
    
    double sumSq = 0.0;
    int count = 0;
    
    for (const auto& obs : observations) {
        if (!obs.outlier) {
            sumSq += obs.decResidual * obs.decResidual;
            count++;
        }
    }
    
    return count > 0 ? sqrt(sumSq / count) : 0.0;
}

Observatory Observatory::fromMPCCode(const std::string& code) {
    Observatory obs;
    obs.code = code;
    obs.isSpacecraft = false;
    
    // Database ridotto degli osservatori più comuni
    // In una implementazione completa si caricerebbe da ObsCodes.txt
    
    if (code == "500") {
        obs.name = "Geocenter";
        obs.location = GeographicCoordinates(0, 0, 0);
    } else if (code == "568") {
        obs.name = "Mauna Kea";
        obs.location = GeographicCoordinates(-155.4681, 19.8259, 4160);
    } else if (code == "703") {
        obs.name = "Catalina Sky Survey";
        obs.location = GeographicCoordinates(-110.7317, 32.4165, 2790);
    } else if (code == "704") {
        obs.name = "Lincoln Laboratory ETS";
        obs.location = GeographicCoordinates(-106.3089, 33.9783, 2027);
    } else if (code == "G96") {
        obs.name = "Mt. Lemmon Survey";
        obs.location = GeographicCoordinates(-110.7889, 32.4428, 2791);
    } else if (code == "C51") {
        obs.name = "WISE";
        obs.isSpacecraft = true;
    } else if (code == "F51") {
        obs.name = "Pan-STARRS 1";
        obs.location = GeographicCoordinates(-156.2569, 20.7075, 3055);
    } else if (code == "F52") {
        obs.name = "Pan-STARRS 2";
        obs.location = GeographicCoordinates(-156.2569, 20.7075, 3055);
    } else if (code == "J04") {
        obs.name = "ESA Optical Ground Station";
        obs.location = GeographicCoordinates(-16.5114, 28.2986, 2390);
    } else {
        obs.name = "Unknown Observatory (" + code + ")";
        obs.location = GeographicCoordinates(0, 0, 0);
    }
    
    return obs;
}

void ObservationSet::saveToFile(const std::string& filename) const {
    std::ofstream out(filename);
    if (!out) {
        throw std::runtime_error("Cannot open file: " + filename);
    }
    
    out << "# Observation Set\n";
    out << "# Number of observations: " << numberOfObservations << "\n";
    out << "# Arc length: " << arcLength << " days\n";
    out << "# First obs: " << firstObservation.jd << "\n";
    out << "# Last obs:  " << lastObservation.jd << "\n";
    out << "# RMS residual: " << getRMSResidual() << " arcsec\n";
    out << "#\n";
    out << "# Format: Date(JD) RA(deg) Dec(deg) RA_res(as) Dec_res(as) Total_res(as) Outlier Observatory\n";
    out << "#\n";
    
    out << std::fixed << std::setprecision(6);
    for (const auto& obs : observations) {
        out << obs.epoch.jd << " "
            << obs.obs.ra * RAD_TO_DEG << " "
            << obs.obs.dec * RAD_TO_DEG << " "
            << obs.raResidual << " "
            << obs.decResidual << " "
            << obs.totalResidual << " "
            << (obs.outlier ? "1" : "0") << " "
            << obs.observatoryCode << "\n";
    }
    
    out.close();
}

} // namespace ioccultcalc
