#include "ioccultcalc/orbit_determination.h"
#include "ioccultcalc/coordinates.h"
#include "ioccultcalc/time_utils.h"
#include <cmath>
#include <algorithm>
#include <stdexcept>

namespace ioccultcalc {

double OrbitFitResult::getPositionUncertainty(const JulianDate& time) const {
    // Stima semplificata dell'incertezza posizionale
    // Propaga la matrice di covarianza
    
    double dt = time.jd - improvedElements.epoch.jd;
    
    // Incertezza cresce con tempo (approssimazione lineare)
    double baseUncertainty = sqrt(sigma_a * sigma_a + sigma_h * sigma_h + 
                                  sigma_k * sigma_k) * AU;
    
    // Fattore di crescita temporale
    double timeGrowth = 1.0 + fabs(dt) / 365.25;
    
    return baseUncertainty * timeGrowth;
}

void OrbitFitResult::getErrorEllipse(const JulianDate& time,
                                     double& semiMajor, double& semiMinor, 
                                     double& angle) const {
    // Calcola ellisse di errore dalla matrice covarianza
    // Implementazione semplificata - usa solo componenti principali
    
    double sigma_ra = sqrt(sigma_h * sigma_h + sigma_k * sigma_k);
    double sigma_dec = sqrt(sigma_p * sigma_p + sigma_q * sigma_q);
    
    semiMajor = std::max(sigma_ra, sigma_dec) * 3600.0; // arcsec
    semiMinor = std::min(sigma_ra, sigma_dec) * 3600.0;
    angle = atan2(sigma_dec, sigma_ra) * RAD_TO_DEG;
}

class OrbitDetermination::Impl {
public:
    AstDynEquinoctialElements initialElements;
    ObservationSet observations;
    FitOptions options;
    Ephemeris ephemeris;
    
    bool useNumericalIntegration;
    bool usePlanetaryPerturbations;
    
    Impl() : useNumericalIntegration(false), usePlanetaryPerturbations(false) {}
};

OrbitDetermination::OrbitDetermination() : pImpl(new Impl()) {}

OrbitDetermination::~OrbitDetermination() = default;

void OrbitDetermination::setInitialElements(const AstDynEquinoctialElements& elements) {
    pImpl->initialElements = elements;
    pImpl->ephemeris.setElements(elements);
}

void OrbitDetermination::setObservations(const ObservationSet& observations) {
    pImpl->observations = observations;
}

void OrbitDetermination::setFitOptions(const FitOptions& options) {
    pImpl->options = options;
}

OrbitDetermination::FitOptions OrbitDetermination::getFitOptions() const {
    return pImpl->options;
}

void OrbitDetermination::enableNumericalIntegration(bool enable) {
    pImpl->useNumericalIntegration = enable;
}

void OrbitDetermination::enablePlanetaryPerturbations(bool enable) {
    pImpl->usePlanetaryPerturbations = enable;
}

OrbitFitResult OrbitDetermination::fitOrbit() {
    return differentialCorrection(20, 1e-10, pImpl->options.removeOutliers);
}

OrbitFitResult OrbitDetermination::differentialCorrection(
    int maxIterations,
    double convergenceTolerance,
    bool robustFit) {
    
    OrbitFitResult result;
    result.initialElements = pImpl->initialElements;
    result.numberOfObservations = pImpl->observations.observations.size();
    
    if (result.numberOfObservations < 3) {
        throw std::runtime_error("Need at least 3 observations for orbit determination");
    }
    
    AstDynEquinoctialElements currentElements = pImpl->initialElements;
    double previousRMS = 1e10;
    
    for (int iter = 0; iter < maxIterations; ++iter) {
        result.iterations = iter + 1;
        
        // 1. Calcola residui con elementi correnti
        computeResiduals(currentElements, pImpl->observations);
        
        // 2. Opzionale: rimuovi outliers
        if (robustFit && iter > 0) {
            pImpl->observations.filterOutliers(pImpl->options.outlierThreshold);
        }
        
        // 3. Calcola Jacobiana e vettore residui
        auto jacobian = computeJacobian(currentElements, pImpl->observations);
        auto residuals = computeResidualVector(currentElements, pImpl->observations);
        
        // 4. Calcola pesi
        std::vector<double> weights;
        if (pImpl->options.useWeights) {
            for (const auto& obs : pImpl->observations.observations) {
                if (!obs.outlier) {
                    weights.push_back(obs.getWeight());
                    weights.push_back(obs.getWeight());
                }
            }
        } else {
            weights.resize(residuals.size(), 1.0);
        }
        
        // 5. Risolvi sistema least squares
        auto corrections = solveLeastSquares(jacobian, residuals, weights);
        
        // 6. Applica correzioni agli elementi
        currentElements.a += corrections[0];
        currentElements.h += corrections[1];
        currentElements.k += corrections[2];
        currentElements.p += corrections[3];
        currentElements.q += corrections[4];
        currentElements.lambda += corrections[5];
        
        // 7. Controlla convergenza
        double correctionNorm = 0.0;
        for (double c : corrections) {
            correctionNorm += c * c;
        }
        correctionNorm = sqrt(correctionNorm);
        
        // Calcola RMS residui
        computeResiduals(currentElements, pImpl->observations);
        double currentRMS = pImpl->observations.getRMSResidual();
        
        // Convergenza se correzioni piccole e RMS stabile
        if (correctionNorm < convergenceTolerance && 
            fabs(currentRMS - previousRMS) < 0.001) {
            result.converged = true;
            break;
        }
        
        previousRMS = currentRMS;
    }
    
    // Risultati finali
    result.improvedElements = currentElements;
    result.rmsResidual = pImpl->observations.getRMSResidual();
    result.raRMS = pImpl->observations.getRAResidualRMS();
    result.decRMS = pImpl->observations.getDecResidualRMS();
    
    // Conta outliers
    result.numberOfOutliers = 0;
    for (const auto& obs : pImpl->observations.observations) {
        if (obs.outlier) result.numberOfOutliers++;
    }
    
    // Gradi di libertà
    result.degreesOfFreedom = result.numberOfObservations * 2 - 6;
    
    // Chi-quadro
    result.chi2 = 0.0;
    for (const auto& obs : pImpl->observations.observations) {
        if (!obs.outlier) {
            double weight = obs.getWeight();
            result.chi2 += weight * obs.totalResidual * obs.totalResidual;
        }
    }
    
    // Stima incertezze (diagonale matrice covarianza)
    // Semplificazione - dovrebbe essere calcolata dalla Jacobiana
    result.sigma_a = result.rmsResidual * 0.1 / AU;
    result.sigma_h = result.rmsResidual * 0.01;
    result.sigma_k = result.rmsResidual * 0.01;
    result.sigma_p = result.rmsResidual * 0.005;
    result.sigma_q = result.rmsResidual * 0.005;
    result.sigma_lambda = result.rmsResidual * DEG_TO_RAD;
    
    return result;
}

void OrbitDetermination::computeResiduals(const AstDynEquinoctialElements& elements,
                                         ObservationSet& observations) {
    Ephemeris eph(elements);
    
    for (auto& obs : observations.observations) {
        if (obs.outlier) continue;
        
        // Calcola effemeridi per questa osservazione
        obs.computed = computeEphemeris(elements, obs);
        
        // Calcola residui (O-C)
        double dRA = (obs.obs.ra - obs.computed.ra) * cos(obs.obs.dec);
        double dDec = obs.obs.dec - obs.computed.dec;
        
        obs.raResidual = dRA * RAD_TO_DEG * 3600.0;     // arcsec
        obs.decResidual = dDec * RAD_TO_DEG * 3600.0;   // arcsec
        obs.totalResidual = sqrt(obs.raResidual * obs.raResidual + 
                                obs.decResidual * obs.decResidual);
    }
}

EquatorialCoordinates OrbitDetermination::computeEphemeris(
    const AstDynEquinoctialElements& elements,
    const AstrometricObservation& obs) {
    
    // Calcola effemeridi geocentriche
    Ephemeris eph(elements);
    EphemerisData ephData = eph.compute(obs.epoch);
    
    // Se l'osservatorio non è geocentrico, applica correzione topocent rica
    if (obs.observatoryCode != "500") {
        // Converti posizione osservatorio in ECEF
        Vector3D observerECEF = Coordinates::geographicToECEF(obs.observerLocation);
        
        // Ruota al sistema equatoriale (semplificazione - ignora precessione/nutazione)
        double gmst = TimeUtils::gmst(obs.epoch);
        double cosGMST = cos(gmst);
        double sinGMST = sin(gmst);
        
        Vector3D observerEquatorial;
        observerEquatorial.x = observerECEF.x * cosGMST - observerECEF.y * sinGMST;
        observerEquatorial.y = observerECEF.x * sinGMST + observerECEF.y * cosGMST;
        observerEquatorial.z = observerECEF.z;
        
        // Converti km -> AU
        observerEquatorial = observerEquatorial * (1.0 / AU);
        
        // Posizione asteroide geocentrico
        Vector3D asteroidGeo = Coordinates::equatorialToCartesian(ephData.geocentricPos);
        asteroidGeo = asteroidGeo * ephData.distance;
        
        // Posizione topocentric a
        Vector3D asteroidTopo = asteroidGeo - observerEquatorial;
        
        // Riconverti in coordinate equatoriali
        return Coordinates::cartesianToEquatorial(asteroidTopo);
    }
    
    return ephData.geocentricPos;
}

std::vector<std::vector<double>> OrbitDetermination::computeJacobian(
    const AstDynEquinoctialElements& elements,
    const ObservationSet& observations) {
    
    // Jacobiana: derivate parziali di (RA, Dec) rispetto agli elementi orbitali
    // Usa differenze finite
    
    const double delta = 1e-8;
    int nObs = 0;
    for (const auto& obs : observations.observations) {
        if (!obs.outlier) nObs++;
    }
    
    std::vector<std::vector<double>> jacobian(nObs * 2, std::vector<double>(6, 0.0));
    
    // Per ogni elemento orbitale
    for (int j = 0; j < 6; ++j) {
        AstDynEquinoctialElements elemPlus = elements;
        
        // Perturba elemento j
        switch(j) {
            case 0: elemPlus.a += delta * elements.a; break;
            case 1: elemPlus.h += delta; break;
            case 2: elemPlus.k += delta; break;
            case 3: elemPlus.p += delta; break;
            case 4: elemPlus.q += delta; break;
            case 5: elemPlus.lambda += delta; break;
        }
        
        // Calcola effemeridi perturbate
        int row = 0;
        for (const auto& obs : observations.observations) {
            if (obs.outlier) continue;
            
            auto pos0 = computeEphemeris(elements, obs);
            auto posPlus = computeEphemeris(elemPlus, obs);
            
            // Derivata RA
            double dRA = (posPlus.ra - pos0.ra) * cos(pos0.dec);
            double deltaElem = (j == 0) ? delta * elements.a : delta;
            jacobian[row * 2][j] = (dRA * RAD_TO_DEG * 3600.0) / deltaElem;
            
            // Derivata Dec
            double dDec = posPlus.dec - pos0.dec;
            jacobian[row * 2 + 1][j] = (dDec * RAD_TO_DEG * 3600.0) / deltaElem;
            
            row++;
        }
    }
    
    return jacobian;
}

std::vector<double> OrbitDetermination::computeResidualVector(
    const AstDynEquinoctialElements& elements,
    const ObservationSet& observations) {
    
    std::vector<double> residuals;
    
    for (const auto& obs : observations.observations) {
        if (!obs.outlier) {
            residuals.push_back(obs.raResidual);
            residuals.push_back(obs.decResidual);
        }
    }
    
    return residuals;
}

std::vector<double> OrbitDetermination::solveLeastSquares(
    const std::vector<std::vector<double>>& jacobian,
    const std::vector<double>& residuals,
    const std::vector<double>& weights) {
    
    // Risolve sistema normale: (J^T W J) x = J^T W r
    // Dove J = Jacobiana, W = matrice pesi, r = residui
    
    int m = jacobian.size();    // numero equazioni (2 * nObs)
    int n = 6;                  // numero parametri
    
    // Calcola J^T W J
    std::vector<std::vector<double>> JtWJ(n, std::vector<double>(n, 0.0));
    
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            for (int k = 0; k < m; ++k) {
                JtWJ[i][j] += jacobian[k][i] * weights[k] * jacobian[k][j];
            }
        }
    }
    
    // Calcola J^T W r
    std::vector<double> JtWr(n, 0.0);
    for (int i = 0; i < n; ++i) {
        for (int k = 0; k < m; ++k) {
            JtWr[i] += jacobian[k][i] * weights[k] * residuals[k];
        }
    }
    
    // Risolvi sistema lineare con eliminazione Gaussiana
    // (implementazione semplificata - per produzione usare LAPACK)
    
    // Forward elimination
    for (int i = 0; i < n; ++i) {
        // Trova pivot
        int maxRow = i;
        for (int k = i + 1; k < n; ++k) {
            if (fabs(JtWJ[k][i]) > fabs(JtWJ[maxRow][i])) {
                maxRow = k;
            }
        }
        
        // Swap rows
        std::swap(JtWJ[i], JtWJ[maxRow]);
        std::swap(JtWr[i], JtWr[maxRow]);
        
        // Eliminate
        for (int k = i + 1; k < n; ++k) {
            double factor = JtWJ[k][i] / JtWJ[i][i];
            for (int j = i; j < n; ++j) {
                JtWJ[k][j] -= factor * JtWJ[i][j];
            }
            JtWr[k] -= factor * JtWr[i];
        }
    }
    
    // Back substitution
    std::vector<double> corrections(n, 0.0);
    for (int i = n - 1; i >= 0; --i) {
        corrections[i] = JtWr[i];
        for (int j = i + 1; j < n; ++j) {
            corrections[i] -= JtWJ[i][j] * corrections[j];
        }
        corrections[i] /= JtWJ[i][i];
    }
    
    return corrections;
}

// Metodi statici per orbit determination iniziale

AstDynEquinoctialElements OrbitDetermination::gaussMethod(
    const AstrometricObservation& obs1,
    const AstrometricObservation& obs2,
    const AstrometricObservation& obs3) {
    
    // Metodo di Gauss - implementazione semplificata
    // Richiede 3 osservazioni ben distribuite
    
    // TODO: Implementare metodo di Gauss completo
    // Per ora, restituisce elementi dummy
    
    AstDynEquinoctialElements elem;
    elem.a = 2.5; // AU (guess asteroide main belt)
    elem.epoch = obs2.epoch;
    
    return elem;
}

AstDynEquinoctialElements OrbitDetermination::laplaceMethod(
    const AstrometricObservation& obs1,
    const AstrometricObservation& obs2) {
    
    // Metodo di Laplace - implementazione semplificata
    
    // TODO: Implementare metodo di Laplace completo
    
    AstDynEquinoctialElements elem;
    elem.a = 2.5;
    elem.epoch = obs1.epoch;
    
    return elem;
}

// OrbitQuality

int OrbitQuality::computeConditionCode(const ObservationSet& observations) {
    // Condition code 0-9 secondo convenzione MPC
    // 0 = ottimo (arc > 10 anni, molte obs)
    // 9 = molto incerto (arc < 1 giorno, poche obs)
    
    double arcYears = observations.arcLength / 365.25;
    int nObs = observations.numberOfObservations;
    
    if (arcYears >= 10 && nObs >= 100) return 0;
    if (arcYears >= 5 && nObs >= 50) return 1;
    if (arcYears >= 2 && nObs >= 20) return 2;
    if (arcYears >= 1 && nObs >= 10) return 3;
    if (arcYears >= 0.5 && nObs >= 5) return 4;
    if (arcYears >= 0.1 && nObs >= 3) return 5;
    if (arcYears >= 0.03) return 6;
    if (arcYears >= 0.01) return 7;
    if (arcYears >= 0.003) return 8;
    return 9;
}

double OrbitQuality::computeUncertaintyParameter(
    const OrbitFitResult& fitResult,
    double daysFromLastObs) {
    
    // Stima incertezza effemeridi (arcsec) in funzione del tempo
    double baseUncertainty = fitResult.rmsResidual;
    double timeGrowth = 1.0 + daysFromLastObs / 365.25;
    
    return baseUncertainty * timeGrowth;
}

bool OrbitQuality::isOrbitReliable(
    const OrbitFitResult& fitResult,
    const ObservationSet& observations,
    double minimumArcDays,
    int minimumObservations) {
    
    if (!fitResult.converged) return false;
    if (observations.arcLength < minimumArcDays) return false;
    if (observations.numberOfObservations < minimumObservations) return false;
    if (fitResult.rmsResidual > 5.0) return false; // > 5 arcsec
    
    return true;
}

} // namespace ioccultcalc
