#include "ioccultcalc/orbit_fitter.h"
#include "ioccultcalc/ephemeris.h"
#include "ioccultcalc/force_model.h"
#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/math_utils.h"
#include "ioccultcalc/time_utils.h"
#include "ioccultcalc/coordinates.h"
#include <cmath>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iomanip>
#include <map>

namespace ioccultcalc {

// Implementazione interna
class OrbitFitter::Impl {
public:
    std::shared_ptr<ISPReader> reader;
    OrbitPropagator propagator;
    
    explicit Impl(std::shared_ptr<ISPReader> r) : reader(r) {
        PropagatorOptions opts;
        opts.integrator = IntegratorType::RA15;
        opts.usePlanetaryPerturbations = true;
        opts.useRelativisticCorrections = true;
        opts.tolerance = 1e-12;
        opts.stepSize = 0.5; // giorni
        
        propagator = OrbitPropagator(opts);
    }

    // Calcola posizione osservatore in ICRF (J2000)
    Vector3D getObserverVector(const std::string& mpc_code, const JulianDate& jd) {
        return Coordinates::observerPosition(mpc_code, jd);
    }
};

OrbitFitter::OrbitFitter(std::shared_ptr<ISPReader> reader) 
    : pImpl(new Impl(reader)) {}

OrbitFitter::~OrbitFitter() { delete pImpl; }

OrbitFitResult OrbitFitter::fit(const OrbitalElements& initialElements,
                                const ObservationSet& observations,
                                const OrbitFitOptions& options) {
    
    std::cout << "\n[Scientific Engine] Orbit Fitter (Professional Grade)\n";
    std::cout << "--------------------------------------------------\n";
    
    OrbitFitResult result;
    result.fittedElements = initialElements;
    result.observations = observations;
    
    // Filtra osservazioni valide (copia locale per il fitting)
    ObservationSet workingSet = observations;
    workingSet.computeStatistics();
    
    OrbitalElements currentElements = initialElements;
    double previousRMS = 1e10;
    
    const int nParams = 6; // a, e, i, Omega, omega, M
    
    for (int iter = 0; iter < options.maxIterations; iter++) {
        std::cout << "Iterazione " << (iter + 1) << "... ";
        
        // 1. Calcola effemeridi con Light-Time Iterativo e N-Body
        std::vector<EquatorialCoordinates> computed;
        computeEphemerides(currentElements, workingSet, computed);
        
        // 2. Calcola residui e RMS
        double sumWeightRes2 = 0;
        int nValid = 0;
        for (size_t i = 0; i < workingSet.observations.size(); i++) {
            auto& obs = workingSet.observations[i];
            if (obs.outlier) continue;

            obs.computed = computed[i];
            
            // Residui corretti per cos(dec)
            double dRA = (obs.obs.ra - obs.computed.ra) * std::cos(obs.obs.dec) * RAD_TO_DEG * 3600.0;
            double dDec = (obs.obs.dec - obs.computed.dec) * RAD_TO_DEG * 3600.0;
            
            obs.raResidual = dRA;
            obs.decResidual = dDec;
            obs.totalResidual = std::sqrt(dRA * dRA + dDec * dDec);
            
            // Schema dei pesi: 1/sigma^2
            // Gaia DR3: 0.001", Pre-1950: 2.0", Default: 0.5"
            double sigma = 0.5;
            if (obs.observatoryCode == "247") sigma = 0.001; // Esempio per Gaia
            else if (obs.epoch.jd < 2433282.5) sigma = 2.0; // Prima del 1950
            
            double weight = 1.0 / (sigma * sigma);
            sumWeightRes2 += weight * (dRA * dRA + dDec * dDec);
            nValid++;
        }
        
        double rmsTotal = std::sqrt(sumWeightRes2 / (2.0 * nValid));
        std::cout << "RMS = " << std::fixed << std::setprecision(4) << rmsTotal << "\"\n";
        
        // 3. Outlier Rejection (> 3 sigma dopo la 2a iterazione)
        if (options.rejectOutliers && iter >= 1) {
            int rejected = 0;
            for (auto& obs : workingSet.observations) {
                if (obs.outlier) continue;
                if (std::abs(obs.raResidual) > 3.0 * rmsTotal || std::abs(obs.decResidual) > 3.0 * rmsTotal) {
                    obs.outlier = true;
                    rejected++;
                }
            }
            if (rejected > 0) {
                std::cout << "   [Rigetto] " << rejected << " osservazioni fuori da 3-sigma\n";
                result.nOutliers += rejected;
            }
        }
        
        // 4. Test convergenza
        if (std::abs(rmsTotal - previousRMS) < options.convergenceThreshold) {
            result.converged = true;
            break;
        }
        previousRMS = rmsTotal;

        // 5. Jacobiano Numerico (Differenze Finite Centrate)
        std::vector<std::vector<double>> jacobian;
        computeJacobian(currentElements, workingSet, jacobian);
        
        // 6. Sistema Normale A^T W A x = A^T W b
        std::vector<std::vector<double>> ATA(nParams, std::vector<double>(nParams, 0.0));
        std::vector<double> ATb(nParams, 0.0);
        
        for (size_t i = 0; i < workingSet.observations.size(); i++) {
            if (workingSet.observations[i].outlier) continue;
            
            // Peso dell'osservazione
            double sigma = 0.5;
            if (workingSet.observations[i].observatoryCode == "247") sigma = 0.001;
            else if (workingSet.observations[i].epoch.jd < 2433282.5) sigma = 2.0;
            double w = 1.0 / (sigma * sigma);
            
            double b_ra = workingSet.observations[i].raResidual;
            double b_dec = workingSet.observations[i].decResidual;
            
            for (int j = 0; j < nParams; j++) {
                ATb[j] += w * (jacobian[2*i][j] * b_ra + jacobian[2*i+1][j] * b_dec);
                for (int k = 0; k < nParams; k++) {
                    ATA[j][k] += w * (jacobian[2*i][j] * jacobian[2*i][k] + 
                                     jacobian[2*i+1][j] * jacobian[2*i+1][k]);
                }
            }
        }
        
        // 7. Solutore Robusto (SVD via Jacobi)
        std::vector<double> corrections(nParams);
        if (!MathUtils::solveSVD(ATA, ATb, corrections, 1e-12)) {
            std::cerr << "ERRORE: Sistema normale singolare\n";
            result.converged = false;
            break;
        }
        
        // 8. Aggiorna elementi
        currentElements = updateElements(currentElements, corrections);
        result.iterations = iter + 1;
        result.fittedElements = currentElements;
        result.rmsResidual = rmsTotal;
    }
    
    result.observations = workingSet;
    return result;
}

void OrbitFitter::computeEphemerides(const OrbitalElements& elements,
                                    const ObservationSet& observations,
                                    std::vector<EquatorialCoordinates>& computed) {
    computed.clear();
    computed.reserve(observations.observations.size());
    
    AstDynEquinoctialElements eqElements = elements.toEquinoctial();
    
    for (const auto& obs : observations.observations) {
        // 1. Posizione osservatore (ICRF)
        Vector3D R_obs = pImpl->getObserverVector(obs.observatoryCode, obs.epoch);
        
        // 2. Light-Time Correction Iterativa (3 step)
        double t_obs = obs.epoch.jd;
        double t_emit = t_obs;
        Vector3D r_ast_helio;
        double rho = 1.0; 
        
        for (int i = 0; i < 3; i++) {
            // Propagazione N-Body ad alta precisione
            OrbitState state = pImpl->propagator.propagate(eqElements, JulianDate(t_emit));
            r_ast_helio = state.position;
            
            // Vettore geometrico Osservatore -> Asteroide
            Vector3D rho_vec = r_ast_helio - R_obs;
            rho = rho_vec.magnitude();
            
            // Aggiorna tempo di emissione (rho in AU, c in AU/day)
            t_emit = t_obs - (rho / 173.1446); // 173.1446 = c in AU/day
        }
        
        // 3. Direzione apparente (ICRF)
        Vector3D rho_vec = r_ast_helio - R_obs;
        double ra = std::atan2(rho_vec.y, rho_vec.x);
        if (ra < 0) ra += TWO_PI;
        double dec = std::asin(rho_vec.z / rho);
        
        computed.emplace_back(ra, dec, rho * AU);
    }
}

void OrbitFitter::computeJacobian(const OrbitalElements& elements,
                                 const ObservationSet& observations,
                                 std::vector<std::vector<double>>& jacobian) {
    int nObs = observations.observations.size();
    int nParams = 6;
    jacobian.assign(2 * nObs, std::vector<double>(nParams, 0.0));
    
    std::vector<double> deltas = { 1e-7, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8 };
    
    for (int p = 0; p < nParams; p++) {
        OrbitalElements p_plus = elements;
        OrbitalElements p_minus = elements;
        
        switch(p) {
            case 0: p_plus.a += deltas[p]; p_minus.a -= deltas[p]; break;
            case 1: p_plus.e += deltas[p]; p_minus.e -= deltas[p]; break;
            case 2: p_plus.i += deltas[p]; p_minus.i -= deltas[p]; break;
            case 3: p_plus.Omega += deltas[p]; p_minus.Omega -= deltas[p]; break;
            case 4: p_plus.omega += deltas[p]; p_minus.omega -= deltas[p]; break;
            case 5: p_plus.M += deltas[p]; p_minus.M -= deltas[p]; break;
        }
        
        std::vector<EquatorialCoordinates> eph_plus, eph_minus;
        computeEphemerides(p_plus, observations, eph_plus);
        computeEphemerides(p_minus, observations, eph_minus);
        
        for (int i = 0; i < nObs; i++) {
            double dRA = (eph_plus[i].ra - eph_minus[i].ra) / (2.0 * deltas[p]);
            double dDec = (eph_plus[i].dec - eph_minus[i].dec) / (2.0 * deltas[p]);
            
            jacobian[2*i][p] = dRA * std::cos(observations.observations[i].obs.dec) * RAD_TO_DEG * 3600.0;
            jacobian[2*i+1][p] = dDec * RAD_TO_DEG * 3600.0;
        }
    }
}

OrbitalElements OrbitFitter::updateElements(const OrbitalElements& elements,
                                           const std::vector<double>& corrections) {
    OrbitalElements updated = elements;
    updated.a += corrections[0];
    updated.e += corrections[1];
    updated.i += corrections[2];
    updated.Omega += corrections[3];
    updated.omega += corrections[4];
    updated.M += corrections[5];
    
    auto normalize = [](double& angle) {
        angle = std::fmod(angle, TWO_PI);
        if (angle < 0) angle += TWO_PI;
    };
    normalize(updated.Omega);
    normalize(updated.omega);
    normalize(updated.M);
    
    return updated;
}

void OrbitFitter::computeResiduals(const OrbitalElements& elements, ObservationSet& observations) {
    std::vector<EquatorialCoordinates> computed;
    computeEphemerides(elements, observations, computed);
    for (size_t i = 0; i < observations.observations.size(); i++) {
        observations.observations[i].computed = computed[i];
        double dRA = (observations.observations[i].obs.ra - computed[i].ra) * std::cos(computed[i].dec) * RAD_TO_DEG * 3600.0;
        double dDec = (observations.observations[i].obs.dec - computed[i].dec) * RAD_TO_DEG * 3600.0;
        observations.observations[i].raResidual = dRA;
        observations.observations[i].decResidual = dDec;
        observations.observations[i].totalResidual = std::sqrt(dRA*dRA + dDec*dDec);
    }
}

OrbitalElements OrbitFitter::propagate(const OrbitalElements& elements, const JulianDate& targetEpoch, bool includePerturbations) {
    auto eq = elements.toEquinoctial();
    auto state = pImpl->propagator.propagate(eq, targetEpoch);
    return state.epoch.jd == elements.epoch.jd ? elements : pImpl->propagator.stateToElements(state).toKeplerian();
}

std::vector<std::vector<double>> OrbitFitter::computeCovariance(const OrbitalElements& elements, const ObservationSet& observations) {
    const int nParams = 6;
    std::vector<std::vector<double>> ATA(nParams, std::vector<double>(nParams, 0.0));
    std::vector<std::vector<double>> jacobian;
    
    // 1. Ricalcola lo Jacobiano e la matrice Normale ATA all'ultimo punto di fit
    computeJacobian(elements, observations, jacobian);
    
    for (size_t i = 0; i < observations.observations.size(); i++) {
        if (observations.observations[i].outlier) continue;
        
        double w = observations.observations[i].getWeight(); // 1/sigma^2
        for (int j = 0; j < nParams; j++) {
            for (int k = 0; k < nParams; k++) {
                ATA[j][k] += w * (jacobian[2*i][j] * jacobian[2*i][k] + 
                                  jacobian[2*i+1][j] * jacobian[2*i+1][k]);
            }
        }
    }

    // 2. Decomposizione SVD (via Jacobi) della matrice ATA
    std::vector<std::vector<double>> V(nParams, std::vector<double>(nParams));
    std::vector<double> w_eigen(nParams);
    MathUtils::eigenDecomposition(ATA, V, w_eigen);

    // 3. Inversione della matrice tramite SVD: C = V * [diag(1/w)] * V^T
    std::vector<std::vector<double>> cov(nParams, std::vector<double>(nParams, 0.0));
    const double threshold = 1e-12 * w_eigen[0];

    for (int i = 0; i < nParams; i++) {
        for (int j = 0; j < nParams; j++) {
            for (int k = 0; k < nParams; k++) {
                if (w_eigen[k] > threshold) {
                    cov[i][j] += V[i][k] * V[j][k] / w_eigen[k];
                }
            }
        }
    }

    return cov;
}

void ResidualAnalysis::computeStatistics(const ObservationSet& observations,
                                      double& meanRA, double& meanDec,
                                      double& rmsRA, double& rmsDec,
                                      double& rmsTotal) {
    meanRA = meanDec = rmsRA = rmsDec = rmsTotal = 0.0;
    int n = 0;
    for (const auto& obs : observations.observations) {
        if (obs.outlier) continue;
        meanRA += obs.raResidual;
        meanDec += obs.decResidual;
        rmsRA += obs.raResidual * obs.raResidual;
        rmsDec += obs.decResidual * obs.decResidual;
        n++;
    }
    if (n > 0) {
        meanRA /= n;
        meanDec /= n;
        rmsRA = std::sqrt(rmsRA / n);
        rmsDec = std::sqrt(rmsDec / n);
        rmsTotal = std::sqrt((rmsRA * rmsRA + rmsDec * rmsDec) / 2.0);
    }
}

bool ResidualAnalysis::detectSystematicBias(const ObservationSet& observations,
                                         double& biasRA, double& biasDec) {
    double mRA, mDec, rRA, rDec, rTot;
    computeStatistics(observations, mRA, mDec, rRA, rDec, rTot);
    biasRA = mRA;
    biasDec = mDec;
    // Bias significativo se > 0.1 arcsec o > 0.5 sigma
    return (std::abs(mRA) > 0.1 || std::abs(mDec) > 0.1);
}

std::vector<int> ResidualAnalysis::residualHistogram(const ObservationSet& observations,
                                                  int nBins, double& binWidth) {
    std::vector<int> hist(nBins, 0);
    double maxRes = 0;
    for (const auto& obs : observations.observations) {
        if (!obs.outlier) maxRes = std::max(maxRes, std::abs(obs.totalResidual));
    }
    binWidth = (maxRes > 0) ? (maxRes / nBins) : 0.1;
    for (const auto& obs : observations.observations) {
        if (obs.outlier) continue;
        int bin = std::min(nBins - 1, (int)(obs.totalResidual / binWidth));
        hist[bin]++;
    }
    return hist;
}

} // namespace ioccultcalc
