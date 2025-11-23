/**
 * @file parallel_algorithms.cpp
 * @brief Implementation of advanced parallelization
 */

#include "parallel_algorithms.h"
#include "ioccultcalc/types.h"
#include "ioccultcalc/orbit_propagator.h"
#include "ioccultcalc/star_catalog.h"
#include <algorithm>
#include <chrono>
#include <iostream>
#include <cmath>

#ifdef _OPENMP
#include <omp.h>
#endif

namespace ioccultcalc {

// ============================================================================
// Configuration and Statistics
// ============================================================================

ParallelConfig::ParallelConfig()
    : numThreads(0)
    , backend(ParallelBackend::OPENMP)
    , enableLoadBalancing(true)
    , chunkSize(0)
    , enableProfiling(false)
{}

ParallelStatistics::ParallelStatistics()
    : numTasks(0)
    , numThreadsUsed(1)
    , totalTimeSeconds(0.0)
    , computeTimeSeconds(0.0)
    , overheadPercent(0.0)
    , speedup(1.0)
    , efficiency(1.0)
{}

void ParallelStatistics::print() const {
    std::cout << "Parallel Execution Statistics:\n";
    std::cout << "  Tasks: " << numTasks << "\n";
    std::cout << "  Threads: " << numThreadsUsed << "\n";
    std::cout << "  Total time: " << totalTimeSeconds << " s\n";
    std::cout << "  Compute time: " << computeTimeSeconds << " s\n";
    std::cout << "  Overhead: " << overheadPercent << " %\n";
    std::cout << "  Speedup: " << speedup << "x\n";
    std::cout << "  Efficiency: " << (efficiency * 100.0) << " %\n";
}

BatchPropagationResult::BatchPropagationResult()
    : success(true)
{}

// Thread-local statistics
thread_local ParallelStatistics g_lastStats;

// ============================================================================
// ParallelPropagator Implementation
// ============================================================================

ParallelStatistics ParallelPropagator::getStatistics() {
    return g_lastStats;
}

// ============================================================================
// ParallelOccultationSearch Implementation (Stub)
// ============================================================================

ParallelStatistics ParallelOccultationSearch::getStatistics() {
    return g_lastStats;
}

// ============================================================================
// ParallelTransforms Implementation
// ============================================================================

std::vector<Vector3D> ParallelTransforms::applyAberrationBatch(
    const std::vector<Vector3D>& positions,
    const std::vector<Vector3D>& velocities,
    const Vector3D& earthPos,
    double jd,
    const ParallelConfig& config)
{
    size_t n = positions.size();
    std::vector<Vector3D> apparent(n);
    
    int numThreads = config.numThreads ? config.numThreads : getOptimalThreadCount();
    
#ifdef _OPENMP
    #pragma omp parallel for num_threads(numThreads)
    for (size_t i = 0; i < n; ++i) {
        // Apply planetary aberration
        Vector3D geo = positions[i] - earthPos;
        double distance = geo.magnitude();
        double lightTime = distance / 173.144632674; // AU/day to c
        
        apparent[i] = positions[i] - velocities[i] * lightTime;
    }
#else
    for (size_t i = 0; i < n; ++i) {
        Vector3D geo = positions[i] - earthPos;
        double distance = geo.magnitude();
        double lightTime = distance / 173.144632674;
        apparent[i] = positions[i] - velocities[i] * lightTime;
    }
#endif
    
    return apparent;
}

std::vector<Vector3D> ParallelTransforms::applyTopocentricBatch(
    const std::vector<Vector3D>& geocentricPos,
    const Observer& observer,
    const std::vector<double>& jds,
    const ParallelConfig& config)
{
    size_t n = geocentricPos.size();
    std::vector<Vector3D> topocentric(n);
    
    int numThreads = config.numThreads ? config.numThreads : getOptimalThreadCount();
    
#ifdef _OPENMP
    #pragma omp parallel for num_threads(numThreads)
    for (size_t i = 0; i < n; ++i) {
        // Apply topocentric correction
        // (simplified - actual would use topocentric.h)
        topocentric[i] = geocentricPos[i];
    }
#else
    for (size_t i = 0; i < n; ++i) {
        topocentric[i] = geocentricPos[i];
    }
#endif
    
    return topocentric;
}

std::vector<Vector3D> ParallelTransforms::convertFrameBatch(
    const std::vector<Vector3D>& positions,
    CoordinateFrame sourceFrame,
    CoordinateFrame targetFrame,
    const std::vector<double>& jds,
    const ParallelConfig& config)
{
    size_t n = positions.size();
    std::vector<Vector3D> converted(n);
    
    int numThreads = config.numThreads ? config.numThreads : getOptimalThreadCount();
    
#ifdef _OPENMP
    #pragma omp parallel for num_threads(numThreads)
    for (size_t i = 0; i < n; ++i) {
        // Frame conversion logic
        converted[i] = positions[i];
    }
#else
    for (size_t i = 0; i < n; ++i) {
        converted[i] = positions[i];
    }
#endif
    
    return converted;
}

// ============================================================================
// Utility Functions
// ============================================================================

int getOptimalThreadCount() {
#ifdef _OPENMP
    return omp_get_max_threads();
#else
    return 1;
#endif
}

ParallelBackend getCurrentBackend() {
#ifdef _OPENMP
    return ParallelBackend::OPENMP;
#else
    return ParallelBackend::SERIAL;
#endif
}

bool isOpenMPAvailable() {
#ifdef _OPENMP
    return true;
#else
    return false;
#endif
}

bool isTBBAvailable() {
    return false; // TBB not implemented yet
}

void setGlobalThreadCount(int numThreads) {
#ifdef _OPENMP
    if (numThreads > 0) {
        omp_set_num_threads(numThreads);
    }
#endif
}

void setParallelizationEnabled(bool enable) {
#ifdef _OPENMP
    if (!enable) {
        omp_set_num_threads(1);
    } else {
        omp_set_num_threads(omp_get_max_threads());
    }
#endif
}

double estimateParallelEfficiency(
    size_t numTasks,
    double avgTaskTimeMs,
    size_t numThreads)
{
    // Amdahl's law approximation
    double overhead = 0.05; // 5% overhead
    double parallelFraction = 0.95;
    
    double theoreticalSpeedup = 1.0 / ((1.0 - parallelFraction) + parallelFraction / numThreads);
    double efficiency = theoreticalSpeedup / numThreads;
    
    // Account for task granularity
    if (numTasks < numThreads) {
        efficiency *= static_cast<double>(numTasks) / numThreads;
    }
    
    return std::max(0.0, std::min(1.0, efficiency * (1.0 - overhead)));
}

// ============================================================================
// ThreadPool Stub
// ============================================================================

class ThreadPool::Impl {
public:
    size_t numThreads;
    
    explicit Impl(size_t n) : numThreads(n) {}
};

ThreadPool::ThreadPool(size_t numThreads)
    : pImpl(new Impl(numThreads))
{}

ThreadPool::~ThreadPool() {
    delete pImpl;
}

void ThreadPool::wait() {
    // Wait for tasks (stub)
}

size_t ThreadPool::getNumThreads() const {
    return pImpl->numThreads;
}

} // namespace ioccultcalc
