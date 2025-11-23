/**
 * @file parallel_algorithms.h
 * @brief Advanced parallelization for occultation search and propagation
 * 
 * This module extends OpenMP parallelization to more compute-intensive operations:
 * - Batch asteroid propagation
 * - Multi-target occultation searches
 * - Star catalog queries
 * - Ephemeris generation
 * 
 * Key Features:
 * - Task-based parallelism for load balancing
 * - Thread-safe data structures
 * - Configurable thread counts
 * - Performance monitoring
 * 
 * Supported Backends:
 * - OpenMP (primary, cross-platform)
 * - std::thread (fallback)
 * - TBB (optional, Intel Threading Building Blocks)
 * 
 * Thread Safety:
 * All parallel operations are thread-safe and do not require external locking.
 * 
 * Performance Guidelines:
 * - Use parallelization for > 100 asteroids
 * - Optimal thread count: typically number of physical cores
 * - Monitor overhead with getParallelStatistics()
 * 
 * Example:
 * ```cpp
 * ParallelConfig config;
 * config.numThreads = 8;
 * config.enableLoadBalancing = true;
 * 
 * std::vector<Asteroid> asteroids = loadAsteroids();
 * auto results = ParallelPropagator::propagateBatch(
 *     asteroids, startJD, endJD, stepDays, config
 * );
 * ```
 */

#ifndef PARALLEL_ALGORITHMS_H
#define PARALLEL_ALGORITHMS_H

#include <vector>
#include <functional>
#include <string>

namespace ioccultcalc {

// Forward declarations
struct Vector3D;
struct Observer;
struct OccultationEvent;
struct Star;
struct OrbitalElements;
enum class CoordinateFrame;

class Asteroid;
class Propagator;

/**
 * @brief Parallelization backend type
 */
enum class ParallelBackend {
    OPENMP,      ///< OpenMP (primary, cross-platform)
    STD_THREAD,  ///< std::thread (fallback)
    TBB,         ///< Intel Threading Building Blocks (optional)
    SERIAL       ///< No parallelization (for debugging)
};

/**
 * @brief Configuration for parallel operations
 */
struct ParallelConfig {
    int numThreads;              ///< Number of threads (0 = auto-detect)
    ParallelBackend backend;     ///< Parallelization backend
    bool enableLoadBalancing;    ///< Enable dynamic load balancing
    size_t chunkSize;            ///< Work chunk size (0 = auto)
    bool enableProfiling;        ///< Collect performance statistics
    
    ParallelConfig();
};

/**
 * @brief Statistics for parallel execution
 */
struct ParallelStatistics {
    size_t numTasks;             ///< Total number of tasks
    size_t numThreadsUsed;       ///< Threads actually used
    double totalTimeSeconds;     ///< Total wall time
    double computeTimeSeconds;   ///< Pure compute time
    double overheadPercent;      ///< Threading overhead %
    double speedup;              ///< Speedup vs serial
    double efficiency;           ///< Parallel efficiency
    
    ParallelStatistics();
    void print() const;
};

/**
 * @brief Batch propagation results
 */
struct BatchPropagationResult {
    std::vector<Vector3D> positions;  ///< Propagated positions
    std::vector<Vector3D> velocities; ///< Propagated velocities
    std::vector<double> jds;          ///< Julian dates
    bool success;                     ///< Overall success flag
    std::string errorMessage;         ///< Error description if failed
    
    BatchPropagationResult();
};

// ============================================================================
// Parallel Propagator (Framework - stub implementations)
// ============================================================================

/**
 * @brief High-performance parallel asteroid propagation framework
 * 
 * NOTE: This is a framework/API definition for future parallel implementations.
 * Current implementations are stubs that demonstrate the OpenMP structure.
 */
class ParallelPropagator {
public:
    /**
     * @brief Get statistics from last parallel operation
     */
    static ParallelStatistics getStatistics();
};

// ============================================================================
// Parallel Occultation Search (Framework - stub implementations)
// ============================================================================

/**
 * @brief High-performance parallel occultation search framework
 * 
 * NOTE: This is a framework/API definition for future parallel implementations.
 */
class ParallelOccultationSearch {
public:
    /**
     * @brief Get statistics from last parallel search
     */
    static ParallelStatistics getStatistics();
};

// ============================================================================
// Parallel Transform Operations
// ============================================================================

/**
 * @brief Parallel coordinate transformations and corrections
 */
class ParallelTransforms {
public:
    /**
     * @brief Apply aberration correction to batch of positions
     * 
     * @param positions Geometric positions
     * @param velocities Velocities
     * @param earthPos Earth position
     * @param jd Julian date
     * @param config Parallelization configuration
     * @return Apparent positions
     */
    static std::vector<Vector3D> applyAberrationBatch(
        const std::vector<Vector3D>& positions,
        const std::vector<Vector3D>& velocities,
        const Vector3D& earthPos,
        double jd,
        const ParallelConfig& config = ParallelConfig()
    );
    
    /**
     * @brief Apply topocentric correction to batch
     * 
     * @param geocentricPos Geocentric positions
     * @param observer Observer location
     * @param jds Julian dates (for parallax)
     * @param config Parallelization configuration
     * @return Topocentric positions
     */
    static std::vector<Vector3D> applyTopocentricBatch(
        const std::vector<Vector3D>& geocentricPos,
        const Observer& observer,
        const std::vector<double>& jds,
        const ParallelConfig& config = ParallelConfig()
    );
    
    /**
     * @brief Batch coordinate frame conversion
     * 
     * @param positions Positions in source frame
     * @param sourceFrame Source coordinate frame
     * @param targetFrame Target coordinate frame
     * @param jds Julian dates (for precession)
     * @param config Parallelization configuration
     * @return Positions in target frame
     */
    static std::vector<Vector3D> convertFrameBatch(
        const std::vector<Vector3D>& positions,
        CoordinateFrame sourceFrame,
        CoordinateFrame targetFrame,
        const std::vector<double>& jds,
        const ParallelConfig& config = ParallelConfig()
    );
};

// ============================================================================
// Thread Pool (Internal)
// ============================================================================

/**
 * @brief Thread pool for task-based parallelism
 * 
 * Internal class for managing worker threads and task queue.
 * Used automatically by parallel algorithms when load balancing is enabled.
 */
class ThreadPool {
public:
    explicit ThreadPool(size_t numThreads);
    ~ThreadPool();
    
    // Submit task for execution
    template<typename F>
    void submit(F&& task);
    
    // Wait for all tasks to complete
    void wait();
    
    // Get number of active threads
    size_t getNumThreads() const;
    
private:
    class Impl;
    Impl* pImpl;
    
    // Disable copy/move
    ThreadPool(const ThreadPool&) = delete;
    ThreadPool& operator=(const ThreadPool&) = delete;
};

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * @brief Get optimal thread count for current system
 * @return Number of threads (typically physical core count)
 */
int getOptimalThreadCount();

/**
 * @brief Get current parallelization backend
 * @return Active backend type
 */
ParallelBackend getCurrentBackend();

/**
 * @brief Check if OpenMP is available
 * @return True if compiled with OpenMP support
 */
bool isOpenMPAvailable();

/**
 * @brief Check if TBB is available
 * @return True if compiled with TBB support
 */
bool isTBBAvailable();

/**
 * @brief Set global thread count
 * @param numThreads Number of threads (0 = auto-detect)
 */
void setGlobalThreadCount(int numThreads);

/**
 * @brief Enable/disable parallel execution globally
 * @param enable True to enable parallelization
 */
void setParallelizationEnabled(bool enable);

/**
 * @brief Estimate parallel efficiency for workload
 * 
 * @param numTasks Number of independent tasks
 * @param avgTaskTimeMs Average task execution time (ms)
 * @param numThreads Number of threads
 * @return Estimated parallel efficiency (0-1)
 */
double estimateParallelEfficiency(
    size_t numTasks,
    double avgTaskTimeMs,
    size_t numThreads
);

} // namespace ioccultcalc

#endif // PARALLEL_ALGORITHMS_H
