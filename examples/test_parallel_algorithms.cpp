/**
 * @file test_parallel_algorithms.cpp
 * @brief Minimal test suite for parallel algorithms framework
 */

#include "parallel_algorithms.h"
#include "ioccultcalc/types.h"
#include <iostream>
#include <iomanip>
#include <chrono>
#include <vector>

using namespace ioccultcalc;

void testParallelConfig() {
    std::cout << "========================================\n";
    std::cout << "TEST: Parallel Configuration\n";
    std::cout << "========================================\n";
    
    ParallelConfig config;
    std::cout << "Default config:\n";
    std::cout << "  Threads: " << (config.numThreads == 0 ? "auto" : std::to_string(config.numThreads)) << "\n";
    std::cout << "  Backend: " << (config.backend == ParallelBackend::OPENMP ? "OpenMP" : "Other") << "\n";
    std::cout << "  Load balancing: " << (config.enableLoadBalancing ? "yes" : "no") << "\n";
    std::cout << "PASS\n\n";
}

void testThreadDetection() {
    std::cout << "========================================\n";
    std::cout << "TEST: Thread Detection\n";
    std::cout << "========================================\n";
    
    int optimalThreads = getOptimalThreadCount();
    bool openmpAvailable = isOpenMPAvailable();
    bool tbbAvailable = isTBBAvailable();
    ParallelBackend backend = getCurrentBackend();
    
    std::cout << "Optimal thread count: " << optimalThreads << "\n";
    std::cout << "OpenMP available: " << (openmpAvailable ? "yes" : "no") << "\n";
    std::cout << "TBB available: " << (tbbAvailable ? "yes" : "no") << "\n";
    std::cout << "Current backend: ";
    switch (backend) {
        case ParallelBackend::OPENMP: std::cout << "OpenMP\n"; break;
        case ParallelBackend::STD_THREAD: std::cout << "std::thread\n"; break;
        case ParallelBackend::TBB: std::cout << "TBB\n"; break;
        case ParallelBackend::SERIAL: std::cout << "Serial\n"; break;
    }
    
    std::cout << (optimalThreads >= 1 ? "PASS" : "FAIL") << "\n\n";
}

void testBasicVectorOperations() {
    std::cout << "========================================\n";
    std::cout << "TEST: Basic Parallel Operations (OpenMP demonstration)\n";
    std::cout << "========================================\n";
    
    const int N = 1000;
    std::vector<double> data(N);
    std::vector<double> results(N);
    
    // Initialize
    for (int i = 0; i < N; ++i) {
        data[i] = i * 0.01;
    }
    
    auto startTime = std::chrono::high_resolution_clock::now();
    
#ifdef _OPENMP
    #pragma omp parallel for
    for (int i = 0; i < N; ++i) {
        // Simple computation
        results[i] = data[i] * data[i] + 2.0 * data[i] + 1.0;
    }
#else
    for (int i = 0; i < N; ++i) {
        results[i] = data[i] * data[i] + 2.0 * data[i] + 1.0;
    }
#endif
    
    auto endTime = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = endTime - startTime;
    
    std::cout << "Computed " << N << " values\n";
    std::cout << "Time: " << (elapsed.count() * 1000.0) << " ms\n";
    std::cout << "Sample result [500]: " << results[500] << "\n";
    std::cout << "PASS\n\n";
}



void testParallelEfficiency() {
    std::cout << "========================================\n";
    std::cout << "TEST: Parallel Efficiency Estimation\n";
    std::cout << "========================================\n";
    
    double eff1 = estimateParallelEfficiency(1000, 10.0, 4);
    double eff2 = estimateParallelEfficiency(100, 10.0, 4);
    double eff3 = estimateParallelEfficiency(2, 10.0, 4);
    
    std::cout << std::fixed << std::setprecision(3);
    std::cout << "1000 tasks, 4 threads: efficiency = " << eff1 << "\n";
    std::cout << "100 tasks, 4 threads: efficiency = " << eff2 << "\n";
    std::cout << "2 tasks, 4 threads: efficiency = " << eff3 << "\n";
    
    bool pass = eff1 > eff2 && eff2 > eff3 && eff1 > 0.5 && eff3 < 0.6;
    std::cout << (pass ? "PASS" : "FAIL") << " (efficiency decreases with fewer tasks)\n\n";
}

int main() {
    std::cout << "\n";
    std::cout << "================================================\n";
    std::cout << "PARALLEL ALGORITHMS FRAMEWORK TEST SUITE\n";
    std::cout << "================================================\n\n";
    
    try {
        testParallelConfig();
        testThreadDetection();
        testBasicVectorOperations();
        testParallelEfficiency();
        
        std::cout << "================================================\n";
        std::cout << "ALL FRAMEWORK TESTS COMPLETED\n";
        std::cout << "================================================\n\n";
        
    } catch (const std::exception& e) {
        std::cerr << "\n*** TEST FAILED WITH EXCEPTION ***\n";
        std::cerr << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
