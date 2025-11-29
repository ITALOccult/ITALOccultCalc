/**
 * @file test_asteroid_filter.cpp
 * @brief Test suite for asteroid filtering system
 */

#include <iostream>
#include <iomanip>
#include <cassert>
#include "ioccultcalc/asteroid_filter.h"

using namespace ioccultcalc;

// Test helper
void printTestResult(const std::string& testName, bool passed) {
    std::cout << "[" << (passed ? "✓" : "✗") << "] " << testName << std::endl;
}

// Test 1: Basic numeric comparisons
void testNumericComparisons() {
    std::cout << "\n=== Test 1: Numeric Comparisons ===" << std::endl;
    
    AsteroidProperties props;
    props.number = 433;
    props.name = "Eros";
    props.diameter = 16.8;
    props.H = 10.4;
    props.a = 1.458;
    props.e = 0.223;
    props.i = 10.8;
    
    // Test greater than
    FilterCondition cond1 = FilterCondition::parse("diameter > 15");
    bool result1 = cond1.evaluate(props);
    printTestResult("diameter > 15 (should be true)", result1);
    assert(result1);
    
    // Test less than
    FilterCondition cond2 = FilterCondition::parse("H < 11");
    bool result2 = cond2.evaluate(props);
    printTestResult("H < 11 (should be true)", result2);
    assert(result2);
    
    // Test equal
    FilterCondition cond3 = FilterCondition::parse("i == 10.8");
    bool result3 = cond3.evaluate(props);
    printTestResult("i == 10.8 (should be true)", result3);
    assert(result3);
    
    // Test between
    FilterCondition cond4 = FilterCondition::parse("a between 1.4 and 1.5");
    bool result4 = cond4.evaluate(props);
    printTestResult("a between 1.4 and 1.5 (should be true)", result4);
    assert(result4);
}

// Test 2: String comparisons
void testStringComparisons() {
    std::cout << "\n=== Test 2: String Comparisons ===" << std::endl;
    
    AsteroidProperties props;
    props.number = 1;
    props.name = "Ceres";
    props.orbit_class = "MBA";
    props.spectral_type = "C";
    props.diameter = 939.0;
    
    // Test IN operator
    FilterCondition cond1 = FilterCondition::parse("orbit_class in [MBA, IMB, OMB]");
    bool result1 = cond1.evaluate(props);
    printTestResult("orbit_class in [MBA, IMB, OMB] (should be true)", result1);
    assert(result1);
    
    // Test NOT IN operator
    FilterCondition cond2 = FilterCondition::parse("spectral_type not in [S, M, E]");
    bool result2 = cond2.evaluate(props);
    printTestResult("spectral_type not in [S, M, E] (should be true)", result2);
    assert(result2);
    
    // Test EQUAL
    FilterCondition cond3 = FilterCondition::parse("orbit_class = MBA");
    bool result3 = cond3.evaluate(props);
    printTestResult("orbit_class = MBA (should be true)", result3);
    assert(result3);
}

// Test 3: AsteroidRange with WHERE conditions
void testWhereConditions() {
    std::cout << "\n=== Test 3: WHERE Conditions (AND logic) ===" << std::endl;
    
    AsteroidRange range = AsteroidRangeBuilder()
        .from(1).to(1000)
        .where("diameter > 50")
        .where("H < 10")
        .where("orbit_class in [MBA]")
        .build();
    
    // Test asteroid that matches all conditions
    AsteroidProperties ceres;
    ceres.number = 1;
    ceres.diameter = 939.0;
    ceres.H = 3.3;
    ceres.orbit_class = "MBA";
    bool result1 = range.matches(ceres);
    printTestResult("Ceres matches (large MBA, H<10)", result1);
    assert(result1);
    
    // Test asteroid that fails one condition
    AsteroidProperties eros;
    eros.number = 433;
    eros.diameter = 16.8;
    eros.H = 10.4;
    eros.orbit_class = "NEA";
    bool result2 = range.matches(eros);
    printTestResult("Eros doesn't match (small, H>10, NEA)", !result2);
    assert(!result2);
}

// Test 4: AsteroidRange with WHERENOT conditions
void testWhereNotConditions() {
    std::cout << "\n=== Test 4: WHERENOT Conditions (exclusion) ===" << std::endl;
    
    AsteroidRange range = AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("diameter > 10")
        .whereNot("i > 30")
        .whereNot("e > 0.5")
        .build();
    
    // Test asteroid that passes (low i, low e)
    AsteroidProperties vesta;
    vesta.number = 4;
    vesta.diameter = 525.0;
    vesta.i = 7.1;
    vesta.e = 0.09;
    bool result1 = range.matches(vesta);
    printTestResult("Vesta passes (low i, low e)", result1);
    assert(result1);
    
    // Test asteroid excluded by high inclination
    AsteroidProperties pallas;
    pallas.number = 2;
    pallas.diameter = 512.0;
    pallas.i = 34.8;
    pallas.e = 0.23;
    bool result2 = range.matches(pallas);
    printTestResult("Pallas excluded (high i)", !result2);
    assert(!result2);
}

// Test 5: Explicit list
void testExplicitList() {
    std::cout << "\n=== Test 5: Explicit List ===" << std::endl;
    
    std::vector<int> list = {1, 4, 10, 433, 951};
    AsteroidRange range = AsteroidRangeBuilder()
        .explicitList(list)
        .where("diameter > 5")
        .build();
    
    AsteroidProperties eros;
    eros.number = 433;
    eros.diameter = 16.8;
    bool result1 = range.matches(eros);
    printTestResult("Eros in list and passes filter", result1);
    assert(result1);
    
    AsteroidProperties ida;
    ida.number = 243;
    ida.diameter = 31.4;
    bool result2 = range.matches(ida);
    printTestResult("Ida not in list (excluded)", !result2);
    assert(!result2);
}

// Test 6: Preset filters
void testPresets() {
    std::cout << "\n=== Test 6: Preset Filters ===" << std::endl;
    
    // Test large MBA preset
    AsteroidRange largeMBA = AsteroidFilterPresets::largeMBA();
    
    AsteroidProperties hygiea;
    hygiea.number = 10;
    hygiea.diameter = 407.0;
    hygiea.H = 5.4;
    hygiea.orbit_class = "MBA";
    hygiea.a = 3.14;
    bool result1 = largeMBA.matches(hygiea);
    printTestResult("Hygiea matches largeMBA preset", result1);
    assert(result1);
    
    // Test NEA preset
    AsteroidRange nea = AsteroidFilterPresets::nearEarth();
    
    AsteroidProperties apophis;
    apophis.number = 99942;
    apophis.diameter = 0.37;
    apophis.orbit_class = "Apollo";
    apophis.a = 0.92;
    bool result2 = nea.matches(apophis);
    printTestResult("Apophis matches nearEarth preset", result2);
    assert(result2);
}

// Test 7: Complex filter combination
void testComplexFilter() {
    std::cout << "\n=== Test 7: Complex Filter Combination ===" << std::endl;
    
    AsteroidRange range = AsteroidRangeBuilder()
        .from(1).to(100000)
        .where("diameter > 50")
        .where("H < 10")
        .where("orbit_class in [MBA, IMB, OMB]")
        .where("a > 2.2")
        .where("a < 3.2")
        .whereNot("i > 30")
        .whereNot("e > 0.3")
        .build();
    
    std::cout << "\nFilter description:\n" << range.toString() << std::endl;
    
    // Test various asteroids
    AsteroidProperties ceres;
    ceres.number = 1;
    ceres.diameter = 939.0;
    ceres.H = 3.3;
    ceres.orbit_class = "MBA";
    ceres.a = 2.77;
    ceres.e = 0.08;
    ceres.i = 10.6;
    bool result1 = range.matches(ceres);
    printTestResult("Ceres matches complex filter", result1);
    assert(result1);
}

// Test 8: Range generation
void testRangeGeneration() {
    std::cout << "\n=== Test 8: Range Generation ===" << std::endl;
    
    AsteroidRange range = AsteroidRangeBuilder()
        .from(1).to(10)
        .build();
    
    auto list = range.getAsteroidList();
    bool result = (list.size() == 10 && list[0] == 1 && list[9] == 10);
    printTestResult("Range 1-10 generates 10 elements", result);
    assert(result);
    
    std::cout << "Generated list: ";
    for (int num : list) {
        std::cout << num << " ";
    }
    std::cout << std::endl;
}

// Test 9: Edge cases
void testEdgeCases() {
    std::cout << "\n=== Test 9: Edge Cases ===" << std::endl;
    
    AsteroidProperties props;
    props.number = 1;
    props.diameter = 0.0;
    props.H = 99.9;
    props.albedo = 0.0;
    
    // Test with zero values
    FilterCondition cond1 = FilterCondition::parse("diameter > 0");
    bool result1 = !cond1.evaluate(props);
    printTestResult("diameter = 0 fails > 0 test", result1);
    assert(result1);
    
    // Test with missing data (empty string)
    props.orbit_class = "";
    FilterCondition cond2 = FilterCondition::parse("orbit_class in [MBA, NEA]");
    bool result2 = !cond2.evaluate(props);
    printTestResult("Empty orbit_class fails IN test", result2);
    assert(result2);
}

int main() {
    std::cout << "╔═══════════════════════════════════════════╗" << std::endl;
    std::cout << "║  IOccultCalc Asteroid Filter Test Suite  ║" << std::endl;
    std::cout << "╚═══════════════════════════════════════════╝" << std::endl;
    
    try {
        testNumericComparisons();
        testStringComparisons();
        testWhereConditions();
        testWhereNotConditions();
        testExplicitList();
        testPresets();
        testComplexFilter();
        testRangeGeneration();
        testEdgeCases();
        
        std::cout << "\n╔═══════════════════════════════════════════╗" << std::endl;
        std::cout << "║       ALL TESTS PASSED SUCCESSFULLY!      ║" << std::endl;
        std::cout << "╚═══════════════════════════════════════════╝" << std::endl;
        
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "\n[ERROR] Test failed with exception: " << e.what() << std::endl;
        return 1;
    }
}
