# Placeholder for tests
# To be implemented with a testing framework like Google Test

add_executable(test_time_utils test_time_utils.cpp)
target_link_libraries(test_time_utils ioccultcalc)

add_test(NAME TimeUtilsTest COMMAND test_time_utils)

# Test OrbitPropagator con RKF78 integrato
add_executable(test_orbit_propagator_rkf78 test_orbit_propagator_rkf78.cpp)
target_link_libraries(test_orbit_propagator_rkf78 ioccultcalc)

# Test propagazione vs JPL Horizons (-60 a +60 giorni)
add_executable(test_propagation_vs_horizons test_propagation_vs_horizons.cpp)
target_link_libraries(test_propagation_vs_horizons ioccultcalc)

# Test propagazione (4) Vesta con dati JPL attuali
add_executable(test_vesta_propagation test_vesta_propagation.cpp)
target_link_libraries(test_vesta_propagation ioccultcalc)
