# Placeholder for tests
# To be implemented with a testing framework like Google Test

add_executable(test_time_utils test_time_utils.cpp)
target_link_libraries(test_time_utils ioccultcalc)

add_test(NAME TimeUtilsTest COMMAND test_time_utils)

# Test perturbazioni Sierks (file mancante)
# add_executable(test_perturbations_sierks test_perturbations_sierks.cpp)
# target_link_libraries(test_perturbations_sierks ioccultcalc)

# Test veloce Sierks (file mancante)
# add_executable(test_sierks_quick test_sierks_quick.cpp)
# target_link_libraries(test_sierks_quick ioccultcalc)

# Debug Keplero (file mancante)
# add_executable(test_keplero_debug test_keplero_debug.cpp)
# target_link_libraries(test_keplero_debug ioccultcalc)

# Test propagator backward (file mancante)
# add_executable(test_propagator_backward test_propagator_backward.cpp)
# target_link_libraries(test_propagator_backward ioccultcalc)

# Debug conversione (file mancante)
# add_executable(test_conversion_debug test_conversion_debug.cpp)
# target_link_libraries(test_conversion_debug ioccultcalc)

# Test forward/backward (file mancante)
# add_executable(test_forward test_forward.cpp)
# target_link_libraries(test_forward ioccultcalc)

# Test SPK Ceres (file mancante)
# add_executable(test_spk_ceres test_spk_ceres.cpp)
# target_link_libraries(test_spk_ceres ioccultcalc)

# Test propagazione avanti (file mancante)
# add_executable(test_propagation_forward test_propagation_forward.cpp)
# target_link_libraries(test_propagation_forward ioccultcalc)

# Test modello forze OrbFit
# add_executable(test_orbfit_force_model test_orbfit_force_model.cpp)
# target_link_libraries(test_orbfit_force_model ioccultcalc)
# 
# # Test propagazione diretta OrbFit
# add_executable(test_orbfit_propagation test_orbfit_propagation.cpp)
# target_link_libraries(test_orbfit_propagation ioccultcalc)

# Test confronto RKF78 vs RK4 (richiede orbfit_force_model.h)
# add_executable(test_rkf78_comparison test_rkf78_comparison.cpp)
# target_link_libraries(test_rkf78_comparison ioccultcalc)

# Test OrbitPropagator con RKF78 integrato
add_executable(test_orbit_propagator_rkf78 test_orbit_propagator_rkf78.cpp)
target_link_libraries(test_orbit_propagator_rkf78 ioccultcalc)

# Test propagazione vs JPL Horizons (-60 a +60 giorni)
add_executable(test_propagation_vs_horizons test_propagation_vs_horizons.cpp)
target_link_libraries(test_propagation_vs_horizons ioccultcalc)

# Test propagazione (4) Vesta con dati JPL attuali
add_executable(test_vesta_propagation test_vesta_propagation.cpp)
target_link_libraries(test_vesta_propagation ioccultcalc)

# Test SPK frames
# add_executable(test_spk_frames test_spk_frames.cpp)
# target_link_libraries(test_spk_frames ioccultcalc ${CSPICE_LIB})

# Test ASTNUM output format
add_executable(test_astnum_output test_astnum_output.cpp)
target_link_libraries(test_astnum_output PRIVATE ioccultcalc)

# Test Asteroid List Reader (input ASTNUM_LIST)
add_executable(test_asteroid_list_reader test_asteroid_list_reader.cpp)
target_link_libraries(test_asteroid_list_reader PRIVATE ioccultcalc)

# Test Combined Lists (range + filtri + file multipli)
add_executable(test_combined_lists test_combined_lists.cpp)
target_link_libraries(test_combined_lists PRIVATE ioccultcalc)

# Test con file localasteroid reale
add_executable(test_localasteroid_real test_localasteroid_real.cpp)
target_link_libraries(test_localasteroid_real PRIVATE ioccultcalc)

# Test validazione file .oop preset
add_executable(test_oop_presets test_oop_presets.cpp)
# No dependencies - standalone parser

# Test integrazione .oop con AsteroidListReader
add_executable(test_oop_with_asteroid_lists test_oop_with_asteroid_lists.cpp)
target_link_libraries(test_oop_with_asteroid_lists PRIVATE ioccultcalc)
