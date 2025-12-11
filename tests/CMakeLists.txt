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

# Test integrazione ITALOccultLibrary/AstDyn
add_executable(test_astdyn_integration test_astdyn_integration.cpp)
target_link_libraries(test_astdyn_integration ioccultcalc)

add_test(NAME AstDynIntegrationTest COMMAND test_astdyn_integration)

# Test OccultationSearch
add_executable(test_occultation_search test_occultation_search.cpp)
target_link_libraries(test_occultation_search ioccultcalc)

add_test(NAME OccultationSearchTest COMMAND test_occultation_search)

# Test OccultationSearch per asteroide 17030
add_executable(test_occultation_search_17030 test_occultation_search_17030.cpp)
target_link_libraries(test_occultation_search_17030 ioccultcalc)

add_test(NAME OccultationSearch17030Test COMMAND test_occultation_search_17030)

# Test debug propagazione
add_executable(test_propagation_debug test_propagation_debug.cpp)
target_link_libraries(test_propagation_debug ioccultcalc)

# Test propagazione Phase1
add_executable(test_phase1_propagation test_phase1_propagation.cpp)
target_link_libraries(test_phase1_propagation ioccultcalc)

# Test confronto Horizons
add_executable(test_horizons_comparison test_horizons_comparison.cpp)
target_link_libraries(test_horizons_comparison ioccultcalc)

# Test verifica epoca
add_executable(test_epoch_verification test_epoch_verification.cpp)
target_link_libraries(test_epoch_verification ioccultcalc)

# Test verifica elementi
add_executable(test_elements_verification test_elements_verification.cpp)
target_link_libraries(test_elements_verification ioccultcalc)

# Test confronto diretto Horizons
add_executable(test_horizons_direct test_horizons_direct.cpp)
target_link_libraries(test_horizons_direct ioccultcalc)

# Test sistema riferimento AstDyS
add_executable(test_astdys_reference_system test_astdys_reference_system.cpp)
target_link_libraries(test_astdys_reference_system ioccultcalc)

# Test verifica frame ECLM vs ICRF
add_executable(test_frame_verification test_frame_verification.cpp)
target_link_libraries(test_frame_verification ioccultcalc)

# Test confronto conversione equinoctiali
add_executable(test_equinoctial_conversion test_equinoctial_conversion.cpp)
target_link_libraries(test_equinoctial_conversion ioccultcalc)

# Test confronto OrbFit
add_executable(test_orbfit_comparison test_orbfit_comparison.cpp)
target_link_libraries(test_orbfit_comparison ioccultcalc)

# Test metodo OrbFit
add_executable(test_orbfit_method test_orbfit_method.cpp)
target_link_libraries(test_orbfit_method ioccultcalc)

# Test conversione geocentrica
add_executable(test_geocentric_conversion test_geocentric_conversion.cpp)
target_link_libraries(test_geocentric_conversion ioccultcalc)

# Test riepilogo OccultationSearch
add_executable(test_occultation_search_summary test_occultation_search_summary.cpp)
target_link_libraries(test_occultation_search_summary ioccultcalc)

# Test occultazione 17030 - 28 novembre
add_executable(test_occultation_17030_28nov test_occultation_17030_28nov.cpp)
target_link_libraries(test_occultation_17030_28nov ioccultcalc)

# Test calcolo distanza
add_executable(test_distance_calculation test_distance_calculation.cpp)
target_link_libraries(test_distance_calculation ioccultcalc)

# Test verifica posizione asteroide 17030
add_executable(test_17030_position_verification test_17030_position_verification.cpp)
target_link_libraries(test_17030_position_verification ioccultcalc)

# Test parsing allnum.cat e conversioni gradi/radianti
add_executable(test_allnum_parsing test_allnum_parsing.cpp)
target_link_libraries(test_allnum_parsing ioccultcalc)

# Test confronto effemeridi
add_executable(test_ephemeris_comparison test_ephemeris_comparison.cpp)
target_link_libraries(test_ephemeris_comparison ioccultcalc)

# Test confronto JPL vs VSOP
add_executable(test_jpl_vsop_comparison test_jpl_vsop_comparison.cpp)
target_link_libraries(test_jpl_vsop_comparison ioccultcalc)

# Test Phase1 vs Horizons
add_executable(test_phase1_vs_horizons test_phase1_vs_horizons.cpp)
target_link_libraries(test_phase1_vs_horizons ioccultcalc)