# Tools directory - Utilities command-line

add_executable(occult_calc occult_calc.cpp)
target_link_libraries(occult_calc PRIVATE ioccultcalc)

# Test utility per AstDyS
add_executable(test_astdys test_astdys.cpp)
target_link_libraries(test_astdys PRIVATE ioccultcalc)

# Test utility per osservazioni .rwo
add_executable(test_rwo test_rwo.cpp)
target_link_libraries(test_rwo PRIVATE ioccultcalc)

# Test utility per Gaia DR3
add_executable(test_gaia test_gaia.cpp)
target_link_libraries(test_gaia PRIVATE ioccultcalc)

# Installa la utility
install(TARGETS occult_calc
    RUNTIME DESTINATION bin
)
