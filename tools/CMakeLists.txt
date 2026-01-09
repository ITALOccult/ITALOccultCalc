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

# Gaia cache downloader - Download locale catalogo Gaia
add_executable(gaia_cache_downloader gaia_cache_downloader.cpp)
target_link_libraries(gaia_cache_downloader PRIVATE ioccultcalc)

# Inspect star utility
add_executable(inspect_star inspect_star.cpp)
target_link_libraries(inspect_star PRIVATE ioccultcalc)

# Allnum database builder - Crea/aggiorna database SQLite da allnum.cat
add_executable(build_allnum_database build_allnum_database.cpp)
target_link_libraries(build_allnum_database PRIVATE ioccultcalc)

# Map generator utility
add_executable(map_generator map_generator.cpp)
target_link_libraries(map_generator PRIVATE ioccultcalc)

# Installa le utilities
install(TARGETS occult_calc gaia_cache_downloader build_allnum_database map_generator
    RUNTIME DESTINATION bin
)
