cmake_minimum_required(VERSION 3.15)
project(IOccultCalc VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Options
option(BUILD_TESTS "Build tests" ON)
option(BUILD_EXAMPLES "Build examples" ON)

# Find required packages
find_package(CURL REQUIRED)
find_package(LibXml2 REQUIRED)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CURL_INCLUDE_DIRS}
    ${LIBXML2_INCLUDE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/external/cspice/include
)

# Source files
set(SOURCES
    src/astdys_client.cpp
    src/gaia_client.cpp
    src/orbital_elements.cpp
    src/ephemeris.cpp
    src/occultation_predictor.cpp
    src/kml_exporter.cpp
    src/occult4_xml.cpp
    src/prediction_report.cpp
    src/preston_parser.cpp
    src/coordinates.cpp
    src/time_utils.cpp
    src/observation.cpp
    src/mpc_client.cpp
    src/orbit_determination.cpp
    src/orbit_fitter.cpp
    src/orbit_determination_gauss.cpp
    src/force_model.cpp
    src/jpl_ephemeris.cpp
    src/orbit_propagator.cpp
    src/jpl_horizons_client.cpp
    src/jpl_de_downloader.cpp
    src/spk_reader.cpp
    src/spice_spk_reader.cpp
    src/jpleph.cpp
    src/ra15_integrator.cpp
)

# Header files
set(HEADERS
    include/ioccultcalc/astdys_client.h
    include/ioccultcalc/gaia_client.h
    include/ioccultcalc/orbital_elements.h
    include/ioccultcalc/ephemeris.h
    include/ioccultcalc/occultation_predictor.h
    include/ioccultcalc/kml_exporter.h
    include/ioccultcalc/occult4_xml.h
    include/ioccultcalc/prediction_report.h
    include/ioccultcalc/preston_parser.h
    include/ioccultcalc/coordinates.h
    include/ioccultcalc/time_utils.h
    include/ioccultcalc/types.h
    include/ioccultcalc/observation.h
    include/ioccultcalc/mpc_client.h
    include/ioccultcalc/orbit_determination.h
    include/ioccultcalc/force_model.h
    include/ioccultcalc/jpl_ephemeris.h
    include/ioccultcalc/orbit_propagator.h
    include/ioccultcalc/jpl_horizons_client.h
    include/ioccultcalc/jpl_de_downloader.h
    include/ioccultcalc/spk_reader.h
    include/ioccultcalc/spice_spk_reader.h
    include/ioccultcalc/jpleph.h
    include/ioccultcalc/jpl_int.h
    include/ioccultcalc/get_bin.h
    include/ioccultcalc/numerical_integrator.h
    include/ioccultcalc/relativistic_corrections.h
    include/ioccultcalc/star_catalog.h
    include/ioccultcalc/asteroid_shape.h
    include/ioccultcalc/uncertainty_propagation.h
    include/ioccultcalc/ra15_integrator.hpp
)

# Create library
add_library(ioccultcalc STATIC ${SOURCES} ${HEADERS})

target_link_libraries(ioccultcalc
    CURL::libcurl
    ${LIBXML2_LIBRARIES}
    ${CMAKE_CURRENT_SOURCE_DIR}/external/cspice/lib/cspice.a
)

target_include_directories(ioccultcalc PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Examples
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Tools
add_subdirectory(tools)

# Tests
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# Installation
install(TARGETS ioccultcalc
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin
)

install(DIRECTORY include/ioccultcalc
    DESTINATION include
)
