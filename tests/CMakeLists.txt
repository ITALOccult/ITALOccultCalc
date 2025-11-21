# Placeholder for tests
# To be implemented with a testing framework like Google Test

add_executable(test_time_utils test_time_utils.cpp)
target_link_libraries(test_time_utils ioccultcalc)

add_test(NAME TimeUtilsTest COMMAND test_time_utils)
