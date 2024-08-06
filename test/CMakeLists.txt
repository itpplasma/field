cmake_minimum_required(VERSION 3.10)

add_executable(test_get_field_type.x test_get_field_type.f90)
add_executable(test_example_field.x test_example_field.f90)
add_library(test_util SHARED test_util.f90)
target_link_libraries(test_get_field_type.x PRIVATE get_field_type field test_util)
target_link_libraries(test_example_field.x PRIVATE example_field test_util)
