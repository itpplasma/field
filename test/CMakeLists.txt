cmake_minimum_required(VERSION 3.10)

add_executable(test_create_field.x test_create_field.f90)
add_executable(test_example_field.x test_example_field.f90)
add_library(test_util SHARED test_util.f90)
target_link_libraries(test_create_field.x PRIVATE create_field field test_util)
target_link_libraries(test_example_field.x PRIVATE example_field test_util)
