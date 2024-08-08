cmake_minimum_required(VERSION 3.10)

add_executable(test_libneo_field.x test_libneo_field.f90)
add_executable(test_libneo_example_field.x test_libneo_example_field.f90)
add_executable(test_libneo_biotsavart_field.x test_libneo_biotsavart_field.f90)
add_library(test_util SHARED test_util.f90)
target_link_libraries(test_libneo_field.x PRIVATE libneo_field
                                                  libneo_field_base
                                                  test_util)
target_link_libraries(test_libneo_example_field.x PRIVATE libneo_example_field 
                                                          test_util)
target_link_libraries(test_libneo_biotsavart_field.x PRIVATE libneo_biotsavart_field 
                                                             test_util)
