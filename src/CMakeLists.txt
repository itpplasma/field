cmake_minimum_required(VERSION 3.10)

add_library(create_field SHARED create_field.f90)
add_library(example_field SHARED example_field.f90)
add_library(field SHARED field.f90)
target_link_libraries(example_field PRIVATE field)
target_link_libraries(create_field PRIVATE field example_field)

install(TARGETS field DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

install(FILES ${CMAKE_BINARY_DIR}/include/field.mod 
    DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
