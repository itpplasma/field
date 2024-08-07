cmake_minimum_required(VERSION 3.10)

add_library(libneo_field SHARED libneo_field.f90)
add_library(libneo_base_field SHARED libneo_base_field.f90)
add_library(libneo_example_field SHARED libneo_example_field.f90)
add_library(libneo_biotsavart_field SHARED libneo_biotsavart_field.f90)
target_link_libraries(libneo_example_field PRIVATE libneo_base_field)

include_directories(${CMAKE_INSTALL_PREFIX}/include)
set(LOCAL_LIB "${CMAKE_INSTALL_PREFIX}/lib")
find_library(LIB_BIOTSAVART biotsavart PATHS ${LOCAL_LIB})
target_link_libraries(libneo_biotsavart_field PRIVATE libneo_base_field 
                                                      ${LIB_BIOTSAVART})

target_link_libraries(libneo_field PRIVATE libneo_base_field 
                                           libneo_example_field 
                                           libneo_biotsavart_field)

install(TARGETS libneo_field DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

install(FILES ${CMAKE_BINARY_DIR}/include/libneo_field.mod 
    DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
