# Example executables
add_executable(simple_read simple_read.f90)
target_link_libraries(simple_read PRIVATE hsd)
target_include_directories(simple_read PRIVATE ${CMAKE_BINARY_DIR}/modules)

add_executable(config_demo config_demo.f90)
target_link_libraries(config_demo PRIVATE hsd)
target_include_directories(config_demo PRIVATE ${CMAKE_BINARY_DIR}/modules)

add_executable(matrix_demo matrix_demo.f90)
target_link_libraries(matrix_demo PRIVATE hsd)
target_include_directories(matrix_demo PRIVATE ${CMAKE_BINARY_DIR}/modules)

add_executable(visitor_demo visitor_types.f90 visitor_demo.f90)
target_link_libraries(visitor_demo PRIVATE hsd)
target_include_directories(visitor_demo PRIVATE ${CMAKE_BINARY_DIR}/modules)
