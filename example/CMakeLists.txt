# Example executables
add_executable(simple_read simple_read.f90)
target_link_libraries(simple_read PRIVATE hsd)
target_include_directories(simple_read PRIVATE ${CMAKE_BINARY_DIR}/modules)
