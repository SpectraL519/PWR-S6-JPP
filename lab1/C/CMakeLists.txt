cmake_minimum_required(VERSION 3.12)
project(algebra-tests)

# Structure
set(TEST_DIR "test")
set(SOURCE_DIR "source")
set(INCLUDE_DIR "include")
set(BINARY_DIR ".")
set(EXECUTABLE_DIR ".")

# Source files
set(ALGEBRA_SOURCE ${SOURCE_DIR}/algebra.c)
set(ALGEBRA_RECURSIVE_SOURCE ${SOURCE_DIR}/algebra_recursive.c)
set(PY_ALGEBRA_SOURCE ${SOURCE_DIR}/py_algebra.c)
set(PY_PACKAGE_DIR "/mnt/c/coding/pwr_private/S6_JPP/lab1/Python")
set(TEST_SOURCES ${TEST_DIR}/test_algebra.c "$ENV{cunity_path}/unity.c")

# Default compile options
set(DEFAULT_C_FLAGS "-Wall -Wextra -Wcast-align -Wconversion -Wunreachable-code -Wuninitialized -pedantic -g -O3")

# Set compile options
if(NOT DEFINED CMAKE_C_FLAGS)
    set(CMAKE_C_FLAGS ${DEFAULT_C_FLAGS} CACHE STRING "Default C compile flags" FORCE)
endif()

# Include directories
include_directories(${INCLUDE_DIR} ${Python3_INCLUDE_DIRS} $ENV{cunity_path})


# Executable for algebra test
add_executable(test_algebra ${TEST_SOURCES} ${ALGEBRA_SOURCE})
set_target_properties(test_algebra PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${BINARY_DIR}"
    C_STANDARD 17
    C_STANDARD_REQUIRED YES
)
target_compile_options(test_algebra PRIVATE ${CMAKE_C_FLAGS})

# Executable for algebra_recursive test
add_executable(test_algebra_recursive ${TEST_SOURCES} ${ALGEBRA_RECURSIVE_SOURCE})
set_target_properties(test_algebra_recursive PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${BINARY_DIR}"
    C_STANDARD 17
    C_STANDARD_REQUIRED YES
)
target_compile_options(test_algebra_recursive PRIVATE ${CMAKE_C_FLAGS})


# Executable for Python algebra test
find_package(Python3 COMPONENTS Interpreter Development REQUIRED)
add_executable(test_py_algebra ${TEST_SOURCES} ${PY_ALGEBRA_SOURCE})
set_target_properties(test_py_algebra PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${BINARY_DIR}"
    C_STANDARD 17
    C_STANDARD_REQUIRED YES
)
target_compile_options(test_py_algebra PRIVATE ${CMAKE_C_FLAGS})
target_link_libraries(test_py_algebra PRIVATE Python3::Python)
target_compile_definitions(test_py_algebra
    PRIVATE PYENV PY_PKG_PATH="${PY_PACKAGE_DIR}" PY_MODULE="pygebra.algebra"
)

# Executable for Python algebra_recursive test
find_package(Python3 COMPONENTS Interpreter Development REQUIRED)
add_executable(test_py_algebra_recursive ${TEST_SOURCES} ${PY_ALGEBRA_SOURCE})
set_target_properties(test_py_algebra_recursive PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${BINARY_DIR}"
    C_STANDARD 17
    C_STANDARD_REQUIRED YES
)
target_compile_options(test_py_algebra_recursive PRIVATE ${CMAKE_C_FLAGS})
target_link_libraries(test_py_algebra_recursive PRIVATE Python3::Python)
target_compile_definitions(test_py_algebra_recursive
    PRIVATE PYENV PY_PKG_PATH="${PY_PACKAGE_DIR}" PY_MODULE="pygebra.algebra_recursive"
)
