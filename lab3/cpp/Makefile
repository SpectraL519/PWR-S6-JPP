# Structure
INCLUDE_DIR := include
TEST_DIR    := test
DOCTEST_DIR := /mnt/c/coding/libraries/cpp/Doctest

# Get all test source files
TEST_SOURCES := $(wildcard $(TEST_DIR)/*.cpp)

# Extract test names
TEST_NAMES := $(TEST_SOURCES:$(TEST_DIR)/%.cpp=%)

# Compiler
CXX 	  		 := g++
CXX_STD   		 := c++20
CXX_FLAGS 		 := -std=$(CXX_STD) \
	-Wall -Wextra -Wcast-align -Wconversion -Wunreachable-code -Wuninitialized -pedantic -g -O3
CXX_COMPILE_ARGS := -I $(INCLUDE_DIR) -I $(DOCTEST_DIR) $(CXX_FLAGS)

# CMD
RM = rm -f

# Rules
.PHONY: all build_tests run_tests build_and_run_tests clean

all: clean build_tests

build_tests: clean
	@for test_source in $(TEST_SOURCES); do \
		test_name=$$(basename $$test_source .cpp); \
		echo Building: $$test_name; \
		$(CXX) -o $$test_name $$test_source $(CXX_COMPILE_ARGS); \
		done
	@echo Build successful!

run_tests:
	@for test_name in $(TEST_NAMES); do \
		echo ---------------------;\
		echo Running: $$test_name; \
		./$$test_name; \
		done
	@echo ---------------------
	@echo Done!

build_and_run_tests: build_tests run_tests

clean:
	@echo Removing generated files
	@$(RM) $(TEST_NAMES)
	@echo Done!
