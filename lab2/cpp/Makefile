# Structure
INCLUDE_DIR := include
TEST_DIR	:= test
EXEC 		:= test_galois_field
DOCTEST_DIR := /mnt/c/coding/libraries/cpp/Doctest

SOURCES := $(TEST_DIR)/test_galois_field.cpp

# Compiler
CXX := g++
CXX_STD := c++20
CXX_FLAGS += -std=$(CXX_STD) \
	-Wall -Wextra -Wcast-align -Wconversion -Wunreachable-code -Wuninitialized -pedantic -g -O3

# CMD
RM = rm -f

# Rules
.PHONY: all test clean

all: clean test

test:
	@echo Building: test_galois_field
	@$(CXX) -o $(EXEC) $(SOURCES) -I $(INCLUDE_DIR) -I $(DOCTEST_DIR) $(CXX_FLAGS)
	@echo Build successful!

clean:
	@echo Removing generated files
	@$(RM) $(EXEC)
	@echo Done!
