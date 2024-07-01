# Structure
INC_DIR   := include
SRC_DIR   := source
BUILD_DIR := .

# External libraries
CPP_AP_DIR := /mnt/c/coding/projects/cpp/cpp-ap

TARGET := philosophers

# Compiler
CXX 	  		 := g++
CXX_STD   		 := c++20
CXX_FLAGS 		 := -std=$(CXX_STD) \
	-Wall -Wextra -Wcast-align -Wunreachable-code -Wuninitialized -pedantic -g -O3

# CMD
RM = rm -f

# Rules
.PHONY: all build clean

all: clean build

build:
	$(CXX) $(CXX_FLAGS) -o $(TARGET) $(SRC_DIR)/*.cpp -I $(INC_DIR) -I $(CPP_AP_DIR)/include

clean:
	@echo Removing generated files
	@$(RM) $(TARGET)
	@echo Done!
