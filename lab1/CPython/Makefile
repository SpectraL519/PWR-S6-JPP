# Structure
ROOT_DIR   := ..
C_DIR 	   := $(ROOT_DIR)/C
C_INC_DIR  := $(C_DIR)/include
C_SRC_DIR  := $(C_DIR)/source
PY_PKG_DIR := ./pygebra

# Toolchains
CC 	     := gcc
CC_STD   := c17
CC_FLAGS := -std=$(CC_STD) -Wall -Wextra -Werror

PY := /usr/bin/python3

# Source files
C_LIB_SRC   := $(C_SRC_DIR)/algebra.c
C_LIB_R_SRC := $(C_SRC_DIR)/algebra_recursive.c

# Object files
C_LIB_OBJ   := $(PY_PKG_DIR)/algebra.o
C_LIB_R_OBJ := $(PY_PKG_DIR)/algebra_recursive.o

# Shared libraries
C_LIB   := $(PY_PKG_DIR)/algebra.so
C_LIB_R := $(PY_PKG_DIR)/algebra_recursive.so

# Targets
.PHONY: all clibs test clean

all: clean clibs test

clibs: $(C_LIB) $(C_LIB_R)

$(C_LIB):
	@echo Building: $(C_LIB)
	@$(CC) -o $(C_LIB_OBJ) $(C_LIB_SRC) -I $(C_INC_DIR) -c -fpic $(CC_FLAGS)
	@$(CC) -o $(C_LIB) $(C_LIB_OBJ) -shared
	@echo Done

$(C_LIB_R):
	@echo Building: $(C_LIB_R)
	@$(CC) -o $(C_LIB_R_OBJ) $(C_LIB_R_SRC) -I $(C_INC_DIR) -c -fpic $(CC_FLAGS)
	@$(CC) -o $(C_LIB_R) $(C_LIB_R_OBJ) -shared
	@echo Done

test: clibs
	$(PY) -m pytest -v

clean:
	@echo Removing generated files
	@rm -rf $(C_LIB) $(C_LIB_OBJ) $(C_LIB_R) $(C_LIB_R_OBJ) .pytest_cache **/__pycache__
