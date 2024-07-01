# Structure
ROOT_DIR   := /mnt/c/coding/pwr_private/S6_JPP/lab1
C_DIR 	   := $(ROOT_DIR)/C
C_INC_DIR  := $(C_DIR)/include
C_SRC_DIR  := $(C_DIR)/source
ADA_DIR	   := $(ROOT_DIR)/Ada
ATEST_DIR  := $(ROOT_DIR)/AdaTest

# Toolchains
CC 	     := gcc
CC_STD   := c17
CC_FLAGS := -std=$(CC_STD) -Wall -Wextra -Werror

# Source files
C_LIB_SRC   := $(C_SRC_DIR)/algebra.c
C_LIB_R_SRC := $(C_SRC_DIR)/algebra_recursive.c

# Object files
C_LIB_OBJ   := ./algebra.o
C_LIB_R_OBJ := ./algebra_recursive.o

# Targets
.PHONY: all clibs test clean

all: clean clibs test

clibs: $(C_LIB_OBJ) $(C_LIB_R_OBJ)

$(C_LIB_OBJ):
	@echo Building: $(C_LIB_OBJ)
	@$(CC) -o $(C_LIB_OBJ) $(C_LIB_SRC) -I $(C_INC_DIR) -c $(CC_FLAGS)
	@echo Done

$(C_LIB_R_OBJ):
	@echo Building: $(C_LIB_R_OBJ)
	@$(CC) -o $(C_LIB_R_OBJ) $(C_LIB_R_SRC) -I $(C_INC_DIR) -c $(CC_FLAGS)
	@echo Done

test: clibs
	@echo Building: test_calgebra
	@gnatmake -c test_calgebra.adb -I$(ATEST_DIR)
	@gnatbind test_calgebra.ali
	@gnatlink test_calgebra.ali $(C_LIB_OBJ) -o test_calgebra
	@echo Building: test_calgebra_recursive
	@gnatmake -c test_calgebra.adb -I$(ATEST_DIR)
	@gnatbind test_calgebra.ali
	@gnatlink test_calgebra.ali $(C_LIB_R_OBJ) -o test_calgebra_recursive

clean:
	@echo Removing generated files
	@rm -rf $(C_LIB_OBJ) $(C_LIB_R_OBJ) test_calgebra test_calgebra_recursive
	@gnatclean test_calgebra
