# This Makefile is really bad - didn't have the will power to make it better
ROOT_DIR   := /mnt/c/coding/pwr_private/S6_JPP/lab1
C_DIR 	   := $(ROOT_DIR)/C
INC_DIR    := $(C_DIR)/include
SRC_DIR    := $(C_DIR)/source
TEST_DIR   := $(C_DIR)/test
BUILD_DIR  := $(C_DIR)/build_ada
ADA_DIR	   := $(ROOT_DIR)/Ada

# Source files
C_SRC 	  := $(SRC_DIR)/ada_algebra.c
TEST_SRC  := $(TEST_DIR)/test_algebra.c
UNITY_SRC := $(cunity_path)/unity.c
ADA_SRC   := $(ADA_DIR)/algebra.adb


# Targets
.PHONY: all ada_test clean_ada

all: clean_ada ada_test

ada_test: clean_ada
	@echo Setup
	@mkdir $(BUILD_DIR)
	@echo Done
	@echo Building objects
	@cd $(BUILD_DIR) && \
		gcc -c $(C_SRC) -I $(INC_DIR) -DADA_RECURSIVE=0 -o ada_algebra.o -Wno-implicit-function-declaration && \
		gcc -c $(C_SRC) -I $(INC_DIR) -DADA_RECURSIVE=1 -o ada_algebra_recursive.o -Wno-implicit-function-declaration && \
		gcc -c $(TEST_SRC) -I $(INC_DIR) -I $(cunity_path) && \
		gcc -c $(UNITY_SRC) && \
		echo Done && \
		echo Building test executables && \
		gnatmake -c $(ADA_SRC) && \
		gnatbind -n algebra.ali && \
		gnatlink unity.o test_algebra.o ada_algebra.o algebra.ali -o test_ada_algebra && \
		gnatbind -n algebra.ali && \
		gnatlink unity.o test_algebra.o ada_algebra_recursive.o algebra.ali -o test_ada_algebra_recursive && \
		echo Done && \
		echo Cleaning && \
		rm *.o *.ali
	@echo Done

clean_ada:
	@echo Removing generated files
	@rm -rf $(BUILD_DIR)
	@echo Done
