# Structure
ROOT_DIR   := /mnt/c/coding/pwr_private/S6_JPP/lab1
ADA_DIR	   := $(ROOT_DIR)/Ada
ATEST_DIR  := $(ROOT_DIR)/AdaTest

# Targets
.PHONY: all test clean

all: clean test

test:
	@echo Building: test_algebra
	@gnatmake test_algebra.adb -I$(ATEST_DIR)
	@echo Done

clean:
	@echo Removing generated files
	@gnatclean test_algebra
