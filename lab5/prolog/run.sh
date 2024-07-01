#!/bin/bash

# Load the Prolog file and execute the main function
swipl -g "consult('$1'), halt."
