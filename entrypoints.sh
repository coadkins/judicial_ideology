#!/bin/bash

# Function to display help
display_help() {
    echo "Usage: docker run -v /path/to/local/scripts:/home/judicial_ideology/R -v /path/to/local/results:/home/judicial_ideology/results hirt <script_name.R>"
    echo ""
    echo "Options:"
    echo "  help    - Display this help message"
    echo "  <script> - Run the specified R script from /R"
}

# Check if no arguments or help is requested
if [[ $# -eq 0 || "$1" == "help" ]]; then
    display_help
    exit 0
fi

# Get the script name
SCRIPT_NAME=$1

# Verify the script exists
if [ ! -f "/home/judicial_ideology/R/$SCRIPT_NAME" ]; then
    echo "Error: Script /home/judicial_ideology/R/$SCRIPT_NAME not found!"
    exit 1
fi

# Run the R script
echo "Running R script: $SCRIPT_NAME"
Rscript "/home/judicial_ideology/R/$SCRIPT_NAME"
