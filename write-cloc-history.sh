#!/bin/bash

# Extract the version number from project.edn
VERSION=$(grep -m1 'defproject' backend/project.clj | sed 's/^[^"]*"//' | sed 's/".*//')

# Get current date and time in YYYY-MM-DD HH:MM:SS format
CURRENT_DATETIME=$(date +"%Y-%m-%d %H:%M:%S")

# Run cloc and capture the output
CLOC_OUTPUT=$(cloc ./ --include-lang=Clojure,ClojureScript --csv --quiet)

# Extract the line counts for Clojure and ClojureScript
CLOJURE_LINES=$(echo "$CLOC_OUTPUT" | grep 'Clojure,' | cut -d',' -f5)
CLOJURESCRIPT_LINES=$(echo "$CLOC_OUTPUT" | grep 'ClojureScript,' | cut -d',' -f5)

# Sum up the lines
TOTAL_LINES=$((CLOJURE_LINES + CLOJURESCRIPT_LINES))

# Prepare the CSV line
CSV_LINE="$CURRENT_DATETIME,$VERSION,$TOTAL_LINES,$CLOJURE_LINES,$CLOJURESCRIPT_LINES"

# Define the CSV file name
CSV_FILE="loc_history.csv"

# If the file doesn't exist, create it with a header
if [ ! -f "$CSV_FILE" ]; then
    echo "Datetime,Version,Total Lines,Clojure Lines,ClojureScript Lines" > "$CSV_FILE"
fi

# Append the new data to the CSV file
echo "$CSV_LINE" >> "$CSV_FILE"

echo "Lines of code data appended to $CSV_FILE"
echo "Datetime: $CURRENT_DATETIME"
echo "Version: $VERSION"
echo "Total Lines: $TOTAL_LINES"
echo "Clojure Lines: $CLOJURE_LINES"
echo "ClojureScript Lines: $CLOJURESCRIPT_LINES"
