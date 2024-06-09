#!/bin/bash

# Path to the zprint jar file
#ZPRINT_JAR="../zprint-filter-1.2.9"
OPTS="'{:style [:community :sort-require :ns-justify :require-pair :require-justify :respect-bl :respect-nl :justified-original] :comment {:smart-wrap? true} :width 110 :map {:comma? false :sort? false}}'"

# Directory to format (current directory)
DIRECTORY="."

# Find and format all Clojure and ClojureScript files
find "$DIRECTORY" -type f \( -name "*.clj" -o -name "*.cljs" \) -print0 | while IFS= read -r -d '' file; do
  echo "zprint formatting $file"
  #java -jar "$ZPRINT_JAR" < "$file" > "$file.tmp" && mv "$file.tmp" "$file"
  time zprint $OPTS < "$file" > "$file.tmp" && mv "$file.tmp" "$file"
done

echo "Formatting complete."


