#!/bin/bash

# Path to the zprint jar file
# Uncomment and set the path if using the jar
# ZPRINT_JAR="./zprint-filter"

# cp ../frontend/src/rvbbit_frontend/bricks.cljs ./
# cp ../frontend/src/rvbbit_frontend/utility.cljs ./
# cp ../backend/src/rvbbit_backend/websockets.clj ./

# Directory to format (current directory)
DIRECTORY="."

# Find and format all Clojure and ClojureScript files
# find "$DIRECTORY" -type f \( -name "*.clj" -o -name "*.cljs" \) -print0 | while IFS= read -r -d '' file; do
find "$DIRECTORY" -type f \( -name "*.clj" -o -name "*.cljs" -o -name "*.cljc" \) -print0 | while IFS= read -r -d '' file; do
  #echo "Processing $file"

  # Remove full-line comments
  #sed -E '/^\s*;/d' "$file" > "$file.nocomments"
  cp "$file" "$file.nocomments" ## to leave comments for now

  # zprint formatting
  echo "zprint formatting $file"
  #zprint '{:style [:sort-require :ns-justify :justified-original] :comment {:smart-wrap? false} :width 120 :map {:comma? false :sort? false}}' < "$file.nocomments" > "$file.tmp" && mv "$file.tmp" "$file"
  zprint '{:style [:sort-require :ns-justify :respect-nl :respect-bl :justified-original] :comment {:smart-wrap? true} :width 120 :map {:comma? false :sort? false}}' < "$file.nocomments" > "$file.tmp" && mv "$file.tmp" "$file"

  # Clean up temporary file
  rm -f "$file.nocomments"
done

echo "Formatting complete."


