#!/bin/bash

DIRECTORY="backend/defs"

echo "Formatting Rabbit Defs"

# Find and format all Clojure, ClojureScript, and Clojure/ClojureScript files, excluding hidden directories
find "$DIRECTORY" -type f \( -name "*.edn" -o -name "*.rabbit" \) -print0 | while IFS= read -r -d '' file; do
  #echo "Processing $file"

  # Remove full-line comments
  #sed -E '/^\s*;/d' "$file" > "$file.nocomments"
  cp "$file" "$file.nocomments" ## to leave comments for now

  # zprint formatting
  echo "zprint formatting $file"  ##  :respect-nl :respect-bl preseves lots of formatting, be prevents lots of formatting changes... mixed bag
  if ! zprint '{:style [:sort-require :ns-justify :justified-original] :parse-string? true :comment {:count? nil :wrap? nil} :width 120 :map {:comma? false :sort? false}}' < "$file.nocomments" > "$file.tmp"; then
    echo "zprint failed for $file, skipping..."
    rm -f "$file.nocomments" "$file.tmp"
    continue
  fi

  # Move formatted file back to original
  mv "$file.tmp" "$file"

  # Clean up temporary file
  rm -f "$file.nocomments"
done

echo "Formatting complete."


