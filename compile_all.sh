#!/bin/bash

# Check if the current directory path ends with "rvbbit"
if [[ "${PWD}" != */rvbbit ]]; then
    echo "Error: This script must be run from the rvbbit root source directory."
    echo "Current directory: ${PWD}"
    exit 1
fi

# If we're here, we're safe-ish.

clear
cd frontend
./clean-me.sh
npm install
rm -rf frontend/.shadow-cljs
npx shadow-cljs release app
echo "UI compiled. copying to server resources"
cd backend
rm -rf backend/resources/public/
lein clean
cp -r frontend/resources/public backend/resources/public
echo "UI copied. compiling uberjar."
cd backend
mkdir target
ls ./target -l -h
lein clean
lein uberjar
ls ./target -l -h
echo "done."