#!/bin/bash

# Check if the current directory path ends with "rvbbit"
if [[ "${PWD}" != */rvbbit ]]; then
    echo "Error: This script must be run from the rvbbit root source directory."
    echo "Current directory: ${PWD}"
    exit 1
fi

# If we're here, we're safe-ish.

# clear
export LEIN_SNAPSHOTS_IN_RELEASE=true ## for now, until we have a better build process. besides, most of the snapshot deps are mine.
cd frontend
./clean-me.sh
npm install
rm -rf .shadow-cljs
npx shadow-cljs release app
echo "UI compiled. copying to server resources"
cd ..
rm -rf backend/resources/public/
cp -r frontend/resources/public backend/resources/public
rm -rf backend/resources/public/images/gen/
rm -rf backend/resources/public/images/large/
echo "UI copied. compiling uberjar."
cd backend
mkdir target -p
mkdir db -p
ls ./target -l -h
lein clean
lein uberjar
ls ./target -l -h
echo "done."