#!/usr/bin/env bash

# Check if the current directory path ends with "rvbbit"
if [[ "${PWD}" != */rvbbit ]]; then
    echo "Error: This script must be run from the rvbbit root source directory."
    echo "Current directory: ${PWD}"
    exit 1
fi

# If we're here, we're safe-ish.

export LEIN_SNAPSHOTS_IN_RELEASE=true ## for now, until we have a better build process. besides, most of the snapshot deps are mine.

draw_bordered_text() {
    local BRIGHT_CYAN="\e[1;96m"
    local BOLD_BRIGHT_PINK="\e[1;95m"
    local RESET="\e[0m"
    local text="$1"
    local text_width=${#text}
    local total_width=$((text_width + 6))
    local border=$(printf "%0.s#" $(seq 1 $total_width))

    echo -e "${BRIGHT_CYAN}${border}${RESET}"
    echo -e "${BRIGHT_CYAN}## ${RESET}${BOLD_BRIGHT_PINK}${text}${RESET}${BRIGHT_CYAN} ##${RESET}"
    echo -e "${BRIGHT_CYAN}${border}${RESET}"
}

cd frontend
./clean-me.sh
draw_bordered_text "Cleaning, pulling deps, compiling UI..."
npm install
rm -rf .shadow-cljs
npx shadow-cljs release app
draw_bordered_text "UI compiled. copying to server resources"
cd ..
rm -rf backend/resources/public/
cp -r frontend/resources/public backend/resources/public
rm -rf backend/resources/public/images/gen/
rm -rf backend/resources/public/images/large/
draw_bordered_text "UI copied. compiling uberjar."
cd backend
mkdir target -p
mkdir db -p
mkdir data/atoms -p
# ls ./target/*.jar -l -h
lein clean
lein uberjar
ls ./target/*.jar -l -h
find ./target -name "rvbbit*standalone.jar" -exec cp {} rvbbit.jar \;


VERSION=$(grep -m1 'defproject' project.clj | sed 's/^[^"]*"//' | sed 's/".*//')
##VERSION=${VERSION%.*}
DIR_NAME="rvbbit-$VERSION"
rm -rf "../$DIR_NAME"
mkdir -p "../$DIR_NAME"
draw_bordered_text "creating release... $DIR_NAME"
cp -r assets "../$DIR_NAME/assets"
cp -r connections "../$DIR_NAME/connections"
cp -r data "../$DIR_NAME/data"
mkdir -p "../$DIR_NAME/db"
cp -r defs "../$DIR_NAME/defs"
cp -r flows "../$DIR_NAME/flows"
cp -r extras "../$DIR_NAME/extras"
cp -r resources "../$DIR_NAME/resources"
cp -r screens "../$DIR_NAME/screens"
cp user.clj "../$DIR_NAME/"
cp run-rabbit.sh "../$DIR_NAME/"
cp rvbbit.jar "../$DIR_NAME/"

## my dev repo has lots of things in gitignore - but still present locally. wont apply to fresh clones.
rm -rf ../docker-staging
rm -rf "../$DIR_NAME/assets/data-exports"
rm -rf "../$DIR_NAME/data/atoms"
rm -rf "../$DIR_NAME/defs/backup"
rm -f "../$DIR_NAME/defs/secrets.edn"
rm -rf "../$DIR_NAME/extras/node-colorthief/node_modules"
rm "../$DIR_NAME/extras/node-colorthief/package-lock.json"
mkdir -p "../$DIR_NAME/data/atoms"
mkdir -p "../$DIR_NAME/assets/data-exports"

## due to docker "layering" I cannot remove files once added to a layer, so I have to pre-remove them here first
## also only applies to local dev repo, not fresh clones.
mkdir -p ../docker-staging

rm -rf "../$DIR_NAME.zip"
zip -rq "../$DIR_NAME.zip" "../$DIR_NAME/"

cd "../$DIR_NAME"
cp -r * ../docker-staging/
cd ../backend

echo ""
cat data/nname.ans
echo ""
draw_bordered_text "done. go to ./backend/ or ./$DIR_NAME/  and run ./run-rabbit.sh, then visit localhost:8888"
echo ""

# draw_bordered_text "building Docker image..."
# cd ..
# docker build --no-cache -t rvbbit .
# docker run -d -p 8888:8888 -p 3030:3030 -p 8181:8181 -v rvbbit-root:/app/ rvbbit 
# docker pull ghcr.io/ryrobes/rvbbit:latest
# docker run -d -p 8888:8888 -p 3030:3030 -p 8181:8181 -v rvbbit-root2:/app/ ghcr.io/ryrobes/rvbbit:latest

