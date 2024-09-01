const ColorThief = require('colorthief');
const fs = require('fs');
const path = require('path');

if (process.argv.length <= 2) {
    console.log("Usage: " + __filename + " path/to/image");
    process.exit(-1);
}

const originalImagePath = process.argv[2];
let imagePath = originalImagePath;

// Check if file exists
if (!fs.existsSync(originalImagePath)) {
    console.log("File not found: " + originalImagePath);
    process.exit(-1);
}

// If the file is a .webp file, copy and rename it to image.png in the working directory
if (path.extname(originalImagePath).toLowerCase() === '.webp') {
    const targetPath = path.join(process.cwd(), 'image.png');
    fs.copyFileSync(originalImagePath, targetPath);
    imagePath = targetPath;
}

// Function to convert RGB to Hex
function rgbToHex(r, g, b) {
    return "#" + ((1 << 24) + (r << 16) + (g << 8) + b).toString(16).slice(1);
}

// Asynchronous function to get dominant color and palette
async function getColorData(imagePath) {
    try {
        const dominantColor = await ColorThief.getColor(imagePath);
        const palette = await ColorThief.getPalette(imagePath);

        const colorData = {
            dominantColor: rgbToHex(...dominantColor),
            colorPalette: palette.map(rgb => rgbToHex(...rgb))
        };

        console.log(JSON.stringify(colorData, null, 2));
    } catch (error) {
        console.error("Error processing image:", error);
    }
}

// Call the asynchronous function
getColorData(imagePath);

