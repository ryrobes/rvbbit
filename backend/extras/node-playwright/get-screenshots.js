const { chromium } = require('playwright');
const fs = require('fs');
const path = require('path');
const yargs = require('yargs/yargs');
const { hideBin } = require('yargs/helpers');

const argv = yargs(hideBin(process.argv))
  .usage('Usage: $0 [options]')
  .example('$0 -i https://example.com -o ./pics', 'Capture a single URL')
  .example('$0 -i ./edns -b http://localhost:8080', 'Process EDN files from directory')
  .example('$0 -i ./edns -f dashboard', 'Process EDN files containing "dashboard"')
  .option('width', {
    alias: 'W',
    type: 'number',
    description: 'Viewport and screenshot width',
    default: 2560
  })
  .option('height', {
    alias: 'H',
    type: 'number',
    description: 'Viewport and screenshot height',
    default: 1440
  })
  .option('wait', {
    alias: 'w',
    type: 'number',
    description: 'Seconds to wait before taking screenshot',
    default: 60
  })
  .option('output-dir', {
    alias: 'o',
    type: 'string',
    description: 'Directory to save screenshots (will be created if not exists)',
    default: './screenshots'
  })
  .option('base-url', {
    alias: 'b',
    type: 'string',
    description: 'Base URL for generating paths',
    default: 'https://run.rvbbit.com'
  })
  .option('filter', {
    alias: 'f',
    type: 'string',
    description: 'Filter string for EDN files',
    default: ''
  })
  .option('input', {
    alias: 'i',
    type: 'string',
    description: 'Input file, directory, or direct URL'
  })
  .help()
  .argv;

const VIEWPORT = {
  width: argv.width,
  height: argv.height
};

const LOAD_TIMEOUT = 60000;

if (!fs.existsSync(argv.outputDir)) {
  fs.mkdirSync(argv.outputDir, { recursive: true });
}

function getUrls(fileOrDir, baseUrl, filter) {
  // Handle direct URL input
  if (fileOrDir && (fileOrDir.startsWith('http://') || fileOrDir.startsWith('https://'))) {
    return [fileOrDir];
  }

  // Default URLs if no path provided
  if (!fileOrDir) {
    const defaultUrls = ['server-stats', 'server-clock', 'server-stats'];
    return defaultUrls.map(path => `${baseUrl}/#/${path}`);
  }

  // Handle single file
  if (fileOrDir.endsWith('.edn')) {
    if (!fs.existsSync(fileOrDir)) {
      console.error(`File not found: ${fileOrDir}`);
      process.exit(1);
    }
    const filename = path.basename(fileOrDir, '.edn');
    return [`${baseUrl}/#/${filename}`];
  }

  // Handle directory
  if (!fs.existsSync(fileOrDir)) {
    console.error(`Directory not found: ${fileOrDir}`);
    process.exit(1);
  }

  return fs.readdirSync(fileOrDir)
    .filter(file => file.endsWith('.edn') && file.includes(filter))
    .map(file => `${baseUrl}/#/${file.slice(0, -4)}`);
}

async function takeScreenshot(page, url) {
  const urlPath = new URL(url).hash.slice(2);
  const lastPathSegment = urlPath.split('/').filter(Boolean).pop() || 'root';
  const filename = `${lastPathSegment}.jpg`;
  const filepath = path.join(argv.outputDir, filename);
  
  await page.screenshot({ 
    path: filepath,
    clip: {
      x: 0,
      y: 0,
      width: VIEWPORT.width,
      height: VIEWPORT.height
    },
    scale: 'device',
    type: 'jpeg',
    quality: 90
  });
  
  console.log(`Screenshot saved for ${urlPath} at ${new Date().toLocaleTimeString()}`);
}

async function captureUrl(url) {
  const browser = await chromium.launch({ 
    headless: true,
    args: [
      '--disable-dev-shm-usage',
      `--window-size=${VIEWPORT.width},${VIEWPORT.height}`,
      '--force-device-scale-factor=1'
    ]
  });
  
  const context = await browser.newContext({
    viewport: VIEWPORT,
    deviceScaleFactor: 1,
    screen: VIEWPORT
  });
  
  const page = await context.newPage();
  
  try {
    console.log(`Loading ${url}...`);
    await page.goto(url, { timeout: LOAD_TIMEOUT });
    await page.waitForLoadState('networkidle', { timeout: LOAD_TIMEOUT });
    console.log(`Page loaded, waiting ${argv.wait} seconds for full render...`);
    await new Promise(resolve => setTimeout(resolve, argv.wait * 1000));
    await takeScreenshot(page, url);
  } catch (error) {
    console.error(`Error capturing ${url}:`, error);
  } finally {
    await browser.close();
  }
}

async function captureAllUrls() {
  const urls = getUrls(argv.input, argv.baseUrl, argv.filter);
  
  console.log(`Starting sequential screenshot capture for ${urls.length} URLs`);
  console.log(`Screenshots will be saved to ${path.resolve(argv.outputDir)}`);
  console.log(`Viewport: ${VIEWPORT.width}x${VIEWPORT.height}\n`);
  
  for (const url of urls) {
    await captureUrl(url);
  }
  
  console.log('\nCapture complete');
  console.log(`Total screenshots: ${fs.readdirSync(argv.outputDir).length}`);
}

captureAllUrls().catch(console.error);


