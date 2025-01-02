npm install
npx playwright install chromium
echo "Testing screenshots script..."
node ./get-screenshots.js -i https://run.rvbbit.com/#/server-stats -w 10 -o ./ -W 1920 -H 1080
