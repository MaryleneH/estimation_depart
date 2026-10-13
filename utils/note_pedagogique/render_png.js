// Rendu SVG -> PNG net (x3) avec cadrage exact. usage : node render.js fichier.svg [...]
const { chromium } = require("playwright-core"); const fs = require("fs"), path = require("path");
(async () => { const browser = await chromium.launch({ executablePath: "/opt/pw-browsers/chromium-1194/chrome-linux/chrome", args: ["--no-sandbox"] });
  for (const f of process.argv.slice(2)) { const s = fs.readFileSync(f, "utf8"); const m = s.match(/viewBox="0 0 (\d+) (\d+)"/); const w = +m[1] * 3, h = +m[2] * 3;
    const page = await browser.newPage({ viewport: { width: w, height: h } }); await page.goto("file://" + path.resolve(f)); await page.waitForTimeout(150);
    await page.screenshot({ path: f.replace(/\.svg$/, ".png"), clip: { x: 0, y: 0, width: w, height: h } }); await page.close(); console.log(path.basename(f), w, h); }
  await browser.close(); })();
