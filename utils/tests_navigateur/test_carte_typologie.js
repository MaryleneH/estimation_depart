// Test navigateur de la carte interactive : stabilité du survol et du défilement.
// usage : CHROME=/chemin/vers/chrome node test_carte_typologie.js <chemin absolu de carte_typologie.html>  (voir README.md)
const { chromium } = require("playwright-core");
const fichier = process.argv[2];
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROME || undefined, args: ["--no-sandbox"] });
  const resultats = [];
  const ok = (nom, cond, info) => { resultats.push([cond ? "OK " : "KO ", nom, info || ""]); };
  for (const [nomVue, vp] of [["bureau 1368", { width: 1368, height: 820 }], ["mobile 390", { width: 390, height: 844 }]]) {
    const page = await browser.newPage({ viewport: vp, hasTouch: false });
    await page.goto("file://" + fichier);
    await page.waitForTimeout(200);
    const codes = await page.$$eval("path.t[data-cle]", ps => ps.slice(0, 12).map(p => p.getAttribute("data-code")));
    const longCode = await page.evaluate(() => { const D = JSON.parse(document.getElementById("donnees").textContent); let best = null, n = -1;
      Object.keys(D.territoires).forEach(c => { const t = D.territoires[c]; const l = (t.texte || "").length; if (l > n) { n = l; best = c; } }); return best; });
    const mesure = async () => page.evaluate(() => ({ sh: document.scrollingElement.scrollHeight, sw: document.scrollingElement.scrollWidth, y: window.scrollY,
      carteTop: document.getElementById("carte").getBoundingClientRect().top + window.scrollY, detailH: document.getElementById("detail").getBoundingClientRect().height }));
    const survol = async (code) => { const b = await page.$eval("#t-" + code, p => { const r = p.getBoundingClientRect(); return { x: r.left + r.width / 2, y: r.top + r.height / 2 }; });
      await page.mouse.move(b.x, b.y); await page.mouse.move(b.x + 2, b.y + 1); await page.waitForTimeout(30); };
    const avant = await mesure();
    // 1. survol de plusieurs départements : hauteur défilable, largeur et position de la carte inchangées, panneau de hauteur stable
    let stable = true, infos = [];
    for (const c of [...codes, longCode]) {
      await page.evaluate(c => document.getElementById("t-" + c).scrollIntoView({ block: "center" }), c);
      await survol(c); const m = await mesure();
      if (m.sh !== avant.sh || m.sw !== avant.sw || m.carteTop !== avant.carteTop || Math.abs(m.detailH - avant.detailH) > 0.5) { stable = false; infos.push(c + ":" + JSON.stringify(m)); }
      const bulle = await page.$eval("#bulle", b => { const r = b.getBoundingClientRect(); return { d: getComputedStyle(b).display, l: r.left, t: r.top, r: r.right, b: r.bottom, pos: getComputedStyle(b).position }; });
      if (bulle.d === "block" && (bulle.l < 0 || bulle.t < 0 || bulle.r > vp.width || bulle.b > vp.height)) { stable = false; infos.push("bulle hors écran " + c + " " + JSON.stringify(bulle)); }
    }
    ok(nomVue + " : survol de " + (codes.length + 1) + " départements (dont le texte le plus long) sans variation de la hauteur défilable, de la carte ni du panneau", stable, infos.slice(0, 3).join(" | ") + " avant=" + JSON.stringify(avant));
    // 2. défilement à la molette, curseur sur la carte : la position suit les deltas, pas de saut ni de retour
    await page.evaluate(() => window.scrollTo(0, 0)); await page.waitForTimeout(50);
    const c0 = codes[0]; await page.evaluate(c => document.getElementById("t-" + c).scrollIntoView({ block: "center" }), c0); await survol(c0);
    const y0 = await page.evaluate(() => window.scrollY); const pos = [y0];
    for (let i = 0; i < 6; i++) { await page.mouse.wheel(0, 120); await page.waitForTimeout(60); pos.push(await page.evaluate(() => window.scrollY)); }
    for (let i = 0; i < 4; i++) { await page.mouse.wheel(0, -120); await page.waitForTimeout(60); pos.push(await page.evaluate(() => window.scrollY)); }
    const monotone = pos.slice(1, 7).every((v, i) => v >= pos[i]) && pos.slice(7).every((v, i) => v <= pos[6 + i]);
    const m2 = await mesure();
    ok(nomVue + " : défilement molette avec curseur sur la carte, positions monotones et hauteur défilable constante", monotone && m2.sh === avant.sh, "positions=" + pos.join(",") + " sh=" + m2.sh + "/" + avant.sh);
    const bulleApres = await page.$eval("#bulle", b => getComputedStyle(b).display);
    ok(nomVue + " : infobulle masquée pendant le défilement", bulleApres === "none", "display=" + bulleApres);
    // 3. épinglage puis défilement : le panneau garde le département épinglé
    await page.evaluate(c => document.getElementById("t-" + c).scrollIntoView({ block: "center" }), longCode); await survol(longCode);
    await page.$eval("#t-" + longCode, p => p.dispatchEvent(new MouseEvent("click", { bubbles: true })));   // clic au centre géométrique peu fiable (polygones concaves) : événement direct
    await page.waitForTimeout(50);
    if (nomVue.startsWith("bureau")) await page.screenshot({ path: __dirname + "/apercu_" + (fichier.includes("avant") ? "avant" : "apres") + ".png" });
    const titrePin = await page.$eval("#detail-titre", e => e.textContent);
    await page.mouse.wheel(0, 200); await page.waitForTimeout(80); await survol(codes[1]);
    const titreApres = await page.$eval("#detail-titre", e => e.textContent);
    const aide = await page.$eval("#detail-aide", e => e.textContent);
    ok(nomVue + " : épinglage conservé après défilement et survol d’un autre département", titrePin === titreApres && /Épinglé/.test(aide), titrePin + " / " + titreApres);
    await page.$eval("#t-" + longCode, p => p.dispatchEvent(new MouseEvent("click", { bubbles: true })));
    await page.waitForTimeout(50);
    ok(nomVue + " : second clic = libération", !/Épinglé/.test(await page.$eval("#detail-aide", e => e.textContent)));
    // 4. filtrage par la légende
    await page.click('.typo-legende button[data-cle="enjeu"]'); await page.waitForTimeout(50);
    const nDim = await page.$$eval("path.t.dim", ps => ps.length), nAll = await page.$$eval("path.t", ps => ps.length), nEnjeu = await page.$$eval('path.t[data-cle="enjeu"]', ps => ps.length);
    ok(nomVue + " : filtrage légende (autres départements estompés)", nDim === nAll - nEnjeu, nDim + "/" + nAll + " enjeu=" + nEnjeu);
    await page.click('.typo-legende button[data-cle="enjeu"]'); await page.waitForTimeout(50);
    ok(nomVue + " : second clic légende = tout réaffiché", (await page.$$eval("path.t.dim", ps => ps.length)) === 0);
    // 5. légende : aucune entrée « absent » à zéro
    const leg = await page.$eval("#legende", e => e.textContent);
    const nSans = await page.evaluate(() => JSON.parse(document.getElementById("donnees").textContent).comptes.sans);
    ok(nomVue + " : légende sans entrée « absent » quand le compte est nul (sans=" + nSans + ")", nSans > 0 ? /absent de la typologie/.test(leg) : !/absent de la typologie/.test(leg));
    await page.close();
  }
  await browser.close();
  for (const r of resultats) console.log(r[0], r[1], r[2] ? "   [" + r[2] + "]" : "");
  const ko = resultats.filter(r => r[0] === "KO ").length; console.log(ko ? ko + " échec(s)" : "tout est stable"); process.exit(ko ? 1 : 0);
})();
