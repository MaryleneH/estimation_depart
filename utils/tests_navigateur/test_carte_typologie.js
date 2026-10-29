// Test navigateur de la carte interactive : premier écran (tout dans la fenêtre), stabilité du survol et du défilement.
// usage : CHROME=/chemin/vers/chrome node test_carte_typologie.js <chemin absolu de carte_typologie.html>  (voir README.md)
const { chromium } = require("playwright-core");
const fichier = process.argv[2];
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROME || undefined, args: ["--no-sandbox"] });
  const resultats = [];
  const ok = (nom, cond, info) => { resultats.push([cond ? "OK " : "KO ", nom, info || ""]); };
  for (const [nomVue, vp] of [["bureau 1368", { width: 1368, height: 820 }], ["bureau 1280", { width: 1280, height: 720 }], ["mobile 390", { width: 390, height: 844 }]]) {
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
    // 0. écran large : en-tête, carte entière (tous les départements), panneau et légende dans la fenêtre, sans défiler ; seul le panneau défile en interne
    if (nomVue.startsWith("bureau")) {
      const e = await page.evaluate(() => { const r = s => document.querySelector(s).getBoundingClientRect(); const dans = x => x.top >= -0.5 && x.bottom <= innerHeight + 0.5;
        let horsEcran = 0; document.querySelectorAll("path.t").forEach(p => { if (!dans(p.getBoundingClientRect())) horsEcran++; });
        return { y: scrollY, ecran: dans(r(".ecran")), svg: dans(r("#carte svg")), panneau: dans(r(".panneau")), legende: dans(r(".legende-bloc")), horsEcran, ov: getComputedStyle(document.getElementById("detail")).overflowY }; });
      ok(nomVue + " : premier écran entier dans la fenêtre (en-tête, carte et ses " + "départements, panneau, légende), panneau à défilement interne",
         e.y === 0 && e.ecran && e.svg && e.panneau && e.legende && e.horsEcran === 0 && e.ov === "auto", JSON.stringify(e));
    }
    const avant = await mesure();
    // 1. survol de plusieurs départements : hauteur défilable, largeur et position de la carte inchangées, panneau de hauteur stable
    let stable = true, infos = [];
    for (const c of [...codes, longCode]) {
      if (!nomVue.startsWith("bureau")) await page.evaluate(c => document.getElementById("t-" + c).scrollIntoView({ block: "center" }), c);   // écran large : aucun défilement nécessaire
      await survol(c); const m = await mesure();
      if (m.sh !== avant.sh || m.sw !== avant.sw || m.carteTop !== avant.carteTop || Math.abs(m.detailH - avant.detailH) > 0.5) { stable = false; infos.push(c + ":" + JSON.stringify(m)); }
      const bulle = await page.$eval("#bulle", b => { const r = b.getBoundingClientRect(); return { d: getComputedStyle(b).display, l: r.left, t: r.top, r: r.right, b: r.bottom, pos: getComputedStyle(b).position }; });
      if (bulle.d === "block" && (bulle.l < 0 || bulle.t < 0 || bulle.r > vp.width || bulle.b > vp.height)) { stable = false; infos.push("bulle hors écran " + c + " " + JSON.stringify(bulle)); }
    }
    ok(nomVue + " : survol de " + (codes.length + 1) + " départements (dont le texte le plus long) sans variation de la hauteur défilable, de la carte ni du panneau", stable, infos.slice(0, 3).join(" | ") + " avant=" + JSON.stringify(avant));
    if (nomVue.startsWith("bureau")) ok(nomVue + " : aucun défilement de page pendant le survol", (await page.evaluate(() => window.scrollY)) === 0);
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
    const titreProfil = await page.$eval("#detail-titre", e => e.textContent), aideProfil = await page.$eval("#detail-aide", e => e.textContent);
    ok(nomVue + " : clic légende = profil isolé, signification et consigne dans le panneau", /^Profil isolé/.test(titreProfil) && /Consigne de lecture/.test(aideProfil), titreProfil);
    await page.click('.typo-legende button[data-cle="enjeu"]'); await page.waitForTimeout(50);
    ok(nomVue + " : second clic légende = tout réaffiché", (await page.$$eval("path.t.dim", ps => ps.length)) === 0);
    // 5. légende : aucune entrée « absent » à zéro
    const leg = await page.$eval("#legende", e => e.textContent);
    const cptes = await page.evaluate(() => JSON.parse(document.getElementById("donnees").textContent).comptes);
    ok(nomVue + " : légende sans entrée « absent » ni « Aucun salarié observé » (SPEC-PUB-007), aucun compte d'absents embarqué", !/absent de la typologie|Aucun salarié observé/.test(leg) && !("sans" in cptes));
    await page.close();
  }
  await browser.close();
  for (const r of resultats) console.log(r[0], r[1], r[2] ? "   [" + r[2] + "]" : "");
  const ko = resultats.filter(r => r[0] === "KO ").length; console.log(ko ? ko + " échec(s)" : "tout est stable"); process.exit(ko ? 1 : 0);
})();
