// Boot the exported Shinylive (WebAssembly) site in a real browser and
// exercise every kind of download, capturing response bytes and the full
// browser console so failures are observable instead of mysterious.
const { chromium } = require('playwright');
const fs = require('fs');
const path = require('path');

const OUT = path.join(process.cwd(), 'diagnosis-out');
fs.mkdirSync(OUT, { recursive: true });

// Small synthetic ImageJ-style recording: blank first header cell + Means
function makeCsv(file, cells, rows) {
  const hdr = [' ', ...Array.from({ length: cells }, (_, i) => `Mean${i + 1}`)];
  const lines = [hdr.join(',')];
  for (let t = 1; t <= rows; t++) {
    const vals = Array.from({ length: cells }, (_, i) =>
      (100 + i + (t > rows / 3 ? 60 * Math.exp(-((t - rows / 2) ** 2) / (rows * 4)) : 0) + Math.sin(t + i)).toFixed(3)
    );
    lines.push([t, ...vals].join(','));
  }
  fs.writeFileSync(file, lines.join('\n'));
}

function describeBytes(buf) {
  const head = buf.subarray(0, 4);
  const hex = Buffer.from(head).toString('hex');
  if (hex.startsWith('504b0304')) return 'ZIP';
  if (hex.startsWith('89504e47')) return 'PNG';
  if (buf.subarray(0, 200).toString('utf8').match(/<!doctype html|<html/i)) return 'HTML';
  return 'other(' + hex + ')';
}

(async () => {
  const csvPath = path.join(OUT, 'Fixture_Group.csv');
  makeCsv(csvPath, 6, 60);

  const browser = await chromium.launch();
  const page = await browser.newPage({ viewport: { width: 1500, height: 900 } });
  const consoleLog = [];
  page.on('console', (m) => consoleLog.push(`[${m.type()}] ${m.text()}`));
  page.on('pageerror', (e) => consoleLog.push(`[pageerror] ${e.message}`));

  const results = [];
  async function tryDownload(label, action) {
    try {
      const [dl] = await Promise.all([page.waitForEvent('download', { timeout: 60000 }), action()]);
      const fail = await dl.failure();
      if (fail) {
        results.push(`${label}: DOWNLOAD STARTED BUT FAILED reason="${fail}" url=${dl.url()}`);
        return;
      }
      const p = path.join(OUT, 'dl_' + label.replace(/\W+/g, '_'));
      await dl.saveAs(p);
      const buf = fs.readFileSync(p);
      const kind = describeBytes(buf);
      results.push(`${label}: suggested="${dl.suggestedFilename()}" bytes=${buf.length} kind=${kind}`);
      if (kind === 'HTML') {
        results.push(`--- ${label} HTML content (first 2000 chars) ---`);
        results.push(buf.toString('utf8').slice(0, 2000));
        results.push('--- end ---');
      }
    } catch (e) {
      results.push(`${label}: NO DOWNLOAD (${e.message.split('\n')[0]})`);
      await page.screenshot({ path: path.join(OUT, 'fail_' + label.replace(/\W+/g, '_') + '.png') }).catch(() => {});
    }
  }

  page.on('requestfailed', (r) => consoleLog.push(`[requestfailed] ${r.url()} :: ${r.failure() && r.failure().errorText}`));
  page.on('response', (r) => { if (r.status() >= 400) consoleLog.push(`[http ${r.status()}] ${r.url()}`); });

  function dumpConsole(label) {
    console.log(`\n================ BROWSER CONSOLE at ${label} ================`);
    consoleLog.slice(-200).forEach((l) => console.log(l));
  }

  console.log('booting wasm app...');
  await page.goto('http://127.0.0.1:8788', { waitUntil: 'domcontentloaded', timeout: 60000 });
  // The app renders inside the shinylive iframe; packages download at
  // runtime, so first boot on a slow runner can take several minutes
  const app = page.frameLocator('iframe').first();
  try {
    await app.locator('input[type="file"]').waitFor({ state: 'visible', timeout: 540000 });
  } catch (e) {
    await page.screenshot({ path: path.join(OUT, 'boot_timeout.png') }).catch(() => {});
    dumpConsole('BOOT TIMEOUT');
    fs.writeFileSync(path.join(OUT, 'console.txt'), consoleLog.join('\n'));
    throw e;
  }
  await page.waitForTimeout(5000);
  console.log('app booted');
  await page.screenshot({ path: path.join(OUT, 'booted.png') });

  const appFrame = page.frames().find((f) => f !== page.mainFrame() && f.url().includes('app_'));
  await app.locator('input[type="file"]').setInputFiles(csvPath);
  await page.waitForTimeout(5000);
  await app.locator('#load_data-load_btn').click();
  await page.waitForTimeout(25000);
  await page.screenshot({ path: path.join(OUT, 'processed.png') });

  // Probe download endpoints directly, bypassing the download manager:
  // status, headers, and leading bytes of whatever the service worker returns
  async function probeLinks(label) {
    const links = await appFrame.evaluate(() =>
      [...document.querySelectorAll('a.shiny-download-link')].map((a) => ({ id: a.id, href: a.href }))
    );
    results.push(`PROBE(${label}): ${links.length} download links`);
    for (const l of links) {
      try {
        const info = await appFrame.evaluate(async (u) => {
          const r = await fetch(u, { cache: 'no-store' });
          const buf = new Uint8Array(await r.arrayBuffer());
          let head = '';
          for (let i = 0; i < Math.min(buf.length, 220); i++) head += String.fromCharCode(buf[i]);
          return {
            status: r.status,
            ct: r.headers.get('content-type'),
            cd: r.headers.get('content-disposition'),
            len: buf.length,
            head,
          };
        }, l.href);
        results.push(
          `PROBE ${l.id}: status=${info.status} len=${info.len} ct=${info.ct} cd=${info.cd}`
        );
        results.push(`  head=${JSON.stringify(info.head)}`);
      } catch (e) {
        results.push(`PROBE ${l.id}: FETCH THREW ${e.message.split('\n')[0]}`);
      }
    }
  }

  // 1. Plot download via downloadHandler (Time Course tab)
  await app.locator('.sidebar-menu a[href="#shiny-tab-time"]').click();
  await page.waitForTimeout(12000);
  await appFrame.evaluate(() => {
    const h = [...document.querySelectorAll('.tab-pane.active .accordion-header')]
      .find((x) => x.textContent.includes('Plot Controls'));
    if (h) h.click();
  });
  await page.waitForTimeout(3000);
  await tryDownload('timecourse_plot_png', () => app.locator('#time_course-dl_timecourse_plot_local').click());

  // 2. Header CSV via downloadHandler (Data & Export)
  await app.locator('.sidebar-menu a[href="#shiny-tab-data_export"]').click();
  await page.waitForTimeout(10000);
  await probeLinks('export_tab');
  await tryDownload('header_metrics_csv', () => app.locator('#data_export-download_cell_metrics').click());

  // 3. DT corner button (client-side)
  await tryDownload('dt_corner_csv', () => app.locator('#data_export-cell_metrics_table a.buttons-csv').click());

  // 4. Processed-all ZIP via downloadHandler
  await app.locator('#data_export-tables_subtabs a:has-text("Processed Data")').click();
  await page.waitForTimeout(5000);
  await tryDownload('processed_all_zip', () => app.locator('#data_export-download_raw_all').click());

  // 5. Figures ZIP via downloadHandler
  await app.locator('#data_export-data_export_tabs a:has-text("Figure Export")').click();
  await page.waitForTimeout(5000);
  await tryDownload('figures_zip', () => app.locator('#data_export-dl_all_figures').click());

  await browser.close();

  console.log('\n================ DOWNLOAD RESULTS ================');
  results.forEach((r) => console.log(r));
  console.log('\n================ BROWSER CONSOLE (last 150) ================');
  consoleLog.slice(-150).forEach((l) => console.log(l));
  fs.writeFileSync(path.join(OUT, 'results.txt'), results.join('\n') + '\n\n' + consoleLog.join('\n'));

  const failures = results.filter((r) => r.includes('kind=HTML') || r.includes('NO DOWNLOAD'));
  if (failures.length) {
    console.log('\nFAILURES DETECTED:', failures.length);
    process.exit(1);
  }
  console.log('\nAll downloads OK');
})().catch((e) => {
  console.error('HARNESS FAIL:', e);
  process.exit(1);
});
