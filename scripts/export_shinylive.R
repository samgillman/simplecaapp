# Export SimpleCa²⁺ as a Shinylive (WebAssembly) static site.
#
# The exported app runs entirely in the visitor's browser via webR — no R
# server — so the output can be hosted on any static host (Cloudflare Pages,
# GitHub Pages, ...). Uploaded data never leaves the visitor's machine.
#
# Usage (from the repo root):
#   Rscript scripts/export_shinylive.R
# Preview the result locally:
#   Rscript -e 'httpuv::runStaticServer("_shinylive")'

if (!requireNamespace("shinylive", quietly = TRUE)) {
  install.packages("shinylive")
}

# Stage only the files the app needs. Exporting the repo root would bundle
# renv/, docs/, manifest.json, tests/ etc. into the published site.
staging <- file.path(tempdir(), "simpleca-shinylive-app")
unlink(staging, recursive = TRUE)
dir.create(file.path(staging, "R"), recursive = TRUE)
dir.create(file.path(staging, "www"))

stopifnot(
  file.copy("app.R", staging),
  file.copy(list.files("R", full.names = TRUE), file.path(staging, "R")),
  file.copy(list.files("www", full.names = TRUE), file.path(staging, "www"))
)

out_dir <- "_shinylive"
unlink(out_dir, recursive = TRUE)
shinylive::export(staging, out_dir)

# ---- Branded loading splash ------------------------------------------------
# Shinylive's default is a bare spinner while webR prepares the browser-side R
# runtime. Use a separately previewable, tested splash that reports honest
# estimated progress and disappears only when the app UI is ready (or when the
# native Shinylive error/timeout fallback should be revealed).
splash_path <- file.path("assets", "shinylive-loading-splash.html")
stopifnot(file.exists(splash_path))
splash <- paste(readLines(splash_path, warn = FALSE), collapse = "\n")

# Reload the page once when a new service worker takes control, so visitors
# get new deploys on a plain refresh instead of a stale cached build. The
# hadController guard prevents a reload on the very first visit.
sw_reload <- '
<script>
if (navigator.serviceWorker) {
  var hadController = !!navigator.serviceWorker.controller;
  var reloaded = false;
  navigator.serviceWorker.addEventListener("controllerchange", function() {
    if (!hadController) { hadController = true; return; }
    if (reloaded) return;
    reloaded = true;
    window.location.reload();
  });
}
</script>
'

index_path <- file.path(out_dir, "index.html")
html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
stopifnot(grepl("</body>", html, fixed = TRUE))
html <- sub("</body>", paste0(splash, sw_reload, "\n</body>"), html, fixed = TRUE)
writeLines(html, index_path)
cat("Loading splash injected into ", index_path, "\n", sep = "")

# ---- Instant service-worker updates ---------------------------------------
# By default the Shinylive service worker keeps serving the previous cached
# build until the browser swaps workers on its own schedule, so users see
# stale versions after deploys. skipWaiting + clients.claim activates a new
# worker immediately; combined with the reload hook above, a plain refresh
# always yields the current build.
sw_path <- file.path(out_dir, "shinylive-sw.js")
if (file.exists(sw_path)) {
  sw <- paste(readLines(sw_path, warn = FALSE), collapse = "\n")
  sw <- paste0(
    sw,
    "\n\nself.addEventListener('install', function() { self.skipWaiting(); });\n",
    "self.addEventListener('activate', function(event) { event.waitUntil(self.clients.claim()); });\n"
  )
  writeLines(sw, sw_path)
  cat("Service worker patched for instant updates\n")
} else {
  warning("shinylive-sw.js not found; instant-update patch skipped")
}

cat("\nShinylive site written to ", out_dir, "/\n", sep = "")
cat("Preview with: Rscript -e 'httpuv::runStaticServer(\"_shinylive\")'\n")
