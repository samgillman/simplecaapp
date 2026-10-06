# Restore the exporter and its recursive dependencies from the same lockfile
# used by CI. Checking only requireNamespace() is insufficient: cached packages
# can load successfully yet fail when a dependency is loaded lazily (httr2).
# Always reconcile a warm library before loading the exporter; never install
# latest transitive dependencies over the application's pinned rlang/curl.
SHINYLIVE_VERSION <- "0.5.0"
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

renv::restore(
  project = ".",
  lockfile = "renv.lock",
  packages = c("shinylive", "S7"),
  library = .libPaths()[1],
  prompt = FALSE
)

# httr2 is used lazily by assets_download(), so exercise it explicitly here.
stopifnot(
  requireNamespace("httr2", quietly = TRUE),
  requireNamespace("shinylive", quietly = TRUE),
  requireNamespace("S7", quietly = TRUE),
  as.character(utils::packageVersion("shinylive")) == SHINYLIVE_VERSION
)
