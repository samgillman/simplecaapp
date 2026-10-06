# Install the exact Shinylive release used by CI and production deployment.
SHINYLIVE_VERSION <- "0.5.0"
# S7 supplies local metadata for the WebAssembly ggplot2 dependency graph,
# even when the locked native ggplot2 release predates that dependency.
SHINYLIVE_DEPENDENCIES <- c("archive", "gh", "pkgdepends", "renv", "whisker", "S7")
SHINYLIVE_SOURCES <- c(
  paste0("https://cran.r-project.org/src/contrib/shinylive_", SHINYLIVE_VERSION, ".tar.gz"),
  paste0("https://cran.r-project.org/src/contrib/Archive/shinylive/shinylive_", SHINYLIVE_VERSION, ".tar.gz")
)

# Installing a package from an exact source URL uses repos = NULL, so R does
# not resolve its dependencies automatically. Install those from the selected
# CRAN mirror before installing the pinned Shinylive tarball.
missing_dependencies <- SHINYLIVE_DEPENDENCIES[
  !vapply(SHINYLIVE_DEPENDENCIES, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_dependencies)) {
  install.packages(missing_dependencies)
}
still_missing <- SHINYLIVE_DEPENDENCIES[
  !vapply(SHINYLIVE_DEPENDENCIES, requireNamespace, logical(1), quietly = TRUE)
]
if (length(still_missing)) {
  stop("Could not install Shinylive dependencies: ", paste(still_missing, collapse = ", "))
}

if (requireNamespace("shinylive", quietly = TRUE) &&
    as.character(utils::packageVersion("shinylive")) == SHINYLIVE_VERSION) {
  quit(save = "no", status = 0)
}

errors <- character()
for (source in SHINYLIVE_SOURCES) {
  installed <- tryCatch({
    install.packages(source, repos = NULL, type = "source")
    requireNamespace("shinylive", quietly = TRUE) &&
      as.character(utils::packageVersion("shinylive")) == SHINYLIVE_VERSION
  }, error = function(e) {
    errors <<- c(errors, paste(source, conditionMessage(e), sep = ": "))
    FALSE
  })
  if (installed) quit(save = "no", status = 0)
}

stop(
  "Could not install pinned shinylive ", SHINYLIVE_VERSION, ". ",
  paste(errors, collapse = " | ")
)
