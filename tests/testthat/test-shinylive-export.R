test_that("the Shinylive loading screen is production-safe and accessible", {
  splash_path <- file.path(repo_root, "assets", "shinylive-loading-splash.html")
  expect_true(file.exists(splash_path))

  splash <- paste(readLines(splash_path, warn = FALSE), collapse = "\n")

  expect_match(splash, 'role="progressbar"', fixed = TRUE)
  expect_match(splash, 'aria-label="Estimated app loading progress"', fixed = TRUE)
  expect_match(splash, 'id="simpleca-refresh"', fixed = TRUE)
  expect_match(splash, "loading-wrapper-error", fixed = TRUE)
  expect_match(splash, "appReady(document)", fixed = TRUE)
  expect_match(splash, 'indexOf("/webr/packages/")', fixed = TRUE)
  expect_match(splash, "STALL_AFTER_MS = 90000", fixed = TRUE)
  expect_match(splash, "HARD_TIMEOUT_MS = 240000", fixed = TRUE)
  expect_match(splash, "Your data never leaves this device", fixed = TRUE)

  expect_false(grepl('class="demo"', splash, fixed = TRUE))
  expect_false(grepl("runBoot", splash, fixed = TRUE))
  expect_false(grepl("runStall", splash, fixed = TRUE))
  expect_false(grepl("of 78 MB", splash, fixed = TRUE))
})

test_that("the Shinylive exporter injects the reusable loading screen", {
  exporter <- paste(
    readLines(file.path(repo_root, "scripts", "export_shinylive.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(exporter, 'file.path("assets", "shinylive-loading-splash.html")', fixed = TRUE)
  expect_match(exporter, "paste(readLines(splash_path", fixed = TRUE)
  expect_match(exporter, 'paste0(splash, sw_reload, "\\n</body>")', fixed = TRUE)
  expect_match(exporter, 'SHINYLIVE_VERSION <- "0.5.0"', fixed = TRUE)
  expect_match(exporter, '"scripts/install_shinylive.R"', fixed = TRUE)

  installer <- paste(
    readLines(file.path(repo_root, "scripts", "install_shinylive.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(installer, "renv::restore(", fixed = TRUE)
  expect_match(installer, 'packages = c("shinylive", "S7")', fixed = TRUE)
  expect_match(installer, 'requireNamespace("httr2", quietly = TRUE)', fixed = TRUE)
  expect_false(grepl('install.packages(missing_dependencies)', installer, fixed = TRUE))
})

test_that("production deployment is upstream-only and never manages domains", {
  workflow <- paste(
    readLines(
      file.path(repo_root, ".github", "workflows", "deploy-shinylive.yml"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    workflow,
    "github.repository == 'samgillman/simplecaapp'",
    fixed = TRUE
  )
  expect_false(grepl("pages project create", workflow, fixed = TRUE))
  expect_false(grepl("/domains", workflow, fixed = TRUE))
  expect_false(grepl("Attach custom domain", workflow, fixed = TRUE))
  expect_false(grepl("simplecalcium.samgillman.org", workflow, fixed = TRUE))
  expect_match(workflow, "workflow_run:", fixed = TRUE)
  expect_match(workflow, "workflow_run.conclusion == 'success'", fixed = TRUE)
  expect_match(workflow, "Rscript scripts/install_shinylive.R", fixed = TRUE)
  expect_match(workflow, "actions/download-artifact@v4", fixed = TRUE)
  expect_match(workflow, "run-id: ${{ github.event.workflow_run.id }}", fixed = TRUE)
})

test_that("SHA checkouts deploy to the explicit Cloudflare production branch", {
  lines <- trimws(readLines(file.path(repo_root, ".github/workflows/deploy-shinylive.yml")))
  command <- sub("^command: ", "", lines[startsWith(lines, "command: ")])
  expect_length(command, 1L)
  args <- strsplit(command, "[[:space:]]+")[[1]]
  expect_equal(args[1:3], c("pages", "deploy", "_shinylive"))
  expect_identical(sub("^--branch=", "", args[startsWith(args, "--branch=")]), "main")
  # Selecting production must not replace the immutable checkout or the
  # successful CI run's tested artifact with a fresh main-branch build.
  expect_true("ref: ${{ github.event.workflow_run.head_sha || github.sha }}" %in% lines)
  expect_true("run-id: ${{ github.event.workflow_run.id }}" %in% lines)
})

test_that("CI restores dependencies, rejects skips, starts the app, and builds", {
  workflow <- paste(
    readLines(file.path(repo_root, ".github", "workflows", "ci.yml"), warn = FALSE),
    collapse = "\n"
  )
  runner <- paste(readLines(file.path(repo_root, "tests", "testthat.R"), warn = FALSE), collapse = "\n")

  expect_match(workflow, "r-lib/actions/setup-renv@v2", fixed = TRUE)
  expect_match(workflow, "Rscript tests/smoke-app.R", fixed = TRUE)
  expect_match(workflow, "Rscript tests/browser-smoke.R", fixed = TRUE)
  expect_match(workflow, "SIMPLECA_BROWSER_MODE: shinylive", fixed = TRUE)
  expect_match(workflow, "Rscript scripts/export_shinylive.R", fixed = TRUE)
  expect_match(runner, "Unexpected skipped tests", fixed = TRUE)
})


test_that("deployment admits only successful upstream main pushes or manual main runs", {
  lines <- readLines(file.path(repo_root, ".github/workflows/deploy-shinylive.yml"))
  start <- grep("^    if: >-", lines)
  end <- grep("^    runs-on:", lines)
  guard <- parse(text = paste(trimws(lines[seq.int(start + 1L, end - 1L)]), collapse = " "))
  allowed <- function(event = "workflow_run", source_event = "push",
                      repository = "samgillman/simplecaapp", head_repository = repository,
                      branch = "main", conclusion = "success", ref = "refs/heads/main") {
    eval(guard, envir = list(
      github.repository = repository, github.event_name = event, github.ref = ref,
      github.event.workflow_run.event = source_event,
      github.event.workflow_run.head_repository.full_name = head_repository,
      github.event.workflow_run.head_branch = branch,
      github.event.workflow_run.conclusion = conclusion
    ))
  }
  expect_true(allowed())
  expect_false(allowed(source_event = "pull_request"))
  expect_false(allowed(source_event = "workflow_dispatch"))
  expect_false(allowed(head_repository = "fork/simplecaapp"))
  expect_false(allowed(repository = "fork/simplecaapp"))
  expect_false(allowed(branch = "feature"))
  expect_false(allowed(conclusion = "failure"))
  expect_false(allowed(conclusion = "cancelled"))
  expect_true(allowed(event = "workflow_dispatch"))
  expect_false(allowed(event = "workflow_dispatch", ref = "refs/heads/feature"))
  expect_false(allowed(event = "workflow_dispatch", repository = "fork/simplecaapp"))
})

test_that("the restored build library includes S7 metadata required by WebAssembly ggplot2", {
  lock <- jsonlite::fromJSON(file.path(repo_root, "renv.lock"), simplifyVector = FALSE)
  expect_equal(lock$Packages$S7$Version, "0.2.2")
  expect_equal(lock$Packages$S7$Source, "Repository")
  expect_true(requireNamespace("S7", quietly = TRUE))
  expect_type(utils::packageDescription("S7")$Version, "character")
})

# Validate the exporter dependency graph from package DESCRIPTION constraints,
# including dependencies loaded lazily, without relying on the host library.
exporter_lock_problems <- function(lock) {
  packages <- lock$Packages
  base <- c("R", rownames(utils::installed.packages(priority = "base")))
  queue <- c("shinylive", "S7")
  visited <- problems <- character()
  while (length(queue)) {
    pkg <- queue[1]
    queue <- queue[-1]
    if (pkg %in% c(visited, base)) next
    visited <- c(visited, pkg)
    record <- packages[[pkg]]
    if (is.null(record)) {
      problems <- c(problems, paste("Missing locked package:", pkg))
      next
    }
    specs <- unlist(record[c("Depends", "Imports", "LinkingTo")], use.names = FALSE)
    for (spec in specs) {
      parts <- regmatches(spec, regexec(
        "^([^ (]+)(?: *\\((>=|<=|==|>|<) *([^ )]+)\\))?$", trimws(spec), perl = TRUE
      ))[[1]]
      if (!length(parts)) stop("Unrecognized dependency: ", spec)
      name <- parts[2]
      queue <- c(queue, name)
      version <- if (name == "R") lock$R$Version else packages[[name]]$Version
      if (length(parts) >= 4 && nzchar(parts[3]) && !is.null(version)) {
        comparison <- utils::compareVersion(version, parts[4])
        satisfied <- switch(parts[3], ">=" = comparison >= 0, "<=" = comparison <= 0,
          "==" = comparison == 0, ">" = comparison > 0, "<" = comparison < 0)
        if (!satisfied) problems <- c(problems, paste(pkg, "requires", spec, "but locks", version))
      }
    }
  }
  problems
}

test_that("exporter dependencies remain compatible with restored application pins", {
  lock <- jsonlite::fromJSON(file.path(repo_root, "renv.lock"), simplifyVector = FALSE)
  expect_equal(lock$Packages$shinylive$Version, "0.5.0")
  expect_equal(lock$Packages$httr2$Version, "1.1.2")
  expect_equal(lock$Packages$rlang$Version, "1.1.6")
  expect_equal(lock$Packages$curl$Version, "6.2.2")
  expect_length(exporter_lock_problems(lock), 0)

  # Reproduce the warmed cache's newer lazy dependency after app restoration.
  incompatible <- lock
  incompatible$Packages$httr2$Version <- "1.3.0"
  incompatible$Packages$httr2$Imports <- c("rlang (>= 1.3.0)", "curl (>= 8.0.0)")
  expect_equal(length(exporter_lock_problems(incompatible)), 2L)
  expect_match(paste(exporter_lock_problems(incompatible), collapse = "; "), "rlang.*1.1.6")
  lock$Packages$httr2 <- NULL
  expect_match(paste(exporter_lock_problems(lock), collapse = "; "), "Missing locked package: httr2")
})
