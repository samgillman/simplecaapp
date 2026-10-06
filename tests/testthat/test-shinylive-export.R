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
  expect_match(installer, "SHINYLIVE_DEPENDENCIES", fixed = TRUE)
  expect_match(installer, "install.packages(missing_dependencies)", fixed = TRUE)
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
