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
