library(shiny)
library(htmltools)

source(file.path(repo_root, "R", "theme.R"))
source(file.path(repo_root, "R", "components.R"))

test_that("displayed app version matches the current release", {
  expect_identical(SIMPLECA_VERSION, "1.15.0")
})

test_that("theme includes responsive, accessible, and contrast-safe states", {
  css <- as.character(get_unified_theme_css())

  expect_match(css, "a\\.shiny-download-link\\.btn\\.btn-primary")
  expect_match(css, "@media \\(max-width: 767px\\)")
  expect_match(css, "prefers-reduced-motion")
  expect_match(css, "shiny-output-error-validation")
})

test_that("collapsed sidebar does not reserve an empty mini-sidebar rail", {
  css <- as.character(get_unified_theme_css())
  collapsed_layout <- paste(
    "body.sidebar-collapse .main-header .navbar,",
    "body.sidebar-collapse .content-wrapper,",
    "body.sidebar-collapse .main-footer {",
    "margin-left: 0 !important;",
    "}"
  )

  expect_match(
    gsub("\\s+", " ", css),
    gsub("\\s+", " ", collapsed_layout),
    fixed = TRUE
  )
  expect_false(grepl("margin-left: 50px !important;", css, fixed = TRUE))
})

test_that("load steps use three columns or one vertical sequence", {
  css <- gsub("\\s+", " ", as.character(get_unified_theme_css()))

  expect_match(
    css,
    ".load-steps { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr));",
    fixed = TRUE
  )
  expect_match(css, "@media (max-width: 1199px)", fixed = TRUE)
  expect_match(
    css,
    ".load-steps { grid-template-columns: 1fr; gap: 16px; }",
    fixed = TRUE
  )
})

test_that("advanced processing settings precede the Process Data action", {
  skip_if_not_installed("shinydashboard")
  suppressPackageStartupMessages(library(shinydashboard))

  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_load_data.R"), envir = module_env)
  markup <- as.character(module_env$mod_load_data_ui("load_data"))

  advanced_position <- regexpr("Advanced Options", markup, fixed = TRUE)[1]
  process_position <- regexpr("Process Data", markup, fixed = TRUE)[1]
  expect_gt(advanced_position, 0)
  expect_gt(process_position, 0)
  expect_lt(advanced_position, process_position)
  expect_match(markup, 'aria-expanded="false"', fixed = TRUE)
  expect_match(markup, "Used only when Time is missing", fixed = TRUE)
  expect_match(markup, "Baseline frames (F\u2080)", fixed = TRUE)
  expect_match(markup, "F\u2080 is the mean fluorescence across the selected frames", fixed = TRUE)
  expect_false(grepl("pp_baseline_method", markup, fixed = TRUE))
  expect_false(grepl("Rolling Minimum", markup, fixed = TRUE))
  expect_false(grepl("Percentile", markup, fixed = TRUE))
})

test_that("accordion markup exposes keyboard and screen-reader state", {
  ui <- accordion(
    id = "test-accordion",
    title = "Test section",
    content = div("Test content"),
    expanded = FALSE,
    icon = "sliders"
  )
  markup <- as.character(ui)

  expect_match(markup, 'role="button"', fixed = TRUE)
  expect_match(markup, 'tabindex="0"', fixed = TRUE)
  expect_match(markup, 'aria-expanded="false"', fixed = TRUE)
  expect_match(markup, 'aria-hidden="true"', fixed = TRUE)
  expect_match(markup, "event.key", fixed = TRUE)
  expect_match(markup, "Enter", fixed = TRUE)
})

test_that("file input repair is scoped to relevant DOM additions", {
  js <- as.character(get_accordion_js())

  expect_match(js, "hasNewFileInput", fixed = TRUE)
  expect_match(js, "fileInputFixQueued", fixed = TRUE)
  expect_false(grepl("shiny:value", js, fixed = TRUE))
})
