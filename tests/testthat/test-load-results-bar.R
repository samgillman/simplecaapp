test_that("the results bar waits for processed data and supports zero-cell results", {
  suppressPackageStartupMessages(library(shiny))

  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_load_data.R"), envir = module_env)
  module_env$theme_box <- function(...) shiny::div(...)
  module_env$primary_button <- function(inputId, label, ...) {
    shiny::actionButton(inputId, label)
  }
  rv <- reactiveValues(
    files = NULL, groups = NULL, dts = list(), long = NULL,
    summary = NULL, metrics = NULL, colors = NULL,
    raw_traces = list(), baselines = list(), baseline_method = NULL,
    baseline_frames = NULL
  )

  testServer(module_env$mod_load_data_server, args = list(rv = rv), {
    session$flushReact()
    # req() raises a silent validation condition, never a visible R error.
    expect_error(output$results_bar, class = "shiny.silent.error")

    rv$dts <- list(recording = data.table::data.table(Time = pulse_time()))
    session$flushReact()
    expect_error(output$results_bar, class = "shiny.silent.error")

    rv$metrics <- data.frame(Cell = character())
    session$flushReact()
    markup <- paste(as.character(output$results_bar), collapse = " ")
    expect_match(markup, "<b>0</b> cells with metrics", fixed = TRUE)
    expect_match(markup, "View Cell Eligibility", fixed = TRUE)

    rv$metrics <- data.frame(Cell = "Cell1")
    session$flushReact()
    markup <- paste(as.character(output$results_bar), collapse = " ")
    expect_match(markup, "<b>1</b> cells with metrics", fixed = TRUE)
    expect_match(markup, "View Results: Time Course", fixed = TRUE)

    clear_processed_state(rv)
    session$flushReact()
    expect_error(output$results_bar, class = "shiny.silent.error")
  })
})
