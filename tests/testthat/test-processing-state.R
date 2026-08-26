# Regression tests for transactional upload processing.

processing_settings <- list(
  baseline_method = "frame_range",
  baseline_frames = c(1, 20),
  sampling_rate = 10
)

make_raw_recording <- function(with_time = FALSE, cells = 1) {
  pulse <- make_pulse_trace()
  traces <- lapply(seq_len(cells), function(i) 100 + (10 * i * pulse))
  names(traces) <- paste0("Cell", seq_len(cells))
  dt <- data.table::as.data.table(traces)
  if (with_time) {
    dt[, Time := pulse_time()]
    data.table::setcolorder(dt, "Time")
  }
  dt
}

test_that("build_processed_state retains all traces when Time is missing", {
  files <- data.frame(
    name = "recording.csv",
    datapath = "recording",
    stringsAsFactors = FALSE
  )
  state <- build_processed_state(
    files,
    processing_settings,
    read_fun = function(path) make_raw_recording(cells = 2)
  )

  expect_equal(state$groups, "recording")
  expect_equal(names(state$dts$recording), c("Time", "Cell1", "Cell2"))
  expect_equal(state$dts$recording$Time, pulse_time())
  expect_equal(nrow(state$metrics), 2)
  expect_match(state$time_messages, "all uploaded columns were retained")
})

test_that("blank ImageJ frame header is not analyzed as an extra cell", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  raw_trace <- 100 * (1 + make_pulse_trace())
  writeLines(
    c(",Mean1", sprintf("%d,%.8f", seq_along(raw_trace), raw_trace)),
    path
  )
  files <- data.frame(
    name = "imagej-export.csv",
    datapath = path,
    stringsAsFactors = FALSE
  )

  state <- build_processed_state(files, processing_settings)

  expect_equal(names(state$dts[[1]]), c("Time", "Mean1"))
  expect_equal(state$dts[[1]]$Time, pulse_time())
  expect_equal(nrow(state$metrics), 1)
  expect_equal(state$metrics$Cell, "Mean1")
  expect_match(state$time_messages, "unnamed sequential first column")
})

test_that("build_processed_state keeps only accepted files and relabels them", {
  files <- data.frame(
    name = c("bad.csv", "good.csv"),
    datapath = c("bad", "good"),
    stringsAsFactors = FALSE
  )
  state <- build_processed_state(
    files,
    processing_settings,
    read_fun = function(path) {
      if (identical(path, "bad")) stop("unreadable fixture")
      make_raw_recording(with_time = TRUE)
    }
  )

  expect_equal(state$files$name, "good.csv")
  expect_equal(state$groups, "good")
  expect_named(state$dts, "good")
  expect_equal(unique(state$metrics$Group), "good")
  expect_equal(state$skipped_files, "bad.csv")
  expect_match(state$skipped_details$reason, "unreadable fixture")
})

test_that("build_processed_state uses only the selected frame-range mean", {
  files <- data.frame(
    name = "recording.csv",
    datapath = "recording",
    stringsAsFactors = FALSE
  )

  state <- build_processed_state(
    files,
    processing_settings,
    read_fun = function(path) make_raw_recording(with_time = TRUE)
  )

  expect_equal(state$baseline_method, "frame_range")
  expect_equal(unname(state$baselines$recording["Cell1"]), 100)
  expect_equal(nrow(state$metrics), 1)

  for (removed_method in c("rolling_min", "percentile")) {
    settings <- processing_settings
    settings$baseline_method <- removed_method
    expect_error(
      build_processed_state(
        files,
        settings,
        read_fun = function(path) make_raw_recording(with_time = TRUE)
      ),
      "Only frame-range baseline correction is supported",
      info = removed_method
    )
  }
})

test_that("per-file column mappings control Time and trace exclusions", {
  files <- data.frame(
    name = c("elapsed.csv", "frames.csv"),
    datapath = c("elapsed", "frames"),
    stringsAsFactors = FALSE
  )
  settings <- processing_settings
  settings$column_mappings <- list(
    list(
      time_mode = "time",
      time_column = "Seconds",
      excluded_columns = "Cell2"
    ),
    list(
      time_mode = "frame",
      time_column = "ImageNumber",
      excluded_columns = character()
    )
  )

  state <- build_processed_state(
    files,
    settings,
    read_fun = function(path) {
      traces <- make_raw_recording(cells = 2)
      if (identical(path, "elapsed")) {
        traces[, Seconds := pulse_time()]
        data.table::setcolorder(traces, "Seconds")
      } else {
        traces[, ImageNumber := seq_len(.N)]
        data.table::setcolorder(traces, "ImageNumber")
      }
      traces
    }
  )

  expect_equal(names(state$dts$elapsed), c("Time", "Cell1"))
  expect_equal(state$dts$elapsed$Time, pulse_time())
  expect_equal(names(state$dts$frames), c("Time", "Cell1", "Cell2"))
  expect_equal(state$dts$frames$Time, pulse_time())
  expect_equal(nrow(state$metrics), 3)
})

test_that("all-invalid batches fail without producing a partial state", {
  files <- data.frame(
    name = c("bad-a.csv", "bad-b.csv"),
    datapath = c("bad-a", "bad-b"),
    stringsAsFactors = FALSE
  )
  expect_error(
    build_processed_state(
      files,
      processing_settings,
      read_fun = function(path) stop("cannot read ", path)
    ),
    "No uploaded files could be processed"
  )
})

test_that("commit_processed_state validates a complete transaction", {
  files <- data.frame(
    name = "new.csv",
    datapath = "new",
    stringsAsFactors = FALSE
  )
  state <- build_processed_state(
    files,
    processing_settings,
    read_fun = function(path) make_raw_recording(with_time = TRUE)
  )
  rv <- new.env(parent = emptyenv())
  for (field in processed_state_fields) rv[[field]] <- paste0("old-", field)

  commit_processed_state(rv, state)
  expect_equal(rv$groups, "new")
  expect_equal(rv$files$name, "new.csv")
  expect_equal(nrow(rv$metrics), 1)

  previous <- lapply(processed_state_fields, function(field) rv[[field]])
  names(previous) <- processed_state_fields
  incomplete <- state
  incomplete$metrics <- NULL
  expect_error(commit_processed_state(rv, incomplete), "incomplete")
  for (field in processed_state_fields) {
    expect_identical(rv[[field]], previous[[field]], info = field)
  }
})

test_that("clear_processed_state removes a complete stale transaction", {
  rv <- new.env(parent = emptyenv())
  for (field in processed_state_fields) rv[[field]] <- paste0("old-", field)

  clear_processed_state(rv)

  expect_null(rv$files)
  expect_null(rv$groups)
  expect_null(rv$metrics)
  expect_null(rv$summary)
  expect_equal(rv$dts, list())
  expect_equal(rv$raw_traces, list())
  expect_equal(rv$baselines, list())
})

test_that("selecting a new file invalidates the previously processed dataset", {
  skip_if_not_installed("shiny")
  suppressPackageStartupMessages(library(shiny))

  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_load_data.R"), envir = module_env)
  # Keep the server test independent of shinydashboard, which is a UI-only
  # dependency and is not needed to exercise the transaction.
  module_env$theme_box <- function(...) shiny::div(...)
  module_env$primary_button <- function(inputId, label, ...) {
    shiny::actionButton(inputId, label)
  }

  valid_path <- tempfile(fileext = ".csv")
  invalid_path <- tempfile(fileext = ".csv")
  on.exit(unlink(c(valid_path, invalid_path)), add = TRUE)
  data.table::fwrite(make_raw_recording(), valid_path)
  writeLines(c("NotATrace", "bad", "data"), invalid_path)

  upload_row <- function(name, path) {
    data.frame(
      name = name,
      size = unname(file.info(path)$size),
      type = "text/csv",
      datapath = path,
      stringsAsFactors = FALSE
    )
  }

  rv <- shiny::reactiveValues(
    files = NULL, groups = NULL, dts = list(), long = NULL,
    summary = NULL, metrics = NULL, colors = NULL,
    raw_traces = list(), baselines = list(), baseline_method = NULL,
    baseline_frames = NULL
  )

  shiny::testServer(module_env$mod_load_data_server, args = list(rv = rv), {
    session$setInputs(
      upload_mode = "single",
      pp_baseline_frames = c(1, 20),
      pp_baseline_start = 1,
      pp_baseline_end = 20,
      pp_sampling_rate = 10
    )
    session$setInputs(data_files = upload_row("valid.csv", valid_path))
    session$flushReact()
    session$setInputs(load_btn = 1)
    session$flushReact()

    expect_equal(rv$files$name, "valid.csv")
    expect_equal(rv$groups, "valid")
    expect_equal(names(rv$dts$valid), c("Time", "Cell1"))
    expect_match(paste(as.character(output$results_bar), collapse = " "), "60 timepoints")
    session$setInputs(data_files = upload_row("invalid.csv", invalid_path))
    session$flushReact()

    expect_null(rv$files)
    expect_null(rv$groups)
    expect_null(rv$metrics)
    expect_null(rv$summary)
    expect_equal(rv$dts, list())
    expect_match(
      paste(as.character(output$process_status), collapse = " "),
      "Settings changed — click Process Data to update results",
      fixed = TRUE
    )

    session$setInputs(load_btn = 2)
    session$flushReact()

    expect_null(rv$metrics)
    expect_equal(rv$dts, list())
    expect_match(
      paste(as.character(output$process_status), collapse = " "),
      "no new results were committed",
      fixed = TRUE
    )
  })
})

test_that("typed baseline bounds are used for processing", {
  skip_if_not_installed("shiny")
  suppressPackageStartupMessages(library(shiny))

  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_load_data.R"), envir = module_env)
  module_env$theme_box <- function(...) shiny::div(...)
  module_env$primary_button <- function(inputId, label, ...) {
    shiny::actionButton(inputId, label)
  }

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  data.table::fwrite(make_raw_recording(), path)
  upload <- data.frame(
    name = "typed-window.csv",
    size = unname(file.info(path)$size),
    type = "text/csv",
    datapath = path,
    stringsAsFactors = FALSE
  )
  rv <- shiny::reactiveValues(
    files = NULL, groups = NULL, dts = list(), long = NULL,
    summary = NULL, metrics = NULL, colors = NULL,
    raw_traces = list(), baselines = list(), baseline_method = NULL,
    baseline_frames = NULL
  )

  shiny::testServer(module_env$mod_load_data_server, args = list(rv = rv), {
    session$setInputs(
      upload_mode = "single",
      pp_sampling_rate = 10
    )
    session$setInputs(data_files = upload)
    session$flushReact()
    session$setInputs(pp_baseline_start = 5, pp_baseline_end = 12)
    session$flushReact()
    session$setInputs(load_btn = 1)
    session$flushReact()

    expect_equal(rv$baseline_frames, c(5L, 12L))

    session$setInputs(pp_baseline_start = 6)
    session$flushReact()
    session$flushReact()

    expect_null(rv$metrics)
    expect_equal(rv$dts, list())
    expect_match(
      paste(as.character(output$process_status), collapse = " "),
      "Settings changed — click Process Data to update results",
      fixed = TRUE
    )
  })
})

test_that("load module applies the uploaded file's column controls", {
  skip_if_not_installed("shiny")
  suppressPackageStartupMessages(library(shiny))

  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_load_data.R"), envir = module_env)
  module_env$theme_box <- function(...) shiny::div(...)
  module_env$primary_button <- function(inputId, label, ...) {
    shiny::actionButton(inputId, label)
  }

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  dt <- make_raw_recording(cells = 2)
  dt[, Seconds := pulse_time()]
  data.table::setcolorder(dt, "Seconds")
  data.table::fwrite(dt, path)
  upload <- data.frame(
    name = "mapped.csv",
    size = unname(file.info(path)$size),
    type = "text/csv",
    datapath = path,
    stringsAsFactors = FALSE
  )
  key <- module_env$column_mapping_key(upload$name)
  rv <- shiny::reactiveValues(
    files = NULL, groups = NULL, dts = list(), long = NULL,
    summary = NULL, metrics = NULL, colors = NULL,
    raw_traces = list(), baselines = list(), baseline_method = NULL,
    baseline_frames = NULL
  )

  shiny::testServer(module_env$mod_load_data_server, args = list(rv = rv), {
    session$setInputs(
      upload_mode = "single",
      pp_sampling_rate = 10
    )
    session$setInputs(data_files = upload)
    session$flushReact()

    expect_match(
      paste(as.character(output$column_mapping_ui), collapse = " "),
      "No Time/Frame column detected",
      fixed = TRUE
    )

    mapping_inputs <- list(
      "time",
      "Seconds",
      "Cell2"
    )
    names(mapping_inputs) <- c(
      paste0("time_mode_", key),
      paste0("time_column_", key),
      paste0("exclude_columns_", key)
    )
    do.call(session$setInputs, mapping_inputs)
    session$flushReact()
    session$setInputs(load_btn = 1)
    session$flushReact()

    expect_equal(names(rv$dts$mapped), c("Time", "Cell1"))
    expect_equal(rv$dts$mapped$Time, pulse_time())
    expect_equal(nrow(rv$metrics), 1)

    changed_mapping <- list("time", "Seconds", "Cell1")
    names(changed_mapping) <- names(mapping_inputs)
    do.call(session$setInputs, changed_mapping)
    session$flushReact()

    expect_null(rv$metrics)
    expect_equal(rv$dts, list())
    expect_match(
      paste(as.character(output$process_status), collapse = " "),
      "Settings changed — click Process Data to update results",
      fixed = TRUE
    )
  })
})
