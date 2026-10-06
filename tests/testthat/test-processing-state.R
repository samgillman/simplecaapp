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

test_that("already-normalized input is preserved and zero baselines are retained", {
  files <- data.frame(
    name = "normalized.csv", datapath = "normalized",
    stringsAsFactors = FALSE
  )
  settings <- processing_settings
  settings$input_data_mode <- "dff0"
  original <- data.table::data.table(
    Time = pulse_time(),
    Cell1 = make_pulse_trace(baseline_vals = rep(c(-0.01, 0.01), 10))
  )

  state <- build_processed_state(files, settings, read_fun = function(path) original)

  expect_identical(state$input_data_mode, "dff0")
  expect_equal(state$dts$normalized$Cell1, original$Cell1)
  expect_true(is.na(state$baselines$normalized[["Cell1"]]))
  expect_length(state$dropped_cells, 0)
  expect_equal(state$metrics$Baseline_SD, stats::sd(original$Cell1[1:20]))
  expect_equal(state$metrics$Peak_dFF0, 1)
  expect_equal(state$summary$n_cells, rep(1L, nrow(original)))
  expect_true(all(is.na(state$summary$sem_dFF0)))
  expect_equal(
    state$processing_manifest$value[state$processing_manifest$field == "input_data_mode"],
    "dff0"
  )
})

test_that("raw fluorescence mode still rejects invalid F0 traces", {
  files <- data.frame(name = "raw.csv", datapath = "raw", stringsAsFactors = FALSE)
  expect_error(
    build_processed_state(
      files, processing_settings,
      read_fun = function(path) data.table::data.table(
        Time = pulse_time(), Cell1 = make_pulse_trace()
      )
    ),
    "zero, negative, or missing baseline"
  )
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

test_that("the canonical browser baseline is used for processing and invalidation", {
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
    session$setInputs(pp_baseline_start = 5, pp_baseline_end = 12,
      pp_baseline_frames = c(5, 12))
    session$flushReact()
    session$setInputs(load_btn = 1)
    session$flushReact()

    expect_equal(rv$baseline_frames, c(5L, 12L))

    session$setInputs(pp_baseline_start = 6, pp_baseline_frames = c(6, 12))
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
  key <- module_env$column_mapping_key(ensure_upload_ids(upload)$upload_id)
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

test_that("multi-file staging retains uploads with identical basenames", {
  skip_if_not_installed("shiny")
  suppressPackageStartupMessages(library(shiny))

  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_load_data.R"), envir = module_env)
  module_env$theme_box <- function(...) shiny::div(...)
  module_env$primary_button <- function(inputId, label, ...) shiny::actionButton(inputId, label)

  paths <- c(tempfile(fileext = ".csv"), tempfile(fileext = ".csv"))
  on.exit(unlink(paths), add = TRUE)
  data.table::fwrite(make_raw_recording(), paths[1])
  data.table::fwrite(make_raw_recording(), paths[2])
  upload_row <- function(path) data.frame(
    name = "Results.csv", size = unname(file.info(path)$size),
    type = "text/csv", datapath = path, stringsAsFactors = FALSE
  )
  rv <- shiny::reactiveValues(
    files = NULL, groups = NULL, dts = list(), long = NULL,
    summary = NULL, metrics = NULL, colors = NULL,
    raw_traces = list(), baselines = list(), input_data_mode = NULL,
    baseline_method = NULL, baseline_frames = NULL, sampling_rate = NULL,
    processing_manifest = NULL
  )

  shiny::testServer(module_env$mod_load_data_server, args = list(rv = rv), {
    session$setInputs(upload_mode = "multi", pp_sampling_rate = 10)
    session$setInputs(data_files = upload_row(paths[1]))
    session$flushReact()
    session$setInputs(data_files = upload_row(paths[2]))
    session$flushReact()

    feedback <- paste(as.character(output$upload_feedback), collapse = " ")
    expect_match(feedback, "2 files ready", fixed = TRUE)
    expect_match(feedback, "Results.csv (1)", fixed = TRUE)
    expect_match(feedback, "Results.csv (2)", fixed = TRUE)

    session$setInputs(load_btn = 1)
    session$flushReact()
    expect_equal(nrow(rv$files), 2)
    expect_equal(rv$groups, c("Results", "Results_1"))
  })
})


test_that("duplicate headers are rejected before any distinct trace is lost", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("Time,Cell,Cell", sprintf("%d,%d,%d", 0:11,
    c(100, 100, 200, 300, rep(100, 8)),
    c(rep(100, 6), 400, rep(100, 5)))), path)
  dt <- safe_read(path)
  expect_equal(names(dt), c("Time", "Cell", "Cell"))
  expect_false(identical(dt[[2]], dt[[3]]))
  expect_error(inspect_column_mapping(dt), "Duplicate column headers.*Cell")
  for (mode in c("auto", "time", "generated")) {
    expect_error(apply_column_mapping(dt, list(time_mode = mode, time_column = "Time")),
      "Duplicate column headers.*Cell", info = mode)
  }
  expect_error(build_processed_state(data.frame(name = "duplicate.csv", datapath = path),
    modifyList(processing_settings, list(baseline_frames = c(1, 2)))),
    "Duplicate column headers.*Cell")
})

test_that("processing requires usable post-baseline observations", {
  files <- data.frame(name = "recording.csv", datapath = "recording")
  for (mode in c("raw_fluorescence", "dff0")) {
    for (end in c(60, 80)) {
      expect_error(build_processed_state(files,
        modifyList(processing_settings, list(input_data_mode = mode, baseline_frames = c(1, end))),
        read_fun = function(path) make_raw_recording(with_time = TRUE)),
        "Baseline must end before the final frame")
    }
    state <- build_processed_state(files,
      modifyList(processing_settings, list(input_data_mode = mode, baseline_frames = c(1, 59))),
      read_fun = function(path) make_raw_recording(with_time = TRUE))
    expect_true(is.finite(state$metrics$Peak_dFF0))
    dt <- make_raw_recording(with_time = TRUE)
    dt$Cell1[21:60] <- NA_real_
    expect_error(build_processed_state(files,
      modifyList(processing_settings, list(input_data_mode = mode)), read_fun = function(path) dt),
      "no usable post-baseline observations")
  }
})

test_that("processing in the same flush as a baseline edit commits the edited window", {
  suppressPackageStartupMessages(library(shiny))
  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_load_data.R"), envir = module_env)
  module_env$theme_box <- function(...) shiny::div(...)
  module_env$primary_button <- function(inputId, label, ...) shiny::actionButton(inputId, label)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  data.table::fwrite(make_raw_recording(), path)
  upload <- data.frame(name = "recording.csv", size = file.info(path)$size,
    type = "text/csv", datapath = path)
  rv <- shiny::reactiveValues()
  shiny::testServer(module_env$mod_load_data_server, args = list(rv = rv), {
    session$setInputs(upload_mode = "single", pp_sampling_rate = 10,
      pp_baseline_start = 1, pp_baseline_end = 20, pp_baseline_frames = c(1, 20))
    session$setInputs(data_files = upload)
    session$setInputs(load_btn = 1)
    expect_equal(rv$baseline_frames, c(1L, 20L))
    # Submit the click before the edit to exercise observer queue ordering.
    session$setInputs(load_btn = 2, pp_baseline_end = 12,
      pp_baseline_frames = c(1, 12))
    session$flushReact()
    expect_equal(rv$baseline_frames, c(1L, 12L))
    expect_equal(nrow(rv$metrics), 1L)
    expect_identical(process_state(), "success")
    # Client echoes from synchronized controls must not invalidate this commit.
    session$setInputs(pp_baseline_frames = c(1, 12))
    expect_identical(process_state(), "success")
    session$setInputs(load_btn = 3, pp_baseline_frames = c(2, 10))
    session$flushReact()
    expect_equal(rv$baseline_frames, c(2L, 10L))
    expect_identical(process_state(), "success")
    # Delayed numeric notifications are not an independent source of truth.
    session$setInputs(pp_baseline_start = 1, pp_baseline_end = 60)
    expect_identical(process_state(), "success")
    expect_equal(rv$baseline_frames, c(2L, 10L))
    expect_equal(nrow(rv$metrics), 1L)
    # A genuine change to the canonical pair still clears committed results.
    session$setInputs(pp_baseline_frames = c(2, 11))
    expect_identical(process_state(), "stale")
    expect_null(rv$metrics)
    expect_null(rv$processing_manifest)
  })
})
