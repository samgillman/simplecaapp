test_that("declared fluorescence and ratio representations normalize equivalently", {
  fraction <- 0.2 * make_pulse_trace()
  frames <- c(1, 20)
  inputs <- list(raw_fluorescence = 100 * (1 + fraction),
    corrected_fluorescence = 100 * (1 + fraction),
    dff0 = fraction, dff0_percent = 100 * fraction,
    ratio = 2 * (1 + fraction), drr0 = fraction, drr0_percent = 100 * fraction)
  for (mode in names(inputs)) {
    x <- normalize_signal_trace(inputs[[mode]], frames, mode)
    expect_equal(x$trace, fraction, tolerance = 1e-12, info = mode)
    expect_identical(x$quality$Status, "Valid")
    expect_identical(x$quality$Normalized_units, "fraction")
    expect_identical(x$quality$Raw_QC_available,
      mode %in% c("raw_fluorescence", "corrected_fluorescence"))
  }
  expect_identical(signal_input_spec("ratio")$label, "ΔR/R₀")
  expect_error(signal_input_spec("zscore"), "Unsupported input units")
})

test_that("raw validity and warning flags are separate from response detection", {
  baseline <- rep(c(.001, .005), 10)
  x <- normalize_signal_trace(c(baseline, rep(.01, 40)), c(1, 20))
  expect_identical(x$quality$Status, "Valid")
  expect_match(x$quality$QC_flags, "reference_below_3_baseline_sd_review")
  expect_equal(x$f0, .003)
  expect_length(x$trace, 60)
  for (baseline in c(0, -1, 1e-7, Inf, NA_real_)) {
    invalid <- normalize_signal_trace(c(rep(baseline, 20), rep(1, 40)), c(1, 20))
    expect_null(invalid$trace)
    expect_identical(invalid$quality$Status, "Excluded")
    expect_match(invalid$quality$Reason, "normalization baseline")
  }
  normalized <- normalize_signal_trace(rep(0, 60), c(1, 20), "dff0")
  expect_identical(normalized$quality$Status, "Valid")
  expect_false(normalized$quality$Raw_QC_available)
  expect_true(is.na(normalized$quality$F0))
  expect_equal(normalized$trace, rep(0, 60))
})

test_that("elapsed Time and companion Frame do not add a phantom cell", {
  dt <- data.table::data.table(Frame = seq_len(60), Time_s = pulse_time(),
    CellA = 100 * (1 + make_pulse_trace()))
  expect_identical(inspect_column_mapping(dt)$trace_columns, "CellA")
  files <- data.frame(name = "clock.csv", datapath = "clock")
  state <- build_processed_state(files, list(baseline_frames = c(1, 20)),
    read_fun = function(path) dt)
  expect_identical(names(state$dts$clock), c("Time", "CellA"))
  expect_equal(state$dts$clock$Time, dt$Time_s)
  expect_identical(state$cell_quality$Cell, "CellA")
})

test_that("percent inputs are converted once and conversion is recorded", {
  files <- data.frame(name = "percent.csv", datapath = "percent")
  dt <- data.table::data.table(Time = pulse_time(), CellA = 100 * make_pulse_trace())
  state <- build_processed_state(files, list(input_data_mode = "dff0_percent",
    baseline_frames = c(1, 20)), read_fun = function(path) dt)
  expect_equal(state$dts$percent$CellA, make_pulse_trace())
  expect_equal(state$metrics$Peak_dFF0, 1)
  expect_equal(state$raw_traces$percent$CellA, dt$CellA)
  manifest <- stats::setNames(state$processing_manifest$value, state$processing_manifest$field)
  expect_identical(manifest[["input_percent_divisor"]], "100")
  expect_identical(manifest[["normalized_units"]], "fraction")
  expect_false(state$cell_quality$Raw_QC_available)
})

test_that("all-excluded recordings can retain quality and mapped cell identities", {
  files <- data.frame(name = "excluded.csv", datapath = "excluded")
  state <- build_processed_state(files, list(baseline_frames = c(1, 20),
    retain_unscorable_recordings = TRUE), read_fun = function(path) {
      data.table::data.table(Time = pulse_time(), Zero = rep(0, 60), Negative = rep(-1, 60))
    })
  expect_named(state$dts, "excluded")
  expect_identical(names(state$dts$excluded), "Time")
  expect_named(state$baselines$excluded, c("Zero", "Negative"))
  expect_equal(nrow(state$cell_quality), 2)
  expect_true(all(state$cell_quality$Status == "Excluded"))
  expect_equal(nrow(state$metrics), 0)
  expect_equal(nrow(state$summary), 0)
  expect_true(all(c("Group", "Cell", "Peak_dFF0") %in% names(state$metrics)))
})

test_that("optional peak outlier flags keep valid large responses", {
  files <- data.frame(name = "peaks.csv", datapath = "peaks")
  dt <- data.table::data.table(Time = pulse_time())
  for (i in seq_len(5)) dt[[paste0("Cell", i)]] <- 100 *
    (1 + (if (i == 5) 20 else .1) * make_pulse_trace())
  state <- build_processed_state(files, list(baseline_frames = c(1, 20),
    flag_peak_outliers = TRUE), read_fun = function(path) dt)
  expect_equal(nrow(state$metrics), 5)
  expect_true(all(state$cell_quality$Status == "Valid"))
  expect_match(state$cell_quality$QC_flags[state$cell_quality$Cell == "Cell5"],
    "extreme_normalized_peak_review")
  expect_equal(state$metrics$Peak_dFF0[state$metrics$Cell == "Cell5"], 20)
})

test_that("ratio labels preserve UI identities and replace only scientific text", {
  tag <- htmltools::tags$div(id = "dff0_control", htmltools::tags$span("Peak ΔF/F₀"))
  rendered <- as.character(signal_display_tags(tag, "ratio"))
  expect_match(rendered, 'id="dff0_control"', fixed = TRUE)
  expect_match(rendered, "Peak ΔR/R₀", fixed = TRUE)
})
