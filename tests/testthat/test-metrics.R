# Ground-truth tests for calculate_cell_metrics() and helpers.
# The synthetic pulse trace is defined in helper-source.R with hand-computed
# expected values; because it is piecewise linear, the app's interpolation and
# trapezoid integration should reproduce them exactly.

metrics_module_env <- new.env(parent = globalenv())
sys.source(file.path(repo_root, "R", "mod_metrics.R"), envir = metrics_module_env)

test_that("single-group violin plots stay compact", {
  expect_equal(metrics_module_env$metrics_violin_width("recording-1"), 0.35)
  expect_equal(
    metrics_module_env$metrics_violin_width(c("recording-1", "recording-1")),
    0.35
  )
  expect_equal(
    metrics_module_env$metrics_violin_width(c("control", "treated")),
    0.8
  )

  single_axis <- metrics_module_env$metrics_plotly_xaxis("recording-1", "violin")
  expect_equal(single_axis$range, c(-0.5, 2.5))
  expect_false(single_axis$autorange)

  expect_null(
    metrics_module_env$metrics_plotly_xaxis(c("control", "treated"), "violin")$range
  )
  expect_null(metrics_module_env$metrics_plotly_xaxis("recording-1", "boxswarm")$range)
})

test_that("pulse trace metrics match hand-computed ground truth (dFF0 input)", {
  x <- make_pulse_trace()
  t <- pulse_time()

  m <- calculate_cell_metrics(x, t, baseline_frames = c(1, 20), data_is_dFF0 = TRUE)

  expect_equal(m$Peak_dFF0, 1.0)
  expect_equal(m$Time_to_Peak, 2.9)
  expect_equal(m$Response_Amplitude, 1.0)
  expect_equal(m$Time_to_25_Peak, 2.15)
  expect_equal(m$Time_to_50_Peak, 2.4)
  expect_equal(m$Time_to_75_Peak, 2.65)
  expect_equal(m$Rise_Time, 0.8)
  expect_equal(m$Calcium_Entry_Rate, 1.0)
  expect_equal(m$FWHM, 1.0)
  expect_identical(m$FWHM_Censored, FALSE)
  expect_true(is.na(m$FWHM_Lower_Bound))
  expect_equal(m$Half_Width, 0.5)
  expect_equal(m$AUC, 1.0)
  # Flat zero baseline has zero SD, so SNR is undefined by design
  expect_equal(m$Baseline_SD, 0)
  expect_true(is.na(m$SNR))
})

test_that("SNR and Baseline_SD reflect baseline noise", {
  base <- rep(c(-0.01, 0.01), 10) # mean 0, known SD
  x <- make_pulse_trace(baseline_vals = base)
  t <- pulse_time()

  m <- calculate_cell_metrics(x, t, baseline_frames = c(1, 20), data_is_dFF0 = TRUE)

  expected_sd <- stats::sd(base)
  expect_equal(m$Baseline_SD, expected_sd)
  expect_equal(m$SNR, 1.0 / expected_sd)
  # Noise in the baseline must not disturb the response-phase metrics
  expect_equal(m$Peak_dFF0, 1.0)
  expect_equal(m$Rise_Time, 0.8)
  expect_equal(m$FWHM, 1.0)
})

test_that("missing baseline samples do not shift the frame window", {
  base <- rep(c(-0.01, 0.01), 10)
  x <- make_pulse_trace(baseline_vals = base)
  x[1] <- NA_real_

  m <- calculate_cell_metrics(
    x, pulse_time(), baseline_frames = c(1, 20), data_is_dFF0 = TRUE
  )

  expect_equal(m$Baseline_SD, stats::sd(x[1:20], na.rm = TRUE))
  expect_equal(m$Time_to_Peak, 2.9)
  expect_equal(m$Peak_dFF0, 1.0)
})

test_that("raw fluorescence input is normalized to the same result", {
  # Raw trace: F0 = 100, peak = 200 -> identical dFF0 pulse after (F - F0)/F0
  x_raw <- 100 * (1 + make_pulse_trace())
  t <- pulse_time()

  m <- calculate_cell_metrics(x_raw, t, baseline_frames = c(1, 20), data_is_dFF0 = FALSE)

  expect_equal(m$Peak_dFF0, 1.0)
  expect_equal(m$Time_to_Peak, 2.9)
  expect_equal(m$Rise_Time, 0.8)
  expect_equal(m$AUC, 1.0)
  expect_equal(m$FWHM, 1.0)
})

test_that("AUC is the net area: deflections below baseline subtract", {
  # Pulse up (area +1.0) followed by an identical pulse down (area -1.0)
  up <- make_pulse_trace()
  down <- -make_pulse_trace()[21:60]
  x <- c(up, down)
  t <- seq(0, by = 0.1, length.out = length(x))

  m <- calculate_cell_metrics(x, t, baseline_frames = c(1, 20), data_is_dFF0 = TRUE)

  expect_equal(m$AUC, 0)
})

test_that("AUC does not bridge an interval containing a missing sample", {
  x <- make_pulse_trace()
  x[25] <- NA_real_
  t <- pulse_time()

  m <- calculate_cell_metrics(x, t, baseline_frames = c(1, 20), data_is_dFF0 = TRUE)

  lower <- seq_len(length(x) - 1L)
  upper <- lower + 1L
  valid_pairs <- is.finite(x[lower]) & is.finite(x[upper]) &
    is.finite(t[lower]) & is.finite(t[upper])
  expected <- sum(
    (t[upper[valid_pairs]] - t[lower[valid_pairs]]) *
      (x[lower[valid_pairs]] + x[upper[valid_pairs]]) / 2
  )

  expect_equal(m$AUC, expected)
  expect_lt(m$AUC, 1.0)
})

test_that("sustained responses are reported as right-censored FWHM", {
  x <- c(rep(0, 20), seq(0.1, 1.0, by = 0.1), rep(1.0, 30))
  t <- pulse_time()

  m <- calculate_cell_metrics(x, t, baseline_frames = c(1, 20), data_is_dFF0 = TRUE)

  expect_true(is.na(m$FWHM))
  expect_identical(m$FWHM_Censored, TRUE)
  expect_equal(m$FWHM_Lower_Bound, 3.5)
  expect_true(is.na(m$Half_Width))
})

test_that("undefined response widths are not labelled as uncensored", {
  m <- calculate_cell_metrics(
    rep(0, 60), pulse_time(), baseline_frames = c(1, 20), data_is_dFF0 = TRUE
  )

  expect_true(is.na(m$FWHM))
  expect_true(is.na(m$FWHM_Censored))
  expect_true(is.na(m$FWHM_Lower_Bound))
})

test_that("a missing crossing is not censored when later observations fall below half-max", {
  x <- make_pulse_trace()
  # Remove both endpoints of the falling half-max pair. There is evidence below
  # half-max later, but no adjacent pair from which to estimate the crossing.
  x[35:36] <- NA_real_

  m <- calculate_cell_metrics(
    x, pulse_time(), baseline_frames = c(1, 20), data_is_dFF0 = TRUE
  )

  expect_true(is.na(m$FWHM))
  expect_true(is.na(m$FWHM_Censored))
  expect_true(is.na(m$FWHM_Lower_Bound))
})

test_that("baseline covering the whole trace yields an all-NA row", {
  x <- make_pulse_trace()
  t <- pulse_time()

  m <- calculate_cell_metrics(x, t, baseline_frames = c(1, length(x)), data_is_dFF0 = TRUE)

  expect_true(is.na(m$Peak_dFF0))
  expect_true(is.na(m$AUC))
  # Baseline SD is still reported (it was computable)
  expect_false(is.na(m$Baseline_SD))
})

test_that("traces too short to analyze return an all-NA row", {
  m <- calculate_cell_metrics(rep(0.5, 5), seq(0, 0.4, by = 0.1))
  expect_true(all(is.na(unlist(m))))
})

test_that("every return path yields the same columns in the same order", {
  ref <- names(empty_metrics_row())

  full <- calculate_cell_metrics(make_pulse_trace(), pulse_time(),
    baseline_frames = c(1, 20), data_is_dFF0 = TRUE
  )
  short <- calculate_cell_metrics(rep(0, 5), seq(0, 0.4, by = 0.1))
  all_baseline <- calculate_cell_metrics(make_pulse_trace(), pulse_time(),
    baseline_frames = c(1, 60), data_is_dFF0 = TRUE
  )
  all_na_input <- calculate_cell_metrics(rep(NA_real_, 30), pulse_time(30))

  expect_named(full, ref)
  expect_named(short, ref)
  expect_named(all_baseline, ref)
  expect_named(all_na_input, ref)
})

test_that("find_rising_crossing_time interpolates linearly", {
  sig <- c(0, 1)
  tm <- c(0, 1)
  expect_equal(find_rising_crossing_time(sig, tm, 0.5, 1, 2), 0.5)
  expect_equal(find_rising_crossing_time(sig, tm, 0.25, 1, 2), 0.25)
  # Threshold never reached in the window
  expect_true(is.na(find_rising_crossing_time(sig, tm, 2, 1, 2)))
})

test_that("threshold crossings never interpolate across missing frames", {
  sig <- c(0, NA_real_, 1, 0)
  tm <- 0:3

  expect_length(find_threshold_crossings(sig, tm, 0.5, "rising"), 0)
  expect_equal(find_threshold_crossings(sig, tm, 0.5, "falling"), 2.5)
  expect_equal(find_rising_crossing_time(sig, tm, 0.5, 1, 4), 2)
})

test_that("compute_metrics_for_dt labels cells and drops unusable ones", {
  dt <- data.table::data.table(
    Time = pulse_time(),
    CellA = make_pulse_trace(),
    CellFlat = rep(0, 60),
    CellNA = rep(NA_real_, 60)
  )

  res <- compute_metrics_for_dt(dt, "grp1", baseline_frames = c(1, 20))

  # CellNA produces an all-NA metrics row and is filtered out
  expect_setequal(res$Cell, c("CellA", "CellFlat"))
  expect_true(all(res$Group == "grp1"))
  expect_equal(res$Cell_ID, paste("grp1", res$Cell, sep = "_"))
  expect_equal(res$Peak_dFF0[res$Cell == "CellA"], 1.0)
})
