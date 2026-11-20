source(file.path(repo_root, "R", "response_protocols.R"))
source(file.path(repo_root, "R", "responders.R"))

protocol_test_v2 <- function(..., start = 5, end = 8, baseline_start = 0, baseline_end = 5, onset = 5) {
  responder_protocol_v2("Provisional evoked-response preset", data.frame(Role = "Stimulus", Stimulus = "Capsaicin",
    Baseline_start_s = baseline_start, Baseline_end_s = baseline_end, Stimulus_onset_s = onset,
    Start_s = start, End_s = end), ...)
}

test_that("versioned protocols separate timing, units, persistence and validation claims", {
  p <- protocol_test_v2(start = 6)
  expect_equal(p$Baseline_end_s, 5)
  expect_equal(p$Stimulus_onset_s, 5)
  expect_equal(p$Start_s, 6)
  expect_equal(p$Duration_mode, "seconds")
  expect_equal(p$Min_duration_s, 1)
  expect_equal(p$Floor_local_fraction, .1)
  expect_equal(p$SD_multiplier, 3)
  expect_equal(p$Validation_status, "provisional")
  expect_equal(p$Threshold_operator, ">=")
  expect_equal(p$SD_estimator, "sample_sd")
  expect_equal(p$Signal_reference, "fixed_reference_fractional_change")
  expect_error(protocol_test_v2(baseline_end = 6), "baseline must end")
  expect_error(protocol_test_v2(start = 4), "response window must start")
  expect_error(protocol_test_v2(floor = 0, noise = 0), "zero threshold")
  expect_error(protocol_test_v2(duration_s = 0), "Duration must be positive")
  expect_error(protocol_test_v2(duration_mode = "frames_per_second"), "seconds or samples")
  p$Signal_reference <- "moving_baseline"
  expect_error(responder_validate_protocol(p), "Signal_reference")
})

test_that("portable JSON roundtrips both contracts and rejects future schemas", {
  v2 <- protocol_test_v2()
  legacy <- responder_protocol("A", 5, 8, 3, .1, 3, 2, NULL)
  for (p in list(v2, legacy)) {
    text <- responder_export_protocol(p)
    imported <- responder_import_protocol(text)
    expect_equal(imported[, names(p)], p, ignore_attr = TRUE)
    expect_identical(responder_protocol_hash(imported), responder_protocol_hash(p))
    expect_identical(responder_protocol_hash(p[, rev(names(p))]), responder_protocol_hash(p))
  }
  future <- sub('"schema_version": 2', '"schema_version": 999', responder_export_protocol(v2), fixed = TRUE)
  expect_error(responder_import_protocol(future), "Unsupported protocol schema")
  expect_error(responder_import_protocol('{"schema":"unknown","schema_version":2}'), "Unsupported")
  expect_error(responder_import_protocol("{broken}"), "Invalid protocol JSON")
  payload <- jsonlite::fromJSON(responder_export_protocol(v2))
  payload$engine_version <- "99.0.0"
  expect_error(responder_import_protocol(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows", na = "null", null = "null"))),
               "Unsupported responder engine")
  payload$engine_version <- responder_engine_version()
  payload$name <- "Different name from window metadata"
  expect_error(responder_import_protocol(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows", na = "null", null = "null"))),
               "metadata and window settings disagree")
  changed <- v2
  changed$Min_duration_s <- 2
  expect_false(identical(responder_protocol_hash(changed), responder_protocol_hash(v2)))
})

test_that("per-field timing overrides preserve the shared protocol and validate each role", {
  p <- protocol_test_v2()
  original <- p
  overrides <- data.frame(Field = "Later field", Role = "Stimulus", Baseline_start_s = 10,
    Baseline_end_s = 15, Stimulus_onset_s = 15, Start_s = 16, End_s = 18)
  imported <- responder_import_protocol(responder_export_protocol(p, overrides = overrides))
  applied <- responder_protocol_for_field(imported, "Later field")
  expect_equal(applied$Baseline_start_s, 10)
  expect_equal(applied$Baseline_end_s, 15)
  expect_equal(applied$Stimulus_onset_s, 15)
  expect_equal(applied$Start_s, 16)
  expect_equal(p, original)
  expect_equal(responder_protocol_for_field(imported, "Other field")[, names(p)], p, ignore_attr = TRUE)
  expect_error(responder_export_protocol(p, overrides = rbind(overrides, overrides)), "one timing override")
  overrides$Start_s <- 14
  expect_error(responder_export_protocol(p, overrides = overrides), "response window must start")
})

test_that("legacy two samples and timestamp duration remain deliberately different", {
  t <- seq(0, 8, .5)
  dt <- data.frame(Time = t, Brief = .2 * (t %in% c(5, 5.5)), Sustained = .2 * (t %in% c(5, 5.5, 6)))
  legacy <- responder_protocol("A", 5, 8, 5, .1, 3, 2, NULL)
  before <- responder_field_results(dt, c("Brief", "Sustained"), "F", legacy)
  imported <- responder_import_protocol(responder_export_protocol(legacy))
  expect_equal(responder_field_results(dt, c("Brief", "Sustained"), "F", imported), before)
  expect_equal(before$calls$Status, c("Responded", "Responded"))
  now <- responder_field_results(dt, c("Brief", "Sustained"), "F", protocol_test_v2())$calls
  expect_equal(now$Status, c("Did not respond", "Responded"))
  expect_equal(now$Onset_s[2], 5)
  expect_equal(now$Confirmation_s[2], 6)
  expect_equal(now$Response_latency_s[2], 0)
  expect_equal(now$Run_duration_s[2], 1)
  expect_equal(now$Run_samples[2], 3)
  expect_equal(now$Stimulus_onset_s, c(5, 5))
})

test_that("seconds use timestamp spans at different rates and expose coarse sampling", {
  for (rate in c(2, 5, 10)) {
    t <- seq(0, 8, 1/rate)
    dt <- data.frame(Time = t, Cell = .2 * (t >= 5 & t <= 6))
    call <- responder_field_results(dt, "Cell", "F", protocol_test_v2())$calls
    expect_equal(call$Status, "Responded")
    expect_equal(call$Run_samples, rate + 1)
    expect_equal(call$Nominal_min_samples, rate + 1)
    expect_equal(call$Confirmation_s, 6)
  }
  p <- protocol_test_v2(start = 6, end = 10, baseline_end = 6, onset = 6, expected_interval_s = 2)
  dt <- data.frame(Time = seq(0, 10, 2), Cell = c(0, 0, 0, .2, .2, 0))
  call <- responder_field_results(dt, "Cell", "F", p)$calls
  expect_equal(call$Status, "Responded")
  expect_equal(call$Confirmation_s, 8)
  expect_match(call$QC_flags, "DURATION_BELOW_SAMPLE_INTERVAL")
})

test_that("missing samples, gaps and partial windows cannot select only positive cells", {
  t <- seq(0, 8, .5)
  dt <- data.frame(Time = t, Positive = .2 * (t >= 5), Flat = 0)
  short <- responder_field_results(dt[t < 8, ], c("Positive", "Flat"), "F", protocol_test_v2())
  expect_equal(short$calls$Status, rep("Unscorable", 2))
  expect_equal(short$counts$Evaluable, 0)
  expect_true(is.na(short$counts$Percent_responding))
  expect_false(short$counts$Window_clipped)
  gap <- responder_field_results(dt[t != 5.5, ], c("Positive", "Flat"), "F", protocol_test_v2())
  expect_equal(gap$calls$Status, rep("Unscorable", 2))
  expect_match(gap$counts$Field_reason, "gap exceeds")
  dt$Positive[t == 5.5] <- NA_real_
  calls <- responder_field_results(dt, c("Positive", "Flat"), "F", protocol_test_v2())$calls
  expect_equal(calls$Status, c("Unscorable", "Did not respond"))
  expect_match(calls$QC_flags[1], "MISSING_WINDOW_VALUES")
  run <- responder_qualifying_run(c(0, .5, 2), rep(.2, 3), .1, "seconds", 1, 2, .75)
  expect_true(is.na(run$start))
  run <- responder_qualifying_run(c(0, .5, 1), c(.2, NA, .2), .1, "seconds", 1, 2, .75)
  expect_true(is.na(run$start))
})

test_that("both amplitude and noise criteria operate in local fractional units", {
  t <- seq(0, 8, .5)
  dt <- data.frame(Time = t, Low = 0, Noisy = 0, Shifted = 1)
  dt$Noisy[t < 5] <- rep(c(-.05, .05), 5)
  dt$Low[t >= 5] <- .12
  dt$Noisy[t >= 5] <- .12
  dt$Shifted[t >= 5] <- 1.24
  calls <- responder_field_results(dt, c("Low", "Noisy", "Shifted"), "F", protocol_test_v2())$calls
  expect_equal(calls$Status, c("Responded", "Did not respond", "Responded"))
  expect_equal(calls$Peak_increase, rep(.12, 3))
  expect_equal(calls$Threshold_increase[2], 3 * sd(rep(c(-.05, .05), 5)))
  expect_equal(calls$Baseline_mean[3], 1)
  expect_equal(calls$Local_F0_relative[3], 2)
  expect_equal(calls$Threshold_source, c("Floor", "Baseline noise", "Floor"))
})

test_that("fluorescence, ratio and percent inputs retain equivalent measurements and honest units", {
  time <- seq(0, 8, .5)
  rise <- .2 * (time >= 5)
  inputs <- list(raw_fluorescence = 100 * (1 + rise), dff0 = rise, dff0_percent = 100 * rise,
                 ratio = 2 * (1 + rise), drr0 = rise, drr0_percent = 100 * rise)
  for (mode in names(inputs)) {
    normalized <- normalize_signal_trace(inputs[[mode]], c(1, 10), mode)
    quality <- normalized$quality
    quality$Field <- "F"
    quality$Cell <- "Cell"
    calls <- responder_field_results(data.frame(Time = time, Cell = normalized$trace), "Cell", "F",
                                      protocol_test_v2(), quality)$calls
    expect_equal(calls$Status, "Responded")
    expect_equal(calls$Window_peak_local_fraction, .2, tolerance = 1e-12)
    expect_equal(calls$Window_AUC_fraction_s, .6, tolerance = 1e-12)
    expect_equal(calls$Normalized_units, "fraction")
    expect_equal(calls$Signal_kind, if (mode %in% c("ratio", "drr0", "drr0_percent")) "ratio" else "fluorescence")
    expect_equal(calls$Input_mode, mode)
    exported <- calls[, setdiff(names(calls), c("Window_peak_local_dFF", "Window_AUC_local_dFF_s")), drop = FALSE]
    file <- tempfile(fileext = ".csv")
    write.csv(exported, file, row.names = FALSE)
    restored <- read.csv(file, check.names = FALSE)
    unlink(file)
    expect_equal(restored$Signal_kind, calls$Signal_kind)
    expect_equal(restored$Window_peak_local_fraction, .2, tolerance = 1e-12)
    expect_equal(restored$Window_AUC_fraction_s, .6, tolerance = 1e-12)
    expect_false(any(grepl("dFF", names(restored), fixed = TRUE)))
    if (mode == "ratio") {
      expect_equal(calls$Input_reference_value, 2)
      expect_equal(calls$Local_reference_value, 2)
      expect_false(calls$Raw_QC_available)
      expect_true(is.na(calls$Raw_F0))
    }
  }
})

test_that("zero noise never turns flat traces into noise-only responders", {
  t <- seq(0, 8, .5)
  dt <- data.frame(Time = t, Flat = 0, Rise = .2 * (t >= 5))
  p <- protocol_test_v2(floor = 0)
  r <- responder_field_results(dt, c("Flat", "Rise"), "F", p)$calls
  expect_equal(r$Status, rep("Unscorable", 2))
  expect_true(all(r$Measurements_available))
  expect_true(all(grepl("ZERO_EFFECTIVE_THRESHOLD", r$QC_flags)))
  p$Noise_floor_local_fraction <- .01
  r <- responder_field_results(dt, c("Flat", "Rise"), "F", p)$calls
  expect_equal(r$Status, c("Did not respond", "Responded"))
  expect_equal(r$Threshold_increase, rep(.03, 2))
})

test_that("baseline variability flags do not silently exclude cells", {
  t <- seq(0, 8, .5)
  dt <- data.frame(Time = t, Variable = 0, Small = -1 + 1e-10, Invalid = -1)
  dt$Variable[t < 5] <- rep(c(-.4, .4), 5)
  dt$Variable[t >= 5] <- 2
  dt$Small[t >= 5] <- -1 + 1.2e-10
  calls <- responder_field_results(dt, c("Variable", "Small", "Invalid"), "F", protocol_test_v2())$calls
  expect_equal(calls$Status, c("Responded", "Responded", "Unscorable"))
  expect_match(calls$QC_flags[1], "BASELINE_SD_AT_LEAST_ONE_THIRD_LOCAL_F0")
  expect_match(calls$QC_flags[2], "VERY_SMALL_LOCAL_F0")
  expect_match(calls$QC_flags[3], "INVALID_LOCAL_BASELINE")
})

test_that("continuous measurements remain available for valid nonresponders", {
  t <- 0:8
  dt <- data.frame(Time = t, Triangle = .2 * (t == 6), Suppression = -.2 * (t >= 5))
  p <- protocol_test_v2()
  first <- responder_field_results(dt, c("Triangle", "Suppression"), "F", p)$calls
  expect_equal(first$Status, rep("Did not respond", 2))
  expect_equal(first$Window_peak_local_dFF, c(.2, -.2))
  expect_equal(first$Window_AUC_local_dFF_s, c(.2, -.6))
  expect_equal(first$Window_peak_local_fraction, first$Window_peak_local_dFF)
  expect_equal(first$Window_AUC_fraction_s, first$Window_AUC_local_dFF_s)
  expect_equal(first$Peak_latency_s, c(1, 0))
  p$Floor_local_fraction <- .5
  second <- responder_field_results(dt, c("Triangle", "Suppression"), "F", p)$calls
  columns <- c("Window_peak_local_dFF", "Window_AUC_local_dFF_s", "Peak_latency_s", "Baseline_mean", "Baseline_SD")
  expect_equal(first[, columns], second[, columns])
  expect_true(all(is.na(first$Response_latency_s)))
})

test_that("window metrics declare their actual sampled integration interval", {
  t <- seq(0, 10, .5)
  dt <- data.frame(Time = t, Cell = .2 * (t >= 5))
  p <- protocol_test_v2(start = 5.2, end = 7.8)
  call <- responder_field_results(dt, "Cell", "F", p)$calls
  expect_equal(call$Window_AUC_start_s, 5.5)
  expect_equal(call$Window_AUC_end_s, 7.5)
  expect_equal(call$Window_AUC_local_dFF_s, .4)
  expect_equal(call$Window_AUC_covered_s, 2)
  expect_equal(call$Window_AUC_coverage_fraction, 2/2.6)
  expect_match(call$AUC_method, "no_extrapolation")
})

test_that("optional legacy annotations preserve original columns and clipped classification", {
  time <- seq(0, 7.5, .5)
  dt <- data.frame(Time = time, Cell = .2 * (time >= 5), Flat = 0)
  protocol <- responder_protocol("Capsaicin", 5, 8, 5, .1, 3, 2, NULL)
  original <- responder_field_results(dt, c("Cell", "Flat"), "F", protocol)
  enriched <- responder_legacy_annotations(dt, c("Cell", "Flat"), "F", protocol, legacy_result = original)
  expect_identical(enriched[, names(original$calls)], original$calls)
  expect_true(all(enriched$Legacy_classification_unchanged))
  expect_equal(enriched$Window_AUC_fraction_s, c(.5, 0))
  expect_equal(enriched$Window_AUC_end_s, c(7.5, 7.5))
  expect_equal(enriched$Confirmation_s, c(5.5, NA))
  expect_equal(enriched$Run_duration_s, c(2.5, NA))
  expect_true(all(enriched$Window_clipped))
  expect_equal(responder_field_results(dt, c("Cell", "Flat"), "F", protocol), original)
})

test_that("delayed scoring does not move baseline into the evoked response", {
  t <- seq(0, 10, .5)
  dt <- data.frame(Time = t, Cell = .2 * (t >= 5))
  p <- protocol_test_v2(start = 7, end = 10)
  call <- responder_field_results(dt, "Cell", "F", p)$calls
  expect_equal(call$Baseline_mean, 0)
  expect_equal(call$Baseline_start_s, 0)
  expect_equal(call$Baseline_end_s, 5)
  expect_equal(call$Peak_increase, .2)
  expect_equal(call$Response_latency_s, 2)
})

test_that("quality reasons distinguish explicit exclusions from unavailable traces", {
  t <- seq(0, 8, .5)
  dt <- data.frame(Time = t, Valid = .2 * (t >= 5))
  quality <- data.frame(Field = "F", Cell = c("Valid", "Invalid"), F0 = c(100, 0),
    Baseline_SD_raw = c(1, 0), Status = c("Valid", "Excluded"),
    Reason = c("", "Invalid raw fluorescence baseline"), QC_flags = c("LOW_F0_WARNING", "INVALID_F0"),
    Raw_QC_available = TRUE)
  r <- responder_field_results(dt, c("Valid", "Invalid", "Absent"), "F", protocol_test_v2(), quality)
  expect_equal(r$calls$Status, c("Responded", "Excluded", "Unscorable"))
  expect_equal(r$counts$Evaluable, 1)
  expect_equal(r$counts$Percent_responding, 100)
  expect_equal(r$calls$Raw_F0[1:2], c(100, 0))
  expect_match(r$calls$QC_flags[1], "LOW_F0_WARNING")
})

test_that("golden fixtures preserve generic-control denominators and measurements", {
  path <- file.path(repo_root, "tests", "fixtures", "response-protocols")
  protocol <- responder_import_protocol(file.path(path, "golden-protocol.json"))
  traces <- read.csv(file.path(path, "golden-traces.csv"), check.names = FALSE)
  expected_calls <- read.csv(file.path(path, "golden-expected-calls.csv"), check.names = FALSE)
  expected_counts <- read.csv(file.path(path, "golden-expected-counts.csv"), check.names = FALSE)
  quality <- data.frame(Field = "Golden field", Cell = "InvalidF0", Status = "Excluded",
                        Reason = "Invalid raw fluorescence baseline", F0 = 0, Raw_QC_available = TRUE)
  result <- responder_field_results(traces, c(names(traces)[-1L], "InvalidF0"), "Golden field", protocol, quality)
  expect_equal(result$calls[, names(expected_calls)], expected_calls, ignore_attr = TRUE, tolerance = 1e-12)
  expect_equal(result$counts[, names(expected_counts)], expected_counts, ignore_attr = TRUE, tolerance = 1e-12)
  expect_equal(result$counts$Control_name, rep("Ionomycin", 3))
  expect_true(all(is.na(result$counts$KCl_effective_end_s)))
})

test_that("legacy annotations preserve conditional denominators and independent control timing", {
  fixture_dir <- file.path(repo_root, "tests", "fixtures", "response-protocols")
  dt <- read.csv(file.path(fixture_dir, "golden-traces.csv"), check.names = FALSE)
  dt <- dt[dt$Time < 15, , drop = FALSE]
  cells <- c(names(dt)[-1L], "InvalidF0")
  protocol <- responder_protocol("Capsaicin", 5, 8, 3, .1, 3, 2, NULL, TRUE, 12, 15)
  original <- responder_field_results(dt, cells, "Golden field", protocol)
  old_counts <- original$counts
  enriched <- responder_legacy_annotations(dt, cells, "Golden field", protocol, legacy_result = original)
  expect_identical(enriched[, names(original$calls)], original$calls)
  expect_identical(original$counts, old_counts)
  expect_equal(old_counts$Percent_responding, c(50, 80, 200/3))
  expect_equal(unique(enriched$Window_AUC_end_s[enriched$Role == "Control" & enriched$Measurements_available]), 14.5)
  expect_equal(enriched$Status[enriched$Cell == "Brief" & enriched$Role == "Stimulus"], "Responded")
  v2 <- responder_import_protocol(file.path(fixture_dir, "golden-protocol.json"))
  overrides <- data.frame(Field = "Later control", Role = "Control", Baseline_start_s = 20,
    Baseline_end_s = 23, Stimulus_onset_s = 23, Start_s = 24, End_s = 27)
  applied <- responder_protocol_for_field(v2, "Later control", overrides)
  expect_equal(applied[applied$Role == "Stimulus", ], v2[v2$Role == "Stimulus", ])
  expect_equal(applied$Stimulus_onset_s[2], 23)
  expect_equal(applied$Start_s[2], 24)
  expect_equal(applied$Baseline_end_s[2], 23)
})

test_that("replicate summaries distinguish biological and cell-weighted estimates", {
  counts <- data.frame(Field = c("A", "B", "Unassigned"), Stimulus = "Capsaicin", Population = "Cells in field",
    Registered = c(2, 10, 2), Responded = c(1, 9, 1), Nonresponders = c(1, 1, 1),
    Evaluable = c(2, 10, 2), Excluded = 0, Unscorable = 0, Percent_responding = c(50, 90, 50))
  meta <- data.frame(Field = c("A", "B"), Replicate = c("Culture 1", "Culture 2"), Group = "Treatment")
  result <- responder_replicate_summary(counts, meta)
  expect_equal(result$groups$Replicates_evaluable, 2)
  expect_equal(result$groups$Mean_replicate_percent, 70)
  expect_equal(result$groups$SD_replicate_percent, sd(c(50, 90)))
  expect_equal(result$groups$Cell_pooled_percent, 100 * 10/12)
  expect_equal(result$unassigned_fields, "Unassigned")
  meta$Replicate <- "Same culture"
  expect_equal(responder_replicate_summary(counts, meta)$groups$Replicates_evaluable, 1)
})

test_that("sensitivity and negative-control diagnostics preserve the applied result", {
  t <- seq(0, 8, .5)
  dt <- data.frame(Time = t, Cell = .2 * (t >= 5), Flat = 0)
  p <- protocol_test_v2()
  original <- responder_field_results(dt, c("Cell", "Flat"), "F", p)
  grid <- data.frame(Floor_local_fraction = c(.1, .2, .3))
  sensitivity <- responder_sensitivity_results(dt, c("Cell", "Flat"), "F", p, grid)
  expect_equal(sensitivity$counts$Responded, c(1, 1, 0))
  expect_equal(sensitivity$counts$Evaluable, rep(2, 3))
  expect_true(all(sensitivity$counts$Analysis_kind == "sensitivity"))
  expect_equal(responder_field_results(dt, c("Cell", "Flat"), "F", p), original)
  windows <- data.frame(Baseline_start_s = 0, Baseline_end_s = 2, Stimulus_onset_s = 2,
                        Start_s = 2, End_s = 4, Diagnostic_window = "Prestimulus pseudo-event")
  control <- responder_control_diagnostics(dt, c("Cell", "Flat"), "F", p, windows)
  expect_equal(control$counts$Apparent_response_percent, 0)
  expect_equal(control$counts$Analysis_kind, "negative_control_diagnostic")
})

test_that("empty registrations and invalid clocks are explicit rather than fabricated zero percentages", {
  p <- protocol_test_v2()
  none <- responder_field_results(data.frame(Time = 0:8), character(), "F", p)
  expect_equal(nrow(none$calls), 0)
  expect_equal(none$counts$Registered, 0)
  expect_true(is.na(none$counts$Percent_responding))
  invalid <- responder_field_results(data.frame(Time = c(0, 1, 1, 3), Cell = 0), "Cell", "F", p)
  expect_equal(invalid$calls$Status, "Unscorable")
  expect_match(invalid$calls$Reason, "strictly increasing")
})
