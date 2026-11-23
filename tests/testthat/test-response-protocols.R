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
  payload$engine_version <- responder_protocol_engine_version(v2)
  payload$name <- "Different name from window metadata"
  expect_error(responder_import_protocol(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows", na = "null", null = "null"))),
               "metadata and window settings disagree")
  changed <- v2
  changed$Min_duration_s <- 2
  expect_false(identical(responder_protocol_hash(changed), responder_protocol_hash(v2)))
})

test_that("unsupported direct API versions cannot fall through to legacy scoring", {
  protocol <- protocol_test_v2()
  protocol$Protocol_version <- 99L
  expect_error(responder_field_results(data.frame(Time = 0:8, Cell = 0), "Cell", "F", protocol),
               "Unsupported response protocol version")
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

larger_noise <- "larger_of_baseline_sd_and_trace_noise"

test_that("the noise estimator is a named, versioned protocol setting that defaults to the baseline SD", {
  p <- protocol_test_v2()
  expect_equal(responder_engine_version(), "2.1.0")
  expect_equal(p$SD_estimator, "sample_sd")
  larger <- protocol_test_v2(sd_estimator = larger_noise)
  expect_equal(larger$SD_estimator, larger_noise)
  # A protocol records the earliest engine release that defines its rule.
  expect_equal(p$Engine_version, "2.0.0")
  expect_equal(larger$Engine_version, "2.1.0")
  expect_equal(jsonlite::fromJSON(responder_export_protocol(larger))$engine_version, "2.1.0")
  expect_false(identical(responder_protocol_hash(larger), responder_protocol_hash(p)))
  imported <- responder_import_protocol(responder_export_protocol(larger))
  expect_equal(imported[, names(larger)], larger, ignore_attr = TRUE)
  expect_identical(responder_protocol_hash(imported), responder_protocol_hash(larger))
  expect_error(protocol_test_v2(sd_estimator = "median_sd"), "Unsupported SD_estimator")
  expect_error(protocol_test_v2(sd_estimator = c("sample_sd", larger_noise)), "one noise estimator")
  expect_match(responder_noise_rule(p), "sample SD of the local prestimulus baseline", fixed = TRUE)
  expect_match(responder_noise_rule(larger), "larger of the sample SD", fixed = TRUE)
  # The sentence is pasted into methods sections: the statistic is named in full.
  expect_match(responder_noise_rule(larger), paste("1.4826 x the median absolute deviation, about their median,",
    "of the differences between consecutive samples over the whole recording, divided by sqrt(2)"), fixed = TRUE)
  expect_false(grepl("MAD", responder_noise_rule(larger), fixed = TRUE))
  expect_match(responder_noise_rule(responder_protocol("A", 5, 8, 3, .1, 3, 2, NULL)), "sample SD", fixed = TRUE)
  expect_equal(responder_protocol_engine_version(p), "2.0.0")
  expect_equal(jsonlite::fromJSON(responder_export_protocol(p))$engine_version, "2.0.0")
  expect_equal(responder_import_protocol(responder_export_protocol(p))$Engine_version, "2.0.0")
  # Engine 2.0.0 knew only the baseline SD.
  larger$Engine_version <- "2.0.0"
  expect_error(responder_validate_protocol(larger), "not defined for engine version 2.0.0")
  # One record and one hash per rule: the baseline SD relabelled 2.1.0 is refused,
  # in memory and as a file, rather than scored under a second hash.
  relabelled <- p
  relabelled$Engine_version <- "2.1.0"
  expect_error(responder_validate_protocol(relabelled), "sample_sd is recorded with engine version 2.0.0, not 2.1.0")
  expect_error(responder_protocol_hash(relabelled), "recorded with engine version 2.0.0, not 2.1.0")
  t <- seq(0, 8, .5)
  expect_error(responder_field_results(data.frame(Time = t, Cell = .2 * (t >= 5)), "Cell", "F", relabelled),
               "recorded with engine version 2.0.0, not 2.1.0")
  file <- gsub('"2.0.0"', '"2.1.0"', responder_export_protocol(p), fixed = TRUE)
  expect_equal(jsonlite::fromJSON(file)$engine_version, "2.1.0")
  expect_error(responder_import_protocol(file), "recorded with engine version 2.0.0, not 2.1.0")
  p$Engine_version <- "2.2.0"
  expect_error(responder_validate_protocol(p), "Unsupported responder engine version")
  windows <- data.frame(Role = c("Stimulus", "Control"), Stimulus = c("Capsaicin", "KCl"),
    Baseline_start_s = c(0, 9), Baseline_end_s = c(5, 12), Stimulus_onset_s = c(5, 12),
    Start_s = c(5, 12), End_s = c(8, 15))
  mixed <- responder_protocol_v2("Two windows", windows)
  mixed$SD_estimator[2] <- larger_noise
  expect_error(responder_validate_protocol(mixed), "share one SD_estimator")
  mixed$SD_estimator <- "sample_sd"
  mixed$Engine_version[2] <- "2.1.0"
  expect_error(responder_validate_protocol(mixed), "Unsupported responder engine version")
})

test_that("whole-trace noise is 1.4826 x the median absolute deviation of consecutive differences / sqrt(2) in local units", {
  t <- 0:12
  y <- c(-.01, .01, -.01, .01, .3, -.3, .3, .3, -.3, .3, -.3, .3, -.3)
  # Twelve differences: .02 -.02 .02 .29 -.6 .6 0 -.6 .6 -.6 .6 -.6.  Their median
  # is .01 and the median absolute deviation from it is .59.
  expected <- 1.4826 * .59 / sqrt(2)
  expect_equal(responder_whole_trace_noise(t, y, 1.5, 3), list(noise = expected, samples = 12L))
  # Eight of the twelve differences of the quiet trace are zero, so its median absolute deviation is zero.
  quiet <- c(-.05, .05, -.05, .05, rep(.2, 9))
  dt <- data.frame(Time = t, Noisy = y, Shifted = 1 + 2 * y, Quiet = quiet)
  score <- function(...) responder_field_results(dt, names(dt)[-1L], "F", protocol_test_v2(start = 4, end = 12,
    baseline_end = 4, onset = 4, duration_mode = "samples", ...))$calls
  before <- score()
  after <- score(sd_estimator = larger_noise)
  expect_equal(before$Status, rep("Responded", 3))
  expect_equal(before$Threshold_increase, c(.1, .1, 3 * sd(quiet[1:4])))
  expect_equal(after$Status, c("Did not respond", "Did not respond", "Responded"))
  expect_equal(after$Whole_trace_noise, c(expected, expected, 0))
  expect_equal(after$Whole_trace_noise_samples, rep(12L, 3))
  expect_equal(after$Noise_source, c("Whole-trace noise", "Whole-trace noise", "Baseline SD"))
  expect_equal(after$Noise_SD_used, c(expected, expected, sd(quiet[1:4])))
  expect_equal(after$Threshold_increase, 3 * c(expected, expected, sd(quiet[1:4])))
  expect_equal(after$Threshold_source, rep("Baseline noise", 3))
  expect_equal(after$Baseline_SD, before$Baseline_SD)
  expect_equal(after$Local_F0_relative, c(1, 2, 1))
  # The estimate is reported after the existing columns; a cell whose baseline SD
  # is the larger keeps every earlier value.
  expect_identical(names(after), c(names(before), "Whole_trace_noise", "Whole_trace_noise_samples", "Noise_source"))
  kept <- setdiff(names(before), c("SD_estimator", "Engine_version", "Protocol_hash", "Applied_protocol_hash"))
  expect_identical(after[3, kept], before[3, kept])
  floored <- score(sd_estimator = larger_noise, noise_floor = 1)
  expect_equal(floored$Noise_source, rep("Noise floor", 3))
  expect_equal(floored$Noise_SD_used, rep(1, 3))
  expect_equal(floored$Whole_trace_noise, after$Whole_trace_noise)
})

test_that("gaps and missing samples do not contribute whole-trace noise samples", {
  time <- c(0, 1, 2, 10, 11)
  y <- c(0, .2, 0, 5, 5.4)
  # Within the gap limit: .2 -.2 .4 (median .2, median absolute deviation .2).
  within <- responder_whole_trace_noise(time, y, 1.5, 3)
  expect_equal(within, list(noise = 1.4826 * .2 / sqrt(2), samples = 3L))
  # Without a limit the step of 5 across the pause would be a fourth difference
  # (median .3, median absolute deviation .3).
  expect_equal(responder_whole_trace_noise(time, y, Inf, 3), list(noise = 1.4826 * .3 / sqrt(2), samples = 4L))
  # A missing sample removes the two differences that touch it, leaving .2 .4 -.2 .3.
  missing <- responder_whole_trace_noise(0:6, c(0, .2, NA, .1, .5, .3, .6), 1.5, 3)
  expect_equal(missing, list(noise = 1.4826 * .1 / sqrt(2), samples = 4L))
  # In a scored field: acquisition pauses after the response window, and one later sample is missing.
  dt <- data.frame(Time = c(0:8, 20:24), Cell = c(0, .02, 0, .02, 0, rep(.5, 4), 3, 3.02, NA, 3.02, 3))
  call <- responder_field_results(dt, "Cell", "F", protocol_test_v2(duration_mode = "samples",
    sd_estimator = larger_noise))$calls
  expect_equal(call$Max_gap_s, 1.5)
  expect_equal(call$Whole_trace_noise_samples, 10L)
  expect_equal(call$Whole_trace_noise, 1.4826 * .02 / sqrt(2) / 1.008)
  expect_equal(call$Baseline_SD, sd(c(0, .02, 0, .02, 0)) / 1.008)
  expect_equal(call$Noise_source, "Whole-trace noise")
  expect_equal(call$Threshold_source, "Floor")
  expect_equal(call$Status, "Responded")
})

test_that("too few differences fall back to the baseline SD and say so", {
  expect_equal(responder_whole_trace_noise(0:2, c(0, .1, 0), 1.5, 3), list(noise = NA_real_, samples = 2L))
  # Three baseline samples and one response sample, separated by missing values:
  # only two differences can be formed, fewer than the three baseline samples required.
  dt <- data.frame(Time = 0:6, Cell = c(0, .02, -.02, NA, NA, NA, .5))
  score <- function(...) responder_field_results(dt, "Cell", "F", protocol_test_v2(baseline_end = 3, onset = 5.5,
    start = 5.5, end = 6, duration_mode = "samples", frames = 1, ...))$calls
  before <- score()
  after <- score(sd_estimator = larger_noise)
  expect_equal(after$Status, "Responded")
  expect_true(is.na(after$Whole_trace_noise))
  expect_equal(after$Whole_trace_noise_samples, 2L)
  expect_equal(after$Noise_source, "Baseline SD")
  expect_match(after$QC_flags, "WHOLE_TRACE_NOISE_UNAVAILABLE", fixed = TRUE)
  expect_false(grepl("WHOLE_TRACE_NOISE", before$QC_flags, fixed = TRUE))
  kept <- setdiff(names(before), c("SD_estimator", "Engine_version", "Protocol_hash", "Applied_protocol_hash", "QC_flags"))
  expect_identical(after[, kept], before[, kept])
  expect_equal(after$Noise_SD_used, .02)
})

test_that("a sample_sd protocol recorded by engine 2.0.0 gives byte-identical output", {
  path <- file.path(repo_root, "tests", "fixtures", "response-protocols")
  sources <- file.path(path, "engine-2.0.0", c("response_protocols.R", "responders.R"))
  # The released files, as pinned by the Fiji plugin's provenance record.
  expect_equal(unname(tools::sha256sum(sources)), c(
    "67d911e969622ea996fd6b7a8dc65bfddc08bbf741f3a87286360970e1851371",
    "2a1161886def3e4879a4bad85dca4d7c5d8659296291502cc97e5df17ce6134f"))
  released <- new.env()
  for (file in sources) sys.source(file, envir = released)
  expect_equal(released$responder_engine_version(), "2.0.0")
  bytes <- function(x) {
    file <- tempfile(fileext = ".csv")
    on.exit(unlink(file))
    write.csv(x, file, row.names = FALSE, na = "")
    readBin(file, "raw", file.info(file)$size)
  }
  same_tables <- function(now, then) {
    expect_identical(now, then)
    for (name in names(then)) expect_identical(bytes(now[[name]]), bytes(then[[name]]))
  }
  # Realistic recordings with a stimulus, a later control and a per-recording override.
  set <- file.path(path, "reference-recordings")
  recordings <- reference_read(set, "recordings.csv")
  a <- reference_recording_input(set, recordings[1, ])
  b <- reference_recording_input(set, recordings[2, ])
  time <- a$dt$Time
  windows <- rbind(a$windows, data.frame(Role = "Control", Stimulus = "KCl", Baseline_start_s = time[221],
    Baseline_end_s = time[240], Stimulus_onset_s = time[240], Start_s = time[240], End_s = time[300]))
  windows$End_s[1] <- time[200]
  overrides <- cbind(Field = b$field, b$windows[, c("Role", "Baseline_start_s", "Baseline_end_s",
    "Stimulus_onset_s", "Start_s", "End_s")])
  recorded <- released$responder_protocol_v2("Recorded by engine 2.0.0", windows, duration_mode = "samples", frames = 2L)
  expect_equal(unique(recorded$Engine_version), "2.0.0")
  expect_identical(responder_export_protocol(recorded, overrides = overrides),
                   released$responder_export_protocol(recorded, overrides = overrides))
  expect_identical(responder_protocol_hash(recorded, overrides), released$responder_protocol_hash(recorded, overrides))
  for (input in list(a, b)) {
    result <- responder_field_results(input$dt, input$cells, input$field, recorded, input$quality, overrides)
    same_tables(result, released$responder_field_results(input$dt, input$cells, input$field, recorded, input$quality, overrides))
    expect_false(any(c("Whole_trace_noise", "Noise_source") %in% names(result$calls)))
    expect_equal(unique(result$calls$Engine_version), "2.0.0")
  }
  expect_true(all(c("Responded", "Did not respond", "Excluded", "Unscorable") %in%
    responder_field_results(a$dt, a$cells, a$field, recorded, a$quality)$calls$Status))
  grid <- data.frame(Scenario = c("Three SD", "Five SD"), SD_multiplier = c(3, 5))
  same_tables(responder_sensitivity_results(a$dt, a$cells, a$field, recorded, grid, a$quality),
    released$responder_sensitivity_results(a$dt, a$cells, a$field, recorded, grid, a$quality))
  null <- data.frame(Baseline_start_s = time[101], Baseline_end_s = time[120], Stimulus_onset_s = time[120],
    Start_s = time[120], End_s = time[200])
  same_tables(responder_control_diagnostics(a$dt, a$cells, a$field, recorded, null, a$quality),
    released$responder_control_diagnostics(a$dt, a$cells, a$field, recorded, null, a$quality))
  # The shared golden fixture is itself an engine 2.0.0 protocol.
  golden <- file.path(path, "golden-protocol.json")
  traces <- read.csv(file.path(path, "golden-traces.csv"), check.names = FALSE)
  quality <- data.frame(Field = "Golden field", Cell = "InvalidF0", Status = "Excluded",
    Reason = "Invalid raw fluorescence baseline", F0 = 0, Raw_QC_available = TRUE)
  cells <- c(names(traces)[-1L], "InvalidF0")
  same_tables(responder_field_results(traces, cells, "Golden field", responder_import_protocol(golden), quality),
    released$responder_field_results(traces, cells, "Golden field", released$responder_import_protocol(golden), quality))
  # Version 1 scoring is untouched, and so is its table with the optional annotations:
  # they are baseline-SD measurements, recorded with the release that defines them.
  legacy <- responder_protocol("Capsaicin", 5, 8, 3, .1, 3, 2, NULL, TRUE, 12, 15)
  short <- traces[traces$Time < 15, , drop = FALSE]
  legacy_then <- released$responder_field_results(short, cells, "Golden field", legacy)
  same_tables(responder_field_results(short, cells, "Golden field", legacy), legacy_then)
  annotated <- responder_legacy_annotations(short, cells, "Golden field", legacy, quality)
  annotated_then <- released$responder_legacy_annotations(short, cells, "Golden field", legacy, quality)
  expect_identical(annotated, annotated_then)
  expect_identical(bytes(annotated), bytes(annotated_then))
  expect_equal(unique(annotated$Annotation_engine_version), "2.0.0")
  # The same on the reference recordings, through the path the app and the Fiji runner take.
  for (input in list(a, b)) {
    old <- responder_protocol("Stimulus", input$windows$Start_s, input$windows$End_s,
      input$windows$Start_s - input$windows$Baseline_start_s, .1, 3, 2, NULL)
    scored <- responder_field_results(input$dt, input$cells, input$field, old, input$quality)
    scored_then <- released$responder_field_results(input$dt, input$cells, input$field, old, input$quality)
    same_tables(scored, scored_then)
    expect_identical(
      bytes(responder_legacy_annotations(input$dt, input$cells, input$field, old, input$quality, scored)),
      bytes(released$responder_legacy_annotations(input$dt, input$cells, input$field, old, input$quality, scored_then)))
  }
  # The same rule written by this engine is the same protocol: same record, hash and tables.
  current <- responder_protocol_v2("Recorded by engine 2.0.0", windows, duration_mode = "samples", frames = 2L)
  expect_identical(current, recorded)
  expect_identical(responder_protocol_hash(current), released$responder_protocol_hash(recorded))
  same_tables(responder_field_results(a$dt, a$cells, a$field, current, a$quality),
    released$responder_field_results(a$dt, a$cells, a$field, recorded, a$quality))
})

test_that("both noise estimators reproduce an independent calculation cell by cell", {
  check <- reference_recordings_check(file.path(repo_root, "tests", "fixtures", "response-protocols",
    "reference-recordings"))
  expect_equal(nrow(check$differences), 0)
  expect_equal(check$summary$Cells, c(26, 8, 26, 8))
  expect_equal(check$summary$Evaluable, c(23, 8, 23, 8))
  expect_equal(check$summary$Calls_agree, check$summary$Cells)
  expect_equal(check$summary$Numbers_differ, rep(0, 4))
  expect_equal(check$summary$Responded, c(9, 3, 8, 3))
  # A baseline SD that is too small by chance: 8.8% from 19 frames, 15.2% over the trace.
  chance <- check$cells[check$cells$Cell == "Cell_23", ]
  expect_equal(chance$Status_sample_sd, "Responded")
  expect_equal(chance$Status_larger, "Did not respond")
  expect_lt(chance$Baseline_SD, chance$Whole_trace_noise)
  expect_equal(check$cells$Status_larger[check$cells$Cell %in% c("Cell_24", "Cell_25", "Cell_26")],
               c("Excluded", "Excluded", "Unscorable"))
})

test_that("user-facing text says what the whole-trace estimate measures and names the statistic in full", {
  text <- function(...) paste(readLines(file.path(repo_root, ...), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  claims <- list(help = text("R", "mod_help.R"), readme = text("README.md"), changelog = text("CHANGELOG.md"))
  for (name in names(claims)) {
    # It replaces a baseline SD that is too small by chance with frame-to-frame noise;
    # it is not a guarantee about every kind of noise in the cell.
    expect_match(claims[[name]], "a baseline SD that is too small by chance is replaced by the cell.s frame-to-frame noise over the whole recording", info = name)
    expect_false(grepl("real noise", claims[[name]], fixed = TRUE), info = name)
    expect_false(grepl("cannot set a threshold", claims[[name]], fixed = TRUE), info = name)
  }
  described <- c(claims[c("readme", "changelog")], list(interface = text("R", "mod_in_vitro.R"),
    documentation = text("docs", "RESPONSE_PROTOCOLS.md")))
  for (name in names(described)) {
    expect_match(described[[name]], "median absolute deviation", fixed = TRUE, info = name)
    expect_false(grepl("MAD of the differences", described[[name]], fixed = TRUE), info = name)
  }
  expect_match(described$documentation, "MAD is the median absolute deviation", fixed = TRUE)
  expect_match(described$documentation, "measures frame-to-frame noise only", fixed = TRUE)
})
