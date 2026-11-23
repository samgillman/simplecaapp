source(file.path(repo_root, "R", "mod_in_vitro.R"))

applied_state_fixture <- function() {
  t <- seq(0, 20, .5)
  files <- data.frame(name = c("first.csv", "second.csv"), datapath = c("a", "b"))
  state <- build_processed_state(files, list(baseline_frames = c(1, 4)), read_fun = function(path) {
    offset <- if (path == "b") 5 else 0
    data.table::data.table(Time = t, Response = 100 + 20 * (t >= 6 + offset & t <= 8 + offset), Flat = 100)
  })
  state$group_meta$condition <- "Treatment"
  state$group_meta$unit_id <- c("preparation-1", "preparation-2")
  state
}

test_that("draft changes cannot alter an applied run, its traces, or its exports", {
  library(shiny)
  rv <- do.call(reactiveValues, applied_state_fixture()[processed_state_fields])
  testServer(mod_in_vitro_server, args = list(rv = rv), {
    session$setInputs(protocol_name = "Evoked candidate", contract_version = "explicit",
      stimulus = "Capsaicin", onset = 5, baseline_end = 5, baseline = 2, start = 5.5, end = 10,
      floor = .1, noise = 3, frames = 20, duration_mode = "seconds", duration_s = 1,
      auto_gap = TRUE, control = FALSE)
    expect_false(applied_ready())
    session$setInputs(apply_protocol = 1)
    expect_true(applied_ready(), info = analysis_message())
    first <- responder_copy(applied_run())
    calls <- export_rows("calls")
    counts <- export_rows("counts")
    expect_equal(counts$Percent_responding, c(50, 0))
    expect_match(rule_line(), "1 s elapsed sampled span", fixed = TRUE)
    expect_false(grepl("20 samples", rule_line(), fixed = TRUE))
    expect_false(dirty())
    session$setInputs(floor = .5, baseline_end = 4.5, start = 6)
    expect_true(dirty())
    expect_identical(export_rows("calls"), calls)
    expect_identical(export_rows("counts"), counts)
    expect_identical(result_data(), first$snapshot$dts)
    expect_match(output$applied_status$html, "Unapplied changes")
    expect_equal(active_windows(selected_call())$Baseline_end, 5)
    expect_equal(active_windows(selected_call())$Start, 5.5)
    expect_match(methods_text(), first$protocol_hash, fixed = TRUE)
    session$setInputs(apply_protocol = 2)
    expect_false(dirty())
    expect_false(identical(applied_run()$id, first$id))
    expect_equal(results()$counts$Percent_responding, c(0, 0))
    # Invalid draft input leaves the last applied result available and unchanged.
    after <- export_rows("calls")
    session$setInputs(end = 4)
    expect_true(dirty())
    session$setInputs(apply_protocol = 3)
    expect_identical(export_rows("calls"), after)
    expect_match(analysis_message(), "end|start")
  })
})

test_that("scientific metadata invalidates results while presentation edits do not", {
  library(shiny)
  rv <- do.call(reactiveValues, applied_state_fixture()[processed_state_fields])
  testServer(mod_in_vitro_server, args = list(rv = rv), {
    session$setInputs(stimulus = "A", start = 5, end = 10, baseline = 2, floor = .1,
      noise = 3, frames = 2, auto_gap = TRUE, control = FALSE)
    session$setInputs(apply_protocol = 1)
    id <- applied_run()$id
    meta <- rv$group_meta
    meta$label[1] <- "Renamed recording"
    meta$color[1] <- "#112233"
    meta$order <- rev(meta$order)
    rv$group_meta <- meta
    session$flushReact()
    expect_true(applied_ready())
    expect_identical(applied_run()$id, id)
    meta$unit_id[1] <- "different-preparation"
    rv$group_meta <- meta
    session$flushReact()
    expect_false(applied_ready())
    expect_error(export_rows("calls"), class = "shiny.silent.error")
    expect_match(analysis_message(), "experimental-unit")
    session$setInputs(apply_protocol = 2)
    expect_true(applied_ready())
    expect_true("different-preparation" %in% export_rows("units")$Experimental_unit_ID)
    clear_processed_state(rv)
    session$flushReact()
    expect_false(applied_ready())
  })
})

test_that("independent timing, generic controls and complete audit bundles use the applied snapshot", {
  library(shiny)
  rv <- do.call(reactiveValues, applied_state_fixture()[processed_state_fields])
  testServer(mod_in_vitro_server, args = list(rv = rv), {
    session$setInputs(protocol_name = "Independent timing", contract_version = "explicit",
      stimulus = "Capsaicin", onset = 5, baseline_end = 4.5, baseline = 2, start = 5.5, end = 10,
      floor = .1, noise = 3, frames = 2, duration_mode = "seconds", duration_s = 1,
      auto_gap = TRUE, control = TRUE, control_name = "Ionomycin", control_onset = 17,
      control_baseline_end = 17, control_baseline = 2, control_start = 17, control_end = 20)
    timing_overrides(data.frame(Field = "second", Role = "Stimulus", Baseline_start_s = 8,
      Baseline_end_s = 10, Stimulus_onset_s = 10, Start_s = 10.5, End_s = 15))
    session$setInputs(apply_protocol = 1)
    expect_true(applied_ready(), info = analysis_message())
    primary <- export_rows("counts")
    primary <- primary[primary$Population == "Cells in field" & primary$Role == "Stimulus", ]
    expect_equal(primary$Percent_responding, c(50, 50))
    expect_equal(primary$Stimulus_onset_s, c(5, 10))
    expect_equal(primary$Baseline_end_s, c(4.5, 10))
    expect_equal(primary$Start_s, c(5.5, 10.5))
    expect_true(all(is.na(primary$KCl_start_s)))
    expect_true("Ionomycin-positive cells" %in% results()$counts$Population)
    expect_equal(sum(export_rows("units")$Evaluable[export_rows("units")$Population == "Cells in field" &
      export_rows("units")$Role == "Stimulus"]), 4)
    expect_true(all(c("Analysis_run_ID", "Protocol_hash", "Engine_version", "Signal_kind") %in% names(export_rows("metrics"))))
    path <- tempfile(fileext = ".zip")
    on.exit(unlink(path), add = TRUE)
    write_analysis_bundle(path)
    contents <- utils::unzip(path, list = TRUE)$Name
    expect_true(all(c("counts.csv", "calls.csv", "metrics.csv", "qc.csv", "units.csv", "unit_summary.csv",
      "response_protocol.json", "analysis_manifest.json", "processing_manifest.csv", "methods.txt") %in% contents))
    directory <- tempfile(); dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    utils::unzip(path, exdir = directory)
    imported <- responder_import_protocol(file.path(directory, "response_protocol.json"))
    expect_identical(responder_protocol_hash(imported), applied_run()$protocol_hash)
    manifest <- jsonlite::fromJSON(file.path(directory, "analysis_manifest.json"))
    expect_identical(manifest$analysis_run_id, applied_run()$id)
    saved <- data.table::fread(file.path(directory, "counts.csv"))
    expect_true(all(saved$Analysis_run_ID == applied_run()$id))
    expect_true(all(saved$Protocol_hash == applied_run()$protocol_hash))
    # The reviewed baseline shading and threshold remain on the applied coordinate system.
    row <- selected_call()
    expect_equal(active_windows(row)$Baseline_start, row$Baseline_start_s)
  })
})

test_that("experimental-unit summaries pool within units and never manufacture missing IDs", {
  counts <- data.frame(Field = c("a", "b", "c", "d"), Role = "Stimulus", Stimulus = "ATP",
    Population = "Cells in field", Responded = c(1, 9, 9, 1), Nonresponders = c(1, 1, 1, 9),
    Evaluable = c(2, 10, 10, 10), Excluded = 0, Unscorable = 0)
  metadata <- data.frame(key = c("a", "b", "c", "d"), condition = "C", unit_id = c("u1", "u1", "u2", ""))
  x <- responder_unit_results(counts, metadata)
  expect_equal(x$units$Fields, c(2, 1))
  expect_equal(x$units$Percent_responding, c(100 * 10/12, 90))
  expect_equal(x$summary$Experimental_units, 2)
  expect_equal(x$summary$Mean_unit_percent, mean(c(100 * 10/12, 90)))
  expect_equal(x$summary$SEM_unit_percent, stats::sd(c(100 * 10/12, 90))/sqrt(2))
  expect_identical(x$unassigned_fields, "d")
  metadata$unit_id <- ""
  empty <- responder_unit_results(counts, metadata)
  expect_equal(nrow(empty$units), 0)
  expect_equal(nrow(empty$summary), 0)
  expect_length(empty$unassigned_fields, 4)
})

test_that("per-window imports remain exact and cannot silently lose hidden control criteria", {
  library(shiny)
  rv <- do.call(reactiveValues, applied_state_fixture()[processed_state_fields])
  windows <- data.frame(Role = c("Stimulus", "Control"), Stimulus = c("Capsaicin", "Ionomycin"),
    Baseline_start_s = c(3, 14), Baseline_end_s = c(5, 16), Stimulus_onset_s = c(5, 16),
    Start_s = c(5.5, 16), End_s = c(10, 20))
  original <- responder_protocol_v2("Split criteria", windows, floor = c(.1, .3),
    duration_s = c(1, 2), frames = c(2, 3), gap = c(NA_real_, 1),
    expected_interval_s = c(NA_real_, .5), noise_floor = c(0, .002))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  responder_export_protocol(original, path)
  testServer(mod_in_vitro_server, args = list(rv = rv), {
    session$setInputs(protocol_name = "Split criteria", contract_version = "explicit",
      validation_status = "provisional", stimulus = "Capsaicin", onset = 5, baseline_end = 5,
      baseline = 2, start = 5.5, end = 10, floor = .1, noise = 3, frames = 2,
      duration_mode = "seconds", duration_s = 1, min_baseline_samples = 3, noise_floor = 0,
      expected_interval_s = NA_real_, auto_gap = TRUE, control = TRUE,
      control_name = "Ionomycin", control_onset = 16, control_baseline_end = 16,
      control_baseline = 2, control_start = 16, control_end = 20)
    session$setInputs(import_protocol = list(datapath = path, name = "split.json"))
    expect_null(protocol_error())
    expect_identical(responder_protocol_hash(draft_protocol()), responder_protocol_hash(original))
    session$setInputs(apply_protocol = 1)
    expect_true(applied_ready(), info = analysis_message())
    before <- export_rows("calls")
    expect_equal(unique(before$Floor_local_fraction[before$Role == "Control"]), .3)
    expect_equal(unique(before$Min_duration_s[before$Role == "Control"]), 2)
    session$setInputs(start = 6)
    expect_match(protocol_error(), "different per-window rules")
    expect_true(dirty())
    session$setInputs(apply_protocol = 2)
    expect_identical(export_rows("calls"), before)
    expect_identical(applied_run()$protocol_hash, responder_protocol_hash(original))
  })
})

test_that("the noise estimate is an explicit Version 2 choice that defaults to the baseline SD", {
  library(shiny)
  larger <- "larger_of_baseline_sd_and_trace_noise"
  state <- applied_state_fixture()
  # A quiet baseline before a noisy recording: the whole-trace estimate is the larger one.
  state$dts$first$Flat <- rep(c(0, .001, 0, -.001, .3, -.3, .3, .3, -.3), length.out = nrow(state$dts$first))
  rv <- do.call(reactiveValues, state[processed_state_fields])
  recorded <- tempfile(fileext = ".json")
  on.exit(unlink(recorded), add = TRUE)
  testServer(mod_in_vitro_server, args = list(rv = rv), {
    session$setInputs(protocol_name = "Noise estimate", contract_version = "explicit",
      validation_status = "provisional", stimulus = "Capsaicin", onset = 2, baseline_end = 2, baseline = 2,
      start = 2, end = 10, floor = .1, noise = 3, frames = 2, duration_mode = "samples", duration_s = 1,
      min_baseline_samples = 3, noise_floor = 0, expected_interval_s = NA_real_, auto_gap = TRUE, control = FALSE)
    session$setInputs(apply_protocol = 1)
    expect_true(applied_ready(), info = analysis_message())
    before <- export_rows("calls")
    expect_equal(unique(before$SD_estimator), "sample_sd")
    expect_false(any(c("Whole_trace_noise", "Noise_source") %in% names(before)))
    expect_equal(before$Status[before$Field == "first" & before$Cell == "Flat"], "Responded")
    expect_false(grepl("whole-trace", rule_line(), fixed = TRUE))
    expect_match(methods_text(), "Noise is the sample SD of the local prestimulus baseline.", fixed = TRUE)
    expect_equal(analysis_manifest()$noise_estimator, "sample_sd")
    # The baseline SD rule is the one engine 2.0.0 defined, and is recorded as such.
    expect_equal(unique(before$Engine_version), "2.0.0")
    expect_equal(analysis_manifest()$engine_version, "2.0.0")
    expect_equal(analysis_manifest()$annotation_engine_version, "2.1.0")
    expect_match(methods_text(), "scoring contract 2.0.0; measurement annotations 2.1.0", fixed = TRUE)
    session$setInputs(sd_estimator = larger)
    expect_true(dirty())
    expect_identical(export_rows("calls"), before)
    session$setInputs(apply_protocol = 2)
    expect_false(dirty())
    after <- export_rows("calls")
    expect_equal(unique(after$SD_estimator), larger)
    expect_identical(tail(names(results()$calls), 3), c("Whole_trace_noise", "Whole_trace_noise_samples", "Noise_source"))
    flat <- after[after$Field == "first" & after$Cell == "Flat", ]
    expect_equal(flat$Status, "Did not respond")
    expect_equal(flat$Noise_source, "Whole-trace noise")
    expect_gt(flat$Whole_trace_noise, flat$Baseline_SD)
    # The engine still says the noise criterion set this threshold; the table ("Set by")
    # and the hover text of the threshold plot both name the estimate behind it.
    expect_equal(flat$Threshold_source, "Baseline noise")
    shown <- jsonlite::fromJSON(output$calls, simplifyVector = FALSE)$x$data
    expect_equal(unlist(shown[[5]])[unlist(shown[[1]]) == "Flat"], "Whole-trace noise")
    hover <- unlist(jsonlite::fromJSON(output$distribution, simplifyVector = FALSE)$x$data[[1]]$text)
    expect_length(hover, 4)
    expect_match(hover[grepl("first \u00b7 Flat<br>", hover, fixed = TRUE)],
      "<br>Did not respond \u00b7 Whole-trace noise<br>", fixed = TRUE)
    expect_false(any(grepl("Baseline noise", hover, fixed = TRUE)))
    expect_equal(after$Status[after$Cell == "Response"], before$Status[before$Cell == "Response"])
    expect_match(rule_line(), "larger of baseline SD and whole-trace noise", fixed = TRUE)
    expect_match(methods_text(), "1.4826 x the median absolute deviation, about their median, of the differences", fixed = TRUE)
    expect_false(grepl("MAD", methods_text(), fixed = TRUE))
    expect_false(grepl("MAD", analysis_manifest()$noise_rule, fixed = TRUE))
    manifest <- analysis_manifest()
    expect_equal(manifest$noise_estimator, larger)
    expect_match(manifest$noise_rule, "whole-trace noise", fixed = TRUE)
    expect_equal(manifest$engine_version, "2.1.0")
    expect_equal(unique(after$Engine_version), "2.1.0")
    expect_true(all(c("Whole_trace_noise", "Whole_trace_noise_samples", "Noise_source") %in% names(export_rows("qc"))))
    responder_export_protocol(applied_run()$protocol, recorded)
    # The choice belongs to Version 2; the legacy contract keeps its baseline SD.
    session$setInputs(contract_version = "legacy")
    expect_match(protocol_error(), "Version 2 scoring contract", fixed = TRUE)
    session$setInputs(sd_estimator = "sample_sd")
    expect_null(protocol_error())
    # An imported protocol keeps its estimator while the displayed controls match it.
    session$setInputs(contract_version = "explicit", sd_estimator = larger,
      import_protocol = list(datapath = recorded, name = "noise.json"))
    expect_null(protocol_error())
    expect_identical(responder_protocol_hash(draft_protocol()), applied_run()$protocol_hash)
    expect_equal(imported_controls()$sd_estimator, larger)
    # The baseline SD relabelled engine 2.1.0 would be the same rule under a second hash:
    # the import is refused with the reason, and the draft and the applied run stay as they were.
    kept <- imported_protocol()
    windows <- data.frame(Role = "Stimulus", Stimulus = "Capsaicin", Baseline_start_s = 0, Baseline_end_s = 2,
      Stimulus_onset_s = 2, Start_s = 2, End_s = 10)
    writeLines(gsub('"2.0.0"', '"2.1.0"', responder_export_protocol(responder_protocol_v2("Relabelled", windows)),
      fixed = TRUE), recorded)
    session$setInputs(import_protocol = list(datapath = recorded, name = "relabelled.json"))
    expect_match(analysis_message(),
      "Protocol import: SD_estimator sample_sd is recorded with engine version 2.0.0, not 2.1.0", fixed = TRUE)
    expect_identical(imported_protocol(), kept)
    expect_equal(unique(export_rows("calls")$Engine_version), "2.1.0")
  })
})

test_that("a noise-limited threshold is displayed by the estimate behind it", {
  rows <- data.frame(Threshold_source = c("Floor", "Baseline noise", "Baseline noise", "Baseline noise", NA),
    Noise_source = c("Whole-trace noise", "Whole-trace noise", "Baseline SD", NA, NA))
  expect_identical(responder_threshold_set_by(rows), c("Floor", "Whole-trace noise", "Baseline SD", "Baseline noise", NA))
  # A baseline-SD protocol reports no Noise_source and keeps the engine's wording.
  expect_identical(responder_threshold_set_by(rows["Threshold_source"]), rows$Threshold_source)
  expect_identical(responder_threshold_set_by(rows[0, ]), character())
})

test_that("the opening legacy contract keeps its calls table under engine 2.1.0", {
  library(shiny)
  rv <- do.call(reactiveValues, applied_state_fixture()[processed_state_fields])
  testServer(mod_in_vitro_server, args = list(rv = rv), {
    session$setInputs(stimulus = "Capsaicin", start = 5, end = 10, baseline = 2, floor = .1,
      noise = 3, frames = 2, auto_gap = TRUE, control = FALSE)
    session$setInputs(apply_protocol = 1)
    expect_true(applied_ready(), info = analysis_message())
    expect_equal(responder_protocol_version(applied_run()$protocol), 1L)
    calls <- export_rows("calls")
    expect_equal(unique(calls$Engine_version), "legacy-1")
    # The optional measurements are baseline-SD measurements, recorded as engine 2.0.0
    # defined them; nothing in the table names the engine that ran.
    expect_equal(unique(calls$Annotation_engine_version), "2.0.0")
    expect_false(any(vapply(calls, function(column) any(column %in% responder_engine_version()), logical(1))))
    expect_false(any(c("Whole_trace_noise", "Noise_source") %in% names(calls)))
    expect_equal(analysis_manifest()$engine_version, "legacy-1")
    expect_equal(analysis_manifest()$annotation_engine_version, responder_engine_version())
    expect_equal(analysis_manifest()$noise_estimator, "sample_sd")
  })
})
