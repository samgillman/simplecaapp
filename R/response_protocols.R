# Portable, versioned response protocols.  Version 1 is the unchanged historical
# consecutive-sample contract; version 2 makes timing and persistence explicit.
responder_engine_version <- function() "2.1.0"

# Engine releases whose version-2 protocols this engine scores, with the noise
# estimators each release defined.
responder_engine_estimators <- function() list(
  "2.0.0" = "sample_sd",
  "2.1.0" = c("sample_sd", "larger_of_baseline_sd_and_trace_noise"))

# A protocol records the earliest release that defines its rule, not the release
# that wrote it.  A rule engine 2.0.0 already knew therefore keeps its hash and
# its results, and still reads wherever that engine is carried.  Validation
# refuses any other release for the rule.
responder_estimator_engine <- function(estimator) {
  releases <- responder_engine_estimators()
  names(releases)[vapply(releases, function(known) estimator %in% known, logical(1))][1]
}

# The engine release whose rules a protocol records.
responder_protocol_engine_version <- function(protocol) {
  if (responder_protocol_version(protocol) == 1L) "legacy-1" else protocol$Engine_version[1]
}

# One sentence for methods text and manifests: the noise behind the SD multiplier.
responder_noise_rule <- function(protocol) {
  estimator <- if (responder_protocol_version(protocol) == 1L) "sample_sd" else protocol$SD_estimator[1]
  switch(estimator,
    sample_sd = "Noise is the sample SD of the local prestimulus baseline.",
    larger_of_baseline_sd_and_trace_noise = paste("Noise is the larger of the sample SD of the local prestimulus",
      "baseline and the whole-trace noise (1.4826 x the median absolute deviation, about their median, of the",
      "differences between consecutive samples over the whole recording, divided by sqrt(2))."),
    stop("Unsupported SD_estimator setting."))
}

responder_protocol_v2 <- function(name, windows, floor = 0.10, noise = 3,
                                  duration_mode = "seconds", duration_s = 1,
                                  frames = 2L, gap = NULL,
                                  min_baseline_samples = 3L, noise_floor = 0,
                                  validation_status = "provisional",
                                  expected_interval_s = NULL,
                                  sd_estimator = "sample_sd") {
  if (!is.data.frame(windows)) stop("Protocol windows must be a data frame.")
  if ("Onset_s" %in% names(windows) && !"Stimulus_onset_s" %in% names(windows)) {
    names(windows)[names(windows) == "Onset_s"] <- "Stimulus_onset_s"
  }
  if ("Onset_s" %in% names(windows)) stop("Use Stimulus_onset_s for the applied stimulus time.")
  required <- c("Role", "Stimulus", "Baseline_start_s", "Baseline_end_s",
                "Stimulus_onset_s", "Start_s", "End_s")
  if (!all(required %in% names(windows)))
    stop("Protocol windows require: ", paste(required, collapse = ", "), ".")
  out <- as.data.frame(windows[, required, drop = FALSE], stringsAsFactors = FALSE)
  n <- nrow(out)
  value <- function(x, label) {
    if (!length(x) %in% c(1L, n)) stop(label, " must have one value or one value per window.")
    rep(x, length.out = n)
  }
  if (!is.character(sd_estimator) || length(sd_estimator) != 1L) stop("Choose one noise estimator.")
  if (!sd_estimator %in% unlist(responder_engine_estimators())) stop("Unsupported SD_estimator setting.")
  out$Protocol_version <- 2L
  out$Protocol_name <- name
  out$Engine_version <- responder_estimator_engine(sd_estimator)
  out$Validation_status <- validation_status
  out$Signal_reference <- "fixed_reference_fractional_change"
  out$Baseline_s <- out$Baseline_end_s - out$Baseline_start_s
  out$Floor_local_fraction <- value(floor, "Amplitude floor")
  out$SD_multiplier <- value(noise, "SD multiplier")
  out$Duration_mode <- value(duration_mode, "Duration mode")
  out$Min_duration_s <- value(duration_s, "Minimum duration")
  out$Consecutive_samples <- value(frames, "Consecutive samples")
  out$Max_gap_s <- value(if (is.null(gap)) NA_real_ else gap, "Maximum gap")
  out$Expected_interval_s <- value(if (is.null(expected_interval_s)) NA_real_ else expected_interval_s,
                                  "Expected sample interval")
  out$Min_baseline_samples <- value(min_baseline_samples, "Minimum baseline samples")
  out$Noise_floor_local_fraction <- value(noise_floor, "Measurement-noise floor")
  out$SD_estimator <- sd_estimator
  out$Threshold_operator <- ">="
  out$Coverage_policy <- "complete_windows"
  out$Response_direction <- "increase"
  out$Smoothing <- "none"
  rownames(out) <- NULL
  responder_validate_protocol(out)
  out
}

responder_protocol_version <- function(protocol) {
  if (!is.data.frame(protocol)) stop("A response protocol must be a data frame.")
  if (!"Protocol_version" %in% names(protocol)) return(1L)
  version <- unique(protocol$Protocol_version)
  if (length(version) != 1L || is.na(version) || !version %in% c(1L, 2L))
    stop("Unsupported response protocol version.")
  as.integer(version)
}

responder_validate_protocol <- function(protocol) {
  version <- responder_protocol_version(protocol)
  required <- c("Role", "Stimulus", "Start_s", "End_s", "Baseline_s",
                "Floor_local_fraction", "SD_multiplier", "Consecutive_samples", "Max_gap_s")
  if (version == 2L) required <- c(required, "Protocol_name", "Engine_version",
    "Validation_status", "Signal_reference", "Baseline_start_s", "Baseline_end_s",
    "Stimulus_onset_s", "Duration_mode", "Min_duration_s", "Min_baseline_samples",
    "Noise_floor_local_fraction", "Expected_interval_s", "SD_estimator", "Threshold_operator",
    "Coverage_policy", "Response_direction", "Smoothing")
  missing <- setdiff(required, names(protocol))
  if (length(missing)) stop("Protocol is missing: ", paste(missing, collapse = ", "), ".")
  if (!nrow(protocol) %in% 1:2 || anyDuplicated(protocol$Role) ||
      !identical(as.character(protocol$Role), c("Stimulus", if (nrow(protocol) == 2L) "Control")))
    stop("Use one primary Stimulus row, followed by at most one Control row.")
  text_columns <- c("Role", "Stimulus", if (version == 2L) c("Protocol_name", "Engine_version",
    "Validation_status", "Signal_reference", "Duration_mode", "SD_estimator", "Threshold_operator",
    "Coverage_policy", "Response_direction", "Smoothing"))
  if (any(vapply(protocol[text_columns], function(x)
      !is.character(x) || anyNA(x) || any(!nzchar(trimws(x))), logical(1))))
    stop("Protocol names and text settings must be nonempty strings.")
  if (anyDuplicated(tolower(trimws(protocol$Stimulus))))
    stop("Primary stimulus and control must have distinct names.")
  numeric_columns <- c("Start_s", "End_s", "Baseline_s", "Floor_local_fraction",
    "SD_multiplier", "Consecutive_samples", if (version == 2L) c("Baseline_start_s", "Baseline_end_s",
      "Stimulus_onset_s", "Min_duration_s", "Min_baseline_samples", "Noise_floor_local_fraction"))
  if (any(vapply(protocol[numeric_columns], function(x) !is.numeric(x) || any(!is.finite(x)), logical(1))))
    stop("Protocol timing and threshold settings must be finite numbers.")
  for (column in c("Max_gap_s", if (version == 2L) "Expected_interval_s")) {
    x <- protocol[[column]]
    if ((!is.numeric(x) && !all(is.na(x))) || any(!is.na(x) & (!is.finite(x) | x <= 0)))
      stop(column, " must be positive or null for automatic timing.")
  }
  if (any(protocol$End_s <= protocol$Start_s) || any(protocol$Baseline_s <= 0))
    stop("Response end must follow response start, and baseline duration must be positive.")
  if (any(protocol$Floor_local_fraction < 0) || any(protocol$SD_multiplier < 0) ||
      any(protocol$Consecutive_samples < 1 | protocol$Consecutive_samples != trunc(protocol$Consecutive_samples)))
    stop("Thresholds must be nonnegative and consecutive samples must be a positive integer.")
  if (version == 1L) {
    if (any(protocol$Floor_local_fraction <= 0)) stop("Legacy amplitude floors must be positive.")
    return(invisible(protocol))
  }
  if (length(unique(protocol$Protocol_name)) != 1L ||
      length(unique(protocol$Validation_status)) != 1L)
    stop("All windows must share one protocol name and validation status.")
  if (any(!protocol$Validation_status %in% c("provisional", "user_defined", "validated_by_user")))
    stop("Validation status must be provisional, user_defined or validated_by_user.")
  engine <- unique(protocol$Engine_version)
  estimators <- responder_engine_estimators()
  if (length(engine) != 1L || !engine %in% names(estimators))
    stop("Unsupported responder engine version; use the recorded engine or explicitly migrate the protocol.")
  estimator <- unique(protocol$SD_estimator)
  if (length(estimator) != 1L) stop("All windows must share one SD_estimator setting.")
  if (!estimator %in% unlist(estimators)) stop("Unsupported SD_estimator setting.")
  recorded <- responder_estimator_engine(estimator)
  if (!estimator %in% estimators[[engine]])
    stop("SD_estimator ", estimator, " is not defined for engine version ", engine,
         "; record the protocol with engine version ", recorded, ".")
  # One record, and so one hash, per rule: a later release that still scores the
  # rule is not a second name for it.
  if (engine != recorded)
    stop("SD_estimator ", estimator, " is recorded with engine version ", recorded, ", not ", engine,
         "; a protocol names the earliest engine release that defines its rule.")
  supported <- list(Signal_reference = "fixed_reference_fractional_change",
    Threshold_operator = ">=", Coverage_policy = "complete_windows", Response_direction = "increase",
    Smoothing = "none")
  for (column in names(supported)) {
    if (any(protocol[[column]] != supported[[column]])) stop("Unsupported ", column, " setting.")
  }
  if (any(!protocol$Duration_mode %in% c("seconds", "samples")))
    stop("Duration mode must be seconds or samples.")
  if (any(protocol$Min_duration_s <= 0) || any(protocol$Noise_floor_local_fraction < 0) ||
      any(protocol$Min_baseline_samples < 2 | protocol$Min_baseline_samples != trunc(protocol$Min_baseline_samples)))
    stop("Duration must be positive, the noise floor nonnegative, and minimum baseline samples an integer of at least two.")
  if (any(protocol$Floor_local_fraction == 0 & protocol$SD_multiplier == 0))
    stop("An amplitude criterion or a positive noise multiplier is required; a zero threshold is not allowed.")
  if (any(protocol$Baseline_end_s <= protocol$Baseline_start_s) ||
      any(protocol$Baseline_end_s > protocol$Stimulus_onset_s) ||
      any(protocol$Start_s < protocol$Stimulus_onset_s))
    stop("The baseline must end at or before stimulus onset, and the response window must start at or after onset.")
  tolerance <- 16 * .Machine$double.eps * pmax(1, abs(protocol$Baseline_s))
  if (any(abs(protocol$Baseline_s - (protocol$Baseline_end_s - protocol$Baseline_start_s)) > tolerance))
    stop("Baseline_s must match the explicit baseline start and end.")
  if (nrow(protocol) == 2L) {
    if (max(protocol$Stimulus_onset_s) < min(protocol$End_s))
      stop("Primary stimulus and control response intervals must not overlap.")
    # Do not silently use another stimulus's response interval as a baseline.
    for (i in 1:2) {
      j <- 3L - i
      if (max(protocol$Baseline_start_s[i], protocol$Stimulus_onset_s[j]) <
          min(protocol$Baseline_end_s[i], protocol$End_s[j]))
        stop("Each stimulus requires a baseline outside the other stimulus's response interval.")
    }
  }
  invisible(protocol)
}

responder_validate_overrides <- function(overrides, protocol) {
  if (is.null(overrides) || (is.data.frame(overrides) && !nrow(overrides))) return(invisible(overrides))
  if (!is.data.frame(overrides) || !all(c("Field", "Role") %in% names(overrides)))
    stop("Timing overrides require Field and Role columns.")
  if (responder_protocol_version(protocol) != 2L)
    stop("Independent timing overrides require a version 2 protocol.")
  timings <- c("Baseline_start_s", "Baseline_end_s", "Stimulus_onset_s", "Start_s", "End_s")
  allowed <- c("Field", "Role", "Stimulus", timings)
  if (length(setdiff(names(overrides), allowed))) stop("Unknown timing override columns.")
  if (anyNA(overrides$Field) || any(!nzchar(trimws(overrides$Field))) ||
      anyNA(overrides$Role) || any(!overrides$Role %in% protocol$Role))
    stop("Every timing override requires an existing role and a nonempty field identifier.")
  if (anyDuplicated(paste(overrides$Field, overrides$Role, sep = "\r")))
    stop("Only one timing override is allowed for each Field and Role.")
  for (column in intersect(timings, names(overrides))) {
    x <- overrides[[column]]
    if ((!is.numeric(x) && !all(is.na(x))) || any(!is.na(x) & !is.finite(x)))
      stop("Timing overrides must contain finite numbers or null for unchanged settings.")
  }
  if ("Stimulus" %in% names(overrides)) {
    expected <- protocol$Stimulus[match(overrides$Role, protocol$Role)]
    if (anyNA(overrides$Stimulus) || any(overrides$Stimulus != expected))
      stop("Timing override stimulus names must match their protocol roles.")
  }
  invisible(overrides)
}

responder_protocol_for_field <- function(protocol, field, overrides = NULL) {
  responder_validate_protocol(protocol)
  if (is.null(overrides)) overrides <- attr(protocol, "field_overrides")
  responder_validate_overrides(overrides, protocol)
  out <- protocol
  attr(out, "field_overrides") <- NULL
  if (!is.null(overrides) && nrow(overrides)) {
    rows <- overrides[overrides$Field == field, , drop = FALSE]
    columns <- intersect(c("Baseline_start_s", "Baseline_end_s", "Stimulus_onset_s", "Start_s", "End_s"), names(rows))
    for (i in seq_len(nrow(rows))) {
      target <- match(rows$Role[i], out$Role)
      for (column in columns) if (!is.na(rows[[column]][i])) out[[column]][target] <- rows[[column]][i]
    }
    out$Baseline_s <- out$Baseline_end_s - out$Baseline_start_s
  }
  responder_validate_protocol(out)
  out
}

responder_protocol_payload <- function(protocol, overrides = NULL) {
  responder_validate_protocol(protocol)
  if (is.null(overrides)) overrides <- attr(protocol, "field_overrides")
  responder_validate_overrides(overrides, protocol)
  if (!is.null(overrides) && nrow(overrides)) {
    for (field in unique(overrides$Field)) responder_protocol_for_field(protocol, field, overrides)
    overrides <- overrides[order(overrides$Field, overrides$Role), sort(names(overrides)), drop = FALSE]
  } else overrides <- NULL
  version <- responder_protocol_version(protocol)
  windows <- as.data.frame(protocol, stringsAsFactors = FALSE)
  attr(windows, "field_overrides") <- NULL
  windows <- windows[, sort(names(windows)), drop = FALSE]
  rownames(windows) <- NULL
  list(schema = "simpleca.response-protocol", schema_version = version,
       engine_version = responder_protocol_engine_version(protocol),
       name = if (version == 2L) protocol$Protocol_name[1] else "Legacy consecutive-sample protocol",
       validation_status = if (version == 2L) protocol$Validation_status[1] else "legacy_unvalidated",
       signal_reference = if (version == 2L) "fixed_reference_fractional_change" else "fixed_reference_fractional_dff0",
       windows = windows, field_overrides = overrides)
}

responder_export_protocol <- function(protocol, file = NULL, overrides = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Protocol JSON import/export requires the jsonlite package.")
  payload <- responder_protocol_payload(protocol, overrides)
  json <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows", na = "null",
                                       null = "null", digits = NA, pretty = TRUE))
  if (!is.null(file)) writeLines(json, file, useBytes = TRUE)
  json
}

responder_import_protocol <- function(json_or_file) {
  if (!is.character(json_or_file) || length(json_or_file) != 1L || is.na(json_or_file))
    stop("Provide one protocol JSON string or file path.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Protocol JSON import/export requires the jsonlite package.")
  is_json <- grepl("^\\s*\\{", json_or_file)
  text <- if (is_json) json_or_file else {
    if (!file.exists(json_or_file)) stop("Protocol file does not exist.")
    paste(readLines(json_or_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  payload <- tryCatch(jsonlite::fromJSON(text, simplifyDataFrame = TRUE),
                      error = function(e) stop("Invalid protocol JSON: ", conditionMessage(e)))
  if (!identical(payload$schema, "simpleca.response-protocol") ||
      !is.numeric(payload$schema_version) || length(payload$schema_version) != 1L ||
      is.na(payload$schema_version) || !payload$schema_version %in% c(1L, 2L))
    stop("Unsupported protocol schema or schema version.")
  supported_engines <- if (payload$schema_version == 2L) names(responder_engine_estimators()) else "legacy-1"
  if (!is.character(payload$engine_version) || length(payload$engine_version) != 1L ||
      !payload$engine_version %in% supported_engines)
    stop("Unsupported responder engine version; use the recorded engine or explicitly migrate the protocol.")
  expected_reference <- if (payload$schema_version == 2L) "fixed_reference_fractional_change" else
    "fixed_reference_fractional_dff0"
  if (!identical(payload$signal_reference, expected_reference)) stop("Unsupported protocol signal reference.")
  protocol <- payload$windows
  if (!is.data.frame(protocol)) stop("Protocol JSON must contain a windows array.")
  if (payload$schema_version == 2L) {
    if (!"Protocol_version" %in% names(protocol)) protocol$Protocol_version <- 2L
    if (!"Protocol_name" %in% names(protocol)) protocol$Protocol_name <- payload$name
    if (!"Engine_version" %in% names(protocol)) protocol$Engine_version <- payload$engine_version
    if (!"Validation_status" %in% names(protocol)) protocol$Validation_status <- payload$validation_status
    if (!"Signal_reference" %in% names(protocol)) protocol$Signal_reference <- payload$signal_reference
  }
  if (responder_protocol_version(protocol) != payload$schema_version)
    stop("Protocol schema and window versions disagree.")
  # JSON null has no numeric type; restore the declared numeric columns rather
  # than leaving all-null columns as logical vectors.  Restore v1's original
  # column order as well so historical exported result tables stay identical.
  for (column in intersect(c("Max_gap_s", "Expected_interval_s"), names(protocol)))
    if (all(is.na(protocol[[column]]))) protocol[[column]] <- as.numeric(protocol[[column]])
  if (payload$schema_version == 1L) {
    order <- c("Role", "Stimulus", "Start_s", "End_s", "Baseline_s", "Floor_local_fraction",
               "SD_multiplier", "Consecutive_samples", "Max_gap_s")
    protocol <- protocol[, c(intersect(order, names(protocol)), setdiff(names(protocol), order)), drop = FALSE]
  }
  responder_validate_protocol(protocol)
  if (payload$schema_version == 2L) {
    metadata <- c(name = "Protocol_name", engine_version = "Engine_version",
                  validation_status = "Validation_status", signal_reference = "Signal_reference")
    for (key in names(metadata)) {
      if (!is.character(payload[[key]]) || length(payload[[key]]) != 1L ||
          is.na(payload[[key]]) || any(protocol[[metadata[[key]]]] != payload[[key]]))
        stop("Top-level protocol metadata and window settings disagree: ", key, ".")
    }
  }
  overrides <- payload$field_overrides
  if (!is.null(overrides) && !is.data.frame(overrides)) stop("Timing overrides must be an array of rows or null.")
  responder_validate_overrides(overrides, protocol)
  if (!is.null(overrides) && nrow(overrides)) {
    for (field in unique(overrides$Field)) responder_protocol_for_field(protocol, field, overrides)
    attr(protocol, "field_overrides") <- overrides
  }
  protocol
}

responder_protocol_hash <- function(protocol, overrides = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Protocol hashing requires the jsonlite package.")
  json <- as.character(jsonlite::toJSON(responder_protocol_payload(protocol, overrides), auto_unbox = TRUE,
    dataframe = "rows", na = "null", null = "null", digits = NA, pretty = FALSE))
  path <- tempfile("simpleca-protocol-")
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw(enc2utf8(json)), path)
  unname(tools::md5sum(path))
}
