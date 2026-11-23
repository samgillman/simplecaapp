# Per-cell calls use the same normalized wide traces as Time Course and Heatmap.
responder_protocol <- function(stimulus, start, end, baseline, floor, noise, frames, gap,
                               control = FALSE, control_start = NULL, control_end = NULL) {
  number <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  stimulus <- trimws(stimulus)
  if (length(stimulus) != 1L || is.na(stimulus) || !nzchar(stimulus))
    stop("Enter a stimulus name.")
  if (!all(vapply(list(start, end), number, logical(1))) || end <= start)
    stop("Enter the stimulus start and end in seconds from the recording clock. The end must follow the start.")
  if (!all(vapply(list(baseline, floor, noise, frames), number, logical(1))) ||
      baseline <= 0 || floor <= 0 || noise < 0 || frames < 1 || frames != trunc(frames) ||
      !(is.null(gap) || (number(gap) && gap > 0)))
    stop("Choose a positive baseline length, amplitude floor and maximum gap, a nonnegative SD multiplier, and a whole number of consecutive samples.")
  windows <- data.frame(Role = "Stimulus", Stimulus = stimulus, Start_s = start, End_s = end)
  if (isTRUE(control)) {
    if (tolower(stimulus) == "kcl") stop("Use KCl as the stimulus alone, or enter a different primary stimulus.")
    if (!all(vapply(list(control_start, control_end), number, logical(1))) || control_end <= control_start)
      stop("Enter a KCl start and end in seconds. The end must follow the start.")
    if (start - baseline <= control_end && control_start - baseline <= end)
      stop("Separate the stimulus and KCl windows so each has its own prestimulus baseline.")
    windows <- rbind(windows, data.frame(Role = "Control", Stimulus = "KCl",
                                          Start_s = control_start, End_s = control_end))
  }
  transform(windows, Baseline_s = baseline, Floor_local_fraction = floor, SD_multiplier = noise,
            Consecutive_samples = frames, Max_gap_s = if (is.null(gap)) NA_real_ else gap)
}

responder_window_calls <- function(dt, cells, field, window) {
  time <- dt[["Time"]]
  start <- window$Start_s
  end <- window$End_s
  left <- start - window$Baseline_s
  # Include timestamps that differ from a boundary only by machine rounding.
  tol <- 8 * .Machine$double.eps * max(1, abs(c(left, start, end)))
  recording_end <- tail(time, 1L)
  overrun <- end - recording_end
  clipped <- overrun > tol && overrun <= window$Max_gap_s + tol
  window$Window_clipped <- clipped
  window$Window_warning <- if (clipped) {
    sprintf("Window end clipped from %g to %g s", end, recording_end)
  } else ""
  if (clipped) end <- recording_end
  pre <- which(time >= left - tol & time < start - tol)
  during <- which(time >= start - tol & time <= end + tol)
  idx <- c(pre, during)
  reason <- if (time[1] > left + tol || tail(time, 1) < end - tol) {
    sprintf("Recording does not span the full baseline and response window (needs %g to %g s; recording covers %g to %g s)",
            left, end, time[1], recording_end)
  } else if (length(pre) < 3L) {
    "Fewer than three prestimulus baseline samples"
  } else if (length(during) < window$Consecutive_samples) {
    "Too few response samples for the consecutive-sample rule"
  } else if (max(diff(c(left, time[idx], end)), time[during[1]] - start) > window$Max_gap_s + tol) {
    "Sample gap exceeds the field limit"
  } else {
    ""
  }
  window$Effective_end_s <- if (overrun > window$Max_gap_s + tol) NA_real_ else min(end, recording_end)
  window$Field_reason <- reason
  out <- data.frame(Field = field, Cell = cells, window, Status = "Unscorable",
                    Reason = reason, Baseline_mean = NA_real_,
                    Baseline_SD = NA_real_, Threshold_increase = NA_real_, Peak_increase = NA_real_,
                    Floor_threshold = window$Floor_local_fraction, Noise_threshold = NA_real_,
                    Threshold_source = NA_character_, Margin = NA_real_, Onset_s = NA_real_,
                    Peak_s = NA_real_, Run_start_s = NA_real_, Run_end_s = NA_real_,
                    row.names = NULL, check.names = FALSE)
  for (i in seq_along(cells)) {
    y <- dt[[cells[i]]]
    if (is.null(y)) {
      out$Status[i] <- "Excluded"
      out$Reason[i] <- "Invalid raw fluorescence baseline (F0)"
      next
    }
    if (nzchar(reason)) next
    if (any(!is.finite(y[idx]))) {
      out$Reason[i] <- "Missing values in baseline or response window"
      next
    }
    base <- mean(y[pre])
    local_f0 <- 1 + base
    if (local_f0 <= sqrt(.Machine$double.eps)) {
      out$Reason[i] <- "Local baseline fluorescence is zero, negative or too small"
      next
    }
    noise <- stats::sd(y[pre]) / local_f0
    noise_threshold <- window$SD_multiplier * noise
    threshold <- max(window$Floor_local_fraction, noise_threshold)
    rise <- (y[during] - base) / local_f0
    runs <- rle(rise >= threshold)
    first <- which(runs$values & runs$lengths >= window$Consecutive_samples)[1]
    positive <- !is.na(first)
    if (positive) {
      last <- sum(runs$lengths[seq_len(first)])
      out$Run_start_s[i] <- time[during[last - runs$lengths[first] + 1L]]
      out$Run_end_s[i] <- time[during[last]]
      out$Onset_s[i] <- out$Run_start_s[i]
    }
    peak <- which.max(rise)
    out$Peak_s[i] <- time[during[peak]]
    out$Margin[i] <- rise[peak] / threshold
    out$Noise_threshold[i] <- noise_threshold
    out$Threshold_source[i] <- if (noise_threshold > window$Floor_local_fraction) "Baseline noise" else "Floor"
    out$Status[i] <- if (positive) "Responded" else "Did not respond"
    out$Reason[i] <- if (positive) "Sustained threshold crossing" else "No sustained threshold crossing"
    out$Baseline_mean[i] <- base
    out$Baseline_SD[i] <- noise
    out$Threshold_increase[i] <- threshold
    out$Peak_increase[i] <- rise[peak]
  }
  out
}

responder_field_results <- function(dt, cells, field, protocol, cell_quality = NULL, overrides = NULL) {
  # Keep historical calls byte-for-byte in their original scoring path.  Version
  # 2 is opt-in and never changes the two-sample rule or endpoint clipping of v1.
  if ("Protocol_version" %in% names(protocol)) {
    version <- responder_protocol_version(protocol)
    if (version == 2L)
      return(responder_field_results_v2(dt, cells, field, protocol, cell_quality, overrides))
  }
  protocol$Gap_mode <- ifelse(is.na(protocol$Max_gap_s), "Automatic", "Manual")
  intervals <- diff(dt[["Time"]])
  protocol$Max_gap_s[is.na(protocol$Max_gap_s)] <- 1.5 * stats::median(intervals[intervals > 0])
  protocol$Response_reference <- "Fraction of local prestimulus fluorescence"
  calls <- lapply(seq_len(nrow(protocol)), function(i)
    responder_window_calls(dt, cells, field, protocol[i, , drop = FALSE]))
  count <- function(rows, population, stimulus) {
    yes <- sum(rows$Status == "Responded")
    no <- sum(rows$Status == "Did not respond")
    n <- yes + no
    data.frame(Field = field, Stimulus = stimulus, Population = population,
               Registered = nrow(rows), Responded = yes, Nonresponders = no, Evaluable = n,
               Excluded = sum(rows$Status == "Excluded"), Unscorable = sum(rows$Status == "Unscorable"),
               Percent_responding = if (n) 100 * yes / n else NA_real_)
  }
  counts <- Map(function(rows, stimulus) count(rows, "Cells in field", stimulus),
                calls, protocol$Stimulus)
  if (length(calls) == 2L) {
    positive_cells <- calls[[2]]$Cell[calls[[2]]$Status == "Responded"]
    primary <- calls[[1]]
    counts[[3]] <- count(primary[primary$Cell %in% positive_cells, , drop = FALSE],
                          "KCl-positive cells", protocol$Stimulus[1])
  }
  counts <- do.call(rbind, counts)
  # The KCl-positive denominator uses primary calls but also depends on control coverage.
  source_row <- c(seq_along(calls), if (length(calls) == 2L) 1L)
  diagnostics <- c("Effective_end_s", "Window_clipped", "Window_warning", "Field_reason")
  windows <- do.call(rbind, lapply(calls, function(x) x[1L, diagnostics, drop = FALSE]))
  counts <- cbind(counts, windows[source_row, , drop = FALSE])
  if (length(calls) == 2L && !counts$Registered[3]) {
    counts$Field_reason[3] <- if (counts$Evaluable[2] > 0L) "No KCl responders" else {
      reason <- counts$Field_reason[2]
      paste("KCl control:", if (nzchar(reason)) reason else "No evaluable cells")
    }
  }
  control <- if (length(calls) == 2L) windows[2, , drop = FALSE] else NULL
  for (column in c("Effective_end_s", "Window_warning", "Field_reason")) {
    name <- paste0("KCl_", tolower(column))
    counts[[name]] <- if (!is.null(control)) control[[column]] else if (column == "Effective_end_s") NA_real_ else ""
  }
  settings <- protocol[source_row, setdiff(names(protocol), "Stimulus"), drop = FALSE]
  list(calls = do.call(rbind, calls), counts = cbind(counts, settings))
}

# The common observed interval and the latest end allowed by per-field clipping.
# A reversed interval represents recordings with no common coverage.
responder_bounds <- function(dts, gap = NULL) {
  if (!length(dts)) return(NULL)
  spans <- vapply(dts, function(dt) {
    time <- dt[["Time"]]
    intervals <- diff(time)
    limit <- if (is.null(gap)) 1.5 * stats::median(intervals[intervals > 0]) else gap
    c(start = time[1], end = tail(time, 1L), end_with_gap = tail(time, 1L) + limit)
  }, numeric(3))
  c(start = max(spans["start", ]), end = min(spans["end", ]),
    end_with_gap = min(spans["end_with_gap", ]))
}

responder_defaults <- function(dts, frames) {
  if (!length(dts) || length(frames) != 2L) return(NULL)
  spans <- vapply(dts, function(dt) {
    time <- dt[["Time"]]
    c(start = time[frames[2]], baseline = diff(time[frames]), end = tail(time, 1L))
  }, numeric(3))
  values <- c(start = max(spans["start", ]), end = min(spans["end", ]),
              baseline = min(spans["baseline", ]))
  values[!is.finite(values)] <- NA_real_
  values
}

responder_summary <- function(counts) {
  valid <- counts$Percent_responding[is.finite(counts$Percent_responding)]
  totals <- colSums(counts[, c("Responded", "Nonresponders", "Excluded", "Unscorable", "Evaluable"), drop = FALSE])
  list(totals = totals, pooled = if (totals["Evaluable"]) 100 * totals["Responded"] / totals["Evaluable"] else NA_real_,
       mean = if (length(valid)) mean(valid) else NA_real_, sd = stats::sd(valid), n = length(valid))
}

responder_trace_summary <- function(time, local, status) {
  groups <- split(seq_along(status), status)
  do.call(rbind, lapply(names(groups), function(s) {
    values <- local[, groups[[s]], drop = FALSE]
    values[!is.finite(values)] <- NA_real_
    n <- rowSums(!is.na(values))
    means <- rowMeans(values, na.rm = TRUE)
    means[n == 0] <- NA_real_
    sem <- sqrt(rowSums((values - means)^2, na.rm = TRUE) / (n * (n - 1)))
    sem[n < 2] <- NA_real_
    data.frame(Time = time, Status = s, N = n, Mean = means, SEM = sem)
  }))
}

# First qualifying observed run. Missing values and timestamp gaps break runs.
# Duration is last timestamp minus first timestamp, not sample count / rate.
responder_qualifying_run <- function(time, rise, threshold, duration_mode, duration_s,
                                     frames, max_gap_s) {
  empty <- list(start = NA_integer_, end = NA_integer_, confirmation = NA_integer_,
                samples = NA_integer_, duration = NA_real_)
  if (!length(time) || !is.finite(threshold) || threshold <= 0) return(empty)
  tol <- 8 * .Machine$double.eps * max(1, abs(time), duration_s)
  above <- is.finite(rise) & rise >= threshold
  first <- NA_integer_
  for (i in seq_along(time)) {
    if (!above[i]) { first <- NA_integer_; next }
    if (is.na(first) || (i > 1L && time[i] - time[i - 1L] > max_gap_s + tol)) first <- i
    qualified <- if (duration_mode == "seconds") time[i] - time[first] + tol >= duration_s else
      i - first + 1L >= frames
    if (qualified) {
      last <- i
      while (last < length(time) && above[last + 1L] &&
             time[last + 1L] - time[last] <= max_gap_s + tol) last <- last + 1L
      return(list(start = first, end = last, confirmation = i,
                  samples = last - first + 1L, duration = time[last] - time[first]))
    }
  }
  empty
}

# Whole-trace noise in the units of the trace: 1.4826 x the median absolute
# deviation, about their median, of the differences between consecutive samples,
# divided by sqrt(2).  It measures frame-to-frame noise only.  As for
# runs, a difference counts only when both samples are finite and no further
# apart than the gap limit.  Fewer differences than `min_samples` give no estimate.
responder_whole_trace_noise <- function(time, y, max_gap_s, min_samples) {
  tol <- 8 * .Machine$double.eps * max(1, abs(time))
  step <- diff(y)
  step <- step[is.finite(step) & diff(time) <= max_gap_s + tol]
  list(noise = if (length(step) >= min_samples) stats::mad(step) / sqrt(2) else NA_real_,
       samples = length(step))
}

responder_qc_flags <- function(...) {
  values <- unlist(list(...), use.names = FALSE)
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) return("")
  paste(unique(trimws(unlist(strsplit(values, ";", fixed = TRUE)))), collapse = "; ")
}

responder_window_calls_v2 <- function(dt, cells, field, window, cell_quality = NULL) {
  time <- dt[["Time"]]
  valid_time <- is.numeric(time) && length(time) >= 2L && all(is.finite(time)) && all(diff(time) > 0)
  start <- window$Start_s
  end <- window$End_s
  left <- window$Baseline_start_s
  baseline_end <- window$Baseline_end_s
  tol <- 8 * .Machine$double.eps * max(1, abs(c(left, baseline_end, start, end)))
  pre <- if (valid_time) which(time >= left - tol & time < baseline_end - tol) else integer()
  during <- if (valid_time) which(time >= start - tol & time <= end + tol) else integer()
  max_gap <- window$Max_gap_s
  reason <- if (!valid_time) {
    "Timestamps must be finite, strictly increasing and contain at least two observations"
  } else if (time[1] > left + tol || tail(time, 1L) < end - tol) {
    sprintf("Recording does not span the full baseline and response window (needs %g to %g s; recording covers %g to %g s)",
            left, end, time[1], tail(time, 1L))
  } else if (length(pre) < window$Min_baseline_samples) {
    sprintf("Fewer than %d prestimulus baseline samples", window$Min_baseline_samples)
  } else if (!length(during)) {
    "No observed samples in the response window"
  } else if (!is.finite(max_gap) || max(diff(c(left, time[pre], baseline_end)),
                                      diff(c(start, time[during], end))) > max_gap + tol) {
    "Sample gap exceeds the field limit in the baseline or response window"
  } else ""
  measurable <- !nzchar(reason)
  if (measurable) {
    if (window$Duration_mode == "samples" && length(during) < window$Consecutive_samples)
      reason <- "Too few response samples for the consecutive-sample rule"
    if (window$Duration_mode == "seconds" && diff(range(time[during])) + tol < window$Min_duration_s)
      reason <- "Observed response window is too short to assess the duration rule"
  }
  window$Window_clipped <- FALSE
  window$Window_warning <- if (window$Duration_mode == "seconds" &&
      is.finite(window$Resolved_interval_s) && window$Min_duration_s < window$Resolved_interval_s)
    "Requested duration is shorter than one nominal sample interval; timing is coarsely resolved" else ""
  window$Effective_end_s <- if (valid_time && time[1] <= left + tol && tail(time, 1L) >= end - tol) end else NA_real_
  window$Field_reason <- reason
  window$Requested_response_span_s <- end - start
  window$Response_observed_span_s <- if (length(during)) diff(range(time[during])) else 0
  window$Baseline_observed_span_s <- if (length(pre)) diff(range(time[pre])) else 0
  output_n <- max(1L, length(cells))
  row <- window[rep(1L, output_n), , drop = FALSE]
  out <- data.frame(Field = rep(field, output_n), Cell = if (length(cells)) cells else NA_character_, row,
    Status = rep("Unscorable", output_n), Reason = rep(reason, output_n),
    Baseline_mean = NA_real_, Baseline_SD = NA_real_, Baseline_SD_global = NA_real_,
    Baseline_samples = length(pre), Response_samples = length(during), Local_F0_relative = NA_real_,
    Local_reference_relative = NA_real_, Input_reference_value = NA_real_, Local_reference_value = NA_real_,
    Threshold_increase = NA_real_, Peak_increase = NA_real_,
    Floor_threshold = window$Floor_local_fraction, Noise_threshold = NA_real_, Noise_SD_used = NA_real_,
    Threshold_source = NA_character_, Margin = NA_real_, Onset_s = NA_real_, Confirmation_s = NA_real_,
    Peak_s = NA_real_, Run_start_s = NA_real_, Run_end_s = NA_real_, Run_duration_s = NA_real_,
    Run_samples = NA_integer_, Response_latency_s = NA_real_, Peak_latency_s = NA_real_,
    Window_peak_local_fraction = NA_real_, Window_AUC_fraction_s = NA_real_,
    # Transitional API aliases; portable exports use the modality-neutral names.
    Window_peak_local_dFF = NA_real_, Window_AUC_local_dFF_s = NA_real_,
    Window_AUC_start_s = NA_real_, Window_AUC_end_s = NA_real_, Window_AUC_covered_s = NA_real_,
    Window_AUC_coverage_fraction = NA_real_, AUC_method = "trapezoid_observed_samples_no_extrapolation",
    Eligible = FALSE, Measurements_available = FALSE, Response_window_complete = FALSE,
    QC_flags = "", Raw_F0 = NA_real_, Raw_baseline_SD = NA_real_, Raw_baseline_samples = NA_integer_,
    Local_F0_raw = NA_real_, Raw_QC_available = FALSE,
    Signal_kind = "unspecified", Input_mode = "fixed_reference_fraction",
    Normalized_units = "fraction", Normalization_reference = "fixed",
    row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
  # A sample_sd protocol keeps the engine 2.0.0 table.  The whole-trace estimate
  # is reported in columns appended after the existing ones.
  trace_noise_rule <- window$SD_estimator == "larger_of_baseline_sd_and_trace_noise"
  if (trace_noise_rule) {
    out$Whole_trace_noise <- NA_real_
    out$Whole_trace_noise_samples <- NA_integer_
    out$Noise_source <- NA_character_
  }
  # data.frame recycles scalar columns only for nonempty registrations.
  if (!length(cells)) out <- out[FALSE, , drop = FALSE]
  quality <- if (is.null(cell_quality)) NULL else as.data.frame(cell_quality, stringsAsFactors = FALSE)
  if (!is.null(quality) && "Field" %in% names(quality)) quality <- quality[quality$Field == field, , drop = FALSE]
  for (i in seq_along(cells)) {
    flags <- character()
    q <- if (!is.null(quality) && "Cell" %in% names(quality))
      quality[quality$Cell == cells[i], , drop = FALSE] else NULL
    if (!is.null(q) && nrow(q) > 1L) stop("Cell quality must contain one row per Field and Cell.")
    if (!is.null(q) && nrow(q)) {
      if ("F0" %in% names(q)) out$Raw_F0[i] <- q$F0[1]
      if ("Reference_value" %in% names(q)) out$Input_reference_value[i] <- q$Reference_value[1]
      if ("Baseline_SD_raw" %in% names(q)) out$Raw_baseline_SD[i] <- q$Baseline_SD_raw[1]
      if ("Baseline_samples" %in% names(q)) out$Raw_baseline_samples[i] <- q$Baseline_samples[1]
      if ("Raw_QC_available" %in% names(q)) out$Raw_QC_available[i] <- isTRUE(q$Raw_QC_available[1])
      for (column in c("Signal_kind", "Input_mode", "Normalized_units", "Normalization_reference"))
        if (column %in% names(q) && !is.na(q[[column]][1])) out[[column]][i] <- q[[column]][1]
      if ("QC_flags" %in% names(q)) flags <- c(flags, q$QC_flags[1])
      if ("Status" %in% names(q) && identical(q$Status[1], "Excluded")) {
        out$Status[i] <- "Excluded"
        out$Reason[i] <- if ("Reason" %in% names(q)) q$Reason[1] else "Explicit quality exclusion"
        out$QC_flags[i] <- responder_qc_flags(flags)
        next
      }
    }
    if (!out$Raw_QC_available[i]) flags <- c(flags, "RAW_QC_UNAVAILABLE")
    if (out$Normalized_units[i] != "fraction" || out$Normalization_reference[i] != "fixed") {
      out$Reason[i] <- "The response engine requires fixed-reference fractional input; convert the declared input units first"
      out$QC_flags[i] <- responder_qc_flags(flags, "UNSUPPORTED_SIGNAL_UNITS")
      next
    }
    if (nzchar(window$Window_warning)) flags <- c(flags, "DURATION_BELOW_SAMPLE_INTERVAL")
    out$QC_flags[i] <- responder_qc_flags(flags)
    y <- dt[[cells[i]]]
    if (is.null(y)) {
      out$Reason[i] <- if (!is.null(q) && nrow(q) && "Reason" %in% names(q) &&
          !is.na(q$Reason[1]) && nzchar(q$Reason[1])) q$Reason[1] else
        "Trace unavailable; inspect the import and raw-baseline quality report"
      next
    }
    if (!measurable) next
    if (!is.numeric(y) || length(y) != length(time) || any(!is.finite(y[c(pre, during)]))) {
      out$Reason[i] <- "Missing or nonfinite values in baseline or response window"
      out$QC_flags[i] <- responder_qc_flags(flags, "MISSING_WINDOW_VALUES")
      next
    }
    base <- mean(y[pre])
    local_f0 <- 1 + base
    out$Baseline_mean[i] <- base
    out$Local_F0_relative[i] <- local_f0
    out$Local_reference_relative[i] <- local_f0
    if (is.finite(out$Raw_F0[i])) out$Local_F0_raw[i] <- out$Raw_F0[i] * local_f0
    if (is.finite(out$Input_reference_value[i])) out$Local_reference_value[i] <- out$Input_reference_value[i] * local_f0
    if (!is.finite(local_f0) || local_f0 <= 0) {
      out$Reason[i] <- "Local baseline signal reference is nonpositive or nonfinite"
      out$QC_flags[i] <- responder_qc_flags(flags, "INVALID_LOCAL_BASELINE")
      next
    }
    noise_global <- stats::sd(y[pre])
    noise <- noise_global / local_f0
    if (!is.finite(noise)) {
      out$Reason[i] <- "Baseline variation is not finite"
      next
    }
    if (noise == 0) flags <- c(flags, "ZERO_BASELINE_SD")
    if (noise >= 1/3) flags <- c(flags, "BASELINE_SD_AT_LEAST_ONE_THIRD_LOCAL_F0")
    if (local_f0 <= sqrt(.Machine$double.eps)) flags <- c(flags, "VERY_SMALL_LOCAL_F0")
    estimate <- noise
    if (trace_noise_rule) {
      whole <- responder_whole_trace_noise(time, y, max_gap, window$Min_baseline_samples)
      trace_noise <- whole$noise / local_f0
      out$Whole_trace_noise[i] <- trace_noise
      out$Whole_trace_noise_samples[i] <- whole$samples
      # Too few differences: the baseline SD stands alone, and the cell says so.
      if (!is.finite(trace_noise)) flags <- c(flags, "WHOLE_TRACE_NOISE_UNAVAILABLE")
      larger <- is.finite(trace_noise) && trace_noise > noise
      if (larger) estimate <- trace_noise
      out$Noise_source[i] <- if (window$Noise_floor_local_fraction > estimate) "Noise floor" else
        if (larger) "Whole-trace noise" else "Baseline SD"
    }
    out$QC_flags[i] <- responder_qc_flags(flags)
    noise_used <- max(estimate, window$Noise_floor_local_fraction)
    noise_threshold <- window$SD_multiplier * noise_used
    threshold <- max(window$Floor_local_fraction, noise_threshold)
    rise <- (y[during] - base) / local_f0
    if (any(!is.finite(rise))) {
      out$Reason[i] <- "Local normalization produced nonfinite measurements"
      next
    }
    peak <- which.max(rise)
    out$Peak_s[i] <- time[during[peak]]
    out$Peak_latency_s[i] <- out$Peak_s[i] - window$Stimulus_onset_s
    out$Noise_threshold[i] <- noise_threshold
    out$Noise_SD_used[i] <- noise_used
    out$Threshold_source[i] <- if (noise_threshold > window$Floor_local_fraction) "Baseline noise" else "Floor"
    out$Baseline_SD[i] <- noise
    out$Baseline_SD_global[i] <- noise_global
    out$Threshold_increase[i] <- threshold
    out$Peak_increase[i] <- rise[peak]
    out$Window_peak_local_dFF[i] <- rise[peak]
    out$Window_peak_local_fraction[i] <- rise[peak]
    out$Window_AUC_start_s[i] <- time[during[1L]]
    out$Window_AUC_end_s[i] <- time[tail(during, 1L)]
    out$Window_AUC_covered_s[i] <- out$Window_AUC_end_s[i] - out$Window_AUC_start_s[i]
    out$Window_AUC_coverage_fraction[i] <- min(1, out$Window_AUC_covered_s[i] / (end - start))
    if (length(during) >= 2L) {
      out$Window_AUC_local_dFF_s[i] <- sum(diff(time[during]) * (head(rise, -1L) + tail(rise, -1L)) / 2)
      out$Window_AUC_fraction_s[i] <- out$Window_AUC_local_dFF_s[i]
    }
    out$Measurements_available[i] <- TRUE
    out$Response_window_complete[i] <- TRUE
    if (!is.finite(threshold) || threshold <= 0) {
      out$Reason[i] <- "Noise-only criterion has zero estimated noise; specify a justified nonzero measurement-noise floor"
      out$QC_flags[i] <- responder_qc_flags(flags, "ZERO_EFFECTIVE_THRESHOLD")
      next
    }
    out$Margin[i] <- rise[peak] / threshold
    if (nzchar(reason)) next
    run <- responder_qualifying_run(time[during], rise, threshold, window$Duration_mode,
                                    window$Min_duration_s, window$Consecutive_samples, max_gap)
    positive <- !is.na(run$start)
    if (positive) {
      out$Run_start_s[i] <- time[during[run$start]]
      out$Run_end_s[i] <- time[during[run$end]]
      out$Onset_s[i] <- out$Run_start_s[i]
      out$Confirmation_s[i] <- time[during[run$confirmation]]
      out$Run_samples[i] <- run$samples
      out$Run_duration_s[i] <- run$duration
      out$Response_latency_s[i] <- out$Onset_s[i] - window$Stimulus_onset_s
    }
    out$Eligible[i] <- TRUE
    out$Status[i] <- if (positive) "Responded" else "Did not respond"
    out$Reason[i] <- if (positive) "Sustained threshold crossing" else "No sustained threshold crossing"
  }
  attr(out, "window_diagnostics") <- window[, c("Effective_end_s", "Window_clipped", "Window_warning",
    "Field_reason", "Requested_response_span_s", "Response_observed_span_s", "Baseline_observed_span_s"), drop = FALSE]
  out
}

responder_field_results_v2 <- function(dt, cells, field, protocol, cell_quality = NULL, overrides = NULL) {
  responder_validate_protocol(protocol)
  if (anyDuplicated(cells)) stop("Cell identifiers must be unique within a field.")
  original_hash <- responder_protocol_hash(protocol, overrides)
  protocol <- responder_protocol_for_field(protocol, field, overrides)
  applied_hash <- responder_protocol_hash(protocol)
  protocol$Protocol_hash <- original_hash
  protocol$Applied_protocol_hash <- applied_hash
  intervals <- if (is.numeric(dt[["Time"]])) diff(dt[["Time"]]) else numeric()
  nominal <- if (length(intervals) && all(is.finite(intervals)) && all(intervals > 0)) stats::median(intervals) else NA_real_
  protocol$Resolved_interval_s <- ifelse(is.na(protocol$Expected_interval_s), nominal, protocol$Expected_interval_s)
  protocol$Gap_mode <- ifelse(!is.na(protocol$Max_gap_s), "Manual", ifelse(!is.na(protocol$Expected_interval_s),
    "Expected interval", "Median timestamp interval"))
  protocol$Max_gap_s[is.na(protocol$Max_gap_s)] <- 1.5 * protocol$Resolved_interval_s[is.na(protocol$Max_gap_s)]
  ratio <- protocol$Min_duration_s / protocol$Resolved_interval_s
  # A median interval of 0.19999999999999996 must still imply six observations
  # over one second, not seven, solely because of floating-point rounding.
  nominal_samples <- ceiling(ratio - 16 * .Machine$double.eps * pmax(1, abs(ratio))) + 1
  protocol$Nominal_min_samples <- ifelse(protocol$Duration_mode == "seconds", nominal_samples,
                                         protocol$Consecutive_samples)
  protocol$Response_reference <- "Fraction of local prestimulus signal"
  calls <- lapply(seq_len(nrow(protocol)), function(i)
    responder_window_calls_v2(dt, cells, field, protocol[i, , drop = FALSE], cell_quality))
  count <- function(rows, population, stimulus) {
    yes <- sum(rows$Status == "Responded")
    no <- sum(rows$Status == "Did not respond")
    n <- yes + no
    data.frame(Field = field, Stimulus = stimulus, Population = population,
      Registered = nrow(rows), Responded = yes, Nonresponders = no, Evaluable = n,
      Excluded = sum(rows$Status == "Excluded"), Unscorable = sum(rows$Status == "Unscorable"),
      Percent_responding = if (n) 100 * yes / n else NA_real_, stringsAsFactors = FALSE)
  }
  counts <- Map(function(rows, stimulus) count(rows, "Cells in field", stimulus), calls, protocol$Stimulus)
  control_name <- if (length(calls) == 2L) protocol$Stimulus[2L] else ""
  if (length(calls) == 2L) {
    positive <- calls[[2L]]$Cell[calls[[2L]]$Status == "Responded"]
    rows <- calls[[1L]][calls[[1L]]$Cell %in% positive, , drop = FALSE]
    counts[[3L]] <- count(rows, paste0(control_name, "-positive cells"), protocol$Stimulus[1L])
  }
  counts <- do.call(rbind, counts)
  source_row <- c(seq_along(calls), if (length(calls) == 2L) 1L)
  diagnostics <- do.call(rbind, lapply(calls, attr, "window_diagnostics"))
  counts <- cbind(counts, diagnostics[source_row, , drop = FALSE])
  if (length(calls) == 2L && !counts$Registered[3L]) {
    counts$Field_reason[3L] <- if (counts$Evaluable[2L] > 0L) paste("No", control_name, "responders") else {
      reason <- counts$Field_reason[2L]
      paste0(control_name, " control: ", if (nzchar(reason)) reason else "No evaluable cells")
    }
  }
  counts$Control_name <- control_name
  for (column in c("Effective_end_s", "Window_warning", "Field_reason")) {
    value <- if (length(calls) == 2L) diagnostics[[column]][2L] else if (column == "Effective_end_s") NA_real_ else ""
    counts[[paste0("Control_", tolower(column))]] <- value
    # Retain the old diagnostic names when the actual control is KCl.
    counts[[paste0("KCl_", tolower(column))]] <- if (tolower(control_name) == "kcl") value else
      if (column == "Effective_end_s") NA_real_ else ""
  }
  settings <- protocol[source_row, setdiff(names(protocol), "Stimulus"), drop = FALSE]
  list(calls = do.call(rbind, calls), counts = cbind(counts, settings))
}

# Aggregate fields within explicitly supplied biological replicates.  Fields are
# never silently promoted to biological n; unmapped fields remain visible.
responder_replicate_summary <- function(counts, metadata) {
  required_counts <- c("Field", "Stimulus", "Population", "Registered", "Responded", "Nonresponders",
                       "Evaluable", "Excluded", "Unscorable")
  if (!is.data.frame(counts) || !all(required_counts %in% names(counts)))
    stop("Replicate summaries require an applied response-count table.")
  if (anyDuplicated(paste(counts$Field, counts$Stimulus, counts$Population, sep = "\r")))
    stop("Select one applied analysis before aggregating repeated field/stimulus/population counts.")
  required <- c("Field", "Replicate")
  if (!is.data.frame(metadata) || !all(required %in% names(metadata)) || anyDuplicated(metadata$Field))
    stop("Replicate metadata requires one unique Field and its Replicate per row.")
  if (!"Group" %in% names(metadata)) metadata$Group <- "All recordings"
  matched <- match(counts$Field, metadata$Field)
  usable <- !is.na(matched) & !is.na(metadata$Replicate[matched]) & nzchar(trimws(metadata$Replicate[matched]))
  unassigned <- unique(counts$Field[!usable])
  rows <- counts[usable, , drop = FALSE]
  rows$Replicate <- metadata$Replicate[matched[usable]]
  rows$Group <- metadata$Group[matched[usable]]
  if (!nrow(rows)) return(list(replicates = data.frame(), groups = data.frame(), unassigned_fields = unassigned))
  if (anyNA(rows$Group) || any(!nzchar(trimws(rows$Group)))) stop("Assigned replicates require nonempty group labels.")
  group_by_replicate <- split(rows$Group, rows$Replicate)
  if (any(vapply(group_by_replicate, function(x) length(unique(x)) > 1L, logical(1))))
    stop("A biological replicate cannot belong to multiple groups.")
  count_columns <- c("Registered", "Responded", "Nonresponders", "Evaluable", "Excluded", "Unscorable")
  key <- interaction(rows[c("Group", "Replicate", "Stimulus", "Population")], drop = TRUE, lex.order = TRUE)
  replicates <- do.call(rbind, lapply(split(rows, key), function(x) {
    out <- x[1L, c("Group", "Replicate", "Stimulus", "Population"), drop = FALSE]
    out$Fields <- length(unique(x$Field))
    for (column in count_columns) out[[column]] <- sum(x[[column]])
    out$Percent_responding <- if (out$Evaluable) 100 * out$Responded / out$Evaluable else NA_real_
    out
  }))
  key <- interaction(replicates[c("Group", "Stimulus", "Population")], drop = TRUE, lex.order = TRUE)
  groups <- do.call(rbind, lapply(split(replicates, key), function(x) {
    valid <- x$Percent_responding[is.finite(x$Percent_responding)]
    data.frame(x[1L, c("Group", "Stimulus", "Population"), drop = FALSE],
      Replicates_registered = nrow(x), Replicates_evaluable = length(valid),
      Mean_replicate_percent = if (length(valid)) mean(valid) else NA_real_,
      SD_replicate_percent = stats::sd(valid), Responded = sum(x$Responded), Evaluable = sum(x$Evaluable),
      Cell_pooled_percent = if (sum(x$Evaluable)) 100 * sum(x$Responded) / sum(x$Evaluable) else NA_real_)
  }))
  rownames(replicates) <- rownames(groups) <- NULL
  list(replicates = replicates, groups = groups, unassigned_fields = unassigned)
}

# Optional annotations for historical classifications. The original result table
# is retained verbatim; the caller explicitly opts into extra measurement/QC
# columns computed over each legacy window's actual (possibly clipped) endpoint.
responder_legacy_annotations <- function(dt, cells, field, protocol, cell_quality = NULL,
                                         legacy_result = NULL) {
  if (responder_protocol_version(protocol) != 1L) stop("Legacy annotations require a version 1 protocol.")
  if (is.null(legacy_result)) legacy_result <- responder_field_results(dt, cells, field, protocol)
  original <- legacy_result$calls
  if (!nrow(original)) return(original)
  columns <- c("Stimulus_onset_s", "Baseline_start_s", "Baseline_end_s", "Baseline_samples", "Response_samples",
    "Baseline_SD_global", "Local_F0_relative", "Local_reference_relative", "Input_reference_value", "Local_reference_value",
    "Raw_F0", "Raw_baseline_SD", "Raw_baseline_samples", "Local_F0_raw", "Signal_kind", "Input_mode",
    "Normalized_units", "Normalization_reference",
    "Raw_QC_available", "QC_flags", "Measurements_available", "Response_window_complete", "Eligible",
    "Confirmation_s", "Run_duration_s", "Run_samples", "Response_latency_s", "Peak_latency_s",
    "Window_peak_local_fraction", "Window_AUC_fraction_s", "Window_peak_local_dFF", "Window_AUC_local_dFF_s",
    "Window_AUC_start_s", "Window_AUC_end_s", "Window_AUC_covered_s", "Window_AUC_coverage_fraction", "AUC_method")
  annotated <- vector("list", nrow(protocol))
  for (i in seq_len(nrow(protocol))) {
    old <- original[original$Role == protocol$Role[i], , drop = FALSE]
    effective_end <- if (nrow(old) && is.finite(old$Effective_end_s[1])) old$Effective_end_s[1] else protocol$End_s[i]
    if (effective_end <= protocol$Start_s[i]) effective_end <- protocol$End_s[i]
    windows <- data.frame(Role = "Stimulus", Stimulus = protocol$Stimulus[i],
      Baseline_start_s = protocol$Start_s[i] - protocol$Baseline_s[i], Baseline_end_s = protocol$Start_s[i],
      Stimulus_onset_s = protocol$Start_s[i], Start_s = protocol$Start_s[i], End_s = effective_end)
    candidate <- responder_protocol_v2("Legacy measurement annotations", windows,
      floor = protocol$Floor_local_fraction[i], noise = protocol$SD_multiplier[i], duration_mode = "samples",
      frames = protocol$Consecutive_samples[i], gap = if (is.na(protocol$Max_gap_s[i])) NULL else protocol$Max_gap_s[i],
      validation_status = "user_defined")
    annotation_engine <- responder_protocol_engine_version(candidate)
    measured <- responder_field_results_v2(dt, cells, field, candidate, cell_quality)$calls
    measured$Role <- protocol$Role[i]
    annotated[[i]] <- measured[, c("Role", "Cell", columns), drop = FALSE]
  }
  annotated <- do.call(rbind, annotated)
  matched <- match(paste(original$Role, original$Cell, sep = "\r"), paste(annotated$Role, annotated$Cell, sep = "\r"))
  additions <- setdiff(columns, names(original))
  out <- cbind(original, annotated[matched, additions, drop = FALSE])
  out$Eligible <- original$Status %in% c("Responded", "Did not respond")
  out$Run_duration_s <- original$Run_end_s - original$Run_start_s
  out$Response_latency_s <- original$Onset_s - out$Stimulus_onset_s
  out$Peak_latency_s <- original$Peak_s - out$Stimulus_onset_s
  out$Confirmation_s <- NA_real_
  out$Run_samples <- NA_integer_
  for (i in which(original$Status == "Responded")) {
    time <- dt[["Time"]]
    tol <- 8 * .Machine$double.eps * max(1, abs(c(original$Run_start_s[i], original$Run_end_s[i])))
    observed <- time[time >= original$Run_start_s[i] - tol & time <= original$Run_end_s[i] + tol]
    out$Run_samples[i] <- length(observed)
    if (length(observed) >= original$Consecutive_samples[i])
      out$Confirmation_s[i] <- observed[original$Consecutive_samples[i]]
  }
  # The measurements follow a Version 2 rule and carry its record: the earliest
  # engine release that defines it, so an unchanged rule gives an unchanged table.
  out$Annotation_engine_version <- annotation_engine
  out$Legacy_classification_unchanged <- TRUE
  out$Annotation_note <- "Window measurements use the recorded effective endpoint; historical classifications are unchanged"
  rownames(out) <- NULL
  out
}

# Exploratory reruns never mutate the applied protocol.  By default the control
# criterion stays fixed while the primary response definition is varied.
responder_sensitivity_results <- function(dt, cells, field, protocol, grid, cell_quality = NULL,
                                          role = "Stimulus") {
  if (responder_protocol_version(protocol) != 2L) stop("Sensitivity analysis requires a version 2 protocol.")
  if (!is.data.frame(grid) || !nrow(grid)) stop("Provide a nonempty sensitivity grid.")
  allowed <- c("Floor_local_fraction", "SD_multiplier", "Min_duration_s", "Consecutive_samples", "Duration_mode")
  if (!length(intersect(names(grid), allowed)) || length(setdiff(names(grid), c("Scenario", allowed))))
    stop("Sensitivity grid contains unsupported settings.")
  if (!role %in% protocol$Role) stop("Sensitivity role is not present in the protocol.")
  results <- lapply(seq_len(nrow(grid)), function(i) {
    candidate <- protocol
    for (column in intersect(names(grid), allowed)) candidate[[column]][candidate$Role == role] <- grid[[column]][i]
    responder_validate_protocol(candidate)
    result <- responder_field_results(dt, cells, field, candidate, cell_quality)
    scenario <- if ("Scenario" %in% names(grid)) as.character(grid$Scenario[i]) else paste0("Scenario ", i)
    for (name in c("calls", "counts")) {
      result[[name]]$Scenario <- scenario
      result[[name]]$Analysis_kind <- "sensitivity"
    }
    result
  })
  list(calls = do.call(rbind, lapply(results, `[[`, "calls")),
       counts = do.call(rbind, lapply(results, `[[`, "counts")))
}

# These are observed apparent responses in explicitly supplied negative-control
# windows, not p-values or proof that a preset is validated.
responder_control_diagnostics <- function(dt, cells, field, protocol, windows, cell_quality = NULL) {
  if (responder_protocol_version(protocol) != 2L) stop("Control diagnostics require a version 2 protocol.")
  required <- c("Baseline_start_s", "Baseline_end_s", "Stimulus_onset_s", "Start_s", "End_s")
  if (!is.data.frame(windows) || !nrow(windows) || !all(required %in% names(windows)))
    stop("Control diagnostics require explicit baseline, stimulus-onset and response windows.")
  results <- lapply(seq_len(nrow(windows)), function(i) {
    candidate <- protocol[protocol$Role == "Stimulus", , drop = FALSE]
    attr(candidate, "field_overrides") <- NULL
    for (column in required) candidate[[column]] <- windows[[column]][i]
    candidate$Baseline_s <- candidate$Baseline_end_s - candidate$Baseline_start_s
    responder_validate_protocol(candidate)
    result <- responder_field_results(dt, cells, field, candidate, cell_quality)
    label <- if ("Diagnostic_window" %in% names(windows)) as.character(windows$Diagnostic_window[i]) else paste0("Control window ", i)
    for (name in c("calls", "counts")) {
      result[[name]]$Diagnostic_window <- label
      result[[name]]$Analysis_kind <- "negative_control_diagnostic"
    }
    result$counts$Apparent_response_percent <- result$counts$Percent_responding
    result
  })
  list(calls = do.call(rbind, lapply(results, `[[`, "calls")),
       counts = do.call(rbind, lapply(results, `[[`, "counts")))
}
