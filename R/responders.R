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
    "Recording does not span the full baseline and response window"
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

responder_field_results <- function(dt, cells, field, protocol) {
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
