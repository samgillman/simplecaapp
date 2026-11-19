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
  transform(windows, Baseline_s = baseline, Floor_dFF0 = floor, SD_multiplier = noise,
            Consecutive_samples = frames, Max_gap_s = if (is.null(gap)) NA_real_ else gap)
}

responder_window_calls <- function(dt, cells, field, window) {
  time <- dt[["Time"]]
  start <- window$Start_s
  end <- window$End_s
  left <- start - window$Baseline_s
  # Include timestamps that differ from a boundary only by machine rounding.
  tol <- 8 * .Machine$double.eps * max(1, abs(c(left, start, end)))
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
  out <- data.frame(Field = field, Cell = cells, window, Status = "Unscorable",
                    Reason = reason, Baseline_mean = NA_real_,
                    Baseline_SD = NA_real_, Threshold_increase = NA_real_, Peak_increase = NA_real_,
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
    threshold <- max(window$Floor_dFF0, window$SD_multiplier * noise)
    rise <- (y[during] - base) / local_f0
    runs <- rle(rise >= threshold)
    positive <- any(runs$values & runs$lengths >= window$Consecutive_samples)
    out$Status[i] <- if (positive) "Responded" else "Did not respond"
    out$Reason[i] <- if (positive) "Sustained threshold crossing" else "No sustained threshold crossing"
    out$Baseline_mean[i] <- base
    out$Baseline_SD[i] <- noise
    out$Threshold_increase[i] <- threshold
    out$Peak_increase[i] <- max(rise)
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
  settings <- protocol[match(counts$Stimulus, protocol$Stimulus), setdiff(names(protocol), c("Role", "Stimulus")), drop = FALSE]
  list(calls = do.call(rbind, calls), counts = cbind(counts, settings))
}
