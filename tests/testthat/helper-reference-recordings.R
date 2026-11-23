# Cell-by-cell comparison of the response engine with an independent calculation.
# Used by the unit tests on the synthetic set in tests/fixtures/response-protocols/
# reference-recordings and by tests/reference-recordings.R on recordings kept
# outside the repository.  Base R and the engine sources only.
#
# A reference set is a directory with
#   recordings.csv      Recording, Fluorescence_file, Registry_file (may be blank),
#                       Sampling_hz, Baseline_first_frame, Baseline_last_frame
#   <Fluorescence_file> Frame (1, 2, ...) and one background-corrected column per cell
#   <Registry_file>     Cell, Background_pixels; a cell without background is excluded
#   expected_calls.csv  Recording, Cell, Evaluable, Baseline_SD, Whole_trace_noise,
#                       Threshold_sample_sd, Threshold_larger, Onset_frame_sample_sd,
#                       Onset_frame_larger (onset blank = did not respond)
# The baseline is the stated frames, the stimulus follows its last frame, and the
# response window runs to the end: 10% floor, 3 SD, two consecutive samples.
reference_read <- function(directory, name) utils::read.csv(file.path(directory, name),
  check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN"))

# One recording as the engine receives it: normalized traces, quality rows and windows.
reference_recording_input <- function(directory, recording) {
  raw <- reference_read(directory, recording$Fluorescence_file)
  if (!identical(as.numeric(raw$Frame), as.numeric(seq_len(nrow(raw))))) stop("Frames must be consecutive, starting at 1.")
  cells <- setdiff(names(raw), "Frame")
  frames <- c(recording$Baseline_first_frame, recording$Baseline_last_frame)
  time <- (raw$Frame - 1) / recording$Sampling_hz
  no_background <- character()
  if (!is.na(recording$Registry_file)) {
    registry <- reference_read(directory, recording$Registry_file)
    no_background <- registry$Cell[!(is.finite(registry$Background_pixels) & registry$Background_pixels > 0)]
  }
  dt <- data.frame(Time = time)
  quality <- vector("list", length(cells))
  for (i in seq_along(cells)) {
    normalized <- normalize_signal_trace(as.numeric(raw[[cells[i]]]), frames, "corrected_fluorescence")
    q <- cbind(Field = recording$Recording, Cell = cells[i], normalized$quality, stringsAsFactors = FALSE)
    if (cells[i] %in% no_background) {
      normalized$trace <- NULL
      q$Status <- "Excluded"
      q$Reason <- "Local background unavailable for this ROI"
    }
    if (!is.null(normalized$trace)) dt[[cells[i]]] <- normalized$trace
    quality[[i]] <- q
  }
  start <- time[frames[2] + 1L]
  list(field = recording$Recording, cells = cells, dt = dt, quality = do.call(rbind, quality),
    windows = data.frame(Role = "Stimulus", Stimulus = "Stimulus", Baseline_start_s = time[frames[1]],
      Baseline_end_s = start, Stimulus_onset_s = start, Start_s = start, End_s = time[length(time)]))
}

reference_recordings_check <- function(directory, tolerance = 1e-9) {
  recordings <- reference_read(directory, "recordings.csv")
  expected <- reference_read(directory, "expected_calls.csv")
  estimators <- c(sample_sd = "sample_sd", larger = "larger_of_baseline_sd_and_trace_noise")
  cells <- do.call(rbind, lapply(seq_len(nrow(recordings)), function(i) {
    input <- reference_recording_input(directory, recordings[i, ])
    out <- data.frame(Recording = input$field, Cell = input$cells, stringsAsFactors = FALSE)
    for (rule in names(estimators)) {
      protocol <- responder_protocol_v2("Reference recordings", input$windows, duration_mode = "samples",
        frames = 2L, sd_estimator = estimators[[rule]])
      calls <- responder_field_results(input$dt, input$cells, input$field, protocol, input$quality)$calls
      out[[paste0("Status_", rule)]] <- calls$Status
      out[[paste0("Threshold_", rule)]] <- calls$Threshold_increase
      out[[paste0("Onset_frame_", rule)]] <- match(calls$Onset_s, input$dt$Time)
      out$Baseline_SD <- calls$Baseline_SD
      if (rule == "larger") out$Whole_trace_noise <- calls$Whole_trace_noise
    }
    out
  }))
  at <- match(paste(cells$Recording, cells$Cell, sep = "\r"), paste(expected$Recording, expected$Cell, sep = "\r"))
  if (anyNA(at) || nrow(cells) != nrow(expected)) stop("Expected calls do not list exactly the measured cells.")
  expected <- expected[at, , drop = FALSE]
  same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= tolerance)
  summary <- differences <- list()
  for (rule in names(estimators)) {
    status <- cells[[paste0("Status_", rule)]]
    evaluable <- status %in% c("Responded", "Did not respond")
    onset <- expected[[paste0("Onset_frame_", rule)]]
    call_agrees <- evaluable == expected$Evaluable & (status == "Responded") == (expected$Evaluable & !is.na(onset))
    # Thresholds, noise and onset are compared where both calculations scored the cell.
    both <- evaluable & expected$Evaluable
    numbers_agree <- !both | (same(cells[[paste0("Onset_frame_", rule)]], onset) &
      same(cells[[paste0("Threshold_", rule)]], expected[[paste0("Threshold_", rule)]]) &
      same(cells$Baseline_SD, expected$Baseline_SD) &
      (rule != "larger" | same(cells$Whole_trace_noise, expected$Whole_trace_noise)))
    summary[[rule]] <- do.call(rbind, lapply(unique(cells$Recording), function(name) {
      rows <- cells$Recording == name
      data.frame(Recording = name, SD_estimator = estimators[[rule]], Cells = sum(rows),
        Evaluable = sum(evaluable[rows]), Responded = sum(status[rows] == "Responded"),
        Expected_responded = sum(expected$Evaluable[rows] & !is.na(onset[rows])),
        Calls_agree = sum(call_agrees[rows]), Calls_differ = sum(!call_agrees[rows]),
        Numbers_differ = sum(!numbers_agree[rows]), stringsAsFactors = FALSE)
    }))
    bad <- !call_agrees | !numbers_agree
    differences[[rule]] <- data.frame(SD_estimator = rep(estimators[[rule]], sum(bad)),
      Recording = cells$Recording[bad], Cell = cells$Cell[bad], Status = status[bad],
      Threshold = cells[[paste0("Threshold_", rule)]][bad], Onset_frame = cells[[paste0("Onset_frame_", rule)]][bad],
      Expected_evaluable = expected$Evaluable[bad], Expected_threshold = expected[[paste0("Threshold_", rule)]][bad],
      Expected_onset_frame = onset[bad], stringsAsFactors = FALSE)
  }
  list(cells = cells, summary = do.call(rbind, c(summary, make.row.names = FALSE)),
       differences = do.call(rbind, c(differences, make.row.names = FALSE)))
}
