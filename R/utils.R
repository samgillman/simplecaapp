# ============================ Helper Functions =============================
# Note: this file is sourced by app.R (which attaches all packages) and by the
# test suite, so it must not attach packages itself — use :: qualification.

# Custom operator for handling NULL values
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Check whether an object contains usable data for outputs.
has_data <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.data.frame(x)) return(nrow(x) > 0)
  length(x) > 0
}

# Human-friendly display of file/group identifiers (underscores read as
# spaces in plot titles and axis labels; data keys stay untouched).
pretty_label <- function(x) gsub("_+", " ", x)

# Compact auto plot title: the joined group names while they fit on one
# line, otherwise a plain count so many-file uploads don't overflow the plot
auto_plot_title <- function(groups, max_chars = 60) {
  if (is.null(groups) || length(groups) == 0) {
    return("")
  }
  joined <- pretty_label(paste(groups, collapse = ", "))
  if (nchar(joined) <= max_chars) joined else sprintf("%d groups", length(groups))
}

# Treat a cleared title input as an intentional request for no plot title.
# Returning NULL lets ggplot remove the title grob instead of reserving space
# for an empty string. Automatic dataset titles are populated into the input by
# the module observer and restored explicitly by its Reset button.
optional_plot_title <- function(title) {
  if (is.null(title) || length(title) != 1 || is.na(title) || !nzchar(trimws(title))) {
    return(NULL)
  }
  title
}

# Format summary-table metrics without implying that censored response widths
# are exact observations. Exact FWHM/Half-Width rows describe recovered traces;
# lower-bound rows describe censored traces separately.
format_timecourse_metric_display <- function(metric_col, n, mean_value, sem_value,
                                              total_n, censored_n) {
  metric_col <- as.character(metric_col)
  n <- suppressWarnings(as.integer(n))
  total_n <- suppressWarnings(as.integer(total_n))
  censored_n <- suppressWarnings(as.integer(censored_n))
  exact_metric <- metric_col %in% c("FWHM", "Half_Width")

  if (n > 0) {
    estimate <- sprintf("%.4g ± %.4g", mean_value, sem_value)
    if (identical(metric_col, "FWHM_Lower_Bound")) {
      return(sprintf("%s (n=%d censored)", estimate, n))
    }
    if (exact_metric && censored_n > 0) {
      return(sprintf("%s (n=%d exact; %d censored)", estimate, n, censored_n))
    }
    return(sprintf("%s (n=%d)", estimate, n))
  }

  if (exact_metric && censored_n > 0 && total_n > 0) {
    return(sprintf(
      "Not estimable — 0/%d exact; %d/%d right-censored",
      total_n, censored_n, total_n
    ))
  }
  "— (n=0)"
}

# Wrap long categorical axis labels onto multiple lines so side-by-side
# group names don't collide
wrap_label <- function(x, width = 18) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
         character(1), USE.NAMES = FALSE)
}

#' Deliver a generated file to the visitor's browser as a direct save.
#'
#' Shiny's download links are served over HTTP, which in the WebAssembly
#' build routes through the shinylive service worker and fails, so every
#' download in this app instead ships its bytes over the live Shiny
#' connection to a small client-side handler that saves them as a file.
#' The same path works identically in desktop R.
#'
#' @param session Shiny session.
#' @param filename Name the browser should save the file as.
#' @param writer function(path) that writes the file content to path.
browser_download <- function(session, filename, writer) {
  # Keep the target extension: writers like ggsave infer their output
  # device from it
  ext <- tools::file_ext(filename)
  path <- tempfile(fileext = if (nzchar(ext)) paste0(".", ext) else "")
  on.exit(unlink(path), add = TRUE)
  ok <- tryCatch(
    {
      writer(path)
      TRUE
    },
    error = function(e) {
      shiny::showNotification(
        paste("Download failed:", conditionMessage(e)),
        type = "error", duration = 8
      )
      FALSE
    }
  )
  if (ok && file.exists(path)) {
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    session$sendCustomMessage("simpleca_save_file", list(
      filename = filename,
      b64 = jsonlite::base64_enc(bytes)
    ))
  }
  invisible(NULL)
}

#' Drop-in visual replacement for shiny::downloadButton whose clicks are
#' handled by register_browser_download() instead of an HTTP download link.
browser_download_button <- function(outputId, label = "Download", class = NULL, ...) {
  shiny::actionButton(outputId, label, icon = shiny::icon("download"), class = class, ...)
}

#' Drop-in replacement for a downloadHandler registration: same filename
#' and content functions, but delivery goes through browser_download().
#'
#' @param input,session Shiny module input/session.
#' @param id Button input id (unnamespaced, as used with output$ before).
#' @param filename Filename string or zero-argument function returning one.
#' @param content function(file) writing the file content.
register_browser_download <- function(input, session, id, filename, content) {
  observeEvent(input[[id]], {
    fname <- tryCatch(
      if (is.function(filename)) filename() else filename,
      error = function(e) NULL
    )
    if (is.null(fname) || !nzchar(fname)) {
      shiny::showNotification("Download failed: could not build the file name.",
                              type = "error", duration = 8)
      return(invisible(NULL))
    }
    browser_download(session, fname, content)
  }, ignoreInit = TRUE)
}

#' Write a ZIP archive using only base R.
#'
#' The browser (webR) build has no system zip tool and cannot rely on
#' compiled archive packages, so ZIP downloads are assembled directly.
#' Each file is passed through gzfile(), whose RFC 1952 container supplies
#' both the raw DEFLATE stream and, in its trailer, the CRC32 that the ZIP
#' format requires. (memCompress "gzip" is unsuitable: it emits a zlib
#' stream with an Adler-32 checksum, not a gzip container.)
#'
#' @param zipfile Output path.
#' @param files Character vector of files to store (flat, by basename).
write_zip_archive <- function(zipfile, files) {
  con <- file(zipfile, "wb")
  on.exit(close(con), add = TRUE)

  int2 <- function(x) writeBin(as.integer(x), con, size = 2, endian = "little")
  int4 <- function(x) writeBin(as.integer(x), con, size = 4, endian = "little")

  gzip_bytes <- function(data) {
    gz_path <- tempfile(fileext = ".gz")
    on.exit(unlink(gz_path), add = TRUE)
    gz_con <- gzfile(gz_path, "wb")
    writeBin(data, gz_con)
    close(gz_con)
    readBin(gz_path, "raw", n = file.info(gz_path)$size)
  }

  entries <- list()
  offset <- 0
  for (path in files) {
    data <- readBin(path, "raw", n = file.info(path)$size)
    gz <- gzip_bytes(data)
    n <- length(gz)
    # Minimal fixed-length gzip header expected: magic 1f 8b, FLG 0
    stopifnot(n >= 18, gz[1] == as.raw(0x1f), gz[2] == as.raw(0x8b), gz[4] == as.raw(0))
    crc <- gz[(n - 7):(n - 4)]                       # gzip trailer: CRC32, little-endian
    deflated <- gz[seq.int(11, n - 8)]               # strip 10-byte header + 8-byte trailer
    name <- charToRaw(basename(path))

    # Local file header (version 2.0, method 8 = deflate, zeroed timestamps)
    int4(0x04034b50); int2(20); int2(0); int2(8); int2(0); int2(0)
    writeBin(crc, con)
    int4(length(deflated)); int4(length(data))
    int2(length(name)); int2(0)
    writeBin(name, con)
    writeBin(deflated, con)

    entries[[length(entries) + 1]] <- list(
      name = name, crc = crc,
      csize = length(deflated), usize = length(data), offset = offset
    )
    offset <- offset + 30L + length(name) + length(deflated)
  }

  cd_start <- offset
  for (e in entries) {
    int4(0x02014b50); int2(20); int2(20); int2(0); int2(8); int2(0); int2(0)
    writeBin(e$crc, con)
    int4(e$csize); int4(e$usize)
    int2(length(e$name)); int2(0); int2(0); int2(0); int2(0)
    int4(0); int4(e$offset)
    writeBin(e$name, con)
    offset <- offset + 46L + length(e$name)
  }

  # End of central directory
  int4(0x06054b50); int2(0); int2(0)
  int2(length(entries)); int2(length(entries))
  int4(offset - cd_start); int4(cd_start); int2(0)
  invisible(zipfile)
}

# Human-friendly display of cell identifiers. ImageJ ROI exports name
# columns "Mean1", "Mean2", ... — each is cell N, so display it that way.
# Display-layer only: exported tables and data structures keep raw names.
pretty_cell_label <- function(x) sub("^Mean([0-9]+)$", "Cell \\1", x)

# Compute a "nice" Y-axis step size from data range, targeting ~5 breaks.
compute_auto_y_step <- function(y_range) {
  span <- y_range[2] - min(0, y_range[1])
  if (!is.finite(span) || span <= 0) return(0.5)
  raw_step <- span / 5
  mag <- 10^floor(log10(raw_step))
  r <- raw_step / mag
  nice <- if (r <= 1.5) 1 else if (r <= 3.5) 2.5 else if (r <= 7.5) 5 else 10
  nice * mag
}

# Build a zero-anchored heatmap color scale. A missing, invalid, or zero
# interval keeps the existing automatic behavior; a positive interval gives
# users direct control over the colorbar tick spacing. Limiting the number of
# intervals prevents an accidental tiny value from generating an enormous
# legend and freezing the browser build.
compute_heatmap_scale <- function(max_value, interval = 0, max_intervals = 100L) {
  max_value <- suppressWarnings(as.numeric(max_value)[1])
  if (!is.finite(max_value)) {
    stop("Heatmap color scale requires a finite maximum value.", call. = FALSE)
  }
  max_value <- max(0, max_value)

  requested <- suppressWarnings(as.numeric(interval)[1])
  use_custom <- is.finite(requested) && requested > 0
  step <- if (use_custom) requested else compute_auto_y_step(c(0, max_value))

  max_intervals <- suppressWarnings(as.integer(max_intervals)[1])
  if (!is.finite(max_intervals) || max_intervals < 1L) max_intervals <- 100L

  # The tolerance avoids adding an extra interval when floating-point
  # arithmetic puts an exact multiple infinitesimally above its boundary.
  interval_count <- max(1L, ceiling((max_value / step) - 1e-9))
  if (interval_count > max_intervals) {
    stop(
      sprintf(
        "Color scale interval is too small for this dataset (%d intervals; maximum %d).",
        interval_count, max_intervals
      ),
      call. = FALSE
    )
  }

  breaks <- signif(seq.int(0L, interval_count) * step, 12)
  list(
    step = step,
    upper = breaks[length(breaks)],
    breaks = breaks,
    automatic = !use_custom
  )
}

# Compute evenly spaced Y-axis breaks for a finite range. The supplied endpoints
# are always breaks. When expand is TRUE, they first move out to nice values so
# automatic axes cover the full data range without awkward endpoint labels.
compute_even_y_breaks <- function(y_range, expand = FALSE) {
  if (length(y_range) != 2 || any(!is.finite(y_range))) return(numeric(0))

  limits <- sort(as.numeric(y_range))
  span <- limits[2] - limits[1]
  if (span <= 0) return(limits[1])

  # compute_auto_y_step() normally anchors positive ranges at zero. Base the
  # step on the span here so offset limits such as 10-11 still get ~5 gaps.
  step <- compute_auto_y_step(c(0, span))
  tolerance <- 1e-9

  if (isTRUE(expand)) {
    limits[1] <- floor((limits[1] / step) + tolerance) * step
    limits[2] <- ceiling((limits[2] / step) - tolerance) * step
    span <- limits[2] - limits[1]
  }

  interval_count <- max(1L, round(span / step))
  as.numeric(seq(limits[1], limits[2], length.out = interval_count + 1L))
}

#' Calculate the Y range trained by visible time-course layers
#'
#' @param summary_df Summary data containing mean_dFF0 and sem_dFF0.
#' @param trace_df Optional individual-trace data containing dFF0.
#' @param show_traces Whether the individual trace layer is visible.
#' @param show_average Whether the average line is visible.
#' @param show_ribbon Whether the SEM ribbon is visible.
#' @return A finite numeric range, or c(NA, NA) if nothing is available.
timecourse_visible_y_range <- function(summary_df, trace_df = NULL,
                                       show_traces = TRUE,
                                       show_average = TRUE,
                                       show_ribbon = TRUE) {
  values <- numeric()
  if (isTRUE(show_traces) && !is.null(trace_df) && "dFF0" %in% names(trace_df)) {
    values <- c(values, trace_df$dFF0)
  }
  if (isTRUE(show_average) && !is.null(summary_df) &&
      "mean_dFF0" %in% names(summary_df)) {
    values <- c(values, summary_df$mean_dFF0)
    if (isTRUE(show_ribbon) && "sem_dFF0" %in% names(summary_df)) {
      values <- c(
        values,
        summary_df$mean_dFF0 - summary_df$sem_dFF0,
        summary_df$mean_dFF0 + summary_df$sem_dFF0
      )
    }
  }

  values <- values[is.finite(values)]
  if (length(values) == 0) return(c(NA_real_, NA_real_))
  range(values)
}

# --------------------------- Export helpers ---------------------------

#' Sanitize a value so it can safely be used inside a filename
#' @param x Value to sanitize (character/numeric)
#' @param fallback Value to return when x is NULL/empty/unsuitable
#' @return A safe filename component or fallback/NULL
sanitize_filename_component <- function(x, fallback = NULL) {
  if (is.null(x) || is.na(x)) {
    return(fallback)
  }
  if (is.numeric(x)) x <- format(x, trim = TRUE, scientific = FALSE)
  if (!nzchar(x)) {
    return(fallback)
  }
  cleaned <- gsub("[^A-Za-z0-9_-]+", "_", x)
  cleaned <- gsub("_+", "_", cleaned)
  cleaned <- gsub("^_|_$", "", cleaned)
  if (!nzchar(cleaned)) fallback else cleaned
}

#' Base name (without extension) pulled from the first uploaded file
#' @param rv App reactiveValues list
#' @return A sanitized base name with a default of "data"
export_base_name <- function(rv) {
  if (!is.null(rv$files) && nrow(rv$files) > 0 && "name" %in% names(rv$files)) {
    # One file: exports lead with its original name. Several files: no
    # single file owns the export, so lead with the group count instead
    # of misleadingly naming everything after the first upload
    if (nrow(rv$files) == 1) {
      name <- tools::file_path_sans_ext(basename(rv$files$name[1]))
      sanitize_filename_component(name, "data")
    } else {
      sprintf("%d_groups", nrow(rv$files))
    }
  } else {
    "data"
  }
}

#' Helper to compose a descriptive export filename
#' @param rv Reactive values (for deriving base name)
#' @param parts Character vector of additional filename parts
#' @param ext File extension (without dot)
#' @param include_date Whether to append today's date automatically
#' @param base Optional base-name override (e.g. a specific group's name for
#'   per-group downloads); defaults to the upload-derived base
#' @return A filename string like "dataset_context_2025-11-02.csv"
build_export_filename <- function(rv, parts = character(), ext = "csv", include_date = TRUE, base = NULL) {
  base <- base %||% export_base_name(rv)
  components <- c(base, parts)
  sanitized <- unlist(lapply(components, sanitize_filename_component, fallback = NULL), use.names = FALSE)
  if (include_date) {
    sanitized <- c(sanitized, format(Sys.Date()))
  }
  sprintf("%s.%s", paste(sanitized, collapse = "_"), ext)
}

# Safely read csv or excel files into a data.table
safe_read <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    # First pass: read header-less to find a Time or Frame column
    # Read first 20 lines to inspect structure
    tryCatch(
      {
        preview <- readxl::read_excel(path, n_max = 20, col_names = FALSE, .name_repair = "minimal")

        # Find row with "Time" or "Frame" (case-insensitive)
        header_row <- 0
        for (i in seq_len(nrow(preview))) {
          pass_row <- as.character(preview[i, ])
          if (any(grepl("^(time|frame)", pass_row, ignore.case = TRUE))) {
            header_row <- i
            break
          }
        }

        # If we found a likely header row > 1, skip lines
        skip_n <- if (header_row > 1) header_row - 1 else 0

        # Re-read with correct skip
        dt <- data.table::as.data.table(readxl::read_excel(path, skip = skip_n, .name_repair = "minimal"))
        return(dt)
      },
      error = function(e) {
        warning("Excel read failed: ", e$message)
        return(data.table::data.table())
      }
    )
  } else {
    data.table::fread(path)
  }
}

#' Put a validated time vector first without consuming a cell trace
#'
#' A genuine Time column is preferred. A Frame column—or an unnamed sequential
#' first column from a common ImageJ CSV export—is converted to elapsed seconds
#' using the sampling rate. If none is present, a new Time column is generated
#' and every uploaded column is retained as a potential trace.
#'
#' @param dt A table containing trace data.
#' @param time_col Optional explicit name of the elapsed-time column.
#' @param frame_col Optional explicit name of the frame-index column.
#' @param sampling_rate Sampling frequency in Hz, used for Frame conversion or
#'   generated Time values.
#' @param force_generated Ignore automatic Time/Frame detection and generate
#'   elapsed time from row number and `sampling_rate`.
#' @return A data.table whose first column is numeric `Time`. The `time_info`
#'   attribute describes whether Time was supplied, converted, or generated.
ensure_time_first <- function(dt, time_col = NULL, frame_col = NULL,
                              sampling_rate = 1, force_generated = FALSE) {
  dt <- data.table::copy(data.table::as.data.table(dt))
  if (ncol(dt) == 0) {
    return(dt)
  }

  sampling_rate <- suppressWarnings(as.numeric(sampling_rate)[1])
  if (!is.finite(sampling_rate) || sampling_rate <= 0) {
    stop("Sampling rate must be a finite number greater than zero.")
  }

  explicit_time <- !is.null(time_col)
  explicit_frame <- !is.null(frame_col)
  if (sum(c(explicit_time, explicit_frame, isTRUE(force_generated))) > 1) {
    stop("Choose only one Time source: elapsed-time column, frame column, or generated Time.")
  }

  cols <- names(dt)
  normalized <- gsub("[^a-z0-9]+", "", tolower(cols))

  generated_time <- function() {
    seq(0, by = 1 / sampling_rate, length.out = nrow(dt))
  }

  if (isTRUE(force_generated)) {
    if ("Time" %in% names(dt)) {
      stop("A trace named 'Time' cannot be analyzed while generated Time is selected; exclude that column first.")
    }
    dt[, Time := generated_time()]
    data.table::setcolorder(dt, c("Time", setdiff(names(dt), "Time")))
    attr(dt, "time_info") <- list(
      source = "generated_selected",
      column = NULL,
      sampling_rate = sampling_rate
    )
    return(dt)
  }

  if (explicit_time) {
    if (length(time_col) != 1 || is.na(time_col) || !(time_col %in% cols)) {
      stop("The selected Time column was not found in the uploaded data.")
    }
    time_idx <- match(time_col, cols)
  } else if (explicit_frame) {
    time_idx <- NA_integer_
  } else {
    time_candidates <- which(grepl("^time", normalized, ignore.case = TRUE))
    time_idx <- if (length(time_candidates) > 0) time_candidates[1] else NA_integer_
  }

  if (explicit_frame) {
    if (length(frame_col) != 1 || is.na(frame_col) || !(frame_col %in% cols)) {
      stop("The selected Frame column was not found in the uploaded data.")
    }
    frame_idx <- match(frame_col, cols)
  } else if (explicit_time) {
    frame_idx <- NA_integer_
  } else {
    frame_candidates <- which(grepl(
      "^frames?($|number$|index$|no$)", normalized,
      ignore.case = TRUE
    ))
    frame_idx <- if (length(frame_candidates) > 0) frame_candidates[1] else NA_integer_
  }
  inferred_frame <- FALSE

  # ImageJ-style CSV exports commonly leave the frame-index header blank.
  # fread repairs that blank name to V1 (other readers may use X or ...1).
  # Infer it as Frame only when it is the first column and is exactly a
  # consecutive integer index; an ordinary trace named V1 is otherwise kept.
  if (is.na(frame_idx) && length(cols) > 0 &&
      normalized[1] %in% c("", "v1", "x", "x1", "1", "unnamed", "unnamed0")) {
    first_values <- suppressWarnings(as.numeric(dt[[1]]))
    consecutive_index <- length(first_values) >= 2 &&
      all(is.finite(first_values)) &&
      all(abs(first_values - round(first_values)) < sqrt(.Machine$double.eps)) &&
      all(abs(diff(first_values) - 1) < sqrt(.Machine$double.eps))
    if (consecutive_index) {
      frame_idx <- 1L
      inferred_frame <- TRUE
    }
  }

  if (!is.na(time_idx)) {
    original_name <- cols[time_idx]
    values <- suppressWarnings(as.numeric(dt[[time_idx]]))
    valid <- length(values) == nrow(dt) &&
      all(is.finite(values)) &&
      (length(values) < 2 || all(diff(values) > 0))

    if (!valid && explicit_time) {
      stop("The selected Time column must contain finite, strictly increasing numeric values.")
    }

    data.table::setcolorder(dt, c(time_idx, setdiff(seq_len(ncol(dt)), time_idx)))
    data.table::setnames(dt, 1, "Time")
    dt[[1]] <- if (valid) values else generated_time()
    attr(dt, "time_info") <- list(
      source = if (valid) "time" else "generated_invalid_time",
      column = original_name,
      sampling_rate = sampling_rate
    )
    return(dt)
  }

  if (!is.na(frame_idx)) {
    original_name <- cols[frame_idx]
    frames <- suppressWarnings(as.numeric(dt[[frame_idx]]))
    valid <- length(frames) == nrow(dt) &&
      all(is.finite(frames)) &&
      (length(frames) < 2 || all(diff(frames) > 0))
    if (!valid && explicit_frame) {
      stop("The selected Frame column must contain finite, strictly increasing numeric values.")
    }
    elapsed <- if (valid) (frames - frames[1]) / sampling_rate else generated_time()

    data.table::setcolorder(dt, c(frame_idx, setdiff(seq_len(ncol(dt)), frame_idx)))
    data.table::setnames(dt, 1, "Time")
    dt[[1]] <- elapsed
    attr(dt, "time_info") <- list(
      source = if (valid) {
        if (inferred_frame) "inferred_frame" else "frame"
      } else {
        "generated_invalid_frame"
      },
      column = original_name,
      sampling_rate = sampling_rate
    )
    return(dt)
  }

  dt[, Time := generated_time()]
  data.table::setcolorder(dt, c("Time", setdiff(names(dt), "Time")))
  attr(dt, "time_info") <- list(
    source = "generated_missing",
    column = NULL,
    sampling_rate = sampling_rate
  )
  dt
}

#' Identify raw columns that can serve as numeric traces
#' @param dt Uploaded data before Time normalization.
#' @return Character vector of columns containing at least two finite numbers.
numeric_trace_candidates <- function(dt) {
  dt <- data.table::as.data.table(dt)
  names(dt)[vapply(dt, function(x) {
    values <- suppressWarnings(as.numeric(x))
    sum(is.finite(values)) >= 2
  }, logical(1))]
}

#' Inspect automatic Time detection for the Load Data mapping UI
#' @param dt Uploaded data before processing.
#' @param sampling_rate Sampling frequency in Hz.
#' @return List describing columns, automatic Time handling, and trace defaults.
inspect_column_mapping <- function(dt, sampling_rate = 1) {
  dt <- data.table::copy(data.table::as.data.table(dt))
  converted <- ensure_time_first(dt, sampling_rate = sampling_rate)
  info <- attr(converted, "time_info")
  candidates <- numeric_trace_candidates(dt)
  source_column <- info$column %||% NULL
  trace_columns <- setdiff(candidates, source_column %||% character())
  list(
    columns = names(dt),
    numeric_columns = candidates,
    row_count = nrow(dt),
    time_info = info,
    trace_columns = trace_columns
  )
}

#' Apply one file's explicit column mapping before baseline processing
#' @param dt Uploaded data table.
#' @param mapping Optional list with `time_mode`, `time_column`, and
#'   `excluded_columns`.
#' @param sampling_rate Sampling frequency in Hz.
#' @return Time-normalized table containing only selected numeric traces.
apply_column_mapping <- function(dt, mapping = NULL, sampling_rate = 1) {
  dt <- data.table::copy(data.table::as.data.table(dt))
  mapping <- mapping %||% list()
  mode <- as.character(mapping$time_mode %||% "auto")[1]
  if (!(mode %in% c("auto", "time", "frame", "generated"))) {
    stop("Unknown Time handling mode: ", mode)
  }

  excluded <- unique(as.character(mapping$excluded_columns %||% character()))
  unknown_exclusions <- setdiff(excluded, names(dt))
  if (length(unknown_exclusions) > 0) {
    stop("Excluded column(s) were not found: ", paste(unknown_exclusions, collapse = ", "))
  }

  candidates <- numeric_trace_candidates(dt)
  selected_column <- NULL
  if (mode %in% c("time", "frame")) {
    selected_column <- as.character(mapping$time_column %||% "")[1]
    if (!nzchar(selected_column) || !(selected_column %in% names(dt))) {
      stop("Select a valid column for the chosen Time handling mode.")
    }
  }

  # When generated Time is requested, retain the automatically detected
  # Time/Frame column as metadata rather than accidentally analyzing it.
  auto_source <- NULL
  if (identical(mode, "generated")) {
    auto_probe <- ensure_time_first(dt, sampling_rate = sampling_rate)
    auto_source <- attr(auto_probe, "time_info")$column %||% NULL
  }

  source_to_remove <- if (identical(mode, "auto")) {
    auto_probe <- ensure_time_first(dt, sampling_rate = sampling_rate)
    attr(auto_probe, "time_info")$column %||% NULL
  } else if (mode %in% c("time", "frame")) {
    selected_column
  } else {
    auto_source
  }
  trace_columns <- setdiff(candidates, c(excluded, source_to_remove %||% character()))
  # A second column named Time would collide with the canonical Time output.
  if (!identical(selected_column, "Time")) {
    trace_columns <- setdiff(trace_columns, "Time")
  }
  if (length(trace_columns) == 0) {
    stop("No numeric trace columns remain after applying the column mapping.")
  }

  source_columns <- unique(c(selected_column %||% character(), trace_columns))
  work <- dt[, ..source_columns]
  converted <- switch(mode,
    auto = {
      # Auto detection needs the original source column even though it will
      # not be part of the final trace set.
      full <- ensure_time_first(dt, sampling_rate = sampling_rate)
      info <- attr(full, "time_info")
      keep <- trace_columns[trace_columns %in% names(full)]
      out <- full[, c("Time", keep), with = FALSE]
      attr(out, "time_info") <- info
      out
    },
    time = ensure_time_first(work, time_col = selected_column, sampling_rate = sampling_rate),
    frame = ensure_time_first(work, frame_col = selected_column, sampling_rate = sampling_rate),
    generated = ensure_time_first(work, sampling_rate = sampling_rate, force_generated = TRUE)
  )

  if (ncol(converted) < 2) {
    stop("No numeric trace columns remain after applying the column mapping.")
  }
  converted
}

# Coerce all relevant columns to numeric, handling potential errors
coerce_numeric_dt <- function(dt) {
  # Coerce time column first
  dt[[1]] <- suppressWarnings(as.numeric(dt[[1]]))

  # If only time column exists, return early
  if (ncol(dt) <= 1) {
    return(dt)
  }

  # Identify columns that are not lists (e.g. from bad excel reads)
  non_list_cols <- c(TRUE, vapply(dt[, -1, with = FALSE], function(col) !is.list(col), logical(1)))
  dt <- dt[, which(non_list_cols), with = FALSE]

  if (ncol(dt) <= 1) {
    return(dt)
  }

  # Coerce remaining data columns to numeric and drop unusable columns
  drop_cols <- character(0)
  for (j in seq(2, ncol(dt))) {
    numeric_col <- suppressWarnings(as.numeric(dt[[j]]))
    dt[[j]] <- numeric_col
    if (sum(is.finite(numeric_col)) == 0) {
      drop_cols <- c(drop_cols, names(dt)[j])
    }
  }

  if (length(drop_cols) > 0) {
    keep_names <- setdiff(names(dt), drop_cols)
    dt <- dt[, ..keep_names]
  }

  dt
}

#' Normalize settings used by the upload-processing transaction
#' @param settings Named list of processing options.
#' @return A validated settings list.
normalize_load_settings <- function(settings = list()) {
  method <- as.character(settings$baseline_method %||% "frame_range")[1]
  if (!identical(method, "frame_range")) {
    stop("Only frame-range baseline correction is supported.")
  }

  frames <- suppressWarnings(as.integer(settings$baseline_frames %||% c(1, 20)))
  if (length(frames) < 2 || any(!is.finite(frames[1:2]))) {
    stop("Baseline frames must contain two finite frame numbers.")
  }
  frames <- sort(pmax(1L, frames[1:2]))

  sampling_rate <- suppressWarnings(as.numeric(settings$sampling_rate %||% 1)[1])
  if (!is.finite(sampling_rate) || sampling_rate <= 0) {
    stop("Sampling rate must be a finite number greater than zero.")
  }

  column_mappings <- settings$column_mappings %||% NULL
  if (!is.null(column_mappings) && !is.list(column_mappings)) {
    stop("Column mappings must be supplied as a list.")
  }

  list(
    baseline_method = method,
    baseline_frames = frames,
    sampling_rate = sampling_rate,
    column_mappings = column_mappings
  )
}

#' Describe a Time/Frame normalization for user-facing processing feedback
#' @param info `time_info` attribute returned by ensure_time_first().
#' @param file_name Display name of the uploaded file.
#' @return A message string, or NULL when an existing Time column was used.
describe_time_adjustment <- function(info, file_name) {
  if (is.null(info) || identical(info$source, "time")) {
    return(NULL)
  }
  rate <- format(info$sampling_rate, trim = TRUE, scientific = FALSE)
  switch(info$source,
    frame = sprintf("%s: converted '%s' from frames to seconds at %s Hz.", file_name, info$column, rate),
    inferred_frame = sprintf("%s: treated the unnamed sequential first column as Frame and converted it to seconds at %s Hz.", file_name, rate),
    generated_selected = sprintf("%s: generated Time from row number at %s Hz as selected.", file_name, rate),
    generated_invalid_time = sprintf("%s: replaced invalid '%s' values using %s Hz.", file_name, info$column, rate),
    generated_invalid_frame = sprintf("%s: replaced invalid '%s' frame values using %s Hz.", file_name, info$column, rate),
    generated_missing = sprintf("%s: generated Time using %s Hz; all uploaded columns were retained as traces.", file_name, rate),
    NULL
  )
}

#' Build a complete processed-data state without mutating the live app
#'
#' Per-file read/validation failures are collected as skipped files. The
#' function errors only when the batch has no usable data or a batch-level
#' calculation fails. Callers can therefore commit the returned state as one
#' transaction and leave the previous app state untouched on error.
#'
#' @param files Shiny upload data frame containing at least name and datapath.
#' @param settings Named list accepted by normalize_load_settings().
#' @param read_fun Injectable file reader, primarily for deterministic tests.
#' @param progress Optional function(amount, detail) for progress reporting.
#' @return A named list containing every processed reactive field plus feedback.
build_processed_state <- function(files, settings = list(), read_fun = safe_read,
                                  progress = NULL) {
  if (is.null(files) || !is.data.frame(files) || nrow(files) == 0) {
    stop("Select at least one file before processing.")
  }
  required <- c("name", "datapath")
  if (!all(required %in% names(files))) {
    stop("Uploaded file metadata is missing name or datapath.")
  }
  if (!is.function(read_fun)) {
    stop("read_fun must be a function.")
  }
  settings <- normalize_load_settings(settings)
  report_progress <- function(amount, detail) {
    if (is.function(progress)) progress(amount, detail)
  }

  records <- list()
  skipped <- list()
  n_files <- nrow(files)

  for (i in seq_len(n_files)) {
    file_name <- as.character(files$name[i])
    report_progress(0.1 / n_files, paste0("Reading file ", i, "/", n_files, ": ", basename(file_name)))

    outcome <- tryCatch({
      dt <- read_fun(files$datapath[i])
      if (!is.data.frame(dt) || nrow(dt) < 2 || ncol(dt) < 1) {
        stop("fewer than two rows or no data columns")
      }

      mapping <- if (!is.null(settings$column_mappings) &&
                     length(settings$column_mappings) >= i) {
        settings$column_mappings[[i]]
      } else {
        NULL
      }
      dt <- apply_column_mapping(
        dt,
        mapping = mapping,
        sampling_rate = settings$sampling_rate
      )
      time_info <- attr(dt, "time_info")
      dt <- coerce_numeric_dt(dt)
      if (nrow(dt) < 2 || ncol(dt) < 2) {
        stop("no usable numeric cell traces")
      }

      raw <- data.table::copy(dt)
      report_progress(0.2 / n_files, paste0("Detecting baseline for: ", basename(file_name)))

      start_frame <- min(nrow(dt), max(1L, settings$baseline_frames[1]))
      end_frame <- min(nrow(dt), max(start_frame, settings$baseline_frames[2]))
      f0 <- vapply(seq(2, ncol(dt)), function(j) {
        mean(dt[[j]][start_frame:end_frame], na.rm = TRUE)
      }, numeric(1))

      f0 <- stats::setNames(f0, names(dt)[-1])
      report_progress(0.2 / n_files, paste0("Computing \u0394F/F\u2080 for: ", basename(file_name)))

      bad_f0 <- !is.finite(f0) | f0 <= 1e-6
      for (k in which(!bad_f0)) {
        j <- k + 1
        dt[[j]] <- (dt[[j]] - f0[[k]]) / f0[[k]]
      }
      if (any(bad_f0)) {
        dt[, (names(f0)[bad_f0]) := NULL]
      }
      if (ncol(dt) < 2) {
        stop("all cell traces had a zero, negative, or missing baseline")
      }

      # The app promises metrics after processing. Reject a file when none of
      # its surviving traces has enough valid data to produce even one row.
      metric_probe <- compute_metrics_for_dt(
        dt, "processing_probe", settings$baseline_frames
      )
      if (nrow(metric_probe) == 0) {
        stop("no cell trace had enough valid samples to compute metrics")
      }

      list(
        file_index = i,
        dt = dt,
        raw = raw,
        baselines = f0,
        dropped_cells = names(f0)[bad_f0],
        time_info = time_info
      )
    }, error = function(e) {
      list(error = conditionMessage(e))
    })

    if (!is.null(outcome$error)) {
      skipped[[length(skipped) + 1]] <- data.frame(
        file = file_name,
        reason = outcome$error,
        stringsAsFactors = FALSE
      )
    } else {
      records[[length(records) + 1]] <- outcome
    }
  }

  if (length(records) == 0) {
    reasons <- if (length(skipped) > 0) {
      details <- do.call(rbind, skipped)
      paste(utils::head(paste0(details$file, ": ", details$reason), 3), collapse = "; ")
    } else {
      "no usable numeric traces"
    }
    stop("No uploaded files could be processed. ", reasons)
  }

  accepted_indices <- vapply(records, `[[`, integer(1), "file_index")
  accepted_files <- files[accepted_indices, , drop = FALSE]
  rownames(accepted_files) <- NULL
  labels <- make.unique(
    tools::file_path_sans_ext(basename(as.character(accepted_files$name))),
    sep = "_"
  )

  dts <- stats::setNames(lapply(records, `[[`, "dt"), labels)
  raw_traces <- stats::setNames(lapply(records, `[[`, "raw"), labels)
  baselines <- stats::setNames(lapply(records, `[[`, "baselines"), labels)

  report_progress(0.15, "Summarizing time courses...")
  long <- dplyr::bind_rows(Map(to_long, dts, labels))
  long <- dplyr::filter(long, is.finite(Time), is.finite(dFF0))
  if (nrow(long) == 0) {
    stop("Processing produced no finite time-course values.")
  }
  summary <- long |>
    dplyr::group_by(Group, Time) |>
    dplyr::summarise(
      mean_dFF0 = mean(dFF0, na.rm = TRUE),
      sem_dFF0 = stats::sd(dFF0, na.rm = TRUE) /
        sqrt(max(1, sum(is.finite(dFF0)))),
      sd_dFF0 = stats::sd(dFF0, na.rm = TRUE),
      n_cells = sum(is.finite(dFF0)),
      .groups = "drop"
    )

  report_progress(0.15, "Computing metrics (Peak, AUC, Rise Time...)...")
  metrics <- dplyr::bind_rows(Map(
    function(dt, label) compute_metrics_for_dt(dt, label, settings$baseline_frames),
    dts, labels
  ))
  if (nrow(metrics) == 0) {
    stop("Processing produced no valid cell metrics.")
  }

  skipped_details <- if (length(skipped) > 0) {
    do.call(rbind, skipped)
  } else {
    data.frame(file = character(), reason = character(), stringsAsFactors = FALSE)
  }
  dropped_cells <- unlist(Map(function(record, label) {
    if (length(record$dropped_cells) == 0) return(character())
    paste(label, record$dropped_cells, sep = "/")
  }, records, labels), use.names = FALSE)
  time_messages <- unlist(Map(function(record, file_name) {
    describe_time_adjustment(record$time_info, file_name)
  }, records, as.character(accepted_files$name)), use.names = FALSE)

  list(
    files = accepted_files,
    groups = labels,
    colors = default_group_colors(labels),
    dts = dts,
    raw_traces = raw_traces,
    baselines = baselines,
    baseline_method = settings$baseline_method,
    baseline_frames = settings$baseline_frames,
    long = long,
    summary = summary,
    metrics = metrics,
    skipped_files = skipped_details$file,
    skipped_details = skipped_details,
    dropped_cells = dropped_cells,
    time_messages = time_messages
  )
}

# Fields that must always describe the same successfully processed dataset.
processed_state_fields <- c(
  "files", "groups", "colors", "dts", "raw_traces", "baselines",
  "baseline_method", "baseline_frames", "long", "summary", "metrics"
)

#' Clear every field belonging to a processed dataset
#'
#' Used when uploaded files or preprocessing settings change after a successful
#' run. Removing the complete transaction prevents result views and exports
#' from silently presenting values calculated with older settings.
#' @param rv A Shiny reactiveValues object or environment-like test double.
#' @return `rv`, invisibly.
clear_processed_state <- function(rv) {
  empty <- list(
    files = NULL,
    groups = NULL,
    colors = NULL,
    dts = list(),
    raw_traces = list(),
    baselines = list(),
    baseline_method = NULL,
    baseline_frames = NULL,
    long = NULL,
    summary = NULL,
    metrics = NULL
  )
  for (field in processed_state_fields) {
    rv[[field]] <- empty[[field]]
  }
  invisible(rv)
}

#' Commit a validated processed-data state, rolling back on assignment failure
#' @param rv A Shiny reactiveValues object or environment-like test double.
#' @param state Result from build_processed_state().
commit_processed_state <- function(rv, state) {
  missing_fields <- setdiff(processed_state_fields, names(state))
  if (length(missing_fields) > 0) {
    stop("Processed state is incomplete: ", paste(missing_fields, collapse = ", "))
  }

  previous <- stats::setNames(
    lapply(processed_state_fields, function(field) rv[[field]]),
    processed_state_fields
  )
  tryCatch({
    for (field in processed_state_fields) {
      rv[[field]] <- state[[field]]
    }
  }, error = function(e) {
    for (field in processed_state_fields) {
      rv[[field]] <- previous[[field]]
    }
    stop(e)
  })
  invisible(rv)
}

#' Format timepoint dimensions without summing rows across files
#' @param dts Named list of processed wide tables.
#' @return Human-readable per-file timepoint context.
format_timepoint_context <- function(dts) {
  if (is.null(dts) || length(dts) == 0) {
    return("0 timepoints")
  }
  counts <- vapply(dts, nrow, integer(1))
  if (length(counts) == 1) {
    return(sprintf("%d timepoint%s", counts, if (counts == 1) "" else "s"))
  }
  if (length(unique(counts)) == 1) {
    return(sprintf("%d timepoints/file", counts[1]))
  }
  sprintf("%d\u2013%d timepoints/file", min(counts), max(counts))
}

#' Helper to find the first time a threshold is crossed in a signal
#' This function uses linear interpolation for accuracy and can be constrained
#' to search within a specific window of the signal.
#' @param signal The numeric vector representing the signal.
#' @param time_vec The corresponding numeric vector for time.
#' @param threshold The numeric threshold value to find.
#' @param search_start_idx The starting index for the search window.
#' @param search_end_idx The ending index for the search window.
#' @return The interpolated time of the first crossing, or NA if not found.
find_rising_crossing_time <- function(signal, time_vec, threshold, search_start_idx, search_end_idx) {
  n <- min(length(signal), length(time_vec))
  search_start_idx <- max(1L, suppressWarnings(as.integer(search_start_idx)[1]))
  search_end_idx <- min(n, suppressWarnings(as.integer(search_end_idx)[1]))
  if (!is.finite(search_start_idx) || !is.finite(search_end_idx) ||
      search_start_idx > search_end_idx || !is.finite(threshold)) {
    return(NA_real_)
  }

  crossings <- find_threshold_crossings(
    signal, time_vec, threshold, direction = "rising",
    first_upper_idx = max(2L, search_start_idx),
    last_upper_idx = search_end_idx
  )
  if (length(crossings) > 0) return(crossings[1])

  # If the threshold was already exceeded at the first observed point in the
  # search window, report that observed time without interpolating across a
  # missing sample or an excluded baseline interval.
  search_indices <- seq.int(search_start_idx, search_end_idx)
  observed_above <- search_indices[
    is.finite(signal[search_indices]) & is.finite(time_vec[search_indices]) &
      signal[search_indices] >= threshold
  ]
  if (length(observed_above) == 0) NA_real_ else time_vec[observed_above[1]]
}

#' Find interpolated threshold crossings between consecutive observed frames
#'
#' Missing samples are never removed or bridged: a crossing is only calculated
#' when both adjacent signal and time values are finite. This preserves the
#' original frame meaning of baseline and response windows.
#'
#' @param signal Numeric signal vector.
#' @param time_vec Numeric time vector with the same length as signal.
#' @param threshold Threshold to cross.
#' @param direction Either "rising" or "falling".
#' @param first_upper_idx First upper frame of a candidate adjacent pair.
#' @param last_upper_idx Last upper frame of a candidate adjacent pair.
#' @return Numeric vector of interpolated crossing times.
find_threshold_crossings <- function(signal, time_vec, threshold,
                                     direction = c("rising", "falling"),
                                     first_upper_idx = 2,
                                     last_upper_idx = length(signal)) {
  direction <- match.arg(direction)
  n <- min(length(signal), length(time_vec))
  if (n < 2 || !is.finite(threshold)) return(numeric())

  first_upper_idx <- max(2L, as.integer(first_upper_idx))
  last_upper_idx <- min(n, as.integer(last_upper_idx))
  if (!is.finite(first_upper_idx) || !is.finite(last_upper_idx) ||
      first_upper_idx > last_upper_idx) {
    return(numeric())
  }

  upper <- seq.int(first_upper_idx, last_upper_idx)
  lower <- upper - 1L
  y1 <- signal[lower]
  y2 <- signal[upper]
  t1 <- time_vec[lower]
  t2 <- time_vec[upper]
  observed <- is.finite(y1) & is.finite(y2) & is.finite(t1) &
    is.finite(t2) & t2 > t1

  crosses <- if (identical(direction, "rising")) {
    y1 <= threshold & y2 >= threshold & (y1 < threshold | y2 > threshold)
  } else {
    y1 >= threshold & y2 <= threshold & (y1 > threshold | y2 < threshold)
  }
  keep <- observed & crosses
  if (!any(keep)) return(numeric())

  y1 <- y1[keep]
  y2 <- y2[keep]
  t1 <- t1[keep]
  t2 <- t2[keep]
  t1 + (t2 - t1) * (threshold - y1) / (y2 - y1)
}

#' Calculate exact or right-censored FWHM details
#'
#' @param signal Numeric signal vector in analysis units.
#' @param time_vec Numeric time vector.
#' @param threshold_half Half-maximum threshold.
#' @param peak_idx Original frame index of the detected peak.
#' @param baseline_end_frame Original final baseline frame.
#' @return List containing crossing times, exact FWHM fields, and censoring
#'   metadata. Exact FWHM is NA when the right crossing was not observed.
calculate_fwhm_details <- function(signal, time_vec, threshold_half, peak_idx,
                                   baseline_end_frame) {
  empty <- list(
    t_left = NA_real_, t_right = NA_real_, last_observed_time = NA_real_,
    FWHM = NA_real_, FWHM_Censored = NA, FWHM_Lower_Bound = NA_real_,
    Half_Width = NA_real_
  )
  n <- min(length(signal), length(time_vec))
  peak_idx <- suppressWarnings(as.integer(peak_idx)[1])
  baseline_end_frame <- suppressWarnings(as.integer(baseline_end_frame)[1])
  if (n < 2 || !is.finite(threshold_half) || !is.finite(peak_idx) ||
      peak_idx < 1 || peak_idx > n || !is.finite(baseline_end_frame)) {
    return(empty)
  }

  left_crossings <- find_threshold_crossings(
    signal, time_vec, threshold_half, direction = "rising",
    first_upper_idx = max(2L, baseline_end_frame + 1L),
    last_upper_idx = peak_idx
  )
  if (length(left_crossings) == 0) return(empty)
  t_left <- utils::tail(left_crossings, 1)

  right_crossings <- find_threshold_crossings(
    signal, time_vec, threshold_half, direction = "falling",
    first_upper_idx = peak_idx + 1L,
    last_upper_idx = n
  )
  if (length(right_crossings) > 0) {
    t_right <- right_crossings[1]
    if (is.finite(t_right) && t_right > t_left) {
      width <- t_right - t_left
      return(list(
        t_left = t_left, t_right = t_right,
        last_observed_time = max(time_vec[is.finite(time_vec)], na.rm = TRUE),
        FWHM = width, FWHM_Censored = FALSE,
        FWHM_Lower_Bound = NA_real_, Half_Width = width / 2
      ))
    }
    return(empty)
  }

  observed <- which(is.finite(signal[seq_len(n)]) & is.finite(time_vec[seq_len(n)]))
  if (length(observed) == 0) return(empty)
  last_idx <- max(observed)
  last_time <- time_vec[last_idx]
  observed_after_peak <- observed[observed >= peak_idx]
  still_above <- length(observed_after_peak) > 0 &&
    all(signal[observed_after_peak] >= threshold_half)
  if (still_above && is.finite(last_time) && last_time > t_left) {
    return(list(
      t_left = t_left, t_right = NA_real_, last_observed_time = last_time,
      FWHM = NA_real_, FWHM_Censored = TRUE,
      FWHM_Lower_Bound = last_time - t_left, Half_Width = NA_real_
    ))
  }
  empty
}

#' An all-NA metrics row with the same columns/order as a successful result,
#' so early returns bind cleanly with full rows.
#' @param baseline_sd Baseline SD to preserve when it could be computed.
empty_metrics_row <- function(baseline_sd = NA_real_) {
  data.frame(
    Peak_dFF0 = NA_real_, Time_to_Peak = NA_real_,
    Time_to_25_Peak = NA_real_, Time_to_50_Peak = NA_real_, Time_to_75_Peak = NA_real_,
    Rise_Time = NA_real_, Calcium_Entry_Rate = NA_real_, AUC = NA_real_,
    Response_Amplitude = NA_real_, FWHM = NA_real_,
    FWHM_Censored = NA, FWHM_Lower_Bound = NA_real_,
    Half_Width = NA_real_, Baseline_SD = baseline_sd, SNR = NA_real_
  )
}

#' Function to calculate various metrics for a single cell's time course data
#'
#' This function takes a vector of fluorescence values and a time vector,
#' @param cell_data A numeric vector of fluorescence values.
#' @param time_vec A numeric vector of time points corresponding to the data.
#' @param baseline_frames A numeric vector of length 2 specifying the start and end frames for baseline calculation.
#' @param data_is_dFF0 A logical indicating if the input data is already processed (dF/F0).
#' @return A data.frame with calculated metrics for the cell.
calculate_cell_metrics <- function(cell_data, time_vec, baseline_frames = c(1, 20), data_is_dFF0 = FALSE) {
  n <- min(length(cell_data), length(time_vec))
  if (n == 0) return(empty_metrics_row())
  x <- suppressWarnings(as.numeric(cell_data[seq_len(n)]))
  t <- suppressWarnings(as.numeric(time_vec[seq_len(n)]))
  observed_pair <- is.finite(x) & is.finite(t)
  if (sum(observed_pair) < 10) {
    return(empty_metrics_row())
  }

  frames <- suppressWarnings(as.integer(baseline_frames))
  if (length(frames) < 2 || any(!is.finite(frames[1:2]))) {
    return(empty_metrics_row())
  }
  start_frame <- min(n, max(1L, frames[1]))
  end_frame <- min(n, max(start_frame, frames[2]))
  baseline_idx <- seq.int(start_frame, end_frame)

  # Baseline frames refer to original uploaded rows. Missing samples inside the
  # window are ignored in-place and never cause later response frames to slide
  # into the baseline calculation.
  baseline_vals <- x[baseline_idx]
  baseline_vals <- baseline_vals[is.finite(baseline_vals)]

  if (data_is_dFF0) {
    baseline_raw <- 0
    baseline_sd_raw <- stats::sd(baseline_vals, na.rm = TRUE)
  } else {
    baseline_raw <- mean(baseline_vals, na.rm = TRUE)
    baseline_sd_raw <- stats::sd(baseline_vals, na.rm = TRUE)
  }

  if (!is.finite(baseline_raw)) {
    return(empty_metrics_row())
  }

  if (data_is_dFF0) {
    working_signal <- x
    baseline <- 0
    baseline_sd <- baseline_sd_raw
  } else if (abs(baseline_raw) > 1e-9) {
    working_signal <- (x - baseline_raw) / baseline_raw
    baseline <- 0
    baseline_sd <- stats::sd(working_signal[baseline_idx], na.rm = TRUE)
  } else {
    working_signal <- x
    baseline <- baseline_raw
    baseline_sd <- baseline_sd_raw
  }

  if (!any(is.finite(working_signal))) {
    return(empty_metrics_row())
  }

  # Only look for peaks AFTER the baseline period
  search_region <- working_signal
  search_region[!is.finite(search_region) | !is.finite(t)] <- -Inf
  search_region[seq_len(end_frame)] <- -Inf # Exclude original baseline frames

  peak_idx <- which.max(search_region)
  peak_value <- working_signal[peak_idx]

  # If peak is in baseline or no valid peak found, return NA for all metrics
  if (peak_idx <= end_frame || !is.finite(peak_value)) {
    return(empty_metrics_row(baseline_sd = baseline_sd))
  }

  time_to_peak <- t[peak_idx]
  response_amplitude <- peak_value - baseline

  # Time to % Peak and Rise Time (Robust version)
  tt25 <- tt50 <- tt75 <- rise_time <- ca_entry <- NA_real_
  if (response_amplitude > 1e-3) {
    # Define thresholds
    p10 <- baseline + 0.10 * response_amplitude
    p25 <- baseline + 0.25 * response_amplitude
    p50 <- baseline + 0.50 * response_amplitude
    p75 <- baseline + 0.75 * response_amplitude
    p90 <- baseline + 0.90 * response_amplitude

    # Search for crossings only between the end of the baseline and the peak
    search_start_idx <- min(end_frame + 1, peak_idx)

    tt25 <- find_rising_crossing_time(working_signal, t, p25, search_start_idx, peak_idx)
    tt50 <- find_rising_crossing_time(working_signal, t, p50, search_start_idx, peak_idx)
    tt75 <- find_rising_crossing_time(working_signal, t, p75, search_start_idx, peak_idx)

    # Calculate Rise Time
    t10 <- find_rising_crossing_time(working_signal, t, p10, search_start_idx, peak_idx)
    t90 <- find_rising_crossing_time(working_signal, t, p90, search_start_idx, peak_idx)

    if (!is.na(t10) && !is.na(t90) && t90 > t10) {
      rise_time <- t90 - t10
      if (rise_time > 1e-9) ca_entry <- (0.8 * response_amplitude) / rise_time
    }
  }

  # Net area between the trace and baseline over the whole recording. Only
  # consecutive observed frame pairs contribute, so missing samples are not
  # removed and the trapezoid rule never bridges an unobserved interval.
  lower_idx <- seq_len(n - 1L)
  upper_idx <- lower_idx + 1L
  auc_pairs <- is.finite(t[lower_idx]) & is.finite(t[upper_idx]) &
    is.finite(working_signal[lower_idx]) & is.finite(working_signal[upper_idx]) &
    t[upper_idx] > t[lower_idx]
  auc <- if (any(auc_pairs)) {
    dt_vals <- t[upper_idx[auc_pairs]] - t[lower_idx[auc_pairs]]
    heights <- (working_signal[lower_idx[auc_pairs]] +
      working_signal[upper_idx[auc_pairs]]) / 2
    sum(dt_vals * heights)
  } else {
    NA_real_
  }

  snr <- if (!is.na(baseline_sd) && baseline_sd > 1e-9) response_amplitude / baseline_sd else NA_real_

  fwhm <- NA_real_
  fwhm_censored <- NA
  fwhm_lower_bound <- NA_real_
  half_width <- NA_real_
  if (response_amplitude > 1e-3) {
    threshold_half <- baseline + 0.5 * response_amplitude
    fwhm_details <- calculate_fwhm_details(
      working_signal, t, threshold_half, peak_idx, end_frame
    )
    fwhm <- fwhm_details$FWHM
    fwhm_censored <- fwhm_details$FWHM_Censored
    fwhm_lower_bound <- fwhm_details$FWHM_Lower_Bound
    half_width <- fwhm_details$Half_Width
  }

  data.frame(
    Peak_dFF0 = peak_value, Time_to_Peak = time_to_peak,
    Time_to_25_Peak = tt25, Time_to_50_Peak = tt50, Time_to_75_Peak = tt75,
    Rise_Time = rise_time, Calcium_Entry_Rate = ca_entry, AUC = auc,
    Response_Amplitude = response_amplitude, FWHM = fwhm,
    FWHM_Censored = fwhm_censored, FWHM_Lower_Bound = fwhm_lower_bound,
    Half_Width = half_width, Baseline_SD = baseline_sd, SNR = snr
  )
}

#' Compute metrics for a data.table of cell traces
#'
#' @param dt A data.table with a 'Time' column and cell traces in other columns.
#' @param group_label A character string for the group name.
#' @param baseline_frames A numeric vector of length 2 specifying the start and end frames for baseline calculation.
#' @return A data.table with calculated metrics for each cell.
compute_metrics_for_dt <- function(dt, group_label, baseline_frames = c(1, 20)) {
  time_vec <- dt$Time

  # Identify numeric columns that are not 'Time'
  cell_cols <- names(dt)[sapply(dt, is.numeric) & names(dt) != "Time"]
  if (length(cell_cols) == 0) {
    return(data.frame())
  }

  # Calculate metrics for each cell column
  metrics_list <- lapply(cell_cols, function(col_name) {
    metrics <- calculate_cell_metrics(dt[[col_name]], time_vec, baseline_frames, data_is_dFF0 = TRUE)
    metrics$Group <- group_label
    metrics$Cell <- col_name
    metrics$Cell_ID <- paste(group_label, col_name, sep = "_")
    return(metrics)
  })

  # Combine the list of data.frames into a single data.frame
  result_df <- dplyr::bind_rows(metrics_list)

  # Use original column names for Cell_Label to preserve meaning
  result_df$Cell_Label <- result_df$Cell

  # Reorder columns to have identifiers first
  id_cols <- c("Group", "Cell", "Cell_ID", "Cell_Label")
  metric_cols <- setdiff(names(result_df), id_cols)
  final_df <- result_df[, c(id_cols, metric_cols)]

  # Filter out rows where all metric values are NA
  final_df[rowSums(is.na(final_df[, metric_cols])) < length(metric_cols), ]
}


#' Create a named vector of default colors for groups
#' @param groups A character vector of group names.
#' @return A named character vector of hex color codes.
default_group_colors <- function(groups) {
  n <- length(groups)
  if (n == 0) {
    return(character(0))
  }

  # Use a colorblind-friendly palette for a small number of groups
  if (n <= 8) {
    colors <- RColorBrewer::brewer.pal(max(3, n), "Set2")
  } else {
    # Generate more colors if needed
    colors <- scales::hue_pal()(n)
  }

  stats::setNames(colors[seq_len(n)], groups)
}

#' Convert wide format data to long format
#' @param dt A data.table with a 'Time' column and cell traces.
#' @param group_label A character string for the group name.
#' @return A long format data.table.
to_long <- function(dt, group_label) {
  time_vec <- dt$Time

  # Ensure we only pivot numeric cell columns
  cell_cols <- names(dt)[sapply(dt, is.numeric) & names(dt) != "Time"]
  if (length(cell_cols) == 0) {
    return(data.table::data.table())
  }

  long_dt <- data.table::melt(dt,
    id.vars = "Time",
    measure.vars = cell_cols,
    variable.name = "Cell",
    value.name = "dFF0"
  )

  long_dt[, `:=`(
    Group = group_label,
    Cell_ID = paste(group_label, Cell, sep = "_")
  )]

  return(long_dt)
}

#' Get a formatted label for a metric (for plot axes)
#' @param metric The metric's variable name.
#' @return An expression or character string for the label.
metric_label <- function(metric) {
  switch(metric,
    Peak_dFF0 = "\u0394F/F\u2080",
    Response_Amplitude = "Response Amplitude (\u0394F/F\u2080)",
    Rise_Time = "Rise Time (s)",
    FWHM = "FWHM (s)",
    FWHM_Lower_Bound = "Observed FWHM Lower Bound (s)",
    Half_Width = "Derived Half-Width (FWHM/2, s)",
    AUC = "Signed Net AUC (\u0394F/F\u2080 \u00d7 s)",
    SNR = "SNR (unitless)",
    Time_to_Peak = "Time to Peak (s)",
    Time_to_25_Peak = "Time to 25% Peak (s)",
    Time_to_50_Peak = "Time to 50% Peak (s)",
    Time_to_75_Peak = "Time to 75% Peak (s)",
    Calcium_Entry_Rate = "10–90% Rise Rate (\u0394F/F\u2080/s)",
    Baseline_SD = "Baseline SD (\u0394F/F\u2080)",
    metric
  )
}

#' Get a formatted title for a metric (for plot titles)
#' @param metric The metric's variable name.
#' @return A character string for the title.
metric_title <- function(metric) {
  switch(metric,
    Peak_dFF0 = "Peak \u0394F/F\u2080",
    Response_Amplitude = "Response Amplitude (\u0394F/F\u2080)",
    Rise_Time = "Rise Time (10-90%) (s)",
    FWHM = "FWHM (s)",
    FWHM_Lower_Bound = "Observed FWHM Lower Bound (s)",
    Half_Width = "Derived Half-Width (FWHM/2, s)",
    AUC = "Signed Net Area Under Curve (\u0394F/F\u2080 \u00d7 s)",
    SNR = "Signal-to-Noise Ratio (unitless)",
    Time_to_Peak = "Time to Peak (s)",
    Time_to_25_Peak = "Time to 25% Peak (s)",
    Time_to_50_Peak = "Time to 50% Peak (s)",
    Time_to_75_Peak = "Time to 75% Peak (s)",
    Calcium_Entry_Rate = "10–90% \u0394F/F\u2080 Rise Rate (\u0394F/F\u2080/s)",
    Baseline_SD = "Baseline SD (\u0394F/F\u2080)",
    metric
  )
}
