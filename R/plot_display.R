# Display-only reducers. Never write back to processed data, metrics, calls,
# or CSV sources. Small datasets keep every sample and timestamp.

heatmap_display_field <- function(dt, label, sort_by = "tpeak",
                                  baseline_frames = c(1, 20), max_tiles = 100000L) {
  time <- dt$Time
  cells <- names(dt)[-1L]
  cells <- cells[vapply(cells, function(cell) any(is.finite(dt[[cell]])), logical(1))]
  if (!length(cells)) return(NULL)
  if (sort_by %in% c("tpeak", "amp")) {
    peaks <- lapply(cells, function(cell) post_baseline_peak(dt[[cell]], time, baseline_frames))
    value <- vapply(peaks, function(p) {
      x <- if (sort_by == "tpeak") p$time else p$value
      if (is.finite(x)) x else if (sort_by == "tpeak") Inf else -Inf
    }, numeric(1))
    cells <- cells[order(value, decreasing = sort_by == "amp")]
  }
  # Train the color scale on all samples, even if the picture uses bin means.
  full_range <- range(unlist(lapply(cells, function(cell) {
    y <- dt[[cell]]
    range(y[is.finite(y)])
  })))
  reduced <- length(time) * length(cells) > max_tiles
  if (reduced) {
    n_bins <- max(2L, min(1200L, floor(max_tiles / length(cells))))
    edges <- seq(min(time), max(time), length.out = n_bins + 1L)
    bins <- findInterval(time, edges, all.inside = TRUE)
    centers <- (head(edges, -1L) + tail(edges, -1L)) / 2
    values <- lapply(cells, function(cell) {
      y <- dt[[cell]]
      finite <- is.finite(y)
      totals <- rowsum(y[finite], bins[finite], reorder = FALSE)
      counts <- tabulate(bins[finite], nbins = n_bins)
      means <- rep(NA_real_, n_bins)
      at <- as.integer(rownames(totals))
      means[at] <- totals[, 1] / counts[at]
      means
    })
    left <- head(edges, -1L)
    right <- tail(edges, -1L)
  } else {
    centers <- time
    bounds <- time_bin_edges(time)
    left <- bounds$left
    right <- bounds$right
    values <- lapply(cells, function(cell) {
      y <- dt[[cell]]
      y[!is.finite(y)] <- NA_real_
      y
    })
  }
  data <- data.frame(
    Time = rep(centers, times = length(cells)),
    Cell = rep(seq_along(cells), each = length(centers)),
    Value = unlist(values, use.names = FALSE), Group = label,
    Cell_Label = rep(cells, each = length(centers)),
    Time_Min = rep(left, times = length(cells)),
    Time_Max = rep(right, times = length(cells))
  )
  list(data = data, range = full_range, reduced = reduced,
       frames = length(time), bins = length(centers), cells = length(cells))
}

timecourse_display_data <- function(long, max_points = 100000L) {
  if (is.null(long) || !nrow(long)) return(NULL)
  # Group row indices only: filtering/mutating the full long table here used
  # to copy millions of rows before ggplot made further copies and tooltips.
  groups <- split(seq_len(nrow(long)), interaction(long$Group, long$Cell, drop = TRUE))
  limit <- max(4L, floor(max_points / length(groups)))
  reduced <- FALSE
  pieces <- lapply(groups, function(rows) {
    rows <- rows[is.finite(long$Time[rows])]
    if (length(rows) < 2L) return(NULL)
    rows <- rows[order(long$Time[rows])]
    if (length(rows) <= limit) {
      selected <- long[rows, c("Group", "Cell", "Time", "dFF0"), drop = FALSE]
      selected$dFF0[!is.finite(selected$dFF0)] <- NA_real_
      return(selected)
    }
    reduced <<- TRUE
    time <- long$Time[rows]
    y <- long$dFF0[rows]
    typical <- stats::median(diff(time))
    missing <- !is.finite(y) | c(FALSE, diff(time) > 1.5 * typical)
    bins <- ceiling(seq_along(rows) * max(1L, floor(limit / 4L)) / length(rows))
    selected <- unlist(lapply(split(seq_along(rows), bins), function(at) {
      # A partial/gapped bin stays a gap; never draw a fabricated continuous
      # line through missing samples just to meet the display point budget.
      if (any(missing[at])) return(c(at[1L], tail(at, 1L)))
      sort(unique(c(at[1L], tail(at, 1L), at[which.min(y[at])], at[which.max(y[at])])))
    }), use.names = FALSE)
    gaps <- unique(bins[missing])
    result <- long[rows[selected], c("Group", "Cell", "Time", "dFF0"), drop = FALSE]
    result$dFF0[bins[selected] %in% gaps] <- NA_real_
    result
  })
  result <- dplyr::bind_rows(pieces)
  attr(result, "display_reduced") <- reduced
  attr(result, "source_points") <- nrow(long)
  attr(result, "source_cells") <- length(groups)
  result
}
