# Presentation only: calls and exports always retain the full-resolution scorer output.
responder_trace_bundle <- function(dt, rows) {
  rows <- rows[order(match(rows$Status, c("Responded", "Did not respond")),
    rows$Onset_s, rows$Cell, na.last = TRUE), , drop = FALSE]
  local <- as.matrix(as.data.frame(dt)[rows$Cell])
  if (nrow(rows)) {
    local <- sweep(local, 2, rows$Baseline_mean, "-")
    local <- sweep(local, 2, 1 + rows$Baseline_mean, "/")
  }
  list(rows = rows, time = dt$Time, local = local,
    summary = if (nrow(rows)) responder_trace_summary(dt$Time, local, rows$Status) else data.frame())
}

# Insert explicit breaks at acquisition gaps; retain NA samples in place.
responder_trace_points <- function(time, values, gap) {
  after <- which(diff(time) > gap + 8 * .Machine$double.eps * max(1, abs(time)))
  index <- seq_along(time)
  if (length(after)) index <- unlist(lapply(index, function(i) if (i %in% after) c(i, NA_integer_) else i))
  list(x = time[index], y = values[index])
}

responder_trace_limits <- function(bundles, counts, full = FALSE) {
  if (full) {
    xr <- range(unlist(lapply(bundles, `[[`, "time")), finite = TRUE)
  } else {
    end <- ifelse(is.finite(counts$Effective_end_s), counts$Effective_end_s, counts$End_s)
    pad <- .15 * (end - counts$Start_s)
    xr <- range(c(counts$Start_s - counts$Baseline_s - pad, end + pad), finite = TRUE)
  }
  ys <- unlist(lapply(bundles, function(g) c(g$local, g$summary$Mean - g$summary$SEM,
    g$summary$Mean + g$summary$SEM)), use.names = FALSE)
  yr <- range(c(0, ys), finite = TRUE)
  pad <- max(diff(yr) * .06, .02)
  list(x = xr, y = yr + c(-pad, pad))
}

responder_trace_plot <- function(g, count, label, limits, mode = "individual",
                                 statuses = c("Responded", "Did not respond"),
                                 selected = "", source = "population") {
  colors <- c("Responded" = "#177465", "Did not respond" = "#925223")
  p <- plotly::plot_ly(source = source)
  # Explicit Plotly traces preserve scalar hover metadata without the R
  # attribute mapper recycling it across every sample.
  p$x$attrs <- NULL
  p$x$data <- list()
  gap <- count$Max_gap_s
  add_line <- function(p, x, y, color, width, dash = "solid", opacity = 1, text = NULL, key = NULL, kind = "summary") {
    trace <- list(x = x, y = y, type = "scatter", mode = "lines",
      connectgaps = FALSE, hoverinfo = if (is.null(key)) "skip" else "text",
      line = list(color = color, width = width, dash = dash, simplify = FALSE), opacity = opacity,
      showlegend = FALSE, meta = list(kind = kind, field = count$Field))
    if (!is.null(key)) { trace$key <- I(key); trace$text <- I(text) }
    p$x$data[[length(p$x$data) + 1L]] <- trace
    p
  }
  tooltip <- function(r) sprintf("%s · %s<br>%s<br>Peak %.4g · threshold %.4g<br>Margin %.3g×",
    htmltools::htmlEscape(label), htmltools::htmlEscape(r$Cell), r$Status,
    r$Peak_increase, r$Threshold_increase, r$Margin)
  if (mode != "mean") for (status in intersect(names(colors), statuses)) {
    ids <- which(g$rows$Status == status)
    if (!length(ids)) next
    # Scalar metadata per cell keeps large recordings from repeating the same
    # tooltip and identifier at every timepoint in the browser payload.
    for (i in ids) {
      xy <- responder_trace_points(g$time, g$local[, i], gap)
      p <- add_line(p, xy$x, xy$y, colors[[status]], 1,
        if (status == "Responded") "solid" else "dash", if (mode == "both") .18 else .32,
        I(tooltip(g$rows[i, ])), I(g$rows$Key[i]), "cells")
    }
  }
  if (mode != "individual") for (status in intersect(names(colors), statuses)) {
    s <- g$summary[g$summary$Status == status, , drop = FALSE]
    if (!nrow(s)) next
    # Separate finite SEM runs and acquisition gaps before drawing polygons.
    good <- is.finite(s$SEM) & is.finite(s$Mean)
    run <- cumsum(c(TRUE, diff(s$Time) > gap | !head(good, -1) | !tail(good, -1)))
    for (ids in split(which(good), run[good])) if (length(ids) > 1L) {
      p$x$data[[length(p$x$data) + 1L]] <- list(x = c(s$Time[ids], rev(s$Time[ids])),
        y = c(s$Mean[ids] + s$SEM[ids], rev(s$Mean[ids] - s$SEM[ids])),
        type = "scatter", mode = "lines", fill = "toself", fillcolor = colors[[status]],
        line = list(width = 0), opacity = .13, hoverinfo = "skip", showlegend = FALSE,
        meta = list(kind = "sem", field = count$Field))
    }
    xy <- responder_trace_points(s$Time, s$Mean, gap)
    p <- add_line(p, xy$x, xy$y, colors[[status]], 2.6, if (status == "Responded") "solid" else "dash")
  }
  i <- match(selected, g$rows$Key)
  if (mode != "mean" && !is.na(i) && g$rows$Status[i] %in% statuses) {
    xy <- responder_trace_points(g$time, g$local[, i], gap)
    p <- add_line(p, xy$x, xy$y, colors[[g$rows$Status[i]]], 3,
      if (g$rows$Status[i] == "Responded") "solid" else "dash", text = I(tooltip(g$rows[i, ])),
      key = I(g$rows$Key[i]), kind = "selected")
  }
  end <- if (is.finite(count$Effective_end_s)) count$Effective_end_s else count$End_s
  shade <- function(a, b, color) list(type = "rect", x0 = a, x1 = b, y0 = 0, y1 = 1,
    yref = "paper", fillcolor = color, line = list(width = 0), layer = "below")
  p <- plotly::layout(p, showlegend = FALSE, hovermode = "closest",
    xaxis = list(title = "Time (s)", range = limits$x, zeroline = FALSE),
    yaxis = list(title = "Fractional rise from local baseline", range = limits$y, zeroline = TRUE),
    shapes = list(shade(count$Start_s - count$Baseline_s, count$Start_s, "#edf0f4"),
      shade(count$Start_s, end, "#edf5f7")),
    margin = list(l = 60, r = 12, b = 48, t = 12), dragmode = "zoom")
  p <- plotly::config(p, displaylogo = FALSE, responsive = TRUE, scrollZoom = FALSE)
  plotly::event_register(p, "plotly_click")
}
