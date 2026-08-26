# R/mod_metrics_explained.R

# Annotation palette for the explanation plots. Okabe-Ito hues anchored on
# the app's brand blue (#0072B2, R/theme.R) so annotations match the app
# aesthetic instead of raw R primaries, and stay colorblind-safe.
expl_accent <- "#0072B2"  # measurements: arrows, amplitudes, durations
expl_marker <- "#D55E00"  # peaks, SD bounds, emphasis markers
expl_good   <- "#009E73"  # baseline references, first thresholds, AUC fill
expl_thresh <- "#E69F00"  # mid thresholds (50%, 10/90% guides)

# Keep explanation calculations tied to the exact samples used by the metric
# engine. In particular, do not let a large baseline fluctuation replace the
# post-baseline peak stored in the metrics table.
metric_explanation_peak_index <- function(trace, peak_time) {
  if (!is.data.frame(trace) || !("Time" %in% names(trace)) ||
      nrow(trace) == 0 || !is.finite(peak_time)) {
    return(NA_integer_)
  }
  distance <- abs(trace$Time - peak_time)
  distance[!is.finite(distance)] <- Inf
  if (all(is.infinite(distance))) NA_integer_ else which.min(distance)
}

metric_explanation_baseline_details <- function(trace, baseline_frames) {
  if (!is.data.frame(trace) || !("dFF0" %in% names(trace)) || nrow(trace) == 0) {
    return(NULL)
  }
  frames <- suppressWarnings(as.integer(baseline_frames))
  if (length(frames) < 2 || any(!is.finite(frames[1:2]))) return(NULL)
  start_frame <- min(nrow(trace), max(1L, frames[1]))
  end_frame <- min(nrow(trace), max(start_frame, frames[2]))
  indices <- seq.int(start_frame, end_frame)
  values <- trace$dFF0[indices]
  observed_values <- values[is.finite(values)]
  list(
    indices = indices,
    values = values,
    observed_values = observed_values,
    observed_n = length(observed_values),
    start_frame = start_frame,
    end_frame = end_frame
  )
}

metric_explanation_auc_details <- function(trace) {
  if (!is.data.frame(trace) || !all(c("Time", "dFF0") %in% names(trace)) ||
      nrow(trace) < 2) {
    return(list(valid_pairs = logical(), interval_count = 0L,
                mean_interval = NA_real_))
  }
  lower <- seq_len(nrow(trace) - 1L)
  upper <- lower + 1L
  valid_pairs <- is.finite(trace$Time[lower]) & is.finite(trace$Time[upper]) &
    is.finite(trace$dFF0[lower]) & is.finite(trace$dFF0[upper]) &
    trace$Time[upper] > trace$Time[lower]
  intervals <- trace$Time[upper[valid_pairs]] - trace$Time[lower[valid_pairs]]
  list(
    valid_pairs = valid_pairs,
    interval_count = sum(valid_pairs),
    mean_interval = if (length(intervals) > 0) mean(intervals) else NA_real_
  )
}

mod_metrics_explained_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "metrics_explained",
          fluidRow(
            # No column() wrapper: the box already renders as col-sm-12, and
            # nesting it double-applies the gutter, insetting this row 8px
            # relative to the boxes below.
            theme_box(
              title = "Visual Metric Explanations",
              icon = icon("graduation-cap"),
              status = "primary",
              width = 12,
              collapsible = FALSE,
              div(
                class = "flex-tight",
                style = "display: flex; align-items: flex-end; gap: 16px; flex-wrap: wrap;",
                div(style = "flex: 1; min-width: 260px; padding-bottom: 6px;",
                  p("Select a metric and a cell to see a visual breakdown of the calculation using your own data.",
                    class = "text-muted", style = "margin: 0;")
                ),
                div(style = "width: 280px;",
                  selectInput(ns("metric_to_explain"), "Metric",
                              choices = c("Peak \u0394F/F\u2080" = "peak_dff0",
                                          "Response Amplitude" = "response_amplitude",
                                          "Time to Peak" = "time_to_peak",
                                          "Signal-to-Noise Ratio (SNR)" = "snr",
                                          "Baseline Standard Deviation" = "baseline_sd",
                                          "Rise Time (10-90%)" = "rise_time",
                                          "Time to % Peak" = "time_to_percent_peak",
                                          "FWHM & Derived Half-Width" = "fwhm",
                                          "Area Under Curve (AUC)" = "auc",
                                          "10–90% \u0394F/F\u2080 Rise Rate" = "ca_entry_rate"),
                              selected = "peak_dff0", width = "100%")
                ),
                div(style = "width: 300px;",
                  uiOutput(ns("cell_selector_ui"))
                )
              )
            )
          ),
          fluidRow(
            # Left Column: Explanations
            theme_box(title = "Explanation & Controls", icon = icon("book-open"), status = "primary", solidHeader = TRUE, width = 4, collapsible = FALSE,
              # Same computed height as the Visualization box so both boxes
              # end on one bottom edge; the explanation text scrolls inside
              div(class = "match-plot-short",
                # Dynamic Explanation UI
                uiOutput(ns("explanation_ui")),

                # Export Accordion
                accordion(
                  id = ns("download_accordion"),
                  title = "Export",
                  icon = "download",
                  expanded = FALSE,
                  content = div(
                    fluidRow(
                      column(6, selectInput(ns("dl_format"), "Format", c("PNG"="png", "PDF"="pdf", "SVG"="svg", "TIFF"="tiff"), "png")),
                      column(6, numericInput(ns("dl_dpi"), "DPI", 300, 72, 600, 5))
                    ),
                    browser_download_button(ns("dl_plot"), "Download Plot", class = "btn-primary", style = "width: 100%; margin-top: 8px;")
                  )
                )
              )
            ),

            # Right Column: The plot
            theme_box(title = "Visualization", icon = icon("chart-line"), status = "primary", solidHeader = TRUE, width = 8, collapsible = FALSE,
              # Adapts to window height so the plot isn't cut by the fold on
              # short laptops; capped at the previous fixed height
              div(class = "plot-viewport-short",
                  plotOutput(ns("explanation_plot"), height = "100%")
              )
            )
          )
  )
}

mod_metrics_explained_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$explanation_ui <- renderUI({
      req(input$metric_to_explain)
      get_metric_explanation_content(input$metric_to_explain, ns)
    })
    
    # Helper to safely get baseline frames (defaults to 1-20 if not set)
    # Using reactive isolation to access current value of rv$baseline_frames or default
    get_bl <- function() {
      frames <- rv$baseline_frames
      if (is.null(frames) || length(frames) != 2) c(1, 20) else frames
    }

    # Plot title for a cell. ImageJ-style "Mean1" columns display as
    # "Cell 1" (no "Cell:" prefix, which would read "Cell: Cell 1");
    # any other label keeps the existing "Cell: <label>" form.
    cell_title <- function(lbl) {
      if (grepl("^Mean[0-9]+$", lbl)) pretty_cell_label(lbl) else paste("Cell:", lbl)
    }
    
    explanation_theme <- function() {
      theme_classic(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            axis.title = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10),
            legend.position = "none")
    }
    
    # Use a single cell selector that is always visible
    output$cell_selector_ui <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      # Display names only — the underlying values stay raw so lookups work
      names(cell_choices) <- paste(rv$metrics$Group, "-", pretty_cell_label(rv$metrics$Cell_Label))

      # Ensure proper initialization by returning the select input with explicit ID
      selectInput(
        inputId = ns("selected_cell"),
        label = "Cell",
        choices = cell_choices,
        selected = cell_choices[1],
        width = "100%"
      )
    })

    selected_cell_data <- reactive({
      req(rv$dts, rv$metrics, rv$raw_traces, rv$baselines, input$selected_cell)
      
      cell_id <- input$selected_cell
      
      cell_metric <- dplyr::filter(rv$metrics, Cell_ID == cell_id)
      req(nrow(cell_metric) == 1)
      
      group_name <- as.character(cell_metric$Group[[1]])
      cell_name <- as.character(cell_metric$Cell[[1]])
      
      req(
        group_name %in% names(rv$dts), group_name %in% names(rv$raw_traces),
        cell_name %in% names(rv$dts[[group_name]]),
        cell_name %in% names(rv$raw_traces[[group_name]])
      )
      
      # Use the original wide trace so missing samples retain their uploaded
      # frame positions in every explanation calculation and visualization.
      processed_trace <- rv$dts[[group_name]][, c("Time", cell_name), with = FALSE]
      names(processed_trace) <- c("Time", "dFF0")
      raw_trace <- rv$raw_traces[[group_name]][, c("Time", cell_name), with = FALSE]
      names(raw_trace) <- c("Time", "Fluorescence")
      f0 <- rv$baselines[[group_name]][[cell_name]]
      peak_time_processed <- cell_metric$Time_to_Peak[[1]]
      peak_f_raw <- raw_trace$Fluorescence[which.min(abs(raw_trace$Time - peak_time_processed))]
      
      list(
        processed_trace = processed_trace,
        metric = cell_metric,
        peak_time = peak_time_processed,
        f0 = f0,
        peak_f = peak_f_raw
      )
    })
    
    output$peak_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Baseline fluorescence (F₀): %.2f", data$f0)),
          tags$li(sprintf("Peak fluorescence (F): %.2f", data$peak_f)),
          tags$li(sprintf("Time of peak: %.2f seconds", data$peak_time)),
          tags$li(sprintf("Peak ΔF/F₀ value: %.3f", data$metric$Peak_dFF0))
        )
      )
    })

    output$peak_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      tagList(
        formula_line("Peak ΔF/F<sub>0</sub> = ",
                     frac("F<sub>peak</sub> − F<sub>0</sub>", "F<sub>0</sub>"), " = ",
                     frac(sprintf("%.2f − %.2f", data$peak_f, data$f0), sprintf("%.2f", data$f0))),
        formula_line("= ", frac(sprintf("%.2f", data$peak_f - data$f0), sprintf("%.2f", data$f0)),
                     sprintf(" = %.3f", data$metric$Peak_dFF0)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Peak ΔF/F₀ = %.3f", data$metric$Peak_dFF0), 
            style = "margin: 5px 0 0 0;")
        )
      )
    })

    output$snr_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("Response amplitude: %.3f", data$metric$Response_Amplitude)),
          tags$li(sprintf("Baseline standard deviation: %.3f", data$metric$Baseline_SD)),
          tags$li(sprintf("Signal-to-noise ratio: %.3f", data$metric$SNR))
        )
      )
    })

    output$snr_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      tagList(
        p("SNR is calculated by dividing the signal strength by the noise level:"),
        formula_line("SNR = ", frac("Response Amplitude", "Baseline SD"), " = ",
                     frac(sprintf("%.3f", data$metric$Response_Amplitude), sprintf("%.3f", data$metric$Baseline_SD))),
        formula_line(sprintf("SNR = %.3f", data$metric$SNR)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Signal-to-Noise Ratio = %.3f", data$metric$SNR), 
            style = "margin: 5px 0 0 0;")
        )
      )
    })

    output$response_amp_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()

      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("Baseline value: 0 (after normalization)")),
          tags$li(sprintf("Response Amplitude: %.3f ΔF/F₀", data$metric$Response_Amplitude))
        )
      )
    })

    output$response_amp_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()

      tagList(
        p("For this cell:"),
        formula_line("Response Amplitude = Peak ΔF/F<sub>0</sub> − Baseline"),
        formula_line(sprintf("= %.3f − 0 = %.3f", data$metric$Peak_dFF0, data$metric$Response_Amplitude)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Response Amplitude = %.3f ΔF/F₀", data$metric$Response_Amplitude),
            style = "margin: 5px 0 0 0;")
        )
      )
    })

    output$baseline_sd_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      baseline <- metric_explanation_baseline_details(data$processed_trace, get_bl())
      req(baseline, baseline$observed_n > 0)

      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Baseline frames: %d to %d", baseline$start_frame, baseline$end_frame)),
          tags$li(sprintf("Observed baseline points: %d", baseline$observed_n)),
          tags$li(sprintf("Mean baseline ΔF/F₀: %.4f", mean(baseline$observed_values))),
          tags$li(sprintf("Baseline SD: %.4f ΔF/F₀", data$metric$Baseline_SD))
        )
      )
    })

    output$baseline_sd_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      baseline <- metric_explanation_baseline_details(data$processed_trace, get_bl())
      req(baseline, baseline$observed_n > 0)

      tagList(
        p("For this cell, using baseline frames:"),
        formula_line(sprintf("n = %d observed values, &nbsp;x̄ = %.4f",
                             baseline$observed_n, mean(baseline$observed_values))),
        formula_line("SD = √(&#8202;", frac("Σ<sub>i</sub> (x<sub>i</sub> − x̄)<sup>2</sup>", "n − 1"), "&#8202;)"),
        formula_line(sprintf("Baseline SD = %.4f", data$metric$Baseline_SD)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Baseline SD = %.4f ΔF/F₀", data$metric$Baseline_SD),
            style = "margin: 5px 0 0 0;")
        )
      )
    })

    output$rise_time_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% values and times for this specific cell
      peak_idx <- metric_explanation_peak_index(
        data$processed_trace, data$metric$Time_to_Peak[[1]]
      )
      req(is.finite(peak_idx))
      search_start_idx <- min(get_bl()[2] + 1, peak_idx)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      p10_val <- 0.10 * data$metric$Response_Amplitude
      p90_val <- 0.90 * data$metric$Response_Amplitude
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Response amplitude: %.3f ΔF/F₀", data$metric$Response_Amplitude)),
          tags$li(sprintf("10%% point: %.3f ΔF/F₀ at %.2f seconds", p10_val, t10)),
          tags$li(sprintf("90%% point: %.3f ΔF/F₀ at %.2f seconds", p90_val, t90)),
          tags$li(sprintf("Rise time: %.2f seconds", data$metric$Rise_Time))
        )
      )
    })

    output$rise_time_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% times for this specific cell
      peak_idx <- metric_explanation_peak_index(
        data$processed_trace, data$metric$Time_to_Peak[[1]]
      )
      req(is.finite(peak_idx))
      search_start_idx <- min(get_bl()[2] + 1, peak_idx)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      tagList(
        p("Rise time is calculated by subtracting the time points:"),
        formula_line(sprintf("Rise Time = t<sub>90%%</sub> − t<sub>10%%</sub> = %.2f − %.2f", t90, t10)),
        formula_line(sprintf("Rise Time = %.2f seconds", data$metric$Rise_Time)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Rise Time (10-90%%) = %.2f seconds", data$metric$Rise_Time), 
            style = "margin: 5px 0 0 0;")
        )
      )
    })

    output$ttp_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual threshold values
      p25_val <- 0.25 * data$metric$Peak_dFF0
      p50_val <- 0.50 * data$metric$Peak_dFF0
      p75_val <- 0.75 * data$metric$Peak_dFF0
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("25%% threshold: %.3f ΔF/F₀ at %.2f seconds", p25_val, data$metric$Time_to_25_Peak)),
          tags$li(sprintf("50%% threshold: %.3f ΔF/F₀ at %.2f seconds", p50_val, data$metric$Time_to_50_Peak)),
          tags$li(sprintf("75%% threshold: %.3f ΔF/F₀ at %.2f seconds", p75_val, data$metric$Time_to_75_Peak))
        )
      )
    })

    output$ttp_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual threshold values
      p25_val <- 0.25 * data$metric$Peak_dFF0
      p50_val <- 0.50 * data$metric$Peak_dFF0
      p75_val <- 0.75 * data$metric$Peak_dFF0
      
      tagList(
        p("Each time point represents when the signal first crosses the threshold:"),
        formula_line(sprintf("t<sub>25%%</sub>: ΔF/F<sub>0</sub> reaches %.3f at %.2f s", p25_val, data$metric$Time_to_25_Peak)),
        formula_line(sprintf("t<sub>50%%</sub>: ΔF/F<sub>0</sub> reaches %.3f at %.2f s", p50_val, data$metric$Time_to_50_Peak)),
        formula_line(sprintf("t<sub>75%%</sub>: ΔF/F<sub>0</sub> reaches %.3f at %.2f s", p75_val, data$metric$Time_to_75_Peak)),
        div(class = "metric-result-box",
          h5("Results:"),
          p(sprintf("25%% Peak: %.2f s", data$metric$Time_to_25_Peak), 
            style = "margin: 5px 0 0 0;"),
          p(sprintf("50%% Peak: %.2f s", data$metric$Time_to_50_Peak), 
            style = "margin: 5px 0 0 0;"),
          p(sprintf("75%% Peak: %.2f s", data$metric$Time_to_75_Peak), 
            style = "margin: 5px 0 0 0;")
        )
      )
    })

    output$ttpk_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      recording_start <- min(data$processed_trace$Time, na.rm = TRUE)
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("First Time value: %.2f seconds", recording_start)),
          tags$li(sprintf("Peak ΔF/F₀ value: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("Time of peak: %.2f seconds", data$metric$Time_to_Peak)),
          tags$li("Stimulus latency: Not calculated (no stimulus-onset input)")
        )
      )
    })

    output$ttpk_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      tagList(
        p("The time to peak is found by identifying when the signal reaches its maximum:"),
        formula_line(sprintf("t<sub>peak</sub> = time when ΔF/F<sub>0</sub> = %.3f", data$metric$Peak_dFF0)),
        formula_line(sprintf("t<sub>peak</sub> = %.2f seconds", data$metric$Time_to_Peak)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Peak time coordinate = %.2f seconds", data$metric$Time_to_Peak),
            style = "margin: 5px 0 0 0;")
        )
      )
    })

    fwhm_times <- reactive({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      is_censored <- "FWHM_Censored" %in% names(metric) &&
        isTRUE(metric$FWHM_Censored[[1]])
      req(nrow(trace) > 0, is.finite(metric$FWHM[[1]]) || is_censored)

      half_max <- 0.5 * metric$Response_Amplitude[[1]]
      peak_idx <- metric_explanation_peak_index(trace, metric$Time_to_Peak[[1]])
      req(is.finite(peak_idx))
      details <- calculate_fwhm_details(
        trace$dFF0, trace$Time, half_max, peak_idx,
        min(nrow(trace), get_bl()[2])
      )
      req(is.finite(details$t_left))
      details$half_max_y <- half_max
      details$is_sustained <- isTRUE(details$FWHM_Censored)
      details
    })
    
    output$fwhm_data_points_ui <- renderUI({
      req(fwhm_times(), selected_cell_data())
      times <- fwhm_times()
      metric <- selected_cell_data()$metric
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", metric$Peak_dFF0)),
          tags$li(sprintf("Half-maximum (50%%): %.3f", times$half_max_y)),
          tags$li(sprintf("Left crossing: %.2f seconds", times$t_left)),
          if (times$is_sustained) {
            tagList(
              tags$li(sprintf(
                "Right crossing: Not observed before recording ended at %.2f seconds",
                times$last_observed_time
              )),
              tags$li("Exact FWHM: Not estimable (right-censored)"),
              tags$li(sprintf("Observed FWHM lower bound: ≥ %.2f seconds", metric$FWHM_Lower_Bound)),
              tags$li("Derived Half-Width: Not calculated without an exact FWHM")
            )
          } else {
            tagList(
              tags$li(sprintf("Right crossing: %.2f seconds", times$t_right)),
              tags$li(sprintf("FWHM: %.2f seconds", metric$FWHM)),
              tags$li(sprintf("Derived Half-Width (FWHM/2): %.2f seconds", metric$Half_Width))
            )
          }
        )
      )
    })

    output$fwhm_calculation_ui <- renderUI({
      req(fwhm_times(), selected_cell_data())
      times <- fwhm_times()
      metric <- selected_cell_data()$metric
      
      if (times$is_sustained) {
        tagList(
          p("An exact FWHM requires both half-maximum crossings. The recording ended before the right crossing was observed."),
          formula_line(sprintf(
            "Observed duration = t<sub>end</sub> − t<sub>left</sub> = %.2f − %.2f = %.2f s",
            times$last_observed_time, times$t_left, metric$FWHM_Lower_Bound
          )),
          formula_line(sprintf("FWHM ≥ %.2f s", metric$FWHM_Lower_Bound)),
          p("This response is right-censored: the exact FWHM may be longer, so the derived Half-Width is not calculated.",
            class = "metric-note"),
          div(class = "metric-result-box",
            h5("Result:"),
            p("Exact FWHM = not estimable (right-censored)", style = "margin: 5px 0 0 0;"),
            p(sprintf("Observed FWHM lower bound ≥ %.2f seconds", metric$FWHM_Lower_Bound),
              style = "margin: 5px 0 0 0;")
          )
        )
      } else {
        tagList(
          p("FWHM is calculated as the time difference between crossing points:"),
          formula_line(sprintf(
            "FWHM = t<sub>right</sub> − t<sub>left</sub> = %.2f − %.2f = %.2f s",
            times$t_right, times$t_left, metric$FWHM
          )),
          formula_line("Derived Half-Width = ", frac("FWHM", "2"), " = ",
            frac(sprintf("%.2f", metric$FWHM), "2"), sprintf(" = %.2f s", metric$Half_Width)),
          div(class = "metric-result-box",
            h5("Result:"),
            p(sprintf("FWHM = %.2f seconds", metric$FWHM), style = "margin: 5px 0 0 0;"),
            p(sprintf("Derived Half-Width (FWHM/2) = %.2f seconds", metric$Half_Width), style = "margin: 5px 0 0 0;")
          )
        )
      }
    })

    output$auc_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      auc_details <- metric_explanation_auc_details(trace)
      total_time <- max(trace$Time, na.rm = TRUE) - min(trace$Time, na.rm = TRUE)
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Recording duration: %.2f seconds", total_time)),
          tags$li(sprintf("Integrated adjacent intervals: %d", auc_details$interval_count)),
          tags$li(sprintf("Mean integrated interval: %.3f seconds", auc_details$mean_interval)),
          tags$li(sprintf("Signed net AUC: %.2f ΔF/F₀ × s", data$metric$AUC))
        )
      )
    })
    
    output$auc_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      auc_details <- metric_explanation_auc_details(trace)
      total_time <- max(trace$Time, na.rm = TRUE) - min(trace$Time, na.rm = TRUE)
      
      tagList(
        p("The trapezoidal rule sums signed areas between adjacent observed time points:"),
        formula_line("Area<sub>i</sub> = ", frac("y<sub>i</sub> + y<sub>i+1</sub>", "2"), " × Δt<sub>i</sub>"),
        formula_line(sprintf("Recording span = %.2f seconds; valid adjacent intervals = %d",
                             total_time, auc_details$interval_count)),
        formula_line(sprintf("Signed net AUC = Σ Area<sub>i</sub> = %.2f ΔF/F₀ × s",
                             data$metric$AUC)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Signed net AUC = %.2f ΔF/F₀ × s", data$metric$AUC),
            style = "margin: 5px 0 0 0;")
        )
      )
    })

    output$ca_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% values and times for this specific cell
      peak_idx <- metric_explanation_peak_index(
        data$processed_trace, data$metric$Time_to_Peak[[1]]
      )
      req(is.finite(peak_idx))
      search_start_idx <- min(get_bl()[2] + 1, peak_idx)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      p10_val <- 0.10 * data$metric$Response_Amplitude
      p90_val <- 0.90 * data$metric$Response_Amplitude
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("10%% point: %.3f ΔF/F₀ at %.2f seconds", p10_val, t10)),
          tags$li(sprintf("90%% point: %.3f ΔF/F₀ at %.2f seconds", p90_val, t90)),
          tags$li(sprintf("Rise time: %.2f seconds", data$metric$Rise_Time)),
          tags$li(sprintf("Signal rise: %.3f ΔF/F₀", p90_val - p10_val))
        )
      )
    })

    output$ca_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% values and times for this specific cell
      peak_idx <- metric_explanation_peak_index(
        data$processed_trace, data$metric$Time_to_Peak[[1]]
      )
      req(is.finite(peak_idx))
      search_start_idx <- min(get_bl()[2] + 1, peak_idx)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      p10_val <- 0.10 * data$metric$Response_Amplitude
      p90_val <- 0.90 * data$metric$Response_Amplitude
      
      tagList(
        formula_line("10–90% ΔF/F₀ Rise Rate = ", frac("Signal Rise", "Time Interval"), " = ",
                     frac(sprintf("%.3f − %.3f", p90_val, p10_val), sprintf("%.2f − %.2f", t90, t10))),
        formula_line("= ", frac(sprintf("%.3f", p90_val - p10_val), sprintf("%.2f", data$metric$Rise_Time)),
                     sprintf(" = %.3f ΔF/F₀/s", data$metric$Calcium_Entry_Rate)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("10–90%% ΔF/F₀ Rise Rate = %.3f ΔF/F₀/s", data$metric$Calcium_Entry_Rate),
            style = "margin: 5px 0 0 0;")
        )
      )
    })

    # A single reactive expression to generate the correct plot based on the user's selection
    explanation_plot_obj <- reactive({
      req(selected_cell_data(), input$metric_to_explain)

      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric

      # Use a switch to return the correct ggplot object
      switch(input$metric_to_explain,
        "peak_dff0" = {
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          label_y_pos <- metric$Peak_dFF0 + y_range * 0.05
          
          p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1)
          if (identical(rv$baseline_method, "frame_range") && !is.null(get_bl())) {
            b_start <- trace$Time[min(get_bl()[1], nrow(trace))]
            b_end <- trace$Time[min(get_bl()[2], nrow(trace))]
            p <- p + annotate("rect", xmin = b_start, xmax = b_end, ymin = -Inf, ymax = Inf, fill = "grey95", alpha = 0.5)
          }
          p + geom_segment(data = metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0), color = expl_marker, linetype = "dashed") +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = expl_marker, size = 4) +
            annotate("text", x = metric$Time_to_Peak, y = label_y_pos, label = round(metric$Peak_dFF0, 3), vjust = 0, color = expl_marker, size = 4.5) +
            labs(title = cell_title(metric$Cell_Label), x = "Time (s)", y = "\u0394F/F\u2080") +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "time_to_peak" = {
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          label_y_pos <- metric$Peak_dFF0 + y_range * 0.08
          
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_segment(data = metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0), color = expl_marker, linetype = "dashed") +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = expl_marker, size = 4) +
            annotate("label", x = metric$Time_to_Peak, y = label_y_pos,
                     label = paste("Peak time coordinate =", round(metric$Time_to_Peak, 2), "s"),
                     color = expl_accent, fill = "white", label.size = 0,
                     vjust = 0, fontface = "bold", size = 4.2) +
            labs(title = cell_title(metric$Cell_Label), x = "Time (s)", y = "\u0394F/F\u2080") +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "response_amplitude" = {
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))

          p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1)
          if (identical(rv$baseline_method, "frame_range") && !is.null(get_bl())) {
            b_start <- trace$Time[min(get_bl()[1], nrow(trace))]
            b_end <- trace$Time[min(get_bl()[2], nrow(trace))]
            p <- p + annotate("rect", xmin = b_start, xmax = b_end, ymin = -Inf, ymax = Inf, fill = "grey95", alpha = 0.5)
          }
          p + geom_hline(yintercept = 0, color = expl_good, linetype = "dashed", linewidth = 1) +
            annotate("label", x = max(trace$Time, na.rm = TRUE), y = 0, label = "Baseline (0)",
                     hjust = 1, vjust = -0.35, color = expl_good, fontface = "bold",
                     fill = "white", alpha = 0.85, label.size = 0) +
            geom_segment(data = metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0),
                         color = expl_accent, linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both")) +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = expl_marker, size = 4) +
            annotate("text", x = metric$Time_to_Peak, y = metric$Response_Amplitude / 2,
                     label = sprintf("Amplitude = %.3f", metric$Response_Amplitude),
                     hjust = -0.1, color = expl_accent, fontface = "bold", size = 4.5) +
            labs(title = cell_title(metric$Cell_Label), x = "Time (s)", y = "\u0394F/F\u2080") +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "snr" = {
          baseline <- metric_explanation_baseline_details(trace, get_bl())
          shiny::validate(shiny::need(!is.null(baseline) && baseline$observed_n > 0,
                                      "Could not determine the baseline window for this cell."))
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          x_range <- diff(range(trace$Time, na.rm = TRUE))
          noise_label_x <- trace$Time[baseline$start_frame] + x_range * 0.02

          # Pre-calculate values to avoid scoping issues
          baseline_sd <- metric$Baseline_SD
          baseline_trace <- trace[baseline$indices, , drop = FALSE] %>%
            dplyr::mutate(ymin = -baseline_sd, ymax = baseline_sd)

          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_ribbon(data = baseline_trace, aes(ymin = ymin, ymax = ymax),
                        fill = expl_marker, alpha = 0.2) +
            annotate("label", x = noise_label_x, y = baseline_sd, label = "Baseline Noise (SD)",
                     color = expl_marker, fontface = "bold", size = 4, hjust = 0, vjust = -0.5,
                     fill = alpha("white", 0.7), label.size = NA) +
            annotate("point", x = metric$Time_to_Peak, y = metric$Peak_dFF0, color = expl_accent, size = 4) +
            annotate("text", x = metric$Time_to_Peak, y = metric$Peak_dFF0 + y_range * 0.1, label = "Signal", hjust = 0.5, color = expl_accent, fontface = "bold") +
            labs(title = cell_title(metric$Cell_Label), x = "Time (s)", y = "\u0394F/F\u2080") +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "baseline_sd" = {
          baseline <- metric_explanation_baseline_details(trace, get_bl())
          shiny::validate(shiny::need(!is.null(baseline) && baseline$observed_n > 0,
                                      "Could not determine the baseline window for this cell."))
          b_start_time <- trace$Time[baseline$start_frame]
          b_end_time <- trace$Time[baseline$end_frame]
          baseline_mean <- mean(baseline$observed_values)

          # Pre-calculate values to avoid scoping issues
          baseline_sd <- metric$Baseline_SD
          baseline_trace <- trace[baseline$indices, , drop = FALSE]

          # Baseline shading first so it sits under the data and labels; the
          # SD labels go to the clear right end of the dashed lines with a
          # white backing so they never collide with the y axis or the trace
          ggplot(trace, aes(x = Time, y = dFF0)) +
            annotate("rect", xmin = b_start_time, xmax = b_end_time, ymin = -Inf, ymax = Inf,
                     fill = "grey95", alpha = 0.5) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_hline(yintercept = baseline_mean, color = expl_good, linetype = "solid", linewidth = 0.8) +
            geom_ribbon(data = baseline_trace, aes(ymin = baseline_mean - baseline_sd, ymax = baseline_mean + baseline_sd),
                        fill = expl_marker, alpha = 0.2) +
            geom_hline(yintercept = baseline_mean + baseline_sd, color = expl_marker, linetype = "dashed", linewidth = 0.6) +
            geom_hline(yintercept = baseline_mean - baseline_sd, color = expl_marker, linetype = "dashed", linewidth = 0.6) +
            # One combined label above the +1 SD line: at trace scale the two
            # SD lines nearly coincide, and a below-line label collides with
            # the x axis when the baseline sits at the bottom of the y range
            annotate("label", x = max(trace$Time), y = baseline_mean + baseline_sd,
                     label = sprintf("±1 SD (%.4f)", baseline_sd),
                     hjust = 1, vjust = -0.35, color = expl_marker, fontface = "bold", size = 4,
                     fill = "white", alpha = 0.85, label.size = 0) +
            labs(title = cell_title(metric$Cell_Label), x = "Time (s)", y = "\u0394F/F\u2080") +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "rise_time" = {
          peak_idx <- metric_explanation_peak_index(trace, metric$Time_to_Peak[[1]])
          shiny::validate(shiny::need(is.finite(peak_idx), "Could not locate the stored post-baseline peak for this cell."))
          search_start_idx <- min(get_bl()[2] + 1, peak_idx)
          t10 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.10 * metric$Response_Amplitude, search_start_idx, peak_idx)
          t90 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.90 * metric$Response_Amplitude, search_start_idx, peak_idx)
          shiny::validate(shiny::need(!is.na(t10) && !is.na(t90), "Could not determine 10% or 90% rise time for this cell."))
          p10_val <- 0.10 * metric$Response_Amplitude
          p90_val <- 0.90 * metric$Response_Amplitude
          y_offset <- (max(trace$dFF0, na.rm = TRUE) - min(trace$dFF0, na.rm = TRUE)) * 0.05
          label_y_pos <- p90_val + y_offset
          label_x_pos <- min(trace$Time) + diff(range(trace$Time, na.rm=TRUE)) * 0.01
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_segment(aes(x = min(trace$Time, na.rm = TRUE), y = p10_val, xend = t10, yend = p10_val), color = expl_thresh, linetype = "dotted") +
            geom_segment(aes(x = min(trace$Time, na.rm = TRUE), y = p90_val, xend = t90, yend = p90_val), color = expl_thresh, linetype = "dotted") +
            geom_segment(aes(x = t10, y = 0, xend = t10, yend = p10_val), color = expl_thresh, linetype = "dashed") +
            geom_point(aes(x = !!t10, y = !!p10_val), color = expl_thresh, size = 4) +
            geom_segment(aes(x = t90, y = 0, xend = t90, yend = p90_val), color = expl_thresh, linetype = "dashed") +
            geom_point(aes(x = !!t90, y = !!p90_val), color = expl_thresh, size = 4) +
            annotate("label", x = label_x_pos, y = p10_val, label = "10%", color = expl_thresh, fontface = "bold", hjust = 0,
                     fill = "white", alpha = 0.85, label.size = 0) +
            annotate("label", x = label_x_pos, y = p90_val, label = "90%", color = expl_thresh, fontface = "bold", hjust = 0,
                     fill = "white", alpha = 0.85, label.size = 0) +
            geom_segment(aes(x = t10, xend = t90, y = label_y_pos, yend = label_y_pos), 
                         arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = expl_marker, linewidth = 1) +
            annotate("text", x = mean(c(t10, t90)), y = label_y_pos, 
                     label = paste("Rise Time =", round(metric$Rise_Time, 2), "s"),
                     color = expl_marker, vjust = -0.8, fontface = "bold", size = 4.5) +
            labs(title = cell_title(metric$Cell_Label), x = "Time (s)", y = "\u0394F/F\u2080") +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "time_to_percent_peak" = {
          p25 <- 0.25 * metric$Peak_dFF0
          p50 <- 0.50 * metric$Peak_dFF0
          p75 <- 0.75 * metric$Peak_dFF0
          label_x_pos <- min(trace$Time) + diff(range(trace$Time, na.rm=TRUE)) * 0.01
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_hline(yintercept = p25, color = expl_good, linetype = "dotted") +
            geom_segment(data = metric, aes(x = Time_to_25_Peak, xend = Time_to_25_Peak, y=0, yend=p25), color = expl_good, linetype = "dashed") +
            annotate("label", x = label_x_pos, y = p25, label = "25%", color = expl_good, fontface = "bold", hjust = 0,
                     fill = "white", alpha = 0.85, label.size = 0) +
            geom_hline(yintercept = p50, color = expl_thresh, linetype = "dotted") +
            geom_segment(data = metric, aes(x = Time_to_50_Peak, xend = Time_to_50_Peak, y=0, yend=p50), color = expl_thresh, linetype = "dashed") +
            annotate("label", x = label_x_pos, y = p50, label = "50%", color = expl_thresh, fontface = "bold", hjust = 0,
                     fill = "white", alpha = 0.85, label.size = 0) +
            geom_hline(yintercept = p75, color = expl_marker, linetype = "dotted") +
            geom_segment(data = metric, aes(x = Time_to_75_Peak, xend = Time_to_75_Peak, y=0, yend=p75), color = expl_marker, linetype = "dashed") +
            annotate("label", x = label_x_pos, y = p75, label = "75%", color = expl_marker, fontface = "bold", hjust = 0,
                     fill = "white", alpha = 0.85, label.size = 0) +
            labs(title = cell_title(metric$Cell_Label), x = "Time (s)", y = "\u0394F/F\u2080") +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "fwhm" = {
          times <- fwhm_times()
          shiny::validate(shiny::need(!is.null(times), "Could not calculate FWHM for this cell."))
          y_range <- diff(range(data$processed_trace$dFF0, na.rm = TRUE))
          hwhm_offset <- y_range * 0.15
          width_end <- if (times$is_sustained) times$last_observed_time else times$t_right
          annotation_df <- data.frame(
            x_mid = mean(c(times$t_left, width_end)),
            y_mid = times$half_max_y,
            width_label = if (times$is_sustained) {
              paste("Observed FWHM ≥", round(data$metric$FWHM_Lower_Bound, 2), "s")
            } else {
              paste("FWHM =", round(data$metric$FWHM, 2), "s")
            }
          )
          p <- ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_hline(yintercept = times$half_max_y, color = expl_accent, linetype = "dashed") +
            annotate(
              "segment", x = times$t_left, xend = times$t_left,
              y = 0, yend = times$half_max_y,
              color = expl_accent, linetype = "dotted"
            ) +
            geom_segment(
              data = annotation_df,
              aes(x = times$t_left, xend = width_end, y = y_mid, yend = y_mid),
              arrow = arrow(
                length = unit(0.25, "cm"),
                ends = if (times$is_sustained) "last" else "both"
              ),
              color = expl_marker, linewidth = 1
            ) +
            geom_text(data = annotation_df, aes(x = x_mid, y = y_mid, label = width_label),
                      color = expl_marker, vjust = -1.2, fontface = "bold", size = 4.5) +
            labs(title = cell_title(data$metric$Cell_Label), x = "Time (s)", y = "\u0394F/F\u2080") +
            explanation_theme() + coord_cartesian(clip = "off")
          if (times$is_sustained) {
            p <- p + annotate(
              "text", x = annotation_df$x_mid, y = annotation_df$y_mid,
              label = "(Right crossing not observed before recording ended)",
              color = expl_marker, vjust = -3.5, size = 3.5, fontface = "italic"
            )
          } else {
            hwhm_df <- transform(
              annotation_df,
              x_hwhm_mid = times$t_left + (data$metric$Half_Width / 2),
              hwhm_label = paste("Derived Half-Width =", round(data$metric$Half_Width, 2), "s")
            )
            p <- p +
              annotate(
                "segment", x = times$t_right, xend = times$t_right,
                y = 0, yend = times$half_max_y,
                color = expl_accent, linetype = "dotted"
              ) +
              geom_segment(
                data = hwhm_df,
                aes(x = times$t_left, xend = times$t_left + data$metric$Half_Width,
                    y = y_mid - hwhm_offset, yend = y_mid - hwhm_offset),
                arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
                color = expl_thresh, linewidth = 1
              ) +
              geom_text(
                data = hwhm_df,
                aes(x = x_hwhm_mid, y = y_mid - hwhm_offset, label = hwhm_label),
                color = expl_thresh, vjust = 2, fontface = "bold", size = 4.5
              )
          }
          p
        },
        "auc" = {
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_ribbon(aes(ymin = 0, ymax = dFF0), fill = expl_good, alpha = 0.35) +
            geom_line(color = "gray50", linewidth = 1) +
            labs(title = cell_title(data$metric$Cell_Label), x = "Time (s)", y = "\u0394F/F\u2080",
                 caption = "Signed net AUC: values below zero contribute negatively") +
            explanation_theme() + 
            coord_cartesian(clip = "off")
        },
        "ca_entry_rate" = {
          peak_idx <- metric_explanation_peak_index(trace, metric$Time_to_Peak[[1]])
          shiny::validate(shiny::need(is.finite(peak_idx), "Could not locate the stored post-baseline peak for this cell."))
          search_start_idx <- min(get_bl()[2] + 1, peak_idx)
          t10 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.10 * metric$Response_Amplitude, search_start_idx, peak_idx)
          t90 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.90 * metric$Response_Amplitude, search_start_idx, peak_idx)
          shiny::validate(shiny::need(!is.na(t10) && !is.na(t90), "Could not determine rise time for this cell to calculate rate."))
          p10_val <- 0.10 * metric$Response_Amplitude
          p90_val <- 0.90 * metric$Response_Amplitude
          
          # Calculate positions for cleaner labels
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          x_range <- diff(range(trace$Time, na.rm = TRUE))
          
          ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray60", linewidth = 1.2) +
            # Add horizontal reference lines (subtle)
            geom_hline(yintercept = p10_val, color = "gray80", linetype = "dotted", alpha = 0.8) +
            geom_hline(yintercept = p90_val, color = "gray80", linetype = "dotted", alpha = 0.8) +
            # Add vertical reference lines (subtle)
            geom_vline(xintercept = t10, color = "gray80", linetype = "dotted", alpha = 0.8) +
            geom_vline(xintercept = t90, color = "gray80", linetype = "dotted", alpha = 0.8) +
            # Add the slope line between 10% and 90% points (prominent)
            geom_segment(x = t10, y = p10_val, xend = t90, yend = p90_val,
                         color = expl_accent, linewidth = 3, alpha = 0.9) +
            # Add points at 10% and 90%
            geom_point(x = t10, y = p10_val, color = expl_marker, size=5, stroke = 1) +
            geom_point(x = t90, y = p90_val, color = expl_marker, size=5, stroke = 1) +
            # Simple percentage labels like rise time plot
            annotate("label", x = t10, y = p10_val,
                     label = "10%",
                     color = "white", fill = expl_marker, fontface = "bold", size = 3.5,
                     hjust = 1.2, vjust = 0.5, label.size = 0) +
            annotate("label", x = t90, y = p90_val,
                     label = "90%",
                     color = "white", fill = expl_marker, fontface = "bold", size = 3.5,
                     hjust = -0.2, vjust = 0.5, label.size = 0) +
            # Time interval annotation with arrow - positioned well above trace
            annotate("segment", x = t10, xend = t90,
                     y = max(trace$dFF0, na.rm = TRUE) + y_range * 0.12,
                     yend = max(trace$dFF0, na.rm = TRUE) + y_range * 0.12,
                     arrow = arrow(length = unit(0.25, "cm"), ends = "both", type = "closed"),
                     color = expl_accent, linewidth = 1) +
            annotate("label", x = mean(c(t10, t90)), y = max(trace$dFF0, na.rm = TRUE) + y_range * 0.12,
                     label = sprintf("Δt = %.1f s", data$metric$Rise_Time),
                     color = "white", fill = expl_accent, fontface = "bold", size = 3,
                     hjust = 0.5, vjust = -0.3, label.size = 0) +
            # Final result label at the top RIGHT, clear of the rise-interval
            # annotations (the rise usually sits left on the time axis, so a
            # top-left label collided with the Δt arrow label)
            annotate("label", x = max(trace$Time, na.rm = TRUE),
                     y = max(trace$dFF0, na.rm = TRUE) + y_range * 0.22,
                     label = sprintf("10–90%% ΔF/F₀ Rise Rate = %.3f ΔF/F₀/s", data$metric$Calcium_Entry_Rate),
                     color = "white", fill = expl_accent, fontface = "bold", size = 3.5,
                     hjust = 1, vjust = 0.5, label.size = 0) +
            labs(title = cell_title(metric$Cell_Label), x = "Time (s)", y = "\u0394F/F\u2080") +
            explanation_theme() + 
            coord_cartesian(clip = "off", ylim = c(min(trace$dFF0, na.rm = TRUE) - y_range*0.05, 
                                                   max(trace$dFF0, na.rm = TRUE) + y_range*0.3))
        }
      )
    })
    
    output$explanation_plot <- renderPlot({
      explanation_plot_obj()
    }, res = 96)

    register_browser_download(input, session, "dl_plot",
      filename = function() {
        req(input$metric_to_explain, selected_cell_data())
        metric_part <- sanitize_filename_component(input$metric_to_explain, "metric")
        cell_part <- sanitize_filename_component(selected_cell_data()$metric$Cell_Label, "cell")
        build_export_filename(
          rv,
          parts = c("metric_explanation", metric_part, cell_part),
          ext = input$dl_format %||% "png"
        )
      },
      content = function(file) {
        req(explanation_plot_obj())
        tryCatch({
          plot_obj <- explanation_plot_obj()
          if (is.null(plot_obj)) {
            showNotification("No plot available for download", type = "error", duration = 5)
            return()
          }
          
          ggsave(file, plot = plot_obj, 
                 device = input$dl_format %||% "png", 
                 dpi = input$dl_dpi %||% 300,
                 width = 8, height = 6, 
                 bg = "white")
          
        }, error = function(e) {
          showNotification(paste("Download failed:", e$message), type = "error", duration = 5)
        })
      }
    )
    
  })
}
