# R/mod_time_course.R

mod_time_course_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "time",

          # Full-width stacked sections: plot first, summary second, styling
          # controls last. No side column, so no ragged bottoms and the plot
          # gets the whole content width.
          fluidRow(
            theme_box(title = "Time Course", status = "primary", solidHeader = TRUE, width = 12, collapsible = FALSE,
                plot_type_toggle(ns),
                # Collapsed by default; expanding puts the controls directly
                # above the plot so changes are visible while adjusting
                accordion(
                  id = ns("plot_controls_acc"),
                  title = "Plot Controls",
                  icon = "sliders-h",
                  expanded = FALSE,
                  content = tc_plot_controls(ns)
                ),
                div(id = ns("static_panel"), class = "plot-viewport-tc",
                    withSpinner(plotOutput(ns("timecourse_plot"), height = "100%"), type = 4)
                ),
                shinyjs::hidden(
                  div(id = ns("interactive_panel"), class = "plot-viewport-tc",
                      withSpinner(plotlyOutput(ns("timecourse_plotly"), height = "100%"), type = 4)
                  )
                )
            )
          ),

          fluidRow(
            theme_box(title = "Summary Statistics", status = "info", solidHeader = TRUE, width = 12,
                htmlOutput(ns("tc_summary_table"))
            )
          )
  )
}

# Plot Controls sheet: rendered inside the collapsed accordion at the top of
# the plot box, so adjustments happen with the plot in view
tc_plot_controls <- function(ns) {
  div(
                class = "control-cols",

                div(class = "control-col",
                  div(class = "control-col-title", icon("eye"), "Display"),
                  checkboxInput(ns("tc_show_traces"), "Show individual traces", value = TRUE),
                  sliderInput(ns("tc_trace_transparency"), "Trace transparency (%)",
                              0, 100, 50, 1, width = "100%"),
                  checkboxInput(ns("tc_show_avg_line"), "Show average trace", value = TRUE),
                  checkboxInput(ns("tc_show_ribbon"), "Show SEM ribbon", value = TRUE)
                ),

                div(class = "control-col",
                  div(class = "control-col-title", icon("paint-brush"), "Style"),
                  sliderInput(ns("tc_line_width"), "Line width",
                              0.5, 4, 2.0, 0.1, width = "100%"),
                  colourpicker::colourInput(ns("tc_line_color"), "Line color", value = "#000000"),
                  selectInput(ns("tc_legend_pos"), "Legend position",
                              choices = c("Auto" = "auto", "None" = "none", "Bottom" = "bottom",
                                          "Right" = "right", "Top" = "top", "Left" = "left"),
                              selected = "auto"),
                  selectInput(ns("tc_theme"), "Theme",
                              choices = c("classic", "minimal", "light", "dark"),
                              selected = "classic")
                ),

                div(class = "control-col",
                  div(class = "control-col-title", icon("font"), "Labels & Text"),
                  div(style = "display: flex; align-items: flex-start; gap: 8px;",
                      div(style = "flex: 1;",
                          textInput(ns("tc_title"), "Title", "")
                      ),
                      actionButton(ns("reset_title"), "Reset",
                                   class = "btn-default",
                                   style = "margin-top: 22px; height: 34px; padding: 6px 12px; font-size: 12px;",
                                   title = "Reset title to default (group names)")
                  ),
                  textInput(ns("tc_x"), "X axis label", "Time (s)"),
                  textInput(ns("tc_y"), "Y axis label", "\u0394F/F\u2080"),
                  sliderInput(ns("tc_base_font_size"), "Base font size", 8, 24, 14, 1, width = "100%"),
                  checkboxInput(ns("tc_bold_labels"), "Bold labels", value = TRUE),
                  selectInput(ns("tc_font"), "Font",
                              choices = c("Arial", "Helvetica", "Times", "Courier"),
                              selected = "Arial")
                ),

                div(class = "control-col",
                  div(class = "control-col-title", icon("arrows-alt"), "Axis & Export"),
                  checkboxInput(ns("tc_log_y"), "Log10 Y axis", FALSE),
                  checkboxInput(ns("tc_limits"), "Enable custom axis limits", FALSE),
                  uiOutput(ns("limits_panel")),
                  tags$details(
                    tags$summary(style = "cursor: pointer; font-weight: 600; font-size: 12px; color: var(--color-gray-600); margin: 4px 0 8px 0;",
                                 "Advanced axis options"),
                    div(style = "padding-top: 4px;",
                        textInput(ns("tc_x_breaks"), "X axis breaks (comma-separated)", ""),
                        textInput(ns("tc_y_breaks"), "Y axis breaks (comma-separated)", ""),
                        selectInput(ns("tc_tick_format"), "Tick format",
                                    choices = c("number", "scientific", "percent"),
                                    selected = "number")
                    )
                  ),
                  tags$hr(style = "margin: 10px 0;"),
                  fluidRow(
                    column(6, selectInput(ns("tc_dl_fmt"),"Format",
                                          choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"),
                                          selected = "png")),
                    column(6, selectInput(ns("tc_size_preset"), "Size",
                                          choices = c("6x4 in"="6x4","7x5 in"="7x5","8x6 in"="8x6","10x7.5 in"="10x7.5","12x8 in"="12x8"),
                                          selected = "8x6"))
                  ),
                  fluidRow(
                    column(6, numericInput(ns("tc_dl_w"),"Width (in)", 8, min = 4, max = 30)),
                    column(6, numericInput(ns("tc_dl_h"),"Height (in)", 6, min = 4, max = 30))
                  ),
                  numericInput(ns("tc_dl_dpi"),"DPI", 300, min = 72, max = 600),
                  browser_download_button(ns("dl_timecourse_plot_local"),"Download Time Course",
                                 class = "btn-primary", style = "width: 100%; margin-top: 10px;")
                )
  )
}

mod_time_course_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store the last known groups to detect actual changes
    last_groups <- reactiveVal(NULL)
    last_auto_title <- reactiveVal(NULL)

    # Only auto-update title when groups actually change (new data loaded)
    observeEvent(rv$groups, {
      req(rv$groups)
      current_groups <- paste(rv$groups, collapse = ", ")
      auto_title <- auto_plot_title(rv$groups)

      # Check if this is a real change in groups (new data loaded)
      if (!is.null(last_groups()) && last_groups() != current_groups) {
        # Only update title if it's currently empty or still the auto value
        current_title <- isolate(input$tc_title)
        if (is.null(current_title) || nchar(trimws(current_title)) == 0 ||
            current_title %in% c(last_groups(), last_auto_title())) {
          updateTextInput(session, "tc_title", value = auto_title)
        }
      } else if (is.null(last_groups())) {
        # First time - set initial title
        updateTextInput(session, "tc_title", value = auto_title)
      }

      # Update our stored groups
      last_groups(current_groups)
      last_auto_title(auto_title)
    }, ignoreInit = FALSE)

    # Toggle between static and interactive plot panels
    observeEvent(input$plot_type_toggle, {
      if (identical(input$plot_type_toggle, "Interactive")) {
        shinyjs::hide("static_panel")
        shinyjs::show("interactive_panel")
      } else {
        shinyjs::show("static_panel")
        shinyjs::hide("interactive_panel")
      }
    }, ignoreInit = TRUE)

    # Handle title reset button
    observeEvent(input$reset_title, {
      req(rv$groups)
      if (length(rv$groups) > 0) {
        updateTextInput(session, "tc_title", value = auto_plot_title(rv$groups))
      }
    })

    # Render limits panel (numeric inputs) — isolate defaults to avoid re-render loops
    output$limits_panel <- renderUI({
      if (is.null(input$tc_limits) || !isTRUE(input$tc_limits)) return(NULL)

      ns <- session$ns
      fluidRow(
        column(6, numericInput(ns("tc_xmin"),"X min", isolate(input$tc_xmin) %||% NA_real_)),
        column(6, numericInput(ns("tc_xmax"),"X max", isolate(input$tc_xmax) %||% NA_real_)),
        column(6, numericInput(ns("tc_ymin"),"Y min", isolate(input$tc_ymin) %||% NA_real_)),
        column(6, numericInput(ns("tc_ymax"),"Y max", isolate(input$tc_ymax) %||% NA_real_))
      )
    })

    has_valid_plot_size <- function(output_id, min_width = 120, min_height = 120) {
      width_px <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", ns(output_id), "_width")]]))
      height_px <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", ns(output_id), "_height")]]))
      is.finite(width_px) && is.finite(height_px) && width_px >= min_width && height_px >= min_height
    }

    safe_plot_dim <- function(output_id, axis = c("width", "height"), min_value = 200, max_value = 4000, default_value = 800) {
      axis <- match.arg(axis)
      raw_val <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", ns(output_id), "_", axis)]]))
      if (length(raw_val) != 1 || !is.finite(raw_val) || is.na(raw_val) || raw_val < min_value || raw_val > max_value) {
        return(default_value)
      }
      as.integer(round(raw_val))
    }

    # Build timecourse plot function
    build_timecourse_plot <- function() {
      req(rv$summary)
      summary_df <- rv$summary |>
        dplyr::filter(is.finite(Time), is.finite(mean_dFF0), is.finite(sem_dFF0))
      if (nrow(summary_df) == 0) {
        return(
          ggplot() + theme_void() +
            annotate("text", x = 0.5, y = 0.55, label = "No valid finite values to plot", size = 5, alpha = 0.7) +
            xlim(0, 1) + ylim(0, 1)
        )
      }

      long_df <- NULL
      if (!is.null(rv$long) && nrow(rv$long) > 0) {
        long_df <- rv$long |> dplyr::filter(is.finite(Time), is.finite(dFF0))
      }
      trace_df <- NULL
      show_traces <- isTRUE(input$tc_show_traces)

      p <- ggplot()

      # Add individual traces if requested
      if (show_traces && !is.null(long_df) && nrow(long_df) > 0) {
        trace_df <- long_df |>
          dplyr::group_by(Group, Cell) |>
          dplyr::filter(dplyr::n() > 1) |>
          dplyr::ungroup()

        # Calculate alpha
        transparency_pct <- as.numeric(input$tc_trace_transparency %||% 50)
        alpha_raw <- (100 - transparency_pct) / 100
        alpha_traces <- max(0.08, min(1.0, alpha_raw^1.5))

        # For single group, use gray for individual traces; otherwise use group colors
        groups <- unique(trace_df$Group)
        if (nrow(trace_df) > 0) {
          if (length(groups) == 1) {
            p <- p + geom_line(data=trace_df, aes(x=Time, y=dFF0, group=interaction(Group, Cell),
                                                 text = paste0("Group: ", Group, "\nCell: ", Cell, "\nTime: ", round(Time, 2), "s\nValue: ", round(dFF0, 3))),
                               inherit.aes=FALSE, alpha=alpha_traces, linewidth=0.4, color="gray50")
          } else {
            p <- p + geom_line(data=trace_df, aes(x=Time, y=dFF0, group=interaction(Group, Cell), color=Group,
                                                 text = paste0("Group: ", Group, "\nCell: ", Cell, "\nTime: ", round(Time, 2), "s\nValue: ", round(dFF0, 3))),
                               inherit.aes=FALSE, alpha=alpha_traces, linewidth=0.4)
          }
        }
      }

      # Add ribbon and mean line (conditional on toggle)
      has_line_color <- !is.null(input$tc_line_color) && is.character(input$tc_line_color) &&
        length(input$tc_line_color) == 1 && !is.na(input$tc_line_color) && nzchar(input$tc_line_color)

      ribbon_fill <- if (isTRUE(has_line_color)) input$tc_line_color else "gray50"

      group_counts <- summary_df |>
        dplyr::count(Group, name = "n_points")
      has_segments <- any(group_counts$n_points > 1)

      show_avg <- isTRUE(input$tc_show_avg_line %||% TRUE)
      show_ribbon <- isTRUE(input$tc_show_ribbon %||% TRUE)

      # Ribbon (only if average trace is shown). Explicit group keeps each
      # group's ribbon a separate polygon; without it a multi-file upload
      # draws one self-crossing ribbon sweeping across all groups
      multi_group <- length(unique(summary_df$Group)) > 1
      if (show_avg && show_ribbon && has_segments) {
        ribbon_alpha <- 0.25
        if (multi_group) {
          p <- p +
            geom_ribbon(data=summary_df,
                        aes(x=Time, ymin=mean_dFF0 - sem_dFF0, ymax=mean_dFF0 + sem_dFF0,
                            group=Group, fill=Group),
                        alpha=ribbon_alpha, color=NA)
        } else {
          p <- p +
            geom_ribbon(data=summary_df,
                        aes(x=Time, ymin=mean_dFF0 - sem_dFF0, ymax=mean_dFF0 + sem_dFF0,
                            group=Group),
                        fill=ribbon_fill, alpha=ribbon_alpha, color=NA)
        }
      }

      # Mean line (only if average trace is shown)
      lw <- input$tc_line_width %||% 2.0
      if (show_avg) {
        if (isTRUE(has_line_color)) {
          if (has_segments) {
            # Explicit group: the per-point tooltip text is a discrete
            # aesthetic, so without this each point becomes its own group
            # and geom_line draws no segments at all
            p <- p + geom_line(data=summary_df, aes(x=Time, y=mean_dFF0, color=Group, group=Group,
                                                    text = paste0("Group: ", Group, "\nTime: ", round(Time, 2), "s\nMean: ", round(mean_dFF0, 3), "\nSEM: ", round(sem_dFF0, 3))),
                               linewidth=lw)
          } else {
            p <- p + geom_point(data=summary_df, aes(x=Time, y=mean_dFF0, color=Group,
                                                     text = paste0("Group: ", Group, "\nTime: ", round(Time, 2), "s\nMean: ", round(mean_dFF0, 3), "\nSEM: ", round(sem_dFF0, 3))),
                                size=2.5)
          }
        } else {
          if (has_segments) {
            p <- p + geom_line(data=summary_df, aes(x=Time, y=mean_dFF0, group=Group,
                                                    text = paste0("Group: ", Group, "\nTime: ", round(Time, 2), "s\nMean: ", round(mean_dFF0, 3), "\nSEM: ", round(sem_dFF0, 3))),
                               color="black", linewidth=lw)
          } else {
            p <- p + geom_point(data=summary_df, aes(x=Time, y=mean_dFF0,
                                                     text = paste0("Group: ", Group, "\nTime: ", round(Time, 2), "s\nMean: ", round(mean_dFF0, 3), "\nSEM: ", round(sem_dFF0, 3))),
                                color="black", size=2.5)
          }
        }
      }

      # Apply colors
      groups <- unique(summary_df$Group)
      needs_color_scale <- FALSE
      cols <- NULL

      if (length(groups) > 1 || isTRUE(has_line_color)) {
        needs_color_scale <- TRUE
        cols <- rv$colors

        # The single Line color picker only overrides for one group; with a
        # multi-file upload the per-group palette keeps groups tellable apart
        if (isTRUE(has_line_color) && length(groups) == 1) {
          cols <- stats::setNames(rep(input$tc_line_color, length(groups)), groups)
        }
      }

      if (needs_color_scale) {
        if (!is.null(cols) && length(cols) > 0) {
          missing_groups <- setdiff(groups, names(cols))
          if (length(missing_groups) > 0) {
            default_cols <- rainbow(length(missing_groups))
            names(default_cols) <- missing_groups
            cols <- c(cols, default_cols)
          }
          # Wrapped labels over multiple rows keep long file-derived group
          # names inside the plot width
          p <- p + scale_color_manual(
            values = cols,
            labels = function(x) wrap_label(pretty_label(x), width = 28),
            guide = guide_legend(nrow = ceiling(length(groups) / 3), byrow = TRUE)
          )
          if (multi_group) {
            p <- p + scale_fill_manual(values=cols, guide="none")
          }
        } else {
          p <- p + scale_color_discrete()
        }
      }

      # Labels — derive bold from single toggle
      bold_labels <- isTRUE(input$tc_bold_labels)

      # Y-axis label with plotmath support for default
      y_lab <- if (!is.null(input$tc_y) && !is.na(input$tc_y) && nzchar(input$tc_y) && input$tc_y != "\u0394F/F\u2080") {
        input$tc_y
      } else {
        "\u0394F/F\u2080"
      }

      # The observer above writes the automatic dataset title into the input.
      # Once the user clears that input, NULL removes the plot title entirely;
      # the Reset button remains the explicit way to restore the default.
      title_lab <- optional_plot_title(input$tc_title)

      p <- p + labs(title = title_lab,
                    x = input$tc_x %||% "Time (s)",
                    y = y_lab)

      # Apply theme — derive all font sizes from base_font_size
      base_size <- input$tc_base_font_size %||% 14
      font <- input$tc_font %||% "Arial"
      label_face <- if (bold_labels) "bold" else "plain"

      base_theme <- switch(input$tc_theme %||% "classic",
                           classic=theme_classic(),
                           minimal=theme_minimal(),
                           light=theme_light(),
                           dark=theme_dark())

      # Resolve auto legend position
      legend_pos <- input$tc_legend_pos %||% "auto"
      if (identical(legend_pos, "auto")) {
        legend_pos <- if (length(groups) > 1) "bottom" else "none"
      }

      p <- p + base_theme + theme(
        plot.title = element_text(
          hjust=0.5,
          size=base_size + 4,
          face=label_face,
          family=font
        ),
        plot.subtitle = element_text(
          hjust=0.5,
          size=base_size,
          family=font
        ),
        axis.title = element_text(
          size=base_size,
          face=label_face,
          family=font
        ),
        axis.text = element_text(
          size=max(8, base_size - 2),
          family=font
        ),
        legend.position = legend_pos,
        legend.title = element_blank(),
        legend.text = element_text(size = max(10, base_size - 2), family = font),
        legend.key.width = grid::unit(22, "pt"),
        panel.grid = element_blank()
      )

      # Prepare Y-scale configuration
      y_breaks <- NULL
      y_lab_fun <- NULL

      # Custom X-axis breaks
      if (!is.null(input$tc_x_breaks) && !is.na(input$tc_x_breaks) && nzchar(input$tc_x_breaks)) {
        xb <- suppressWarnings(as.numeric(strsplit(input$tc_x_breaks, ",")[[1]]))
        xb <- xb[is.finite(xb)]
        if (length(xb) > 0) {
          p <- p + scale_x_continuous(breaks=xb)
        }
      }

      # Y-axis breaks: use explicit breaks if provided; otherwise auto-compute from data
      if (!is.null(input$tc_y_breaks) && !is.na(input$tc_y_breaks) && nzchar(input$tc_y_breaks)) {
        yb <- suppressWarnings(as.numeric(strsplit(input$tc_y_breaks, ",")[[1]]))
        yb <- yb[is.finite(yb)]
        if (length(yb) > 0) {
          y_breaks <- yb
          y_lab_fun <- switch(input$tc_tick_format %||% "number",
                              scientific = scales::label_scientific(digits=2),
                              percent = scales::label_percent(accuracy=0.01),
                              function(x) format(x, trim = TRUE, scientific = FALSE))
        }
      } else if (!isTRUE(input$tc_log_y)) {
        # Train automatic breaks on every visible layer. Using only the group
        # mean makes labels collapse at the bottom whenever an individual trace
        # legitimately extends beyond the summary range.
        y_range <- timecourse_visible_y_range(
          summary_df, trace_df,
          show_traces = show_traces,
          show_average = show_avg,
          show_ribbon = show_ribbon
        )
        if (length(y_range) == 2 && is.finite(y_range[1]) && is.finite(y_range[2])) {
          y_breaks <- compute_even_y_breaks(range(c(0, y_range)), expand = TRUE)
          if (length(y_breaks) > 1) {
            y_lab_fun <- function(x) format(x, trim = TRUE, scientific = FALSE)
          }
        }
      }

      # Custom axis limits
      if (isTRUE(input$tc_limits)) {
        xlims <- ylims <- NULL

        if (!is.null(input$tc_xmin) && !is.null(input$tc_xmax) &&
            !is.na(input$tc_xmin) && !is.na(input$tc_xmax)) {
          xlims <- c(input$tc_xmin, input$tc_xmax)
        }

        if (!is.null(input$tc_ymin) && !is.null(input$tc_ymax) &&
            !is.na(input$tc_ymin) && !is.na(input$tc_ymax)) {
          ylims <- c(input$tc_ymin, input$tc_ymax)
        }

        # If Y limits are set and no custom Y breaks, regenerate from limits
        if (!is.null(ylims) && (is.null(input$tc_y_breaks) || is.na(input$tc_y_breaks) || !nzchar(input$tc_y_breaks))) {
          if (!isTRUE(input$tc_log_y)) {
            y_breaks <- compute_even_y_breaks(ylims)
            if (length(y_breaks) > 0) {
              y_lab_fun <- function(x) format(x, trim = TRUE, scientific = FALSE)
            }
          }
        }

        if (!is.null(xlims) || !is.null(ylims)) {
          p <- p + coord_cartesian(xlim=xlims, ylim=ylims)
        }
      }

      # Apply Y scale once (log or linear)
      if (isTRUE(input$tc_log_y)) {
        p <- p + scale_y_log10(breaks = y_breaks, labels = y_lab_fun)
      } else if (!is.null(y_breaks) || !is.null(y_lab_fun)) {
        p <- p + scale_y_continuous(breaks = y_breaks, labels = y_lab_fun)
      }

      return(p)
    }

    # Reactive wrapper for the plot object
    tc_plot_reactive <- reactive({
      suppressWarnings(build_timecourse_plot())
    })

    # Render static plot
    output$timecourse_plot <- renderPlot({
      shiny::validate(shiny::need(
        !is.null(rv$summary) && nrow(rv$summary) > 0,
        "No data loaded. Go to the Load Data tab, upload your files, and click Process Data."
      ))
      tc_plot_reactive()
    })

    # Render interactive plot
    output$timecourse_plotly <- plotly::renderPlotly({
      req(rv$summary, nrow(rv$summary) > 0)
      p <- tc_plot_reactive()

      legend_pos <- input$tc_legend_pos %||% "auto"
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          yaxis = list(title = "\u0394F/F\u2080"),
          legend = list(orientation = if (legend_pos %in% c("none", "auto")) "h" else NULL),
          dragmode = "zoom"
        )
    })

    # Handle size preset changes
    observeEvent(input$tc_size_preset, {
      preset <- input$tc_size_preset
      dims <- switch(preset,
                     "6x4" = c(6,4),
                     "7x5" = c(7,5),
                     "8x6" = c(8,6),
                     "10x7.5" = c(10,7.5),
                     "12x8" = c(12,8),
                     c(8,6))
      updateNumericInput(session, "tc_dl_w", value = dims[1])
      updateNumericInput(session, "tc_dl_h", value = dims[2])
    }, ignoreInit = TRUE)

    # Render summary table
    output$tc_summary_table <- renderUI({
      req(rv$metrics)
      metric_cols <- c("Peak_dFF0","AUC","FWHM","FWHM_Lower_Bound","Half_Width","Calcium_Entry_Rate",
                       "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
      present <- intersect(metric_cols, names(rv$metrics))
      if (length(present) == 0) return(NULL)

      nice_name <- function(cl){
        switch(cl,
               Peak_dFF0 = "Peak \u0394F/F\u2080",
               Calcium_Entry_Rate = "10–90% \u0394F/F\u2080 Rise Rate",
               Time_to_Peak = "Time to Peak (s)",
               Time_to_25_Peak = "Time to 25% Peak (s)",
               Time_to_50_Peak = "Time to 50% Peak (s)",
               Time_to_75_Peak = "Time to 75% Peak (s)",
               Rise_Time = "Rise Time (s)",
               FWHM = "FWHM (exact/recovered, s)",
               FWHM_Lower_Bound = "FWHM Lower Bound (censored, s)",
               Half_Width = "Derived Half-Width (exact FWHM/2, s)",
               AUC = "Signed Net AUC (\u0394F/F\u2080 × s)",
               SNR = "SNR",
               cl)
      }

      # Summarize per experimental group — pooling groups into one mean
      # invites misinterpretation when conditions differ.
      censor_counts <- rv$metrics |>
        dplyr::group_by(Group) |>
        dplyr::summarise(
          total_n = dplyr::n(),
          censored_n = if ("FWHM_Censored" %in% names(rv$metrics)) {
            sum(FWHM_Censored %in% TRUE)
          } else {
            0L
          },
          .groups = "drop"
        )

      stats_df <- rv$metrics |>
        dplyr::select(Group, dplyr::all_of(present)) |>
        tidyr::pivot_longer(dplyr::all_of(present), names_to = "MetricCol", values_to = "Value") |>
        dplyr::group_by(Group, MetricCol) |>
        dplyr::summarise(
          n = sum(is.finite(Value)),
          Mean = mean(Value, na.rm = TRUE),
          SEM = stats::sd(Value, na.rm = TRUE) / max(1, sqrt(sum(is.finite(Value)))),
          .groups = "drop"
        ) |>
        dplyr::left_join(censor_counts, by = "Group") |>
        dplyr::rowwise() |>
        dplyr::mutate(Display = format_timecourse_metric_display(
          MetricCol, n, Mean, SEM, total_n, censored_n
        )) |>
        dplyr::ungroup()

      metric_names <- vapply(present, nice_name, character(1))
      wide <- stats_df |>
        dplyr::mutate(Metric = metric_names[match(MetricCol, present)]) |>
        dplyr::select(Metric, Group, Display) |>
        tidyr::pivot_wider(names_from = Group, values_from = Display)
      wide <- wide[match(metric_names, wide$Metric), , drop = FALSE]

      tb <- knitr::kable(wide, format = "html") |>
        kableExtra::kable_styling(full_width = TRUE,
                                  bootstrap_options = c("condensed", "striped", "hover"))
      note <- if (any(censor_counts$censored_n > 0)) {
        p(
          "Right-censored responses remained above half-maximum when recording ended. Exact rows include recovered responses only; exact widths exceed the reported observed lower bounds.",
          class = "small-help",
          style = "margin: 8px 10px 0;"
        )
      } else {
        NULL
      }
      tagList(htmltools::HTML(tb), note)
    })

    # Download handler
    register_browser_download(input, session, "dl_timecourse_plot_local",
      filename = function() {
        build_export_filename(
          rv,
          parts = "timecourse",
          ext = input$tc_dl_fmt %||% "png"
        )
      },
      content = function(file) {
        req(rv$summary)
        p <- tc_plot_reactive()
        ggplot2::ggsave(file, plot = p, width = input$tc_dl_w, height = input$tc_dl_h, dpi = input$tc_dl_dpi)
      }
    )

    # Return the plot reactive for external use (e.g. global export)
    list(
      plot = tc_plot_reactive
    )
  })
}
