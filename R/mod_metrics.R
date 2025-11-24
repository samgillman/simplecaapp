# R/mod_metrics.R

mod_metrics_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "metrics",
          fluidRow(
            theme_box(title = "Controls", status = "primary", solidHeader = TRUE, width = 4,
                # Metric Selection Accordion
                accordion(
                  id = ns("metric_accordion"),
                  title = "Metric & Display",
                  icon = "chart-bar",
                  expanded = TRUE,
                  content = div(
                    selectInput(ns("metric_name"), "Metric",
                                choices = c("Peak ΔF/F₀" = "Peak_dFF0", "Time to Peak (s)" = "Time_to_Peak",
                                            "Time to 25% Peak (s)" = "Time_to_25_Peak", "Time to 50% Peak (s)" = "Time_to_50_Peak",
                                            "Time to 75% Peak (s)" = "Time_to_75_Peak", "Rise Time (s)" = "Rise_Time",
                                            "FWHM (s)" = "FWHM",
                                            "Half Width (HWHM)" = "Half_Width",
                                            "Ca²⁺ Entry Rate (ΔF/F₀/s)" = "Calcium_Entry_Rate", "AUC" = "AUC",
                                            "SNR" = "SNR"),
                                selected = "Peak_dFF0"),
                    selectInput(ns("metric_plot_style"), "Plot style",
                                choices = c("Box + swarm" = "boxswarm", "Bars" = "bars", "Violin" = "violin"),
                                selected = "bars"),
                    conditionalPanel(paste0("input['", ns("metric_plot_style"), "'] == 'bars'"),
                                     colourpicker::colourInput(ns("metric_bar_color"), "Bar color", value = "#B3B3B3", allowTransparent = FALSE)
                    ),
                    checkboxInput(ns("metric_sort_cells"), "Sort cell bars within group", TRUE),
                    checkboxInput(ns("metric_show_summary"), "Show mean ± SEM", TRUE)
                  )
                ),

                # Labels Accordion
                accordion(
                  id = ns("labels_accordion"),
                  title = "Labels",
                  icon = "tag",
                  expanded = FALSE,
                  content = div(
                    textInput(ns("metric_title"), "Custom title (optional)", ""),
                    checkboxInput(ns("metric_auto_y"), "Auto y-label (use metric units)", TRUE),
                    conditionalPanel(paste0("!input['", ns("metric_auto_y"), "']"),
                                     textInput(ns("metric_y_label"), "Y label", "Value"))
                  )
                ),

                # Appearance Accordion
                accordion(
                  id = ns("appearance_accordion"),
                  title = "Appearance",
                  icon = "paint-brush",
                  expanded = FALSE,
                  content = div(
                    sliderInput(ns("metric_inset_scale"), "Inset size", min = 0.5, max = 3, value = 1, step = 0.1),
                    numericInput(ns("metric_highlight_k"), "Highlight top/bottom K", 0, min = 0, max = 100, step = 1),
                    tags$hr(style = "margin: 12px 0;"),
                    h6("Typography", style = "font-weight: 600; margin-bottom: 8px;"),
                    checkboxInput(ns("metric_bold_axes"), "Bold axis titles", TRUE),
                    selectInput(ns("metric_font"), "Font Family",
                                choices = c("Sans-Serif" = "sans", "Serif" = "serif", "Monospace" = "mono"),
                                selected = "sans"),
                    sliderInput(ns("metric_size"), "Base font size", 8, 22, 14, 1)
                  )
                )
            ),
            theme_box(title = "Metrics Plot", status = "primary", solidHeader = TRUE, width = 8,
                fluidRow(
                  column(12, align = "right",
                         radioGroupButtons(
                           inputId = ns("plot_type_toggle"),
                           label = NULL,
                           choices = c("Static", "Interactive"),
                           selected = "Static",
                           status = "primary",
                           size = "sm"
                         )
                  )
                ),
                conditionalPanel(paste0("input['", ns("plot_type_toggle"), "'] == 'Static'"),
                                 withSpinner(plotOutput(ns("metrics_plot"), height = "640px"), type = 4)
                ),
                conditionalPanel(paste0("input['", ns("plot_type_toggle"), "'] == 'Interactive'"),
                                 withSpinner(plotlyOutput(ns("metrics_plotly"), height = "640px"), type = 4)
                ),
                tags$hr(),
                fluidRow(
                  column(3, selectInput(ns("dl_format"), "Format", c("PNG"="png", "PDF"="pdf", "SVG"="svg", "TIFF"="tiff"), "png")),
                  column(3, selectInput(ns("metric_size_preset"), "Size", choices = c("6x4 in"="6x4","7x5 in"="7x5","8x6 in"="8x6","10x7.5 in"="10x7.5","12x8 in"="12x8"), selected = "8x6")),
                  column(3, numericInput(ns("dl_width"), "Width (in)", 8, 3, 20, 0.5)),
                  column(3, numericInput(ns("dl_height"), "Height (in)", 6, 3, 20, 0.5))
                ),
                fluidRow(
                  column(3, numericInput(ns("dl_dpi"), "DPI", 300, 72, 600, 5)),
                  column(9, div(style = "margin-top:10px; text-align:right;", downloadButton(ns("dl_plot"), "Download Plot", class = "btn-primary")))
                )
            )
          )
  )
}

mod_metrics_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression to build the plot object
    metrics_plot_obj <- reactive({
      # Empty state check
      if (is.null(rv$metrics) || nrow(rv$metrics) == 0) {
        return(
          ggplot() + 
            theme_void() +
            annotate("text", x = 0.5, y = 0.6, label = "Upload data in 'Load Data' then click Process", size = 6, alpha = 0.7) +
            annotate("text", x = 0.5, y = 0.45, label = "Metrics plot will render here", size = 4.5, alpha = 0.6) +
            xlim(0,1) + ylim(0,1)
        )
      }

      metric <- input$metric_name
      df <- dplyr::filter(rv$metrics, is.finite(.data[[metric]]))
      validate(need(nrow(df) > 0, "No finite values for this metric."))
      
      # Determine axis title font face
      axis_face <- if (isTRUE(input$metric_bold_axes)) "bold" else "plain"
      
      base <- theme_classic(base_size=input$metric_size, base_family = input$metric_font) +
        theme(legend.position = "none",
              axis.title.x = element_text(face = axis_face),
              axis.title.y = element_text(face = axis_face),
              axis.text = element_text(),
              plot.margin = margin(10, 25, 10, 10))
      
      y_lab <- if (isTRUE(input$metric_auto_y)) metric_label(metric) else input$metric_y_label
      title_txt <- if (nzchar(input$metric_title)) input$metric_title else metric_title(metric)
      
      # Summary statistics
      mean_val <- mean(df[[metric]], na.rm = TRUE)
      sem_val <- stats::sd(df[[metric]], na.rm = TRUE) / sqrt(nrow(df))
      n_cells <- nrow(df)
      label_df <- data.frame(xpos = 1.5, ypos = max(df[[metric]], na.rm = TRUE) * 0.98,
                             label = sprintf("Mean ± SEM: %.3g ± %.3g\nn = %d", mean_val, sem_val, n_cells))
      
      style <- input$metric_plot_style %||% "boxswarm"
      p <- ggplot()
      
      if (identical(style, "bars")) {
        df2 <- df
        if (isTRUE(input$metric_sort_cells)) {
          df2 <- df2 |> dplyr::arrange(.data[[metric]]) |> dplyr::mutate(Cell_Idx = dplyr::row_number())
        } else {
          df2 <- df2 |> dplyr::mutate(Cell_Idx = dplyr::row_number())
        }
        bar_fill <- input$metric_bar_color %||% "#B3B3B3"
        p <- ggplot(df2, aes(x = Cell_Idx, y = .data[[metric]], 
                             text = paste0("Cell: ", Cell, "\nGroup: ", Group, "\nValue: ", round(.data[[metric]], 3)))) +
          geom_col(width = 0.85, alpha = 0.9, color = "black", fill = bar_fill, linewidth = 0.2)
        
        # Highlight extremes
        k <- as.integer(input$metric_highlight_k %||% 0)
        if (k > 0) {
          ord <- order(df2[[metric]])
          idx <- unique(c(head(ord, k), tail(ord, k)))
          p <- p + geom_col(data = df2[idx, ], aes(x = Cell_Idx, y = .data[[metric]]),
                            width = 0.85, fill = "#5bc0de", color = "black", linewidth = 0.2)
        }
        
        p <- p + labs(x = "Cell number", y = y_lab, title = title_txt) + base +
          scale_x_continuous(breaks = scales::pretty_breaks()) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = max(7, input$metric_size * 0.6)))
        
      } else if (identical(style, "boxswarm")) {
        df$One <- "Cells"
        p <- ggplot(df, aes(x = One, y = .data[[metric]])) +
          geom_boxplot(outlier.shape = NA, width = 0.25, fill = "grey85", color = "black") +
          geom_jitter(width = 0.12, height = 0, size = 1, alpha = 0.5, 
                      aes(text = paste0("Cell: ", Cell, "\nGroup: ", Group, "\nValue: ", round(.data[[metric]], 3)))) +
          labs(x = NULL, y = y_lab, title = title_txt) + base
      } else { # violin
        df$One <- "Cells"
        p <- ggplot(df, aes(x = One, y = .data[[metric]])) +
          geom_violin(trim = FALSE, fill = "grey85", color = "black", width = 0.8) +
          geom_jitter(width = 0.12, height = 0, size = 1, alpha = 0.4,
                      aes(text = paste0("Cell: ", Cell, "\nGroup: ", Group, "\nValue: ", round(.data[[metric]], 3)))) +
          labs(x = NULL, y = y_lab, title = title_txt) + base
      }
      
      # Mean ± SEM overlay
      if (isTRUE(input$metric_show_summary)) {
        p <- p + geom_hline(yintercept = mean_val, color = "#0072B2", linewidth = 0.7) +
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = mean_val - sem_val, ymax = mean_val + sem_val,
                   alpha = 0.08, fill = "#0072B2")
      }
      
      # Inset label (bars only)
      if (identical(style, "bars")) {
        lab_size_val <- max(3, input$metric_size * 0.18) * input$metric_inset_scale
        p <- p + geom_label(data = label_df, aes(x = xpos, y = ypos, label = label),
                            inherit.aes = FALSE, size = lab_size_val,
                            label.size = 0.15, alpha = 0.9, hjust = 0,
                            family = input$metric_font)
      }
      
      p + scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
    })
    
    output$metrics_plot <- renderPlot({ metrics_plot_obj() })

    output$metrics_plotly <- plotly::renderPlotly({
      req(metrics_plot_obj())
      plotly::ggplotly(metrics_plot_obj(), tooltip = "text") %>%
        plotly::layout(
          hoverlabel = list(bgcolor = "white", font = list(family = input$metric_font %||% "sans")),
          xaxis = list(fixedrange = FALSE),
          yaxis = list(fixedrange = FALSE),
          dragmode = "zoom"
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("lasso2d", "select2d")
        )
    })

    observeEvent(input$metric_size_preset, {
      preset <- input$metric_size_preset
      dims <- switch(preset,
                     "6x4" = c(6,4),
                     "7x5" = c(7,5),
                     "8x6" = c(8,6),
                     "10x7.5" = c(10,7.5),
                     "12x8" = c(12,8), c(8,6))
      updateNumericInput(session, "dl_width", value = dims[1])
      updateNumericInput(session, "dl_height", value = dims[2])
    }, ignoreInit = TRUE)
    
    output$dl_plot <- downloadHandler(
      filename = function() {
        metric_name <- gsub(" ", "_", tolower(input$metric_name %||% "metric"))
        build_export_filename(
          rv,
          parts = c(metric_name, "plot"),
          ext = input$dl_format %||% "png"
        )
      },
      content = function(file) {
        ggsave(file, plot = metrics_plot_obj(),
               width = input$dl_width, height = input$dl_height,
               dpi = input$dl_dpi, device = input$dl_format)
      }
    )
    
    list(plot = metrics_plot_obj)
  })
}
