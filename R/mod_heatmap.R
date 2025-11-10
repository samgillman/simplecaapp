# R/mod_heatmap.R

mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "heatmap",
          fluidRow(
            box(title = "Controls", status = "primary", solidHeader = TRUE, width = 4, collapsible = FALSE,
                # Baseline adjustment section
                div(style = "background-color: #fff3cd; padding: 10px; margin-bottom: 15px; border-radius: 5px; border: 1px solid #ffc107;",
                    h5("⚠️ Baseline Adjustment", style = "font-weight: bold; color: #856404; margin-top: 0;"),
                    p("Changing baseline will recalculate all data and metrics.",
                      style = "font-size: 12px; color: #856404; margin-bottom: 10px;"),
                    conditionalPanel(
                      condition = "output.baseline_method_is_frame_range",
                      ns = ns,
                      sliderInput(ns("hm_baseline_frames"), "Baseline Frame Range:",
                                  min = 1, max = 100,
                                  value = c(1, 20),
                                  step = 1),
                      actionButton(ns("apply_baseline"), "Apply Baseline Change",
                                   class = "btn-warning btn-sm",
                                   style = "width: 100%;")
                    ),
                    conditionalPanel(
                      condition = "!output.baseline_method_is_frame_range",
                      ns = ns,
                      p("Only available for 'Frame Range' method.",
                        style = "font-size: 12px; color: #856404; font-style: italic;")
                    )
                ),

                selectInput(ns("hm_sort"),"Sort cells by", choices = c("Time to Peak"="tpeak","Peak Amplitude"="amp","Original"="orig"), selected="tpeak"),
                selectInput(ns("hm_palette"),"Color palette", choices = c("plasma","viridis","magma","inferno","cividis"), selected = "plasma"),
                tags$hr(),
                
                # --- Scale Control ---
                sliderInput(ns("hm_scale_step"), "Scale step size", 
                           min = 0.1, max = 1.0, value = 0.5, step = 0.1,
                           helpText("Controls legend interval spacing")),
                tags$hr(),
                
                # --- Title and Labels ---
                textInput(ns("hm_title"),"Plot title","Population Heatmap"),
                checkboxInput(ns("hm_center_title"), "Center title", value = TRUE),
                textInput(ns("hm_x_label"),"X label","Time (s)"),
                textInput(ns("hm_y_label"),"Y label","Cell"),
                tags$details(
                  tags$summary(style = "cursor:pointer; font-weight:600; color:#0072B2;", "Appearance & Typography"),
                  div(style = "margin-top:8px;",
                      h6("Typography & Sizing", style = "font-weight: bold; margin-top: 15px;"),
                      sliderInput(ns("hm_title_size"),"Title size", 10, 24, 16, 1),
                      checkboxInput(ns("hm_bold_title"), "Bold title", value = TRUE),
                      sliderInput(ns("hm_axis_title_size"),"Axis title size", 8, 24, 14, 1),
                      checkboxInput(ns("hm_bold_axis_title"), "Bold axis titles", value = TRUE),
                      sliderInput(ns("hm_axis_text_size"),"Axis text size", 8, 24, 12, 1),
                      checkboxInput(ns("hm_bold_axis_text"), "Bold axis text", value = FALSE),
                      sliderInput(ns("hm_legend_text_size"),"Legend text size", min = 6, max = 24, value = 10, step = 1),
                      checkboxInput(ns("hm_bold_legend_text"), "Bold legend text", value = FALSE),
                      selectInput(ns("hm_font"), "Font", 
                                  choices = c("Arial", "Helvetica", "Times", "Courier"), 
                                  selected = "Arial")
                  )
                )
            ),
            box(title = "Heatmap", solidHeader = TRUE, width = 8, collapsible = FALSE,
                withSpinner(plotOutput(ns("heatmap_plot"), height = "760px"), type = 4),
                tags$hr(),
                fluidRow(
                  column(3, selectInput(ns("hm_dl_fmt"),"Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), selected = "png")),
                  column(3, selectInput(ns("hm_size_preset"), "Size", choices = c("6x4 in"="6x4","7x5 in"="7x5","8x6 in"="8x6","10x7.5 in"="10x7.5","12x8 in"="12x8"), selected = "8x6")),
                  column(3, numericInput(ns("hm_dl_w"),"Width (in)", 8, min = 4, max = 30)),
                  column(3, numericInput(ns("hm_dl_h"),"Height (in)", 6, min = 4, max = 30))
                ),
                fluidRow(
                  column(3, numericInput(ns("hm_dl_dpi"),"DPI", 300, min = 72, max = 600)),
                  column(9, div(style = "text-align:right; margin-top:6px;", downloadButton(ns("dl_heatmap_plot_local"),"Download Heatmap")))
                )
            )
          )
  )
}

mod_heatmap_server <- function(id, rv) {
    moduleServer(id, function(input, output, session) {

      # Output to control conditional panel visibility
      output$baseline_method_is_frame_range <- reactive({
        identical(rv$baseline_method, "frame_range")
      })
      outputOptions(output, "baseline_method_is_frame_range", suspendWhenHidden = FALSE)

      # Sync slider with current baseline when data is loaded
      observeEvent(rv$baseline_frames, {
        if (!is.null(rv$baseline_frames) && length(rv$baseline_frames) == 2) {
          updateSliderInput(session, "hm_baseline_frames", value = rv$baseline_frames)
        }
      }, ignoreInit = TRUE)

      # Handle baseline change application
      observeEvent(input$apply_baseline, {
        req(rv$raw_traces, rv$baselines, input$hm_baseline_frames)

        # Ensure baseline method is frame_range
        if (!identical(rv$baseline_method, "frame_range")) {
          showNotification("Baseline adjustment only available for Frame Range method", type = "warning", duration = 3)
          return()
        }

        withProgress(message = "Recalculating with new baseline...", value = 0, {
          new_baseline_frames <- input$hm_baseline_frames

          # Update rv$baseline_frames
          rv$baseline_frames <- new_baseline_frames

          # Reprocess all data with new baseline
          dts <- list()
          baselines <- list()

          for (group_name in names(rv$raw_traces)) {
            incProgress(1/length(rv$raw_traces), detail = paste("Processing:", group_name))

            dt <- data.table::copy(rv$raw_traces[[group_name]])

            # Calculate new F0 with new baseline frames
            start_frame <- max(1, as.integer(new_baseline_frames[1]))
            end_frame <- min(nrow(dt), as.integer(new_baseline_frames[2]))

            F0 <- vapply(seq(2, ncol(dt)), function(j) {
              mean(dt[[j]][start_frame:end_frame], na.rm = TRUE)
            }, numeric(1))

            baselines[[group_name]] <- setNames(F0, names(dt)[-1])

            # Apply new ΔF/F₀ transformation
            for (k in seq_along(F0)) {
              j <- k + 1
              f0 <- F0[[k]]
              if (is.finite(f0) && f0 > 1e-6) {
                dt[[j]] <- (dt[[j]] - f0) / f0
              } else if (is.finite(f0) && f0 <= 1e-6) {
                dt[[j]] <- dt[[j]] - f0
              }
            }

            dts[[group_name]] <- dt
          }

          # Update reactive values
          rv$dts <- dts
          rv$baselines <- baselines

          # Recalculate long format and summary
          rv$long <- purrr::imap(dts, ~to_long(.x, .y)) |> dplyr::bind_rows()
          rv$summary <- if (nrow(rv$long) > 0) {
            rv$long |>
              dplyr::group_by(Group, Time) |>
              dplyr::summarise(mean_dFF0 = mean(dFF0, na.rm = TRUE),
                             sem_dFF0 = stats::sd(dFF0, na.rm = TRUE)/sqrt(dplyr::n()),
                             sd_dFF0 = stats::sd(dFF0, na.rm = TRUE),
                             n_cells = dplyr::n(), .groups = "drop")
          } else NULL

          # Recalculate metrics
          rv$metrics <- purrr::imap(dts, ~compute_metrics_for_dt(.x, .y, new_baseline_frames)) |>
            dplyr::bind_rows()

          showNotification("Baseline updated successfully! All metrics recalculated.",
                          type = "message", duration = 3)
        })
      })

      # Simple 0.5 interval scale (reverted from complex data-driven approach)

    heatmap_plot_reactive <- reactive({
      req(rv$dts)
      build_hm <- function(dt, label) {
        time_vec <- dt$Time
        dnum <- coerce_numeric_dt(dt)
        mat <- as.matrix(dnum[, -1])
        
        # Debug: check for NA values
        na_count <- sum(is.na(mat))
        if (na_count > 0) {
          warning("Found ", na_count, " NA values in heatmap data for group: ", label)
        }
        
        valid <- apply(mat, 2, function(x) !all(is.na(x)))
        mat <- mat[, valid, drop=FALSE]
        if (ncol(mat) == 0) return(NULL)
        
        ord <- seq_len(ncol(mat))
        if (input$hm_sort == "tpeak") {
          tpk <- apply(mat, 2, function(x) if (all(is.na(x))) Inf else which.max(x))
          ord <- order(tpk)
        } else if (input$hm_sort == "amp") {
          amp <- apply(mat, 2, function(x) if (all(is.na(x))) -Inf else max(x, na.rm = TRUE))
          ord <- order(amp, decreasing = TRUE)
        }
        mat <- mat[, ord, drop=FALSE]
        
        hm <- expand.grid(Time = time_vec, Cell = seq_len(ncol(mat)))
        hm$Value <- as.vector(mat); hm$Group <- label; hm
      }
      
      all_hm <- purrr::imap(rv$dts, ~build_hm(.x, .y)) |> purrr::compact() |> dplyr::bind_rows()
      validate(need(nrow(all_hm) > 0, "No valid data for heatmap"))
      
      # Dynamic legend breaks based on actual data range (exclude negative values)
      rng <- range(all_hm$Value, na.rm = TRUE)
      
      # Debug: check data range and NA values
      cat("Heatmap data range:", rng[1], "to", rng[2], "\n")
      cat("Total NA values in heatmap:", sum(is.na(all_hm$Value)), "\n")
      cat("Data summary:", summary(all_hm$Value), "\n")
      
      # For visualization: treat negative values as the minimum scale value (darkest color)
      if (rng[1] < 0) {
        cat("Found negative values, treating them as minimum scale value for visualization. Original range:", rng[1], "to", rng[2], "\n")
        # Create a copy for visualization where negatives become the minimum scale value
        all_hm_viz <- all_hm
        all_hm_viz$Value <- pmax(all_hm_viz$Value, 0)
        rng_viz <- range(all_hm_viz$Value, na.rm = TRUE)
        cat("Visualization range (negatives as 0):", rng_viz[1], "to", rng_viz[2], "\n")
      } else {
        all_hm_viz <- all_hm
        rng_viz <- rng
      }
      
      all_hm_viz <- dplyr::arrange(all_hm_viz, Group, Time, Cell)  # Avoid raster seams on some devices
      
      # User-controlled scale step size - always start from 0 for consistent coloring
      scale_step <- input$hm_scale_step
      upper <- ceiling(rng_viz[2] / scale_step) * scale_step
      brks <- seq(0, upper, by = scale_step)
      
      ggplot(all_hm_viz, aes(Time, Cell, fill = Value)) +
        geom_raster() +
        facet_wrap(~ Group, ncol = 1, scales = "free_y") +
        scale_fill_viridis_c(
          name   = expression(Delta*"F/F"[0]),
          option = input$hm_palette,
          limits = c(0, upper),          # dynamic scale from 0 to upper (user-controlled step)
          breaks = brks, labels = brks,
          oob    = scales::squish,       # values above top just saturate
          na.value = "gray90"  # Light gray for missing values instead of transparent
        )+
        guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.3)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(title = input$hm_title, x = input$hm_x_label, y = input$hm_y_label) +
        theme_classic(base_size = 14) +
        theme(
          plot.title = element_text(
            size = input$hm_title_size,
            face = if (isTRUE(input$hm_bold_title)) "bold" else "plain",
            hjust = if (isTRUE(input$hm_center_title)) 0.5 else 0,
            family = input$hm_font
          ),
          plot.subtitle = element_text(margin = margin(b = 4)),
          axis.title = element_text(
            size = input$hm_axis_title_size,
            face = if (isTRUE(input$hm_bold_axis_title)) "bold" else "plain",
            family = input$hm_font
          ),
          axis.text = element_text(
            size = input$hm_axis_text_size,
            face = if (isTRUE(input$hm_bold_axis_text)) "bold" else "plain",
            family = input$hm_font
          ),
          legend.text = element_text(
            size = input$hm_legend_text_size,
            face = if (isTRUE(input$hm_bold_legend_text)) "bold" else "plain",
            family = input$hm_font
          ),
          legend.title = element_text(
            size = max(6, input$hm_legend_text_size + 2),
            family = input$hm_font,
            face = "bold" 
          ),
          strip.background = element_blank(),
          strip.text = element_blank()
        )
    })
    
    observeEvent(input$hm_size_preset, {
      preset <- input$hm_size_preset
      dims <- switch(preset,
                     "6x4" = c(6,4),
                     "7x5" = c(7,5),
                     "8x6" = c(8,6),
                     "10x7.5" = c(10,7.5),
                     "12x8" = c(12,8), c(8,6))
      updateNumericInput(session, "hm_dl_w", value = dims[1])
      updateNumericInput(session, "hm_dl_h", value = dims[2])
    }, ignoreInit = TRUE)
    
    output$heatmap_plot <- renderPlot({
      heatmap_plot_reactive()
    })
    
    output$dl_heatmap_plot_local <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = c("heatmap_plot"),
          ext = input$hm_dl_fmt %||% "png"
        )
      },
      content = function(file) {
        req(heatmap_plot_reactive())
        ggplot2::ggsave(file, plot = heatmap_plot_reactive(), width = input$hm_dl_w, height = input$hm_dl_h, dpi = input$hm_dl_dpi)
      }
    )
    
    list(plot = heatmap_plot_reactive)
  })
}
