# R/mod_load_data.R

mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "load",
          fluidRow(
            # Left Column: Upload and Settings
            column(
              width = 6,
              theme_box(
                title = "Load Data",
                icon = icon("cloud-upload-alt"),
                width = 12,
                div(style = "padding: 8px 0;",
                    fileInput(ns("data_files"), "Upload CSV or Excel (wide format)", 
                              multiple = TRUE,
                              accept = c(".csv", ".xlsx", ".xls"),
                              width = "100%"),
                    
                    # Helper text
                    div(class = "text-muted small", style = "margin-top: -10px; margin-bottom: 15px;",
                        icon("info-circle"), " Format: First column must be 'Time', subsequent columns are cells."
                    ),

                    # Add JavaScript to fix the file input after page loads
                    tags$script(HTML(paste0("
                        $(document).ready(function() {
                          $(document).on('shiny:connected', function() {
                            setTimeout(function() {
                              var fileInput = $('#", ns("data_files"), "');
                              var fileInputElement = fileInput[0];
                              if (fileInputElement) {
                                var buttonArea = fileInput.closest('.form-group').find('.btn-file');
                                var textArea = fileInput.closest('.form-group').find('input[type=\"text\"]');
                                fileInputElement.style.display = 'block';
                                fileInputElement.style.position = 'absolute';
                                fileInputElement.style.top = '0';
                                fileInputElement.style.left = '0';
                                fileInputElement.style.width = '100%';
                                fileInputElement.style.height = '100%';
                                fileInputElement.style.opacity = '0';
                                fileInputElement.style.cursor = 'pointer';
                                fileInputElement.style.zIndex = '1000';
                                var inputGroup = fileInput.closest('.input-group');
                                if (inputGroup.length > 0) inputGroup[0].style.position = 'relative';
                                if (textArea.length > 0) {
                                  textArea.css('cursor', 'pointer');
                                  textArea.on('click', function(e) { e.preventDefault(); fileInputElement.click(); });
                                }
                              }
                            }, 1000);
                          });
                        });
                      ")))
                )
              ),
              
              theme_box(
                title = "Processing Options", 
                icon = icon("cogs"),
                width = 12,
                div(style = "padding: 8px 0;",
                    # Baseline Correction Section
                    h5("Baseline Correction (F₀)", style = "font-weight: 600; color: var(--color-gray-900); border-bottom: 1px solid var(--color-gray-100); padding-bottom: 8px;"),
                    
                    fluidRow(
                      column(6, 
                             selectInput(ns("pp_baseline_method"), "Method",
                                         choices = c("Frame Range" = "frame_range", 
                                                     "Rolling Minimum" = "rolling_min", 
                                                     "Percentile" = "percentile"),
                                         selected = "frame_range", width = "100%")
                      ),
                      column(6,
                             conditionalPanel(paste0("input['", ns("pp_baseline_method"), "'] == 'frame_range'"),
                                              sliderInput(ns("pp_baseline_frames"), "Baseline Frames", min = 1, max = 100, value = c(1, 20), step = 1, width = "100%")
                             ),
                             conditionalPanel(paste0("input['", ns("pp_baseline_method"), "'] == 'rolling_min'"),
                                              numericInput(ns("pp_window_size"), "Window Size (frames)", value = 50, min = 5, step = 1, width = "100%")
                             ),
                             conditionalPanel(paste0("input['", ns("pp_baseline_method"), "'] == 'percentile'"),
                                              numericInput(ns("pp_percentile"), "Percentile (1-50)", value = 10, min = 1, max = 50, step = 1, width = "100%")
                             )
                      )
                    ),
                    
                    # Advanced Options Accordion
                    div(style = "margin-top: 15px;",
                        accordion(
                          id = ns("advanced_opts"),
                          title = "Advanced Options",
                          icon = "sliders-h",
                          content = div(
                            numericInput(ns("pp_sampling_rate"), "Sampling Rate (Hz) - used if Time column missing", value = 1, min = 0.0001, step = 0.1, width = "100%"),
                            p("Note: Only applies if the uploaded file lacks a valid 'Time' column.", class = "text-muted small")
                          )
                        )
                    ),
                    
                    # Action Footer
                    div(style = "margin-top: 24px; display: flex; align-items: center; justify-content: space-between; background: var(--color-gray-50); padding: 15px; border-radius: var(--radius-md);",
                        div(class = "text-muted small", 
                            icon("calculator"), " ΔF/F₀ = (F - F₀)/F₀"
                        ),
                        primary_button(ns("load_btn"), "Process Data", icon = icon("play"), width = "160px")
                    )
                )
              )
            ),
            
            # Right Column: Statistics and Status
            column(
              width = 6,
              theme_box(
                title = "Data Overview",
                icon = icon("chart-pie"), 
                status = "info",
                width = 12,
                fluidRow(
                  column(4, stat_card(textOutput(ns("n_files_text"), inline = TRUE), "Files")),
                  column(4, stat_card(textOutput(ns("n_cells_text"), inline = TRUE), "Total Cells")),
                  column(4, stat_card(textOutput(ns("n_timepoints_text"), inline = TRUE), "Timepoints"))
                )
              ),
              
              theme_box(
                title = "Processing Status",
                icon = icon("tasks"),
                width = 12,
                div(style = "padding: 10px 0;",
                    fluidRow(
                      status_step("file-import", "Files Loaded", 
                                  textOutput(ns("status_files_loaded")), 
                                  color = "var(--color-info)"),
                      status_step("check-circle", "Processing", 
                                  textOutput(ns("status_processing")), 
                                  color = "var(--color-success)"),
                      status_step("calculator", "Metrics", 
                                  textOutput(ns("status_metrics")), 
                                  color = "var(--color-warning)"),
                      status_step("chart-line", "Ready", 
                                  textOutput(ns("status_ready")), 
                                  color = "var(--color-primary-blue)")
                    )
                )
              )
            )
          )
  )
}

mod_load_data_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$load_btn, {
      req(input$data_files)
      withProgress(message="Processing data...", value=0, {
        files <- input$data_files; rv$files <- files
        labels <- tools::file_path_sans_ext(basename(files$name))
        rv$groups <- labels; rv$colors <- default_group_colors(labels)
        dts <- list(); n_files <- nrow(files)
        raw_traces <- list(); baselines <- list()
        
        for (i in seq_len(n_files)) {
          incProgress(1/n_files, detail = paste("Loading:", basename(files$name[i])))
          dt <- safe_read(files$datapath[i])
          if (ncol(dt) < 2) next
          dt <- ensure_time_first(dt) |> coerce_numeric_dt()
          
          if (!all(is.finite(dt[[1]])) || any(diff(dt[[1]]) <= 0, na.rm=TRUE)) {
            sr <- as.numeric(input$pp_sampling_rate %||% 1)
            dt[[1]] <- seq(0, by=1/sr, length.out=nrow(dt))
          }
          
          raw_traces[[labels[i]]] <- data.table::copy(dt)
          
          # Processing logic
          if (identical(input$pp_baseline_method,"frame_range")) {
            start_frame <- max(1, as.integer(input$pp_baseline_frames[1] %||% 2))
            end_frame <- min(nrow(dt), as.integer(input$pp_baseline_frames[2] %||% 20))
            F0 <- vapply(seq(2, ncol(dt)), function(j) mean(dt[[j]][start_frame:end_frame], na.rm=TRUE), numeric(1))
          } else if (identical(input$pp_baseline_method,"rolling_min")) {
            win <- max(5, as.integer(input$pp_window_size %||% 50))
            F0 <- vapply(seq(2, ncol(dt)), function(j) {
              x <- dt[[j]]; if (length(x) < win) return(min(x, na.rm=TRUE))
              rm <- zoo::rollmean(x, k=win, fill=NA); min(rm, na.rm=TRUE)
            }, numeric(1))
          } else {
            pct <- max(1, min(50, as.integer(input$pp_percentile %||% 10)))
            F0 <- vapply(seq(2, ncol(dt)), function(j) stats::quantile(dt[[j]], probs=pct/100, na.rm=TRUE, names=FALSE), numeric(1))
          }
          
          baselines[[labels[i]]] <- setNames(F0, names(dt)[-1])
          
          for (k in seq_along(F0)) {
            j <- k+1; f0 <- F0[[k]]
            if (is.finite(f0) && f0 > 1e-6) {
              dt[[j]] <- (dt[[j]] - f0) / f0
            } else if (is.finite(f0) && f0 <= 1e-6) {
              dt[[j]] <- dt[[j]] - f0
            }
          }
          
          dts[[labels[i]]] <- dt
        }
        
        rv$dts <- dts
        rv$raw_traces <- raw_traces
        rv$baselines <- baselines
        
        rv$baseline_method <- input$pp_baseline_method
        if (identical(rv$baseline_method, "frame_range")) {
          rv$baseline_frames <- c(input$pp_baseline_frames[1] %||% 2, input$pp_baseline_frames[2] %||% 20)
        } else {
          rv$baseline_frames <- NULL
        }
        
        rv$long <- purrr::imap(dts, ~to_long(.x, .y)) |> dplyr::bind_rows()
        rv$summary <- if (nrow(rv$long) > 0) {
          rv$long |>
            dplyr::group_by(Group, Time) |>
            dplyr::summarise(mean_dFF0 = mean(dFF0, na.rm=TRUE),
                             sem_dFF0 = stats::sd(dFF0, na.rm=TRUE)/sqrt(dplyr::n()),
                             sd_dFF0 = stats::sd(dFF0, na.rm=TRUE),
                             n_cells = dplyr::n(), .groups = "drop")
        } else NULL
        
        baseline_frame_range <- if (identical(input$pp_baseline_method, "frame_range")) {
          c(as.integer(input$pp_baseline_frames[1] %||% 2), as.integer(input$pp_baseline_frames[2] %||% 20))
        } else {
          c(1, 20)
        }
        
        rv$metrics <- purrr::imap(dts, ~compute_metrics_for_dt(.x, .y, baseline_frame_range)) |> dplyr::bind_rows()
      })
    })
    
    output$n_files_text <- renderText({ as.character(length(rv$dts)) })
    output$n_cells_text <- renderText({ as.character(if (is.null(rv$metrics)) 0 else nrow(rv$metrics)) })
    output$n_timepoints_text <- renderText({ as.character(sum(purrr::map_int(rv$dts, nrow))) })
    
    output$status_files_loaded <- renderText({ if (is.null(rv$files)) "No files" else paste(nrow(rv$files), "file(s)") })
    output$status_processing <- renderText({ if (is.null(rv$dts) || length(rv$dts) == 0) "Not started" else "Complete" })
    output$status_metrics <- renderText({ if (is.null(rv$metrics)) "Not calculated" else paste(nrow(rv$metrics), "cells") })
    output$status_ready <- renderText({ if (!is.null(rv$metrics) && nrow(rv$metrics) > 0) "Ready" else "Waiting" })
    
    return(rv)
  })
}
