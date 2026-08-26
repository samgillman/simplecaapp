# R/mod_load_data.R

mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "load",

    # One full-width workflow box: the three steps read left to right and
    # share one height, so there is no column ratio to balance and no
    # status box to inflate. Results appear as a slim bar underneath.
    fluidRow(
      theme_box(
        title = "Load & Process",
        icon = icon("cloud-upload-alt"),
        width = 12,
        div(
          class = "load-steps",

          # Step 1: Upload
          div(
            class = "load-step",
            div(class = "load-step-title", span(class = "load-step-num", "1"), "Upload recordings"),
            div(
              class = "segmented-toggle", style = "margin-bottom: 10px;",
              radioButtons(ns("upload_mode"), NULL,
                choices = c("Single file" = "single", "Multiple files" = "multi"),
                selected = "single", inline = TRUE
              )
            ),
            uiOutput(ns("file_input_ui")),
            uiOutput(ns("upload_feedback"))
          ),

          # Step 2: Baseline frame window
          div(
            class = "load-step",
            div(class = "load-step-title", span(class = "load-step-num", "2"), "Baseline frames (F\u2080)"),
            div(class = "small-help",
              "F\u2080 is the mean fluorescence across the selected frames. Choose a stable resting period before the response."
            ),
            sliderInput(ns("pp_baseline_frames"), "Baseline Window (frames)", min = 1, max = 100, value = c(1, 20), step = 1, width = "100%"),
            fluidRow(
              column(6, numericInput(ns("pp_baseline_start"), "Start frame", value = 1, min = 1, max = 100, step = 1, width = "100%")),
              column(6, numericInput(ns("pp_baseline_end"), "End frame", value = 20, min = 1, max = 100, step = 1, width = "100%"))
            ),
            div(class = "small-help",
              "This window also defines baseline noise and the frames excluded from response searches."
            )
          ),

          # Step 3: Process
          div(
            class = "load-step",
            div(class = "load-step-title", span(class = "load-step-num", "3"), "Review & Process"),
            accordion(
              id = ns("advanced_opts"),
              title = "Advanced Options",
              icon = "sliders-h",
              expanded = FALSE,
              content = div(
                numericInput(ns("pp_sampling_rate"), "Sampling rate (Hz)", value = 1, min = 0.0001, step = 0.1, width = "100%"),
                p("Used only when Time is missing: converts Frame to seconds or generates Time.", class = "text-muted small"),
                uiOutput(ns("column_mapping_ui"))
              )
            ),
            p(class = "small-help", style = "margin-bottom: 10px;",
              HTML("&Delta;F/F&#8320; = (F &minus; F&#8320;)/F&#8320; per cell, then all metrics for every trace.")),
            primary_button(ns("load_btn"), "Process Data", icon = icon("play"), width = "100%"),
            uiOutput(ns("process_status"))
          )
        )
      )
    ),

    # Slim results bar after processing
    fluidRow(uiOutput(ns("results_bar"))),

    # First-run guidance
    fluidRow(uiOutput(ns("quick_start")))
  )
}

# Compact input-format table shared by Quick Start (pre-process) and the
# Expected Format reference box (post-process) — defined once so the two
# never drift apart
format_example_block <- function() {
  div(
    style = "display: flex; gap: 24px; align-items: flex-start; flex-wrap: wrap;",
    div(
      style = "flex: 1 1 280px; min-width: 240px;",
      tags$table(
        class = "table table-bordered table-sm",
        style = "width: 100%; font-family: var(--font-mono); font-size: 12px; margin-bottom: 0; background: var(--color-white);",
        tags$thead(
          tags$tr(style = "background: var(--color-gray-50);",
                  tags$th("Time"), tags$th("Cell1"), tags$th("Cell2"), tags$th("..."))
        ),
        tags$tbody(
          tags$tr(tags$td("0.0"), tags$td("120.5"), tags$td("98.2"), tags$td("...")),
          tags$tr(tags$td("0.1"), tags$td("121.0"), tags$td("99.1"), tags$td("..."))
        )
      )
    ),
    div(
      class = "small-help",
      style = "flex: 1 1 240px; min-width: 220px; margin: 0; line-height: 1.6; font-size: 12px;",
      p(style = "margin: 0 0 6px 0;", tags$b("One file per experimental group"), " (CSV or Excel, wide format). Switch the uploader to Multiple files to compare groups side by side."),
      p(style = "margin: 0 0 6px 0;", "Recommended: a Time column in seconds, plus one column per cell trace. If Time is absent, it is generated from the sampling rate; a Frame column or unnamed sequential ImageJ index is converted automatically."),
      p(style = "margin: 0 0 6px 0;", "Advanced Options confirms the Time source for each file and lets you exclude unwanted numeric columns before processing."),
      p(style = "margin: 0;", "ImageJ exports with Mean1, Mean2, ... columns display as Cell 1, Cell 2, ...")
    )
  )
}

# Stable dynamic-input suffix for one uploaded file. Upload staging replaces
# duplicate file names, so a name-derived key remains stable when another file
# is added or removed from a multi-file batch.
column_mapping_key <- function(file_name) {
  clean <- gsub("[^A-Za-z0-9_]+", "_", as.character(file_name))
  checksum <- sum(utf8ToInt(enc2utf8(as.character(file_name)))) %% 100000L
  paste0(clean, "_", checksum)
}

mod_load_data_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track uploaded files for immediate visual feedback
    uploaded_files <- reactiveVal(NULL)
    upload_schemas <- reactiveVal(list())
    process_state <- reactiveVal("idle")
    process_error <- reactiveVal(NULL)
    baseline_max <- reactiveVal(100L)
    baseline_frames_state <- reactiveVal(c(1L, 20L))
    last_processed_signature <- reactiveVal(NULL)

    normalize_baseline_frames <- function(frames, max_frame = baseline_max()) {
      max_frame <- max(1L, suppressWarnings(as.integer(max_frame)[1]))
      frames <- suppressWarnings(as.integer(frames))
      if (length(frames) < 2 || any(!is.finite(frames[1:2]))) {
        frames <- baseline_frames_state()
      }
      frames <- sort(pmax(1L, pmin(max_frame, frames[1:2])))
      as.integer(frames)
    }

    set_baseline_controls <- function(frames, max_frame = baseline_max(),
                                      update_slider = TRUE,
                                      update_start = TRUE,
                                      update_end = TRUE) {
      max_frame <- max(1L, suppressWarnings(as.integer(max_frame)[1]))
      baseline_max(max_frame)
      frames <- normalize_baseline_frames(frames, max_frame)
      baseline_frames_state(frames)
      if (isTRUE(update_slider)) {
        updateSliderInput(session, "pp_baseline_frames", min = 1, max = max_frame, value = frames)
      }
      if (isTRUE(update_start)) {
        updateNumericInput(session, "pp_baseline_start", min = 1, max = max_frame, value = frames[1])
      }
      if (isTRUE(update_end)) {
        updateNumericInput(session, "pp_baseline_end", min = 1, max = max_frame, value = frames[2])
      }
      invisible(frames)
    }

    refresh_upload_schemas <- function(files, reset_baseline = FALSE) {
      if (is.null(files) || nrow(files) == 0) {
        upload_schemas(list())
        set_baseline_controls(c(1, 20), 100)
        return(invisible(NULL))
      }

      schemas <- lapply(seq_len(nrow(files)), function(i) {
        tryCatch({
          dt <- safe_read(files$datapath[i])
          inspect_column_mapping(dt, sampling_rate = input$pp_sampling_rate %||% 1)
        }, error = function(e) {
          list(error = conditionMessage(e), row_count = NA_integer_)
        })
      })
      names(schemas) <- vapply(files$name, column_mapping_key, character(1))
      upload_schemas(schemas)

      row_counts <- vapply(schemas, function(x) x$row_count %||% NA_integer_, integer(1))
      row_counts <- row_counts[is.finite(row_counts) & row_counts >= 1]
      if (length(row_counts) > 0) {
        max_frame <- min(row_counts)
        frames <- if (isTRUE(reset_baseline)) {
          c(1L, min(20L, max_frame))
        } else {
          normalize_baseline_frames(baseline_frames_state(), max_frame)
        }
        set_baseline_controls(frames, max_frame)
      }
      invisible(NULL)
    }

    observeEvent(input$pp_baseline_frames, {
      frames <- normalize_baseline_frames(input$pp_baseline_frames)
      if (!identical(frames, baseline_frames_state())) {
        baseline_frames_state(frames)
        updateNumericInput(session, "pp_baseline_start", value = frames[1])
        updateNumericInput(session, "pp_baseline_end", value = frames[2])
      }
    }, ignoreInit = TRUE)

    observeEvent(input$pp_baseline_start, {
      start <- suppressWarnings(as.integer(input$pp_baseline_start)[1])
      if (!is.finite(start)) return(invisible(NULL))
      current <- baseline_frames_state()
      frames <- normalize_baseline_frames(c(start, max(start, current[2])))
      if (!identical(frames, current)) {
        set_baseline_controls(
          frames,
          update_start = !identical(start, frames[1])
        )
      } else if (!identical(start, frames[1])) {
        updateNumericInput(session, "pp_baseline_start", value = frames[1])
      }
    }, ignoreInit = TRUE)

    observeEvent(input$pp_baseline_end, {
      end <- suppressWarnings(as.integer(input$pp_baseline_end)[1])
      if (!is.finite(end)) return(invisible(NULL))
      current <- baseline_frames_state()
      frames <- normalize_baseline_frames(c(min(current[1], end), end))
      if (!identical(frames, current)) {
        set_baseline_controls(
          frames,
          update_end = !identical(end, frames[2])
        )
      } else if (!identical(end, frames[2])) {
        updateNumericInput(session, "pp_baseline_end", value = frames[2])
      }
    }, ignoreInit = TRUE)

    # Mode-specific uploader: re-rendering on toggle gives a fresh, empty
    # file input, so a multi-file selection cannot leak into single mode
    output$file_input_ui <- renderUI({
      multi <- identical(input$upload_mode, "multi")
      tagList(
        fileInput(ns("data_files"), NULL,
          multiple = multi,
          accept = c(".csv", ".xlsx", ".xls"),
          width = "100%"
        ),
        div(
          class = "small-help", style = "margin-top: -8px; margin-bottom: 8px;",
          if (multi) {
            "One file per experimental group; each file becomes its own group. Browse repeatedly to add files — the list keeps growing until you clear it."
          } else {
            "CSV or Excel, wide format: optional Time or Frame column, plus one column per cell trace."
          }
        )
      )
    })

    # Switching modes discards any pending selection so the feedback panel
    # and Process step always describe files chosen in the current mode
    observeEvent(input$upload_mode, {
      uploaded_files(NULL)
      upload_schemas(list())
      process_state("idle")
      process_error(NULL)
      set_baseline_controls(c(1, 20), 100)
    }, ignoreInit = TRUE)
    
    # Show upload feedback immediately when files are selected, and auto-detect baseline
    observeEvent(input$data_files, {
      files <- input$data_files
      if (is.null(files) || nrow(files) == 0) {
        return(invisible(NULL))
      }

      # Multi mode accumulates: browsing again adds to the staged list
      # (a native file dialog replaces its selection each time, and files in
      # different folders can't be picked in one dialog). Re-selecting a
      # name replaces its older entry.
      if (identical(input$upload_mode, "multi")) {
        staged <- uploaded_files()
        if (!is.null(staged) && nrow(staged) > 0) {
          staged <- staged[!(staged$name %in% files$name), , drop = FALSE]
          files <- rbind(staged, files)
        }
      }
      first_upload <- is.null(uploaded_files()) || nrow(uploaded_files()) == 0
      uploaded_files(files)
      process_state("ready")
      process_error(NULL)
      refresh_upload_schemas(files, reset_baseline = first_upload)
    })

    # Remove one staged file (index sent from the feedback panel) or clear all
    observeEvent(input$remove_file, {
      files <- uploaded_files()
      i <- suppressWarnings(as.integer(input$remove_file))
      if (!is.null(files) && !is.na(i) && i >= 1 && i <= nrow(files)) {
        files <- files[-i, , drop = FALSE]
        remaining <- if (nrow(files) > 0) files else NULL
        uploaded_files(remaining)
        refresh_upload_schemas(remaining)
        process_state(if (nrow(files) > 0) "ready" else "idle")
        process_error(NULL)
      }
    })
    observeEvent(input$clear_files, {
      uploaded_files(NULL)
      refresh_upload_schemas(NULL)
      process_state("idle")
      process_error(NULL)
    })

    output$column_mapping_ui <- renderUI({
      files <- uploaded_files()
      schemas <- upload_schemas()
      if (is.null(files) || nrow(files) == 0) {
        return(p("Upload a file to confirm Time and exclude columns.", class = "text-muted small"))
      }

      rate <- suppressWarnings(as.numeric(input$pp_sampling_rate %||% 1))
      if (!is.finite(rate) || rate <= 0) rate <- 1

      panels <- lapply(seq_len(nrow(files)), function(i) {
        key <- column_mapping_key(files$name[i])
        schema <- schemas[[key]]
        if (is.null(schema) || !is.null(schema$error)) {
          return(div(
            class = "column-mapping-file",
            tags$b(files$name[i]),
            p(paste("Column preview unavailable:", schema$error %||% "unknown read error"),
              class = "text-danger small")
          ))
        }

        mode_id <- paste0("time_mode_", key)
        column_id <- paste0("time_column_", key)
        exclude_id <- paste0("exclude_columns_", key)
        current_mode <- input[[mode_id]] %||% "auto"
        if (!(current_mode %in% c("auto", "time", "frame", "generated"))) current_mode <- "auto"
        detected_column <- schema$time_info$column %||% schema$columns[1]
        current_column <- input[[column_id]] %||% detected_column
        if (!(current_column %in% schema$columns)) current_column <- schema$columns[1]
        current_excluded <- input[[exclude_id]] %||% character()
        current_excluded <- intersect(current_excluded, schema$numeric_columns)

        detection <- switch(schema$time_info$source,
          time = sprintf("Auto-detected '%s' as elapsed time.", schema$time_info$column),
          frame = sprintf("Auto-detected '%s' as frame index; converting at %s Hz.", schema$time_info$column, format(rate, trim = TRUE)),
          inferred_frame = sprintf("Auto-detected the unnamed first column as frame index; converting at %s Hz.", format(rate, trim = TRUE)),
          generated_invalid_time = sprintf("'%s' is invalid as Time; automatic mode will generate Time at %s Hz.", schema$time_info$column, format(rate, trim = TRUE)),
          generated_invalid_frame = sprintf("'%s' is invalid as Frame; automatic mode will generate Time at %s Hz.", schema$time_info$column, format(rate, trim = TRUE)),
          sprintf("No Time/Frame column detected; automatic mode will generate Time at %s Hz.", format(rate, trim = TRUE))
        )
        active_source <- switch(current_mode,
          auto = schema$time_info$column %||% NULL,
          time = current_column,
          frame = current_column,
          generated = schema$time_info$column %||% NULL
        )
        active_traces <- setdiff(
          schema$numeric_columns,
          c(current_excluded, active_source %||% character())
        )
        if (!identical(active_source, "Time")) active_traces <- setdiff(active_traces, "Time")
        mapping_status <- switch(current_mode,
          auto = detection,
          time = sprintf("Using '%s' as elapsed time in seconds.", current_column),
          frame = sprintf("Using '%s' as frame index and converting at %s Hz.", current_column, format(rate, trim = TRUE)),
          generated = sprintf("Generating Time from row number at %s Hz.", format(rate, trim = TRUE))
        )
        mapping_status <- paste0(
          mapping_status, " ", length(active_traces), " trace",
          if (length(active_traces) == 1) "" else "s", " will be analyzed."
        )

        div(
          class = "column-mapping-file",
          tags$div(class = "column-mapping-title", icon("file-alt"), files$name[i]),
          p(sprintf("%d rows · %d numeric column%s available", schema$row_count,
                    length(schema$numeric_columns), if (length(schema$numeric_columns) == 1) "" else "s"),
            class = "text-muted small"),
          selectInput(ns(mode_id), "Time handling",
            choices = c(
              "Auto-detect (recommended)" = "auto",
              "Use selected column as elapsed time (seconds)" = "time",
              "Use selected column as frame index" = "frame",
              "Generate Time from sampling rate" = "generated"
            ),
            selected = current_mode, width = "100%"
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'time' || input['%s'] == 'frame'", ns(mode_id), ns(mode_id)),
            selectInput(ns(column_id), "Time/Frame column",
              choices = schema$columns, selected = current_column, width = "100%")
          ),
          selectizeInput(ns(exclude_id), "Exclude columns from analysis",
            choices = schema$numeric_columns,
            selected = current_excluded,
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "None — analyze all remaining numeric columns")
          ),
          p(mapping_status, class = "column-mapping-detection")
        )
      })

      tagList(
        tags$hr(style = "margin: 14px 0 10px;"),
        tags$div(class = "control-col-title", icon("columns"), "Column Mapping"),
        p("Confirm the Time source for each file. The selected Time/Frame column is never analyzed as a trace.", class = "text-muted small"),
        panels
      )
    })

    collect_column_mappings <- function(files) {
      schemas <- upload_schemas()
      lapply(seq_len(nrow(files)), function(i) {
        key <- column_mapping_key(files$name[i])
        schema <- schemas[[key]]
        if (is.null(schema) || !is.null(schema$error)) {
          return(list(
            time_mode = "auto",
            time_column = NULL,
            excluded_columns = character()
          ))
        }
        mode <- input[[paste0("time_mode_", key)]] %||% "auto"
        column <- input[[paste0("time_column_", key)]] %||%
          (schema$time_info$column %||% schema$columns[1])
        excluded <- input[[paste0("exclude_columns_", key)]] %||% character()
        list(
          time_mode = mode,
          time_column = column,
          excluded_columns = excluded
        )
      })
    }

    current_processing_settings <- function(files = uploaded_files()) {
      list(
        baseline_method = "frame_range",
        baseline_frames = baseline_frames_state(),
        sampling_rate = input$pp_sampling_rate,
        column_mappings = if (is.null(files) || nrow(files) == 0) {
          NULL
        } else {
          collect_column_mappings(files)
        }
      )
    }

    # Everything in this signature changes the numerical analysis. Comparing
    # normalized values prevents slider/numeric synchronization from creating
    # false stale states while still catching dynamic per-file mapping inputs.
    processing_signature <- reactive({
      files <- uploaded_files()
      file_identity <- if (is.null(files) || nrow(files) == 0) {
        NULL
      } else {
        data.frame(
          name = as.character(files$name),
          size = suppressWarnings(as.numeric(files$size)),
          datapath = as.character(files$datapath),
          stringsAsFactors = FALSE
        )
      }
      list(
        upload_mode = input$upload_mode %||% "single",
        files = file_identity,
        settings = normalize_load_settings(current_processing_settings(files))
      )
    })

    invalidate_processed_results <- function() {
      clear_processed_state(rv)
      staged <- uploaded_files()
      process_state(if (!is.null(staged) && nrow(staged) > 0) "stale" else "idle")
      process_error(NULL)
      last_processed_signature(NULL)
      invisible(NULL)
    }

    observeEvent(processing_signature(), {
      committed <- last_processed_signature()
      if (!is.null(committed) && !identical(processing_signature(), committed)) {
        invalidate_processed_results()
      }
    }, ignoreInit = TRUE)
    
    # Render upload feedback UI
    output$upload_feedback <- renderUI({
      files <- uploaded_files()
      if (is.null(files) || nrow(files) == 0) {
        return(NULL)
      }
      
      # Format file sizes
      format_size <- function(bytes) {
        if (bytes < 1024) return(paste0(bytes, " B"))
        if (bytes < 1024^2) return(paste0(round(bytes/1024, 1), " KB"))
        return(paste0(round(bytes/1024^2, 1), " MB"))
      }
      
      multi <- identical(input$upload_mode, "multi")

      # Create file list items
      file_items <- lapply(seq_len(nrow(files)), function(i) {
        tags$div(
          style = "display: flex; align-items: center; padding: 6px 10px; background: white; border-radius: 4px; margin-bottom: 4px; border: 1px solid var(--color-gray-100);",
          icon("file-csv", style = "color: var(--color-success); margin-right: 10px; font-size: 14px;"),
          tags$span(
            style = "flex: 1; font-size: 13px; font-weight: 500; color: var(--color-gray-900); white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
            files$name[i]
          ),
          tags$span(
            style = "font-size: 11px; color: var(--color-gray-600); margin-left: 8px; white-space: nowrap;",
            format_size(files$size[i])
          ),
          if (multi) {
            tags$a(
              href = "#", title = "Remove this file",
              style = "margin-left: 10px; color: var(--color-gray-600); font-weight: 700; font-size: 14px; text-decoration: none; line-height: 1;",
              onclick = sprintf(
                "Shiny.setInputValue('%s', %d, {priority: 'event'}); return false;",
                ns("remove_file"), i
              ),
              HTML("&times;")
            )
          }
        )
      })

      # Clean success container
      tags$div(
        style = "background: var(--color-gray-50); border: 1px solid var(--color-success); border-radius: var(--radius-md); padding: 12px; margin-top: 10px;",

        # Header
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 8px;",
          icon("check-circle", style = "color: var(--color-success); margin-right: 8px;"),
          tags$span(
            style = "font-weight: 600; color: var(--color-gray-900); font-size: 13px;",
            paste0(nrow(files), " file", if (nrow(files) > 1) "s" else "", " ready")
          )
        ),

        # File list
        tags$div(
          style = "max-height: 180px; overflow-y: auto;",
          file_items
        ),

        # Next step hint
        tags$div(
          style = "margin-top: 8px; font-size: 12px; color: var(--color-gray-600); display: flex; align-items: center; gap: 14px; flex-wrap: wrap;",
          tags$span(
            icon("arrow-right", style = "margin-right: 6px;"),
            "Click ", tags$strong("Process Data"), " to analyze"
          ),
          if (multi) {
            tags$span(
              icon("plus", style = "margin-right: 6px;"),
              "Browse again to add more files"
            )
          },
          if (multi && nrow(files) > 1) {
            tags$a(
              href = "#", style = "color: var(--color-gray-600); text-decoration: underline;",
              onclick = sprintf(
                "Shiny.setInputValue('%s', Date.now(), {priority: 'event'}); return false;",
                ns("clear_files")
              ),
              "Clear list"
            )
          }
        )
      )
    })
    
    observeEvent(input$load_btn, {
      req(uploaded_files())
      files <- uploaded_files()
      # Single mode analyzes exactly one file even if a stale selection from
      # the browser dialog slipped through.
      if (!identical(input$upload_mode, "multi") && nrow(files) > 1) {
        files <- files[1, , drop = FALSE]
      }

      settings <- current_processing_settings(files)

      # Build the complete next state first. Any read or processing failure
      # leaves the currently displayed dataset unchanged.
      processed <- tryCatch(
        withProgress(message = "Processing data...", value = 0, {
          build_processed_state(
            files,
            settings,
            progress = function(amount, detail) {
              incProgress(amount, detail = detail)
            }
          )
        }),
        error = function(e) {
          process_state("error")
          process_error(conditionMessage(e))
          showNotification(
            paste("Processing failed; no new results were committed:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )
      if (is.null(processed)) {
        return(invisible(NULL))
      }

      committed <- tryCatch({
        commit_processed_state(rv, processed)
        TRUE
      }, error = function(e) {
        process_state("error")
        process_error(conditionMessage(e))
        showNotification(
          paste("Processing could not be committed; previous results were restored:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        FALSE
      })
      if (!committed) {
        return(invisible(NULL))
      }
      process_state("success")
      process_error(NULL)
      last_processed_signature(isolate(processing_signature()))

      if (length(processed$skipped_files) > 0) {
        skipped_text <- paste0(
          processed$skipped_details$file, ": ",
          processed$skipped_details$reason
        )
        showNotification(
          paste(
            "Skipped file(s):",
            paste(utils::head(skipped_text, 3), collapse = "; "),
            if (length(skipped_text) > 3) "..." else ""
          ),
          type = "warning",
          duration = 10
        )
      }
      if (length(processed$dropped_cells) > 0) {
        showNotification(
          paste0(
            length(processed$dropped_cells),
            " cell(s) excluded because their baseline F\u2080 was zero, negative, or missing (\u0394F/F\u2080 undefined): ",
            paste(utils::head(processed$dropped_cells, 5), collapse = ", "),
            if (length(processed$dropped_cells) > 5) " ..." else ""
          ),
          type = "warning",
          duration = 10
        )
      }
      if (length(processed$time_messages) > 0) {
        showNotification(
          paste(utils::head(processed$time_messages, 3), collapse = " "),
          type = "message",
          duration = 10
        )
      }

      # Success feedback
      if (!is.null(rv$metrics) && nrow(rv$metrics) > 0) {
        n_cells <- nrow(rv$metrics)
        n_groups <- length(rv$groups)
        showNotification(
          paste0("Processed ", n_cells, " cells across ", n_groups, " file(s). Explore results in Time Course, Heatmap, or Metrics tabs."),
          type = "message",
          duration = 6
        )
      }
    })

    # Inline confirmation under the Process button
    output$process_status <- renderUI({
      status <- process_state()
      if (identical(status, "error")) {
        div(
          class = "small-help",
          style = "margin-top: 8px; color: var(--color-danger);",
          icon("exclamation-circle"),
          paste(" Processing failed; no new results were committed.", process_error() %||% "")
        )
      } else if (identical(status, "stale")) {
        div(
          class = "small-help",
          style = "margin-top: 8px; color: var(--color-warning);",
          icon("exclamation-triangle"),
          " Settings changed — click Process Data to update results."
        )
      } else if (identical(status, "success") && !is.null(rv$metrics) && nrow(rv$metrics) > 0) {
        div(class = "small-help", style = "margin-top: 8px; color: var(--color-success);",
            icon("check-circle"), sprintf(" %d cells processed.", nrow(rv$metrics)))
      } else if (!is.null(uploaded_files()) && nrow(uploaded_files()) > 0) {
        div(class = "small-help", style = "margin-top: 8px;", "Ready to process.")
      } else {
        div(class = "small-help", style = "margin-top: 8px;", "Waiting for files.")
      }
    })

    # Slim full-width results bar: the post-processing summary in one line
    # plus the jump to results — replaces the old Data Overview box, whose
    # three numbers could never fill a 2/3-width panel
    output$results_bar <- renderUI({
      req(rv$metrics, nrow(rv$metrics) > 0)
      n_files <- length(rv$dts)
      n_cells <- nrow(rv$metrics)
      timepoint_context <- format_timepoint_context(rv$dts)
      div(
        class = "col-sm-12",
        div(
          class = "results-bar",
          div(
            class = "results-bar-stats",
            icon("check-circle", style = "color: var(--color-success);"),
            HTML(sprintf(
              "<b>%d</b> file%s &nbsp;&middot;&nbsp; <b>%d</b> cells &nbsp;&middot;&nbsp; <b>%s</b> &mdash; processing complete",
              n_files, if (n_files == 1) "" else "s", n_cells, timepoint_context
            ))
          ),
          primary_button(ns("goto_results"), "View Results: Time Course", icon = icon("arrow-right"), width = "260px")
        )
      )
    })
    observeEvent(input$goto_results, {
      shinyjs::runjs("$('a[href=\"#shiny-tab-time\"]').click();")
    })

    # First-visit guidance; disappears once data is processed. Carries the
    # format example itself so the empty state states the format exactly once
    output$quick_start <- renderUI({
      if (!is.null(rv$metrics) && nrow(rv$metrics) > 0) return(NULL)
      theme_box(
        title = "Quick Start",
        icon = icon("rocket"),
        width = 12,
        div(
          style = "display: flex; gap: 32px; flex-wrap: wrap; align-items: flex-start;",
          div(
            style = "flex: 1 1 300px; min-width: 260px;",
            tags$ol(
              style = "margin: 0 0 12px 18px; padding: 0; font-size: 13px; line-height: 2;",
              tags$li(HTML("<b>Upload</b> a recording &mdash; or switch to <b>Multiple files</b> to load one file per experimental group and compare them.")),
              tags$li(HTML("<b>Choose the baseline frames.</b> Set exact Start/End frames by typing or use the slider; Advanced Options confirms Time and excluded columns.")),
              tags$li(HTML("<b>Click Process Data</b>, then explore Time Course, Heatmap, and Metrics."))
            ),
            p(
              style = "margin: 0; font-size: 12px; color: var(--color-gray-600);",
              icon("circle-question"), " Metric definitions and details are in the Help tab.",
              tags$br(),
              icon("lock"), " Your data never leaves this device: all processing runs in your browser."
            )
          ),
          div(
            style = "flex: 1 1 420px; min-width: 300px;",
            format_example_block()
          )
        )
      )
    })

    return(rv)
  })
}
