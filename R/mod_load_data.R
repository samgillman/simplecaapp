# R/mod_load_data.R

baseline_controls_script <- function() {
  htmltools::singleton(tags$script(HTML('
(function($) {
  function sync(root, source) {
    if (!root || root.baselineSyncing) return;
    var range = root.querySelector("input.js-range-slider");
    var numbers = root.querySelectorAll("input[type=number]");
    var binding = $(range).data("shiny-input-binding");
    var slider = $(range).data("ionRangeSlider");
    if (!binding || !slider || numbers.length !== 2) return;
    var frames = binding.getValue(range).slice();
    var start = numbers[0], end = numbers[1];
    if (source === start || source === end) {
      // An empty/partial number is not a new baseline window.
      if (!source.value.trim() || !Number.isFinite(Number(source.value))) return;
      var value = Math.max(slider.options.min,
        Math.min(slider.options.max, Math.trunc(Number(source.value))));
      if (source === start) {
        frames = [value, Math.max(value, frames[1])];
      } else {
        frames = [Math.min(frames[0], value), value];
      }
    }
    root.baselineSyncing = true;
    try {
      start.value = frames[0];
      end.value = frames[1];
      // Shiny setValue emits an immediate change, cancelling any queued slider
      // debounce. Only this canonical pair drives analysis on the server.
      // Do not rebuild the slider while the user is dragging a handle.
      if (source !== range) binding.setValue(range, frames);
    } finally {
      root.baselineSyncing = false;
    }
  }
  $(document).on("input.simplecaBaseline change.simplecaBaseline",
    ".simpleca-baseline-controls input", function() {
      sync(this.closest(".simpleca-baseline-controls"), this);
    });
  document.addEventListener("click", function(event) {
    var button = event.target.closest("button");
    if (!button || !button.id.endsWith("load_btn")) return;
    var root = document.getElementById(button.id.slice(0, -8) + "baseline_controls");
    if (root) sync(root, document.activeElement);
  }, true);
})(window.jQuery);
')))
}


mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "load",

    div(class = "load-layout",
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
            div(class = "load-step-title", span(class = "load-step-num", "2"), "Input & baseline"),
            selectInput(
              ns("input_data_mode"), "Uploaded trace values",
              choices = c("Raw fluorescence" = "raw_fluorescence",
                "Already background-corrected fluorescence" = "corrected_fluorescence",
                "Already ΔF/F₀ — fraction (0.10 = 10%)" = "dff0",
                "Already ΔF/F₀ — percent (10 = 10%)" = "dff0_percent",
                "Fluorescence ratio (e.g. Fura-2 340/380)" = "ratio",
                "Already ΔR/R₀ — fraction" = "drr0",
                "Already ΔR/R₀ — percent" = "drr0_percent"),
              selected = "raw_fluorescence"
            ),
            conditionalPanel(
              sprintf("input['%s'] == 'raw_fluorescence'", ns("input_data_mode")),
              div(class = "small-help", "F₀ is the mean fluorescence across the selected baseline frames after any background subtraction. The normalized trace is (F − F₀)/F₀."),
              checkboxInput(ns("has_background"), "My file includes a background ROI column", value = FALSE),
              conditionalPanel(
                sprintf("input['%s'] === true", ns("has_background")),
                div(class = "small-help", "The background is subtracted from every cell at each frame before F₀ is calculated. If you choose multiple background ROIs, their mean is subtracted at each frame. Only the remaining cell traces enter the analysis."),
                uiOutput(ns("background_ui"))
              )
            ),
            conditionalPanel(
              sprintf("['dff0','dff0_percent','drr0','drr0_percent'].indexOf(input['%s']) >= 0", ns("input_data_mode")),
              div(class = "small-help", "Use a fixed-reference (signal − baseline)/baseline trace. Fractions are preserved; percentages are divided by 100 once. The raw baseline is unavailable for quality checks. Moving-baseline, z-score, and F/F₀ inputs must be converted to the declared units before upload.")
            ),
            conditionalPanel(
              sprintf("['ratio','drr0','drr0_percent'].indexOf(input['%s']) >= 0", ns("input_data_mode")),
              div(class = "small-help", "Ratio changes are labeled ΔR/R₀. Correct the source channels before forming the ratio; the app does not subtract a fluorescence background from ratio data. A 10% ratio change is not a calibrated 10% calcium increase.")
            ),
            conditionalPanel(
              sprintf("input['%s'] == 'corrected_fluorescence'", ns("input_data_mode")),
              div(class = "small-help", "Use unlifted fluorescence that has already been background corrected. No second background subtraction is applied.")
            ),
            tags$details(tags$summary("Quality reporting"),
              checkboxInput(ns("retain_unscorable"), "Retain recordings with no evaluable cells in quality and response counts", TRUE),
              checkboxInput(ns("flag_peak_outliers"), "Flag peaks above Q3 + 3 × IQR for review", FALSE),
              p(class = "small-help", "Review flags never exclude cells. Invalid normalization is reported separately; valid nonresponders remain in the response denominator.")),
            baseline_controls_script(),
            div(id = ns("baseline_controls"), class = "simpleca-baseline-controls",
              sliderInput(ns("pp_baseline_frames"), "Baseline Window (frames)", min = 1, max = 100, value = c(1, 20), step = 1, width = "100%"),
              numericInput(ns("pp_baseline_start"), "Start frame", value = 1, min = 1, max = 100, step = 1, width = "100%"),
              numericInput(ns("pp_baseline_end"), "End frame", value = 20, min = 1, max = 100, step = 1, width = "100%")
            ),
            conditionalPanel("output.needs_rate === true", ns = ns,
              numericInput(ns("pp_sampling_rate"), "Sampling rate (Hz)", value = 1, min = 0.0001, step = 0.1),
              p("Used only when Time is missing or generated from frames.", class = "small-help")),
            conditionalPanel("output.has_preview === true", ns = ns,
              plotOutput(ns("baseline_preview"), height = "115px")),
            accordion(
              id = ns("advanced_opts"),
              title = "Advanced Options",
              icon = "sliders-h",
              expanded = FALSE,
              content = uiOutput(ns("column_mapping_ui"))
            )
          ),
          div(class = "load-step",
            div(class = "load-step-title", span(class = "load-step-num", "3"), "Process recording"),
            shinyjs::disabled(primary_button(ns("load_btn"), "Process Data", icon = icon("play"))),
            uiOutput(ns("process_status"))
          )
        )
      ),
      div(class = "load-guide",
        theme_box("Quick Start", icon = icon("rocket"),
          tags$ol(
            tags$li("Upload a wide-format recording, with one column per cell."),
            tags$li("Choose the input units and baseline frames. Confirm the sampling rate if the file has no elapsed time."),
            tags$li("Process Data, then choose an analysis from the sidebar.")
          ),
          format_example_block(),
          p(class = "small-help", icon("lock"), " On the public site, recordings are processed in your browser.")
        )
      )
    ),
    fluidRow(uiOutput(ns("results_bar")))
  )
}

# Input-format example for the Load Data guide.
format_example_block <- function() {
  div(
    style = "display: flex; gap: 24px; align-items: flex-start; flex-wrap: wrap;",
    div(
      style = "flex: 1 1 280px; min-width: 0;",
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
      style = "flex: 1 1 240px; min-width: 0; margin: 0; line-height: 1.6; font-size: 12px;",
      p(style = "margin: 0 0 6px 0;", tags$b("One recording per file (one field for Percent Responding)"), " (CSV or Excel, wide format). Switch the uploader to Multiple files to compare groups side by side."),
      p(style = "margin: 0 0 6px 0;", "Recommended: a Time column in seconds, plus one column per cell trace. If Time is absent, it is generated from the sampling rate; a Frame column or unnamed sequential ImageJ index is converted automatically."),
      p(style = "margin: 0 0 6px 0;", "Use Advanced Options to check the time source or exclude columns."),
      p(style = "margin: 0 0 6px 0;", "ImageJ exports with Mean1, Mean2, ... columns display as Cell 1, Cell 2, ..."),
      p(style = "margin: 0;", "If the export includes a cell-free background ROI, tick the background box in step 2 so it is subtracted and not counted as a cell.")
    )
  )
}

# Stable dynamic-input suffix for one browser upload. The caller supplies the
# upload ID, not the basename, so duplicate filenames can coexist.
column_mapping_key <- function(upload_id) {
  clean <- gsub("[^A-Za-z0-9_]+", "_", as.character(upload_id))
  checksum <- sum(utf8ToInt(enc2utf8(as.character(upload_id)))) %% 100000L
  paste0(clean, "_", checksum)
}

# Native inputs send an edit on change. Keeping the upload ID in a data
# attribute also allows punctuation in source filenames and display labels.
recording_editor <- function(meta, count, target) {
  control <- function(field, title, type = "text", ...) {
    tags$label(title, tags$input(type = type, value = meta[[field]],
      class = "form-control", `data-field` = field, ...))
  }
  tags$details(class = "recording-editor",
    tags$summary("Recording details"),
    tags$div(class = "recording-editor-fields", `data-upload` = meta$upload_id,
      `data-target` = target,
      onchange = paste(
        "var e = event.target; if (!e.dataset.field || !e.reportValidity()) return;",
        "if (e.dataset.field === 'label' && !e.value.trim()) e.value = e.placeholder;",
        "Shiny.setInputValue(this.dataset.target,",
        "{id:this.dataset.upload,field:e.dataset.field,value:e.value},{priority:'event'});"
      ),
      control("label", "Display name", placeholder = meta$key),
      control("condition", "Condition", placeholder = "Optional"),
      control("unit_id", "Experimental unit ID", placeholder = "Optional"),
      control("color", "Plot color", "color"),
      control("order", "Display position", "number", min = 1, max = count, step = 1, required = "required")
    )
  )
}

mod_load_data_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track uploaded files for immediate visual feedback
    uploaded_files <- reactiveVal(NULL)
    staged_meta <- reactiveVal(NULL)
    order_revision <- reactiveVal(0L)
    stage_files <- function(files) {
      files <- ensure_upload_ids(files)
      staged_meta(recording_registry(files, staged_meta()))
      uploaded_files(files)
    }

    observeEvent(input$recording_edit, {
      edit <- input$recording_edit
      before <- staged_meta()
      meta <- edit_recording(before, edit$id, edit$field, edit$value)
      if (identical(before, meta)) return()
      staged_meta(meta)
      if (edit$field == "order") order_revision(order_revision() + 1L)
      committed <- rv$group_meta
      if (NROW(committed)) {
        positions <- match(committed$upload_id, meta$upload_id)
        if (!anyNA(positions)) {
          updated <- meta[positions, , drop = FALSE]
          updated$n_cells <- committed$n_cells
          view <- recording_view(list(group_meta = updated))
          rv$group_meta <- updated
          rv$groups <- view$levels
          rv$colors <- view$palette
          rv$processing_manifest <- with_recording_manifest(rv$processing_manifest, updated)
        }
      }
    })

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

      files <- ensure_upload_ids(files)
      schemas <- lapply(seq_len(nrow(files)), function(i) {
        tryCatch({
          dt <- safe_read(files$datapath[i])
          inspect_column_mapping(dt, sampling_rate = input$pp_sampling_rate %||% 1)
        }, error = function(e) {
          list(error = conditionMessage(e), row_count = NA_integer_)
        })
      })
      names(schemas) <- vapply(files$upload_id, column_mapping_key, character(1))
      upload_schemas(schemas)

      row_counts <- vapply(schemas, function(x) x$row_count %||% NA_integer_, integer(1))
      row_counts <- row_counts[is.finite(row_counts) & row_counts >= 1]
      if (length(row_counts) > 0) {
        # Batch staging may start with a short or unusable file. Keep the
        # requested endpoint available even when only that file is staged.
        max_frame <- max(row_counts)
        if (identical(input$upload_mode, "multi")) {
          max_frame <- max(max_frame, baseline_frames_state()[2], if (reset_baseline) 20L else 1L)
        }
        frames <- if (isTRUE(reset_baseline)) {
          c(1L, min(20L, max_frame))
        } else {
          normalize_baseline_frames(baseline_frames_state(), max_frame)
        }
        set_baseline_controls(frames, max_frame)
      }
      invisible(NULL)
    }

    # The browser synchronizes the three controls locally and sends one
    # canonical window. Numeric echoes must never feed back into server updates.
    observeEvent(input$pp_baseline_frames, {
      baseline_frames_state(normalize_baseline_frames(input$pp_baseline_frames))
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
            "One recording per file (one field for Percent Responding); each file becomes its own group. Browse repeatedly to add files — the list keeps growing until you clear it."
          } else {
            "CSV or Excel, wide format: optional Time or Frame column, plus one column per cell trace."
          }
        )
      )
    })

    observe({
      shinyjs::toggleState("load_btn", condition = NROW(uploaded_files()) > 0)
    })
    output$session_status <- renderText({
      files <- uploaded_files()
      status <- process_state()
      if (length(rv$dts)) {
        labels <- rv$files$name
        state <- "Processed"
        frames <- rv$baseline_frames
      } else if (NROW(files)) {
        labels <- files$name
        state <- switch(status, stale = "Settings changed", error = "Processing failed", "Ready to process")
        frames <- baseline_frames_state()
      } else return("No recording loaded")
      name <- if (length(labels) == 1L) labels else sprintf("%d recordings", length(labels))
      if (length(rv$dts)) {
        registered <- if (!is.null(rv$cell_quality)) nrow(rv$cell_quality) else nrow(rv$metrics)
        name <- sprintf("%s · %d registered cells · %d with metrics", name, registered, nrow(rv$metrics))
      }
      sprintf("%s · %s · Baseline frames %d–%d", state, name, frames[1], frames[2])
    })

    # Switching modes discards any pending selection so the feedback panel
    # and Process step always describe files chosen in the current mode
    observeEvent(input$upload_mode, {
      stage_files(NULL)
      upload_schemas(list())
      process_state("idle")
      process_error(NULL)
      set_baseline_controls(c(1, 20), 100)
    }, ignoreInit = TRUE)
    
    # Show upload feedback immediately when files are selected, and auto-detect baseline
    observeEvent(input$data_files, {
      files <- ensure_upload_ids(input$data_files)
      if (is.null(files) || nrow(files) == 0) {
        return(invisible(NULL))
      }

      # Multi mode accumulates: browsing again adds to the staged list
      # (a native file dialog replaces its selection each time, and files in
      # different folders can't be picked in one dialog). Each selection keeps
      # its unique browser datapath identity, even when basenames are identical.
      if (identical(input$upload_mode, "multi")) {
        staged <- uploaded_files()
        if (!is.null(staged) && nrow(staged) > 0) {
          files <- rbind(staged, files)
        }
      }
      first_upload <- is.null(uploaded_files()) || nrow(uploaded_files()) == 0
      stage_files(files)
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
        stage_files(remaining)
        refresh_upload_schemas(remaining)
        process_state(if (nrow(files) > 0) "ready" else "idle")
        process_error(NULL)
      }
    })
    observeEvent(input$clear_files, {
      stage_files(NULL)
      refresh_upload_schemas(NULL)
      process_state("idle")
      process_error(NULL)
    })

    background_enabled <- reactive({
      isTRUE(input$has_background) &&
        identical(input$input_data_mode %||% "raw_fluorescence", "raw_fluorescence")
    })

    # One picker per staged file. Columns named like a background ROI are
    # preselected; nothing is ever assigned from intensity alone.
    output$background_ui <- renderUI({
      files <- uploaded_files()
      schemas <- upload_schemas()
      if (!NROW(files)) {
        return(p("Upload a file to choose its background column.", class = "text-muted small"))
      }
      lapply(seq_len(nrow(files)), function(i) {
        key <- column_mapping_key(files$upload_id[i])
        schema <- schemas[[key]]
        if (is.null(schema) || !is.null(schema$error)) return(NULL)
        input_id <- paste0("background_columns_", key)
        detected <- schema$background_candidates %||% character()
        current <- isolate(input[[input_id]])
        selected <- if (is.null(current)) detected else intersect(current, schema$trace_columns)
        hint <- if (length(detected)) {
          sprintf("Detected by name: %s. Change the selection if this is not the background.",
                  paste(detected, collapse = ", "))
        } else if (!is.null(schema$dimmest_column)) {
          sprintf("No column is named BG or Background, so none was selected. The dimmest trace is %s; select it only if it is your background ROI.",
                  schema$dimmest_column)
        } else {
          "No column is named BG or Background. Select the background ROI column."
        }
        div(
          class = "column-mapping-file",
          selectizeInput(ns(input_id),
            if (nrow(files) > 1) paste("Background column ·", upload_display_names(files$name)[i]) else "Background column",
            choices = stats::setNames(schema$trace_columns, pretty_cell_label(schema$trace_columns)),
            selected = selected, multiple = TRUE, width = "100%",
            options = list(plugins = list("remove_button"), placeholder = "Select the background ROI column")
          ),
          p(hint, class = "small-help")
        )
      })
    })

    output$column_mapping_ui <- renderUI({
      files <- uploaded_files()
      schemas <- upload_schemas()
      if (is.null(files) || nrow(files) == 0) {
        return(p("Upload a file to confirm Time and exclude columns.", class = "text-muted small"))
      }

      panels <- lapply(seq_len(nrow(files)), function(i) {
        key <- column_mapping_key(files$upload_id[i])
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
        # Rebuild controls only when the uploaded schema changes. Their live
        # values and sampling-rate feedback must not recreate their own inputs.
        current_mode <- isolate(input[[mode_id]]) %||% "auto"
        if (!(current_mode %in% c("auto", "time", "frame", "generated"))) current_mode <- "auto"
        detected_column <- schema$time_info$column %||% schema$columns[1]
        current_column <- isolate(input[[column_id]]) %||% detected_column
        if (!(current_column %in% schema$columns)) current_column <- schema$columns[1]
        current_excluded <- isolate(input[[exclude_id]]) %||% character()
        current_excluded <- intersect(current_excluded, schema$numeric_columns)

        status_id <- paste0("mapping_status_", key)
        output[[status_id]] <- renderText({
          rate <- suppressWarnings(as.numeric(input$pp_sampling_rate %||% 1))
          if (!is.finite(rate) || rate <= 0) rate <- 1
          current_mode <- input[[mode_id]] %||% "auto"
          current_column <- input[[column_id]] %||% detected_column
          current_excluded <- input[[exclude_id]] %||% character()
          detection <- switch(schema$time_info$source,
            time = sprintf("Auto-detected '%s' as elapsed time.", schema$time_info$column),
            frame = sprintf("Auto-detected '%s' as frame index; converting at %s Hz.", schema$time_info$column, format(rate, trim = TRUE)),
            inferred_frame = sprintf("Auto-detected the unnamed first column as frame index; converting at %s Hz.", format(rate, trim = TRUE)),
            invalid_time = sprintf("'%s' has invalid timestamps; automatic processing is blocked. Correct the timestamps, or confirm the sampling rate above and explicitly choose generated Time.", schema$time_info$column),
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
          if (background_enabled()) {
            active_traces <- setdiff(active_traces, input[[paste0("background_columns_", key)]] %||% character())
          }
          mapping_status <- switch(current_mode,
            auto = detection,
            time = sprintf("Using '%s' as elapsed time in seconds.", current_column),
            frame = sprintf("Using '%s' as frame index and converting at %s Hz.", current_column, format(rate, trim = TRUE)),
            generated = sprintf("Generating Time from row number at %s Hz.", format(rate, trim = TRUE))
          )
          paste0(
            mapping_status, " ", length(active_traces), " trace",
            if (length(active_traces) == 1) "" else "s",
            if (identical(current_mode, "auto") && identical(schema$time_info$source, "invalid_time")) {
              " available after resolving Time."
            } else {
              " will be analyzed."
            }
          )
        })

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
          p(textOutput(ns(status_id), inline = TRUE), class = "column-mapping-detection")
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
        key <- column_mapping_key(files$upload_id[i])
        schema <- schemas[[key]]
        if (is.null(schema) || !is.null(schema$error)) {
          return(list(
            time_mode = "auto",
            time_column = NULL,
            excluded_columns = character(),
            subtract_background = background_enabled(),
            background_columns = character()
          ))
        }
        mode <- input[[paste0("time_mode_", key)]] %||% "auto"
        column <- input[[paste0("time_column_", key)]] %||%
          (schema$time_info$column %||% schema$columns[1])
        excluded <- input[[paste0("exclude_columns_", key)]] %||% character()
        list(
          time_mode = mode,
          time_column = column,
          excluded_columns = excluded,
          subtract_background = background_enabled(),
          background_columns = if (background_enabled()) {
            input[[paste0("background_columns_", key)]] %||% character()
          } else {
            character()
          }
        )
      })
    }

    output$needs_rate <- reactive({
      files <- uploaded_files()
      schemas <- upload_schemas()
      if (!NROW(files)) return(FALSE)
      mappings <- collect_column_mappings(files)
      any(vapply(seq_along(mappings), function(i) {
        mode <- mappings[[i]]$time_mode
        source <- schemas[[column_mapping_key(files$upload_id[i])]]$time_info$source
        mode %in% c("frame", "generated") || (mode == "auto" && length(source) && source != "time")
      }, logical(1)))
    })
    outputOptions(output, "needs_rate", suspendWhenHidden = FALSE)

    # Cache the first usable mapped trace independently of baseline edits.
    preview <- reactive({
      files <- uploaded_files()
      mappings <- if (NROW(files)) collect_column_mappings(files)
      for (i in seq_along(mappings)) {
        corrected <- tryCatch({
          mapped <- coerce_numeric_dt(apply_column_mapping(safe_read(files$datapath[i]), mappings[[i]],
            input$pp_sampling_rate %||% 1))
          subtract_background(mapped, mappings[[i]]$background_columns)
        }, error = function(e) NULL)
        dt <- corrected$dt
        if (NCOL(dt) >= 2L) {
          y <- suppressWarnings(as.numeric(dt[[2L]]))
          y[!is.finite(y)] <- NA_real_
          if (any(!is.na(y))) return(list(
            y = y,
            background = corrected$background$Background_mean,
            label = paste(c(files$name[i], names(dt)[2L],
                            if (!is.null(corrected$background)) "background-subtracted"), collapse = " · ")
          ))
        }
      }
      NULL
    })
    output$has_preview <- reactive(!is.null(preview()))
    outputOptions(output, "has_preview", suspendWhenHidden = FALSE)
    output$baseline_preview <- renderPlot({
      x <- preview()
      req(x)
      frames <- baseline_frames_state()
      par(mar = c(2.4, 3.4, 1.7, 0.8), mgp = c(1.3, .35, 0), tcl = -.2)
      plot(seq_along(x$y), x$y, type = "n", xlab = "Frame", ylab = "", main = x$label, cex.main = .85,
           ylim = range(c(x$y, x$background), na.rm = TRUE))
      rect(frames[1] - .5, par("usr")[3], frames[2] + .5, par("usr")[4], col = "#DCEAF1", border = NA)
      lines(x$y, col = "#284B60")
      # The background that was removed, on the same scale. It should be flat.
      if (!is.null(x$background)) {
        lines(x$background, col = "#8A8F98", lty = 2)
        legend("topright", legend = "Background", col = "#8A8F98", lty = 2, bty = "n", cex = .75)
      }
    }, alt = "Uploaded cell trace with the selected baseline frames shaded and, when background subtraction is on, the background trace.")

    current_processing_settings <- function(files = uploaded_files()) {
      list(
        input_data_mode = input$input_data_mode %||% "raw_fluorescence",
        retain_unscorable_recordings = input$retain_unscorable %||% TRUE,
        flag_peak_outliers = input$flag_peak_outliers %||% FALSE,
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
      display_names <- upload_display_names(files$name)

      schemas <- upload_schemas()
      end_frame <- baseline_frames_state()[2]
      too_short <- vapply(files$upload_id, function(id) {
        n <- schemas[[column_mapping_key(id)]]$row_count
        length(n) == 1L && is.finite(n) && n <= end_frame
      }, logical(1))

      # Create file list items
      order_revision()
      meta <- isolate(staged_meta())
      file_items <- lapply(order(meta$order), function(i) {
        tags$div(class = "recording-upload-row", tags$div(
          style = "display: flex; align-items: center; padding: 6px 10px; background: white; border-radius: 4px; margin-bottom: 4px; border: 1px solid var(--color-gray-100);",
          icon("file-csv", style = "color: var(--color-success); margin-right: 10px; font-size: 14px;"),
          tags$span(
            style = "flex: 1; font-size: 13px; font-weight: 500; color: var(--color-gray-900); white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
            display_names[i]
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
        ), recording_editor(meta[i, , drop = FALSE], nrow(files), ns("recording_edit")))
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

        if (any(too_short)) div(class = "alert alert-warning", role = "status",
          sprintf("These files have no frames after baseline end frame %d and will be skipped: %s.",
                  end_frame, paste(display_names[too_short], collapse = ", "))),
        if (sum(suppressWarnings(as.numeric(files$size)), na.rm = TRUE) >= 25 * 1024^2) {
          div(
            class = "alert alert-warning",
            style = "font-size: 12px; margin: 0 0 8px 0; padding: 8px 10px;",
            icon("exclamation-triangle"),
            " Large browser sessions can require several times the uploaded file size in memory. Consider processing smaller batches if the tab becomes slow."
          )
        },

        # File list
        tags$div(
          style = "max-height: min(60vh, 32rem); overflow-y: auto;",
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
    
    # Run after baseline synchronization and stale-state observers in this
    # flush, so a click delivered with an edit commits the edited settings.
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
            group_meta = staged_meta(),
            progress = function(amount, detail) {
              incProgress(amount, detail = detail)
            }
          )
        }),
        error = function(e) {
          if (inherits(e, "simpleca_import_error") && !length(rv$dts)) rv$import_audit <- e$audit
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
            " cell(s) have invalid normalization baselines and remain recorded in quality/exclusion counts: ",
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
      if (length(rv$dts)) {
        n_cells <- nrow(rv$metrics)
        n_groups <- length(rv$groups)
        showNotification(
          paste0("Processed ", n_groups, " file(s); ", n_cells, " cells have usable general metrics. Review cell eligibility and exclusions in Percent Responding."),
          type = "message",
          duration = 6
        )
      }
    }, priority = -100)

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
      } else if (identical(status, "success") && length(rv$dts)) {
        div(class = "small-help", style = "margin-top: 8px; color: var(--color-success);",
            icon("check-circle"), sprintf(" %d recordings processed; %d cells have usable general metrics.", length(rv$dts), nrow(rv$metrics)))
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
      req(length(rv$dts) > 0, !is.null(rv$metrics))
      n_files <- length(rv$dts)
      n_cells <- nrow(rv$metrics) %||% 0L
      timepoint_context <- format_timepoint_context(rv$dts)
      div(
        class = "col-sm-12",
        div(
          class = "results-bar",
          div(
            class = "results-bar-stats",
            icon("check-circle", style = "color: var(--color-success);"),
            HTML(sprintf(
              "<b>%d</b> file%s &nbsp;&middot;&nbsp; <b>%d</b> cells with metrics &nbsp;&middot;&nbsp; <b>%s</b> &mdash; processing complete",
              n_files, if (n_files == 1) "" else "s", n_cells, timepoint_context
            ))
          ),
          primary_button(ns("goto_results"), if (n_cells) "View Results: Time Course" else "View Cell Eligibility", icon = icon("arrow-right"), width = "260px")
        )
      )
    })
    observeEvent(input$goto_results, {
      target <- if (nrow(rv$metrics)) "time" else "in_vitro"
      shinyjs::runjs(sprintf("$('a[href=\"#shiny-tab-%s\"]').click();", target))
    })

    return(rv)
  })
}
