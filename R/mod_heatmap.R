# R/mod_heatmap.R

mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  analysis_tab(description = "Process a recording to compare fluorescence across cells and time.", tabName = "heatmap",

          fluidRow(
            theme_box(title = "Heatmap", status = "primary", solidHeader = TRUE, width = 12, collapsible = FALSE,
                plot_workspace(ns, hm_plot_controls(ns),
                div(class = "plot-viewport",
                    withSpinner(plotOutput(ns("heatmap_plot"), height = "100%"), type = 4)
                )
                )
            )
          )
  )
}

# Plot settings shared by the desktop rail and narrow-screen layout.
hm_plot_controls <- function(ns) {
  div(
                class = "control-cols",

                div(class = "control-col",
                  div(class = "control-col-title", icon("th"), "Display & Sorting"),
                  selectInput(ns("hm_sort"), "Sort cells by",
                              choices = c("Time to Peak" = "tpeak", "Peak Amplitude" = "amp", "Original" = "orig"),
                              selected = "tpeak"),
                  selectInput(ns("hm_palette"), "Color palette",
                              choices = c("plasma", "viridis", "magma", "inferno", "cividis"),
                              selected = "plasma"),
                  numericInput(
                    ns("hm_scale_interval"),
                    "Color scale interval",
                    value = 0,
                    min = 0,
                    step = 0.05,
                    width = "100%"
                  ),
                  helpText("Use 0 for automatic spacing; enter 0.5 for ticks every 0.5 ΔF/F₀. Negative values are preserved with a zero-centered diverging scale; the palette choice applies to non-negative data. Small datasets use exact timestamp tiles. Large datasets use labeled time-bin means for display and figure downloads; calculations and CSV exports retain every sample.")
                ),

                div(class = "control-col",
                  div(class = "control-col-title", icon("font"), "Labels & Text"),
                  textInput(ns("hm_title"), "Plot title", ""),
                  checkboxInput(ns("hm_center_title"), "Center title", value = TRUE),
                  textInput(ns("hm_x_label"), "X label", "Time (s)"),
                  textInput(ns("hm_y_label"), "Y label", "Cell"),
                  sliderInput(ns("hm_base_font_size"), "Base font size", 8, 24, 14, 1, width = "100%"),
                  checkboxInput(ns("hm_bold_labels"), "Bold labels", value = TRUE),
                  selectInput(ns("hm_font"), "Font",
                              choices = c("Arial", "Helvetica", "Times", "Courier"),
                              selected = "Arial")
                ),

                div(class = "control-col",
                  div(class = "control-col-title", icon("download"), "Export"),
                  fluidRow(
                    column(6, selectInput(ns("hm_dl_fmt"),"Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), selected = "png")),
                    column(6, selectInput(ns("hm_size_preset"), "Size", choices = c("6x4 in"="6x4","7x5 in"="7x5","8x6 in"="8x6","10x7.5 in"="10x7.5","12x8 in"="12x8"), selected = "8x6"))
                  ),
                  fluidRow(
                    column(6, numericInput(ns("hm_dl_w"),"Width (in)", 8, min = 4, max = 30)),
                    column(6, numericInput(ns("hm_dl_h"),"Height (in)", 6, min = 4, max = 30))
                  ),
                  numericInput(ns("hm_dl_dpi"),"DPI", 300, min = 72, max = 600),
                  browser_download_button(ns("dl_heatmap_plot_local"),"Download Heatmap",
                                 class = "btn-primary", style = "width: 100%; margin-top: 10px;")
                )
  )
}

mod_heatmap_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    presentation <- reactive(recording_view(rv))

    # Auto-title from group names
    last_groups_hm <- reactiveVal(NULL)

    last_auto_title_hm <- reactiveVal(NULL)

    observeEvent(presentation()$labels, {
      req(rv$groups)
      current_groups <- paste(presentation()$labels, collapse = ", ")
      auto_title <- auto_plot_title(presentation()$labels)
      current_title <- isolate(input$hm_title)
      old_groups <- last_groups_hm()

      if (is.null(old_groups)) {
        # First load
        updateTextInput(session, "hm_title", value = auto_title)
      } else if (old_groups != current_groups) {
        # Groups changed — only update if title is empty or still the auto value
        if (is.null(current_title) || !nzchar(trimws(current_title)) ||
            current_title %in% c(old_groups, last_auto_title_hm(), "Population Heatmap")) {
          updateTextInput(session, "hm_title", value = auto_title)
        }
      }
      last_groups_hm(current_groups)
      last_auto_title_hm(auto_title)
    }, ignoreInit = FALSE)

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

    heatmap_plot_reactive <- reactive({
      shiny::validate(shiny::need(
        !is.null(rv$dts) && length(rv$dts) > 0,
        "No data loaded. Go to the Load Data tab, upload your files, and click Process Data."
      ))

      total_cells <- sum(vapply(rv$dts, function(dt) max(0L, ncol(dt) - 1L), integer(1)))
      fields <- purrr::imap(rv$dts, function(dt, label) {
        budget <- max(2L, floor(100000 * max(0L, ncol(dt) - 1L) / max(1L, total_cells)))
        heatmap_display_field(dt, label, input$hm_sort %||% "tpeak",
                              rv$baseline_frames %||% c(1, 20), budget)
      }) |> purrr::compact()
      shiny::validate(shiny::need(length(fields) > 0L,
        "No valid data for heatmap. Check that your files contain numeric cell traces."))
      all_hm <- dplyr::bind_rows(lapply(fields, `[[`, "data"))
      rng <- range(unlist(lapply(fields, `[[`, "range")))
      view <- presentation()
      all_hm_viz <- dplyr::mutate(all_hm, Group = factor(Group, levels = view$levels))
      reduced <- names(fields)[vapply(fields, `[[`, logical(1), "reduced")]
      note <- if (length(reduced)) paste0(
        "Display uses time-bin means: ",
        paste(vapply(reduced, function(key) sprintf("%s: %d to %d time bins",
          view$label(key), fields[[key]]$frames, fields[[key]]$bins), character(1)), collapse = "; "),
        ". All cells retained. Metrics and CSVs use all samples.") else NULL

      # Keep automatic spacing by default, or use the positive interval typed
      # by the user. The same reactive plot powers the screen and downloads.
      scale_info <- compute_heatmap_scale(
        max_value = rng[2],
        min_value = rng[1],
        interval = input$hm_scale_interval %||% 0
      )
      lower <- scale_info$lower
      upper <- scale_info$upper
      brks <- scale_info$breaks

      # Derive typography from consolidated controls
      base_size <- input$hm_base_font_size %||% 14
      bold_labels <- isTRUE(input$hm_bold_labels)
      font <- input$hm_font %||% "Arial"
      label_face <- if (bold_labels) "bold" else "plain"

      # Title — use auto-generated or user-provided
      title_text <- input$hm_title
      if (is.null(title_text) || !nzchar(trimws(title_text))) {
        title_text <- if (!is.null(rv$groups) && length(rv$groups) > 0) {
          auto_plot_title(presentation()$labels)
        } else {
          "Heatmap"
        }
      }

      p <- ggplot(all_hm_viz, aes(Time, Cell, fill = Value))
      exact <- all_hm_viz[!as.character(all_hm_viz$Group) %in% reduced, , drop = FALSE]
      if (nrow(exact)) {
        p <- p + geom_rect(data = exact, aes(xmin = Time_Min, xmax = Time_Max,
                                            ymin = Cell - 0.5, ymax = Cell + 0.5))
      }
      # A layer per recording lets raster spacing follow that recording's
      # time span even when other files have different sample rates/durations.
      for (key in reduced) {
        p <- p + geom_raster(data = all_hm_viz[as.character(all_hm_viz$Group) == key, , drop = FALSE],
                             interpolate = FALSE)
      }
      p <- p + facet_wrap(~ Group, ncol = 1, scales = "free_y",
                          labeller = ggplot2::as_labeller(view$label))

      p <- if (isTRUE(scale_info$diverging)) {
        p + scale_fill_gradient2(
          name = signal_axis_label(rv), low = "#3B4CC0", mid = "white", high = "#B40426",
          midpoint = 0, limits = c(lower, upper), breaks = brks, labels = brks,
          oob = scales::squish, na.value = "gray90"
        )
      } else {
        p + scale_fill_viridis_c(
          name = signal_axis_label(rv), option = input$hm_palette,
          limits = c(lower, upper), breaks = brks, labels = brks,
          oob = scales::squish, na.value = "gray90"
        )
      }

      p +
        guides(fill = guide_colorbar(
          frame.colour = "grey30", frame.linewidth = 0.3,
          ticks.colour = "grey30",
          barheight = grid::unit(190, "pt"), barwidth = grid::unit(13, "pt")
        )) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(title = title_text, x = input$hm_x_label, y = input$hm_y_label,
             caption = if (!is.null(note)) paste(strwrap(note, width = 100), collapse = "\n")) +
        theme_classic(base_size = base_size) +
        theme(
          plot.title = element_text(
            size = base_size + 4,
            face = label_face,
            hjust = if (isTRUE(input$hm_center_title)) 0.5 else 0,
            family = font
          ),
          plot.subtitle = element_text(margin = margin(b = 4)),
          axis.title = element_text(
            size = base_size,
            face = label_face,
            family = font
          ),
          axis.text = element_text(
            size = max(8, base_size - 2),
            family = font
          ),
          legend.text = element_text(
            size = max(10, base_size - 2),
            family = font
          ),
          legend.title = element_text(
            size = base_size,
            family = font,
            face = "bold",
            margin = margin(b = 8)
          ),
          strip.background = element_blank(),
          # Label each facet with its group on multi-file uploads; a single
          # group is already named by the title
          strip.text = if (length(rv$dts) > 1) {
            element_text(size = base_size, face = label_face, family = font,
                         hjust = 0, margin = margin(t = 6, b = 4))
          } else {
            element_blank()
          }
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

    register_browser_download(input, session, "dl_heatmap_plot_local",
      filename = function() {
        build_export_filename(
          rv,
          parts = "heatmap",
          ext = input$hm_dl_fmt %||% "png"
        )
      },
      content = function(file) {
        req(heatmap_plot_reactive())
        save_plot_file(file, heatmap_plot_reactive(), input$hm_dl_w, input$hm_dl_h,
                       input$hm_dl_dpi, input$hm_dl_fmt %||% "png")
      }
    )

    list(plot = heatmap_plot_reactive)
  })
}
