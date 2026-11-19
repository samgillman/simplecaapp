mod_in_vitro_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "in_vitro",
    fluidRow(theme_box("Recording and windows", icon = icon("microscope"),
      uiOutput(ns("data_info")),
      conditionalPanel("output.ready === true", ns = ns,
        plotOutput(ns("overview"), height = "270px"),
        fluidRow(
          column(4, textInput(ns("stimulus"), "Stimulus name", "Capsaicin")),
          column(4, numericInput(ns("start"), "Scoring window start (s)", NA_real_)),
          column(4, numericInput(ns("end"), "Scoring window end (s)", NA_real_))
        ),
        fluidRow(
          column(4, checkboxInput(ns("control"), "Include KCl control", FALSE)),
          conditionalPanel("input.control", ns = ns,
            column(4, numericInput(ns("control_start"), "KCl scoring start (s)", NA_real_)),
            column(4, numericInput(ns("control_end"), "KCl scoring end (s)", NA_real_)))
        ),
        uiOutput(ns("protocol_feedback"), `aria-live` = "polite"),
        accordion(ns("rule"), textOutput(ns("rule_summary"), inline = TRUE), icon = "sliders", content = tagList(
          p("A cell responds when its fractional increase relative to its own prestimulus fluorescence meets both thresholds for consecutive samples. Adjust these starting values to your experimental protocol."),
          fluidRow(
            column(4, numericInput(ns("baseline"), "Prestimulus baseline (s)", 10, min = 0)),
            column(4, numericInput(ns("floor"), "Minimum local fractional rise", 0.10, min = 0, step = 0.01)),
            column(4, numericInput(ns("noise"), "Baseline SD multiplier", 3, min = 0, step = 0.1))
          ),
          fluidRow(
            column(4, numericInput(ns("frames"), "Consecutive samples", 2, min = 1, step = 1)),
            column(4, checkboxInput(ns("auto_gap"), "Automatic gap limit per field", TRUE),
                   conditionalPanel("!input.auto_gap", ns = ns,
                     numericInput(ns("gap"), "Maximum sample gap (s)", 1, min = 0, step = 0.1)))
          ),
          p("A 0.10 floor means a 10% rise over local prestimulus fluorescence. Each window needs at least three baseline samples, coverage through its end, and complete cell measurements. The automatic gap limit is 1.5 times the median frame interval in each field.")
        ))
      )
    )),
    conditionalPanel("output.ready === true", ns = ns,
      fluidRow(theme_box("Results by field",
        uiOutput(ns("cards")),
        p("Evaluable cells have a valid response decision, including flat traces. Excluded cells failed normalization. Unscorable cells lack a reliable local baseline or enough observations."),
        conditionalPanel("output.protocol_ok === true", ns = ns,
          browser_download_button(ns("download_counts"), "Download field counts"),
          browser_download_button(ns("download_calls"), "Download cell calls"))
      )),
      conditionalPanel("output.protocol_ok === true", ns = ns,
        fluidRow(theme_box("Cell review",
          fluidRow(column(6,
            selectInput(ns("status"), "Filter by status", c("All", "Responded", "Did not respond", "Excluded", "Unscorable")),
            DT::DTOutput(ns("calls"))),
            column(6, checkboxInput(ns("full"), "Show full recording", FALSE),
              plotOutput(ns("trace"), height = "310px")))
        ))
      )
    )
  )
}

mod_in_vitro_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    fields <- reactive(names(rv$dts))
    output$ready <- reactive(length(fields()) > 0)
    outputOptions(output, "ready", suspendWhenHidden = FALSE)
    output$data_info <- renderUI({
      if (!length(fields()) && !length(rv$import_audit)) return(tagList(
        p("Start with one CSV or Excel file per field and one fluorescence trace per cell."),
        actionButton(session$ns("go_load"), "Open Load Data", icon = icon("database"), class = "btn-primary")
      ))
      audit <- rv$import_audit
      tagList(p(sprintf("%d of %d files processed. Each processed file represents one field of view.",
                        length(fields()), length(audit))),
              tags$ul(lapply(audit, function(item) tags$li(tags$strong(item$file), " — ", item$note))))

    })
    observeEvent(input$go_load, {
      shinydashboard::updateTabItems(session$rootScope(), "sidebar_tabs", "load")
    })
    selected_field <- reactiveVal(NULL)
    observeEvent(input$field, {
      if (input$field %in% fields()) selected_field(input$field)
    })
    field <- reactive({
      choices <- fields()
      req(length(choices))
      if (length(selected_field()) && selected_field() %in% choices) selected_field() else choices[1]
    })
    output$rule_summary <- renderText({
      sprintf("Response rule · ≥%s local rise and ≥%s SD for %s samples",
              scales::percent(input$floor), input$noise, input$frames)
    })
    protocol <- reactive({
      tryCatch(responder_protocol(input$stimulus, input$start, input$end, input$baseline,
        input$floor, input$noise, input$frames, if (isTRUE(input$auto_gap)) NULL else input$gap, input$control,
        input$control_start, input$control_end), error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    protocol_error <- reactive({
      tryCatch({ protocol(); NULL }, shiny.silent.error = function(e) conditionMessage(e))
    })
    output$protocol_ok <- reactive(is.null(protocol_error()))
    outputOptions(output, "protocol_ok", suspendWhenHidden = FALSE)
    output$protocol_feedback <- renderUI({
      message <- protocol_error()
      if (!is.null(message)) div(class = "alert alert-warning", role = "status", message)
    })
    results <- reactive({
      req(length(fields()) > 0)
      windows <- protocol()
      by_field <- lapply(fields(), function(name) {
        responder_field_results(rv$dts[[name]], names(rv$baselines[[name]]), name, windows)
      })
      calls <- do.call(rbind, lapply(by_field, "[[", "calls"))
      counts <- do.call(rbind, lapply(by_field, "[[", "counts"))
      list(calls = calls, counts = counts)
    })
    # Each row has a keyboard-operable button. Clicking anywhere in the row
    # uses the same field name, even when two uploads have identical basenames.
    output$cards <- renderUI({
      ok <- is.null(protocol_error())
      counts <- if (ok) results()$counts
      labels <- if (ok) unique(ifelse(counts$Population == "KCl-positive cells",
        paste(counts$Stimulus, "among KCl responders"), counts$Stimulus)) else "Responders"
      rows <- lapply(fields(), function(name) {
        values <- if (ok) lapply(which(counts$Field == name), function(i) {
          x <- counts[i, ]
          label <- if (x$Evaluable) sprintf("%d/%d (%.1f%%)", x$Responded, x$Evaluable, x$Percent_responding) else
            span(class = "text-warning", if (x$Population == "KCl-positive cells" && x$Registered == 0)
              "Unavailable · no KCl responders" else "Unavailable · no evaluable cells")
          tags$td(label, tags$small(style = "display:block",
            sprintf("%d excluded · %d unscorable", x$Excluded, x$Unscorable)))
        }) else list(tags$td("Enter scoring windows"))
        tags$tr(class = if (identical(name, field())) "info",
          `data-field` = name, `data-target` = session$ns("field"),
          onclick = "Shiny.setInputValue(this.dataset.target,this.dataset.field,{priority:'event'})",
          tags$td(tags$button(type = "button", class = "btn btn-link", name)), values)
      })
      div(class = "table-responsive", tags$table(class = "table table-hover",
        tags$thead(tags$tr(tags$th("Field of view"), lapply(labels, tags$th))), tags$tbody(rows)))
    })
    # Window previews are independent of protocol validation; a valid primary
    # interval stays visible while the optional control interval is being typed.
    preview_windows <- reactive({
      interval <- function(role, start, end) {
        if (length(start) && length(end) && is.finite(start) && is.finite(end) && end > start)
          data.frame(Role = role, Start = start, End = end)
      }
      rbind(interval("Stimulus", input$start, input$end),
            if (isTRUE(input$control)) interval("KCl", input$control_start, input$control_end))
    })
    trace_plot <- function(time, signal, title) {
      plot <- ggplot(data.frame(Time = time, Signal = signal), aes(Time, Signal)) +
        labs(x = "Time (s)", y = "ΔF/F₀", title = title) + theme_minimal(base_size = 12)
      windows <- preview_windows()
      if (NROW(windows)) plot <- plot +
        geom_rect(data = windows, aes(xmin = Start, xmax = End, fill = Role), ymin = -Inf, ymax = Inf,
                  inherit.aes = FALSE, alpha = 0.22) +
        scale_fill_manual("Scoring window", values = c(Stimulus = "#42A995", KCl = "#D7A047"))
      plot + geom_line(colour = "#244365", linewidth = 0.65, na.rm = TRUE)
    }
    overview_data <- reactive({
      dt <- rv$dts[[field()]]
      signal <- rowMeans(as.data.frame(dt)[-1], na.rm = TRUE)
      data.frame(time = dt$Time, signal = signal)
    })
    output$overview <- renderPlot({
      data <- overview_data()
      trace_plot(data$time, data$signal, paste(field(), "· mean cell trace"))
    })
    review_rows <- reactive({
      rows <- results()$calls
      rows <- rows[rows$Field == field(), , drop = FALSE]
      if (length(input$status) && input$status != "All") rows <- rows[rows$Status == input$status, , drop = FALSE]
      rows
    })
    picked <- reactiveVal(NULL)
    observeEvent(input$calls_rows_selected, {
      i <- input$calls_rows_selected
      rows <- review_rows()
      if (length(i) == 1L && i >= 1 && i <= nrow(rows)) picked(rows[i, c("Cell", "Role")])
    })
    review_index <- function(rows) {
      selection <- picked()
      idx <- if (!is.null(selection)) which(rows$Cell == selection$Cell & rows$Role == selection$Role)
      if (length(idx)) idx[1] else if (nrow(rows)) 1L else integer()
    }
    output$calls <- DT::renderDT({
      rows <- review_rows()
      DT::datatable(rows[, c("Cell", "Stimulus", "Status", "Reason", "Peak_increase", "Threshold_increase")],
        colnames = c("Cell", "Window", "Status", "Reason", "Peak local rise", "Required local rise"),
        rownames = FALSE, selection = list(mode = "single", selected = isolate(review_index(rows))),
        options = list(pageLength = 8, scrollX = TRUE)) |>
        DT::formatRound(c("Peak_increase", "Threshold_increase"), 4)
    })
    output$trace <- renderPlot({
      rows <- review_rows()
      validate(need(nrow(rows) > 0, "No cells match this status."))
      row <- rows[review_index(rows), , drop = FALSE]
      dt <- rv$dts[[field()]]
      validate(need(row$Cell %in% names(dt), row$Reason))
      plot <- trace_plot(dt$Time, dt[[row$Cell]], paste(row$Cell, "·", row$Stimulus, "·", row$Status))
      if (is.finite(row$Threshold_increase)) {
        y <- row$Baseline_mean + (1 + row$Baseline_mean) * row$Threshold_increase
        plot <- plot + annotate("segment", x = row$Start_s, xend = row$End_s, y = y, yend = y,
                                colour = "#9C5126", linetype = "dashed")
      }
      if (!isTRUE(input$full)) {
        margin <- (row$End_s - row$Start_s) * 0.15
        limits <- c(row$Start_s - row$Baseline_s - margin, row$End_s + margin)
        if (limits[1] < max(dt$Time) && limits[2] > min(dt$Time))
          plot <- plot + coord_cartesian(xlim = limits)
      }
      plot
    })
    export_rows <- function(which) {
      rows <- results()[[which]]
      control <- protocol()[protocol()$Role == "Control", , drop = FALSE]
      rows$KCl_start_s <- if (nrow(control)) control$Start_s else NA_real_
      rows$KCl_end_s <- if (nrow(control)) control$End_s else NA_real_
      rows$Source_file <- rv$files$name[match(rows$Field, fields())]
      rows$Input_mode <- rv$input_data_mode
      rows$F0_start_frame <- rv$baseline_frames[1]
      rows$F0_end_frame <- rv$baseline_frames[2]
      rows
    }
    export_base <- reactive(if (length(fields()) > 1) sprintf("%d_fields", length(fields())) else export_base_name(rv))
    register_exports <- function(inputs, target_session) {
      for (kind in c("counts", "calls")) local({
        key <- kind
        suffix <- if (key == "calls") "in_vitro_cell_calls" else "in_vitro_counts"
        register_browser_download(inputs, target_session, paste0("download_", key),
          function() build_export_filename(rv, suffix, base = export_base()),
          function(file) data.table::fwrite(export_rows(key), file, na = "NA"))
      })
    }
    register_exports(input, session)
    list(register_exports = register_exports, ready = reactive(length(fields()) > 0 && is.null(protocol_error())))
  })
}
