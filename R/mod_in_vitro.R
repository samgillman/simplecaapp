mod_in_vitro_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "in_vitro",
    fluidRow(theme_box("In Vitro Analysis", icon = icon("microscope"),
      p("Measure the fraction of cells responding to a stimulus in each field of view."),
      uiOutput(ns("data_info"))
    )),
    conditionalPanel("output.ready === true", ns = ns,
      fluidRow(theme_box("Stimulus protocol",
        fluidRow(
          column(4, textInput(ns("stimulus"), "Stimulus name", "Capsaicin")),
          column(4, numericInput(ns("start"), "Response start (s)", NA_real_)),
          column(4, numericInput(ns("end"), "Response end (s)", NA_real_))
        ),
        checkboxInput(ns("control"), "Include a KCl control window", FALSE),
        conditionalPanel("input.control", ns = ns,
          fluidRow(column(4, numericInput(ns("control_start"), "KCl start (s)", NA_real_)),
                   column(4, numericInput(ns("control_end"), "KCl end (s)", NA_real_)))
        ),
        accordion(ns("rule"), "Response rule", icon = "sliders", content = tagList(
          p("A cell responds when its increase above its own prestimulus mean meets both thresholds for consecutive samples. Adjust these starting values to your experimental protocol."),
          fluidRow(
            column(4, numericInput(ns("baseline"), "Prestimulus baseline (s)", 10, min = 0)),
            column(4, numericInput(ns("floor"), "Minimum increase (ΔF/F₀)", 0.10, min = 0, step = 0.01)),
            column(4, numericInput(ns("noise"), "Baseline SD multiplier", 3, min = 0, step = 0.1))
          ),
          fluidRow(
            column(4, numericInput(ns("frames"), "Consecutive samples", 2, min = 1, step = 1)),
            column(4, numericInput(ns("gap"), "Maximum sample gap (s)", 1, min = 0, step = 0.1))
          ),
          p("Each window needs at least three baseline samples, coverage through its end, and complete cell measurements. The same settings apply to every processed field.")
        )),
        uiOutput(ns("protocol_feedback"), `aria-live` = "polite")
      )),
      conditionalPanel("output.protocol_ok === true", ns = ns,
        fluidRow(theme_box("Responder percentages",
          selectInput(ns("field"), "Field of view", choices = character()),
          uiOutput(ns("cards")),
          p("Each percentage uses responding cells divided by evaluable cells. Upload one trace per identified cell, including flat traces, to measure a whole-field percentage."),
          browser_download_button(ns("download_counts"), "Download counts for all fields", class = "btn-default")
        )),
        fluidRow(theme_box("Cell review",
          fluidRow(
            column(6, selectInput(ns("cell"), "Cell", choices = character())),
            column(6, selectInput(ns("role"), "Window to inspect", choices = c("Primary stimulus" = "Stimulus")))
          ),
          plotOutput(ns("trace"), height = "260px"),
          DT::DTOutput(ns("calls")),
          browser_download_button(ns("download_calls"), "Download cell calls for all fields", class = "btn-default")
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
      if (!length(fields())) return(tagList(
        p("Start with one CSV or Excel file per field and one fluorescence trace per cell."),
        actionButton(session$ns("go_load"), "Open Load Data", icon = icon("database"), class = "btn-primary")
      ))
      p(sprintf("%d processed field(s). Enter times in seconds on the same clock shown in Time Course.", length(fields())))
    })
    observeEvent(input$go_load, {
      shinydashboard::updateTabItems(session$rootScope(), "sidebar_tabs", "load")
    })
    observeEvent(fields(), {
      choices <- fields()
      updateSelectInput(session, "field", choices = choices, selected = choices[1])
    }, ignoreNULL = FALSE)
    field <- reactive({
      req(input$field, input$field %in% fields())
      input$field
    })
    observeEvent(list(field(), rv$baselines), {
      cells <- names(rv$baselines[[field()]])
      updateSelectInput(session, "cell", choices = cells, selected = cells[1])
    })
    observeEvent(input$control, {
      roles <- c("Primary stimulus" = "Stimulus")
      if (isTRUE(input$control)) roles <- c(roles, "KCl" = "Control")
      updateSelectInput(session, "role", choices = roles, selected = roles[1])
    })
    protocol <- reactive({
      tryCatch(responder_protocol(input$stimulus, input$start, input$end, input$baseline,
        input$floor, input$noise, input$frames, input$gap, input$control,
        input$control_start, input$control_end), error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    protocol_error <- reactive({
      tryCatch({ protocol(); NULL }, shiny.silent.error = function(e) conditionMessage(e))
    })
    output$protocol_ok <- reactive(is.null(protocol_error()))
    outputOptions(output, "protocol_ok", suspendWhenHidden = FALSE)
    output$protocol_feedback <- renderUI({
      message <- protocol_error()
      if (!is.null(message)) p(class = "text-muted", message)
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
    output$cards <- renderUI({
      rows <- results()$counts
      rows <- rows[rows$Field == field(), , drop = FALSE]
      cards <- lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, ]
        label <- if (row$Population == "KCl-positive cells")
          paste(row$Stimulus, "among KCl responders") else paste(row$Stimulus, "responders")
        pct <- if (is.na(row$Percent_responding)) "Unavailable" else sprintf("%.1f%%", row$Percent_responding)
        column(if (nrow(rows) == 1L) 12 else 4,
          div(class = "well well-sm", tags$strong(label),
              h3(pct, style = "color: var(--color-primary-blue); margin: 8px 0;"),
              p(sprintf("%d of %d evaluable cells", row$Responded, row$Evaluable)),
              span(sprintf("%d did not respond · %d excluded · %d unscorable",
                           row$Nonresponders, row$Excluded, row$Unscorable))))
      })
      do.call(fluidRow, cards)
    })
    current_calls <- reactive({
      rows <- results()$calls
      rows[rows$Field == field(), , drop = FALSE]
    })
    output$calls <- DT::renderDT({
      rows <- current_calls()
      DT::datatable(rows[, c("Cell", "Stimulus", "Status", "Reason", "Peak_increase", "Threshold_increase")],
                    rownames = FALSE, selection = "none", options = list(pageLength = 8, scrollX = TRUE)) |>
        DT::formatRound(c("Peak_increase", "Threshold_increase"), 4)
    })
    output$trace <- renderPlot({
      req(input$cell, input$role)
      rows <- current_calls()
      row <- rows[rows$Cell == input$cell & rows$Role == input$role, , drop = FALSE]
      req(nrow(row) == 1L)
      data <- rv$dts[[field()]]
      validate(need(input$cell %in% names(data), "This cell was excluded because its raw fluorescence baseline was invalid."))
      trace <- data.frame(Time = data$Time, Signal = data[[input$cell]])
      plot <- ggplot(trace, aes(Time, Signal)) +
        annotate("rect", xmin = row$Start_s - row$Baseline_s, xmax = row$Start_s,
                 ymin = -Inf, ymax = Inf, fill = "#DCE5EB", alpha = 0.7) +
        annotate("rect", xmin = row$Start_s, xmax = row$End_s,
                 ymin = -Inf, ymax = Inf, fill = "#C4E6DF", alpha = 0.7) +
        geom_line(colour = "#244365", linewidth = 0.65, na.rm = TRUE) +
        labs(x = "Time (s)", y = "ΔF/F₀", title = paste(row$Cell, "·", row$Stimulus, "·", row$Status),
             caption = "Gray = baseline     Green = response window     Dashed = response threshold") +
        theme_minimal(base_size = 12)
      if (is.finite(row$Threshold_increase)) {
        plot <- plot + annotate("segment", x = row$Start_s, xend = row$End_s,
          y = row$Baseline_mean + row$Threshold_increase, yend = row$Baseline_mean + row$Threshold_increase,
          colour = "#9C5126", linetype = "dashed")
      }
      plot
    })
    export_rows <- function(which) {
      rows <- results()[[which]]
      if (which == "counts") {
        windows <- protocol()
        settings <- windows[match(rows$Stimulus, windows$Stimulus),
                            setdiff(names(windows), c("Role", "Stimulus")), drop = FALSE]
        rows <- cbind(rows, settings)
      }
      control <- protocol()[protocol()$Role == "Control", , drop = FALSE]
      rows$KCl_start_s <- if (nrow(control)) control$Start_s else NA_real_
      rows$KCl_end_s <- if (nrow(control)) control$End_s else NA_real_
      rows$Source_file <- rv$files$name[match(rows$Field, fields())]
      rows$Input_mode <- rv$input_data_mode
      rows$F0_start_frame <- rv$baseline_frames[1]
      rows$F0_end_frame <- rv$baseline_frames[2]
      rows
    }
    register_browser_download(input, session, "download_calls",
      function() build_export_filename(rv, "in_vitro_cell_calls"),
      function(file) data.table::fwrite(export_rows("calls"), file, na = "NA"))
    register_browser_download(input, session, "download_counts",
      function() build_export_filename(rv, "in_vitro_counts"),
      function(file) data.table::fwrite(export_rows("counts"), file, na = "NA"))
  })
}
