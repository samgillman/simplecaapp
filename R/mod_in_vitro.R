responder_colors <- c("Responded" = "#177465", "Did not respond" = "#925223")

mod_in_vitro_ui <- function(id) {
  ns <- NS(id)
  scoring_number <- function(id, label) {
    tags$div(oninput = sprintf("event.target.dataset.userEdited='1'; var n=document.getElementById('%s'); if(n) n.hidden=true;", ns("defaults_notice")),
      numericInput(ns(id), label, NA_real_))
  }
  settings_id <- ns("settings")
  analysis_tab(description = "Process one recording per field to score cells and compare responder percentages.",
    empty_detail = uiOutput(ns("failed_imports")), tabName = "in_vitro",
    fluidRow(theme_box("Percent responding", uiOutput(ns("headline")),
      conditionalPanel("input.control", ns = ns,
        radioButtons(ns("population"), "Population", c("Stimulus" = "primary", "KCl" = "control",
          "Stimulus among KCl responders" = "restricted"), inline = TRUE)),
      tags$div(id = ns("defaults_notice"), class = "alert alert-info", role = "status", hidden = NA),
      uiOutput(ns("protocol_feedback")),
      tags$button(type = "button", class = "btn btn-default btn-sm", "Edit scoring settings",
        onclick = sprintf("var e=document.getElementById('%s'); e.open=true; e.scrollIntoView(); e.querySelector('summary').focus();", settings_id)))),
    conditionalPanel("output.protocol_ok === true", ns = ns,
      fluidRow(theme_box("Response distribution",
        radioButtons(ns("distribution_scale"), NULL, c("Peak / threshold" = "margin", "Peak local rise" = "rise"),
          selected = "margin", inline = TRUE),
        textOutput(ns("distribution_note")), uiOutput(ns("distribution_view")))),
      fluidRow(theme_box("Population traces", textOutput(ns("population_label")),
        plotOutput(ns("population_plot"), height = "300px"),
        tags$details(tags$summary("Heatmap by response and onset"), plotOutput(ns("population_heatmap"), height = "360px"))))
    ),
    fluidRow(theme_box("Results by field", uiOutput(ns("cards")),
      tags$details(tags$summary("Cell count definitions"),
        p("Evaluable includes responders and nonresponders. Excluded cells failed loading normalization. Unscorable cells lack the observations or local baseline needed for a decision.")),
      conditionalPanel("output.protocol_ok === true", ns = ns,
        browser_download_button(ns("download_counts"), "Download field counts"),
        browser_download_button(ns("download_calls"), "Download cell calls")))),
    fluidRow(theme_box("Recording and scoring settings",
      tags$details(id = settings_id, tags$summary(tags$strong("Scoring settings"), " · ",
          textOutput(ns("rule_summary"), inline = TRUE)),
        uiOutput(ns("data_info")), plotOutput(ns("overview"), height = "235px"),
        p("Suggested times come from the recording. Confirm stimulus application and scoring times against your protocol."),
        fluidRow(column(4, textInput(ns("stimulus"), "Stimulus name", "", placeholder = "e.g. Capsaicin")),
          column(4, scoring_number("start", "Scoring window start (s)")),
          column(4, scoring_number("end", "Scoring window end (s)"))),
        fluidRow(column(4, scoring_number("baseline", "Prestimulus baseline (s)")),
          column(4, numericInput(ns("floor"), "Minimum local fractional rise", .10, min = 0, step = .01)),
          column(4, numericInput(ns("noise"), "Baseline SD multiplier", 3, min = 0, step = .1))),
        actionButton(ns("reset_windows"), "Use recording defaults"),
        fluidRow(column(4, numericInput(ns("frames"), "Consecutive samples", 2, min = 1, step = 1)),
          column(4, checkboxInput(ns("auto_gap"), "Automatic gap limit per field", TRUE),
            conditionalPanel("!input.auto_gap", ns = ns,
              numericInput(ns("gap"), "Maximum sample gap (s)", 1, min = 0, step = .1)))),
        checkboxInput(ns("control"), "Include KCl control", FALSE),
        conditionalPanel("input.control", ns = ns, fluidRow(
          column(4, numericInput(ns("control_start"), "KCl scoring start (s)", NULL)),
          column(4, numericInput(ns("control_end"), "KCl scoring end (s)", NULL)))),
        p("A response must meet the larger of the floor and the SD-based threshold for the chosen number of consecutive samples. Each window needs three prestimulus samples. The automatic gap limit is 1.5 times the field’s median frame interval."),
        uiOutput(ns("settings_feedback")),
        conditionalPanel("output.protocol_ok === true", ns = ns,
          tags$button(type = "button", class = "btn btn-primary", "Review results",
            onclick = sprintf("document.getElementById('%s').open=false; document.getElementById('%s').scrollIntoView();", settings_id, ns("headline"))))))),
    conditionalPanel("output.protocol_ok === true", ns = ns,
      fluidRow(theme_box("Cell review",
        selectInput(ns("status"), "Filter cells", c("All", "Near threshold", "Responded", "Did not respond", "Excluded", "Unscorable")),
        DT::DTOutput(ns("calls")),
        actionButton(ns("previous"), "Previous cell"), actionButton(ns("next_cell"), "Next cell"),
        checkboxInput(ns("full"), "Show full recording", FALSE),
        tags$p(`aria-live` = "polite", textOutput(ns("review_label"), inline = TRUE)),
        plotOutput(ns("trace"), height = "365px"))),
      fluidRow(theme_box("Methods", tags$details(tags$summary("Methods for this analysis"),
        textOutput(ns("methods")), tags$button(type = "button", class = "btn btn-default", "Copy methods",
          onclick = sprintf("var e=document.getElementById('%s'); if(navigator.clipboard) navigator.clipboard.writeText(e.textContent);", ns("methods"))))))
    ),
    tags$script(HTML(sprintf("Shiny.addCustomMessageHandler('%s', function(id) {
      var e=document.getElementById(id); if(e) e.scrollIntoView({block:'start'});
    });", ns("review")))),
    tags$script(HTML(sprintf("
      Shiny.addCustomMessageHandler('%s', function(m) {
        var keys=Object.keys(m.values), values={}, edited=false;
        keys.forEach(function(k) {
          var e=document.getElementById(m.prefix+k);
          var keep=e && !m.reset && e.dataset.userEdited;
          edited=edited || !!keep;
          values[k]=keep ? (e.value.trim()==='' ? null : Number(e.value)) : m.values[k];
        });
        var finite=function(v){return typeof v==='number' && Number.isFinite(v);};
        var complete=function(v){return keys.every(function(k){return finite(v[k]);});};
        var bounds=m.bounds;
        var fits=function(v) {
          if (!bounds || !complete(v)) return false;
          var tol=8*Number.EPSILON*Math.max(1,Math.abs(v.start-v.baseline),
            Math.abs(v.start),Math.abs(v.end),Math.abs(bounds.start),Math.abs(bounds.end));
          return v.baseline>0 && v.start<v.end && v.start<bounds.end &&
            v.start-v.baseline>=bounds.start-tol && v.end<=bounds.end_with_gap+tol;
        };
        var notice=document.getElementById(m.prefix+'defaults_notice');
        notice.textContent='';
        notice.hidden=true;
        // Deliberately cleared inputs stay blank. A numerical window carried
        // into a new recording is reset as a whole when its coverage fails.
        var reset=!!m.reset;
        if (edited && complete(values) && !fits(values)) {
          if (fits(m.values)) {
            reset=true;
            notice.textContent='Previous scoring times do not fit the loaded recordings. Reset to '+
              m.values.start+'–'+m.values.end+' s with a '+m.values.baseline+
              ' s local baseline. Check these times against your stimulus protocol.';
          } else {
            notice.textContent='Previous scoring times do not fit, and the recordings have no usable common default window. Review the scoring settings and field coverage messages.';
          }
          notice.hidden=false;
        }
        keys.forEach(function(k) {
          var e=document.getElementById(m.prefix+k);
          if (!e) return;
          if (reset) delete e.dataset.userEdited;
          var v=reset ? m.values[k] : values[k];
          e.value=finite(v) ? v : '';
          window.jQuery(e).trigger('change');
        });
        var settings=document.getElementById(m.prefix+'settings');
        if (!settings.dataset.initialized || !notice.hidden) {settings.open=true; settings.dataset.initialized='1';}
      });", ns("defaults"))))
  )
}

mod_in_vitro_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    presentation <- reactive(recording_view(rv))
    fields <- reactive(names(rv$dts))
    output$ready <- reactive(length(fields()) > 0)
    outputOptions(output, "ready", suspendWhenHidden = FALSE)
    output$data_info <- renderUI({
      audit <- rv$import_audit
      tagList(p(sprintf("%d of %d files processed. Each processed file represents one field of view.",
                        length(fields()), length(audit))),
              tags$ul(lapply(audit, function(item) tags$li(tags$strong(item$file), " — ", item$note))))

    })
    output$failed_imports <- renderUI({
      req(!length(fields()), length(rv$import_audit))
      tags$ul(lapply(rv$import_audit, function(item) tags$li(tags$strong(item$file), " — ", item$note)))
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
    push_defaults <- function(reset = FALSE) {
      values <- responder_defaults(rv$dts, rv$baseline_frames)
      if (length(values)) session$sendCustomMessage(session$ns("defaults"),
        list(values = as.list(values),
          bounds = as.list(responder_bounds(rv$dts, if (isFALSE(input$auto_gap)) input$gap else NULL)),
          reset = reset, prefix = session$ns("")))
    }
    observeEvent(list(rv$dts, rv$baseline_frames), push_defaults())
    observeEvent(input$reset_windows, push_defaults(TRUE))
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
    output$settings_feedback <- renderUI({
      msg <- protocol_error()
      if (!is.null(msg)) div(class = "alert alert-warning", msg)
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
    population <- reactive(if (isTRUE(input$control)) input$population %||% "primary" else "primary")
    role <- reactive(if (population() == "control") "Control" else "Stimulus")
    active_counts <- reactive({
      x <- results()$counts
      subset_name <- if (population() == "restricted") "KCl-positive cells" else "Cells in field"
      x[x$Role == role() & x$Population == subset_name, , drop = FALSE]
    })
    active_calls <- reactive({
      all <- results()$calls
      x <- all[all$Role == role(), , drop = FALSE]
      if (population() == "restricted") {
        eligible <- all[all$Role == "Control" & all$Status == "Responded", c("Field", "Cell")]
        x <- dplyr::semi_join(x, eligible, by = c("Field", "Cell"))
      }
      # URI-encoded components keep identifiers unique and safe in the browser table.
      encode <- function(s) vapply(s, utils::URLencode, "", reserved = TRUE, repeated = TRUE, USE.NAMES = FALSE)
      x$Key <- if (nrow(x)) paste(encode(x$Field), encode(x$Cell), x$Role, sep = ":") else character()
      x
    })
    rule_line <- reactive({
      if (!is.null(protocol_error())) return("Choose the stimulus and scoring window")
      p <- protocol()
      p <- p[p$Role == role(), ]
      sprintf("%s · requested %g–%g s · %g s local baseline · larger of %g%% or %g SD · %d consecutive sample%s · %s",
        p$Stimulus, p$Start_s, p$End_s, p$Baseline_s, 100 * p$Floor_local_fraction, p$SD_multiplier,
        p$Consecutive_samples, if (p$Consecutive_samples == 1) "" else "s",
        if (is.na(p$Max_gap_s)) "automatic gap" else sprintf("%g s gap limit", p$Max_gap_s))
    })
    output$rule_summary <- renderText(rule_line())
    row_notes <- function(x) {
      notes <- c(x$Window_warning, x$Field_reason)
      if (x$Population == "KCl-positive cells" && nzchar(x$KCl_window_warning))
        notes <- c(notes, paste("KCl:", x$KCl_window_warning))
      if (!x$Evaluable && !nzchar(x$Field_reason)) notes <- c(notes, "No evaluable cells")
      paste(unique(notes[nzchar(notes)]), collapse = " · ")
    }
    output$headline <- renderUI({
      req(is.null(protocol_error()))
      x <- active_counts()
      s <- responder_summary(x)
      n <- s$totals
      notes <- vapply(seq_len(nrow(x)), function(i) row_notes(x[i, ]), "")
      tags$div(
        tags$strong(style = "font-size:2.3rem", if (is.finite(s$pooled)) sprintf("%.1f%% responding", s$pooled) else "No evaluable cells"),
        p(sprintf("%d of %d evaluable cells%s%s", n["Responded"], n["Evaluable"],
          if (nrow(x) > 1L) " · pooled across fields" else "",
          if (population() == "restricted") " · among KCl responders" else "")),
        p(sprintf("%d responded · %d did not respond · %d excluded · %d unscorable",
          n["Responded"], n["Nonresponders"], n["Excluded"], n["Unscorable"])),
        if (nrow(x) > 1L && s$n) p(sprintf("Mean of field percentages %.1f%%%s · %d of %d fields evaluable",
          s$mean, if (s$n > 1) sprintf(" ± %.1f percentage points (SD)", s$sd) else "", s$n, nrow(x))),
        p(rule_line()),
        if (any(nzchar(notes))) tags$ul(class = "alert alert-warning", lapply(which(nzchar(notes)), function(i)
          tags$li(presentation()$label(x$Field[i]), " · ", notes[i]))))
    })
    output$cards <- renderUI({
      ok <- is.null(protocol_error())
      counts <- if (ok) active_counts()
      view <- presentation()
      rows <- lapply(intersect(view$levels, fields()), function(name) {
        cells <- if (ok) {
          x <- counts[counts$Field == name, , drop = FALSE]
          lapply(c(x$Responded, x$Nonresponders, x$Evaluable, x$Excluded, x$Unscorable,
            if (x$Evaluable) sprintf("%.1f%%", x$Percent_responding) else "—", row_notes(x)), tags$td)
        } else tags$td("Complete scoring settings")
        tags$tr(class = if (identical(name, field())) "info", `data-field` = name, `data-target` = session$ns("field"),
          onclick = "Shiny.setInputValue(this.dataset.target,this.dataset.field,{priority:'event'})",
          tags$td(tags$button(type = "button", class = "btn btn-link", view$label(name))), cells)
      })
      headings <- if (ok) c("Responded", "Did not respond", "Evaluable", "Excluded", "Unscorable", "% responding", "Notes") else "Status"
      div(class = "table-responsive", tags$table(class = "table table-hover",
        tags$thead(tags$tr(lapply(c("Field of view", headings), tags$th))), tags$tbody(rows)))
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
    trace_plot <- function(time, signal, title, windows = preview_windows()) {
      plot <- ggplot(data.frame(Time = time, Signal = signal), aes(Time, Signal)) +
        labs(x = "Time (s)", y = "ΔF/F₀", title = title) + theme_minimal(base_size = 12)
      if (NROW(windows) && length(input$baseline) && is.finite(input$baseline) && input$baseline > 0) {
        baselines <- transform(windows, Role = "Local baseline", Start = Start - input$baseline, End = Start)
        windows <- rbind(baselines, windows)
      }
      if (NROW(windows)) plot <- plot +
        geom_rect(data = windows, aes(xmin = Start, xmax = End, fill = Role), ymin = -Inf, ymax = Inf,
                  inherit.aes = FALSE, alpha = 0.22) +
        scale_fill_manual("Window", values = c("Local baseline" = "#92A0B5", Stimulus = "#42A995", KCl = "#D7A047"))
      plot + geom_line(colour = "#244365", linewidth = 0.65, na.rm = TRUE)
    }
    overview_data <- reactive({
      dt <- rv$dts[[field()]]
      signal <- rowMeans(as.data.frame(dt)[-1], na.rm = TRUE)
      data.frame(time = dt$Time, signal = signal)
    })
    output$overview <- renderPlot({
      data <- overview_data()
      trace_plot(data$time, data$signal, paste(presentation()$label(field()), "· mean cell trace"))
    })
    picked <- reactiveVal("")
    near_cutoff <- function(x) is.finite(x$Margin) & x$Margin >= .8 & x$Margin <= 1.2
    matches_filter <- function(x) {
      status <- input$status %||% "All"
      switch(status, All = rep(TRUE, nrow(x)), `Near threshold` = near_cutoff(x), x$Status == status)
    }
    review_rows <- reactive({
      x <- active_calls()
      x <- x[x$Field == field() & matches_filter(x), , drop = FALSE]
      distance <- if (identical(input$status, "Near threshold")) abs(x$Margin - 1) else x$Margin
      x[order(distance, x$Cell, na.last = TRUE), , drop = FALSE]
    })
    review_index <- function(rows) {
      i <- match(picked(), rows$Key)
      if (!is.na(i)) i else if (nrow(rows)) 1L else integer()
    }
    selected_call <- reactive({
      rows <- review_rows()
      validate(need(nrow(rows) > 0, "No cells match this population and filter."))
      rows[review_index(rows), , drop = FALSE]
    })
    # Send stable keys from the client so late events after sorting or changing fields
    # cannot select an unrelated row with the same table index.
    output$calls <- DT::renderDT({
      rows <- review_rows()
      cols <- c("Cell", "Status", "Peak_increase", "Threshold_increase", "Threshold_source", "Margin",
        "Onset_s", "Peak_s", "Baseline_mean", "Baseline_SD", "Reason", "Key")
      callback <- DT::JS(sprintf("
        var node=table.table().node(), target='%s';
        function publishOrder(){
          var keys=table.rows({order:'applied'}).data().toArray().map(function(r){return r[11]});
          Shiny.setInputValue(target+'ordered',keys);
        }
        publishOrder(); table.on('order.dt',publishOrder);
        $(node).on('click','tbody tr',function(){var r=table.row(this).data();if(r) Shiny.setInputValue(target+'pick',r[11],{priority:'event'});});
        ", session$ns("")))
      DT::datatable(rows[, cols], rownames = FALSE, selection = list(mode = "single", selected = isolate(review_index(rows))),
        colnames = c("Cell", "Status", "Peak rise", "Threshold", "Set by", "Margin", "Onset (s)", "Peak time (s)",
          "Baseline mean (ΔF/F₀)", "Baseline SD (local)", "Reason", "Key"),
        escape = 1:11, callback = callback, options = list(pageLength = 8, dom = "tip", order = list(), scrollX = TRUE,
          columnDefs = list(list(targets = 11L, visible = FALSE)))) |>
        DT::formatRound(c("Peak_increase", "Threshold_increase", "Margin", "Baseline_mean", "Baseline_SD"), 4)
    }, server = FALSE)
    observeEvent(input$pick, { if (input$pick %in% review_rows()$Key) picked(input$pick) })
    ordering <- reactive({
      keys <- review_rows()$Key
      order <- as.character(input$ordered)
      if (length(keys) && identical(sort(keys), sort(order))) order else keys
    })
    navigate <- function(direction) {
      keys <- ordering()
      i <- match(selected_call()$Key, keys) + direction
      if (length(i) && !is.na(i) && i >= 1L && i <= length(keys)) picked(keys[i])
    }
    observeEvent(input$previous, navigate(-1L))
    observeEvent(input$next_cell, navigate(1L))
    observe({
      x <- review_rows()
      ids <- ordering()
      i <- review_index(x)
      pos <- match(x$Key[i], ids)
      shinyjs::toggleState("previous", length(pos) == 1L && pos > 1L)
      shinyjs::toggleState("next_cell", length(pos) == 1L && pos < length(ids))
      if (length(pos)) {
        proxy <- DT::dataTableProxy("calls", session = session)
        DT::selectRows(proxy, i)
        DT::selectPage(proxy, ceiling(pos / 8))
      }
    })
    evaluable <- reactive({
      x <- active_calls()
      x[is.finite(x$Margin), , drop = FALSE]
    })
    output$distribution_note <- renderText({
      x <- evaluable()
      sprintf("%d of %d evaluable cells within ±20%% of threshold · %d floor-limited · %d noise-limited",
        sum(near_cutoff(x)), nrow(x), sum(x$Threshold_source == "Floor"), sum(x$Threshold_source == "Baseline noise"))
    })
    # Remove the widget when there are no calls. In Shinylive, a Plotly
    # validation error can otherwise leave its previous traces on screen.
    output$distribution_view <- renderUI({
      if (nrow(evaluable()) == 0L) return(p(role = "status", "No evaluable cells in this population."))
      plotly::plotlyOutput(session$ns("distribution"), height = "245px")
    })
    distribution_source <- session$ns("distribution")
    distribution_drawn <- reactiveVal(FALSE)
    output$distribution <- plotly::renderPlotly({
      x <- evaluable()
      validate(need(nrow(x) > 0L, "No evaluable cells in this population."))
      margin <- !identical(input$distribution_scale, "rise")
      x$value <- if (margin) x$Margin else x$Peak_increase
      x <- x[order(x$Key), , drop = FALSE]
      x$y <- as.numeric(x$Status == "Responded") + ((seq_len(nrow(x)) * .61803399) %% 1 - .5) * .45
      x$tooltip <- sprintf("%s · %s<br>%s · %s<br>Peak %.4g · floor %.4g · noise %.4g<br>Margin %.3g×",
        htmltools::htmlEscape(presentation()$label(x$Field)), htmltools::htmlEscape(x$Cell),
        x$Status, x$Threshold_source, x$Peak_increase, x$Floor_threshold, x$Noise_threshold, x$Margin)
      cutoff <- if (margin) 1 else x$Floor_threshold[1]
      shapes <- list(list(type = "line", x0 = cutoff, x1 = cutoff, y0 = 0, y1 = 1, yref = "paper",
        line = list(color = "#354864", dash = "dash")))
      if (margin) shapes <- c(list(list(type = "rect", x0 = .8, x1 = 1.2, y0 = 0, y1 = 1,
        yref = "paper", fillcolor = "#e4edf4", line = list(width = 0), layer = "below")), shapes)
      p <- plotly::plot_ly(data = x, x = ~value, y = ~y, key = ~Key, source = distribution_source,
        type = "scatter", mode = "markers", text = ~tooltip, hoverinfo = "text",
        marker = list(color = unname(responder_colors[x$Status]), size = 9,
          symbol = ifelse(x$Threshold_source == "Floor", "circle", "diamond-open")))
      xr <- range(c(x$value, cutoff, if (margin) c(.8, 1.2)))
      pad <- max(diff(xr) * .04, abs(cutoff) * .05)
      p <- plotly::layout(p, shapes = shapes, hovermode = "closest", showlegend = FALSE,
        xaxis = list(title = if (margin) "Peak rise / cell threshold (×)" else "Peak local fractional rise · diamonds indicate noise-limited cells",
          range = xr + c(-pad, pad)),
        yaxis = list(title = "", tickvals = c(0, 1), ticktext = c("Did not respond", "Responded"), range = c(-.45, 1.45)),
        margin = list(l = 105, r = 15, t = 15, b = 55))
      distribution_drawn(TRUE)
      plotly::event_register(p, "plotly_click")
    })
    point_event <- reactive({
      req(distribution_drawn())
      plotly::event_data("plotly_click", source = distribution_source, priority = "event")
    })
    observeEvent(point_event(), {
      key <- point_event()$key
      req(length(key) == 1L)
      x <- evaluable()
      row <- x[x$Key == key, , drop = FALSE]
      if (nrow(row) == 1L) {
        if (!matches_filter(row)) updateSelectInput(session, "status", selected = "All")
        selected_field(row$Field)
        picked(row$Key)
        session$sendCustomMessage(session$ns("review"), session$ns("calls"))
      }
    })
    field_traces <- reactive({
      rows <- evaluable()
      rows <- rows[rows$Field == field(), , drop = FALSE]
      dt <- rv$dts[[field()]]
      validate(need(nrow(rows) > 0L, "No evaluable cells for this field and population."))
      rows <- rows[order(match(rows$Status, names(responder_colors)), rows$Onset_s, rows$Cell, na.last = TRUE), ]
      local <- sweep(as.matrix(as.data.frame(dt)[rows$Cell]), 2, rows$Baseline_mean, "-")
      list(rows = rows, time = dt$Time, local = sweep(local, 2, 1 + rows$Baseline_mean, "/"))
    })
    active_windows <- function(row) {
      end <- if (is.finite(row$Effective_end_s)) row$Effective_end_s else row$End_s
      data.frame(Role = if (row$Role == "Control") "KCl" else "Stimulus", Start = row$Start_s, End = end)
    }
    output$population_label <- renderText({
      x <- evaluable()
      x <- x[x$Field == field(), , drop = FALSE]
      sprintf("%s · %d responders and %d nonresponders · cell mean ± SEM at each recorded time",
        presentation()$label(field()), sum(x$Status == "Responded"), sum(x$Status == "Did not respond"))
    })
    output$population_plot <- renderPlot({
      g <- field_traces()
      series <- responder_trace_summary(g$time, g$local, g$rows$Status)
      win <- active_windows(g$rows[1, ])
      shades <- rbind(data.frame(Role = "Local baseline", Start = win$Start - g$rows$Baseline_s[1], End = win$Start), win)
      # Break ribbons at missing SEMs, including single-cell populations.
      series$Band <- interaction(series$Status, cumsum(!is.finite(series$SEM)))
      bands <- series[is.finite(series$SEM), , drop = FALSE]
      ggplot(series, aes(Time, Mean, colour = Status)) +
        geom_rect(data = shades, aes(xmin = Start, xmax = End, fill = Role), ymin = -Inf, ymax = Inf,
          inherit.aes = FALSE, alpha = .17) +
        scale_fill_manual("Window", values = c("Local baseline" = "#92A0B5", Stimulus = "#42A995", KCl = "#D7A047")) +
        geom_ribbon(data = bands, aes(ymin = Mean - SEM, ymax = Mean + SEM, group = Band),
          fill = "grey45", colour = NA, alpha = .2, na.rm = TRUE) +
        geom_line(linewidth = .85, na.rm = TRUE) + scale_colour_manual(values = responder_colors) +
        labs(x = "Time (s)", y = "Fractional rise from local baseline") + theme_minimal(base_size = 12)
    })
    output$population_heatmap <- renderPlot({
      g <- field_traces()
      bounds <- time_bin_edges(g$time)
      n <- nrow(g$rows)
      tiles <- data.frame(left = rep(bounds$left, n), right = rep(bounds$right, n),
        cell = rep(seq_len(n), each = length(g$time)), rise = as.vector(g$local))
      ggplot(tiles, aes(xmin = left, xmax = right, ymin = cell - .5, ymax = cell + .5, fill = rise)) +
        geom_rect() + scale_y_reverse() +
        scale_fill_gradient2(low = "#415CBC", mid = "white", high = "#B23C26", name = "Local rise") +
        labs(x = "Time (s)", y = "Cell (status, then onset)") + theme_minimal(base_size = 12)
    })
    units_text <- function(key) {
      if (identical(rv$input_data_mode, "dff0")) return("Uploaded ΔF/F₀ (original F₀ frames unknown).")
      sprintf("ΔF/F₀ uses loading frames %d–%d%s.", rv$baseline_frames[1], rv$baseline_frames[2],
        if (key %in% names(rv$background_traces)) " after per-frame background subtraction" else "")
    }
    output$review_label <- renderText({
      row <- selected_call()
      paste(presentation()$label(row$Field), row$Cell, row$Stimulus, sep = " · ")
    })
    output$trace <- renderPlot({
      row <- selected_call()
      dt <- rv$dts[[row$Field]]
      validate(need(row$Cell %in% names(dt), row$Reason))
      win <- active_windows(row)
      y <- dt[[row$Cell]]
      plot <- trace_plot(dt$Time, y, paste(row$Cell, "·", row$Stimulus, "·", row$Status), win)
      subtitle <- row$Reason
      if (is.finite(row$Threshold_increase)) {
        label <- sprintf("Threshold %.3g (%s)", row$Threshold_increase,
          if (row$Threshold_source == "Floor") "floor" else sprintf("%g SD", row$SD_multiplier))
        refs <- data.frame(label = c("Local baseline", label), y = row$Baseline_mean +
          (1 + row$Baseline_mean) * c(0, row$Threshold_increase))
        refs$start <- c(win$Start - row$Baseline_s, win$Start)
        plot <- plot + geom_segment(data = refs, aes(x = start, xend = win$End, y = y, yend = y, linetype = label),
          inherit.aes = FALSE, colour = "#343434") +
          scale_linetype_manual(NULL, values = stats::setNames(c("dotted", "dashed"), refs$label)) +
          annotate("point", x = row$Peak_s, y = y[match(row$Peak_s, dt$Time)], colour = "#7F4020", size = 2.7) +
          annotate("text", x = row$Peak_s, y = y[match(row$Peak_s, dt$Time)], label = "Peak", vjust = -0.65, size = 3.4)
        if (is.finite(row$Onset_s)) {
          in_run <- dt$Time >= row$Run_start_s & dt$Time <= row$Run_end_s
          run <- data.frame(Time = dt$Time[in_run], Signal = y[in_run])
          if (nrow(run) > 1L) plot <- plot + geom_line(data = run, colour = responder_colors[1], linewidth = 1.7)
          plot <- plot + annotate("point", x = row$Onset_s, y = run$Signal[1], shape = 124,
            colour = "#17493D", size = 6)
        }
        subtitle <- sprintf("Peak %.3g · threshold %.3g · margin %.3g× · %s", row$Peak_increase,
          row$Threshold_increase, row$Margin, if (is.finite(row$Onset_s)) sprintf("onset %g s", row$Onset_s) else "no sustained onset")
      }
      if (!isTRUE(input$full)) {
        margin <- (win$End - win$Start) * .15
        limits <- c(win$Start - row$Baseline_s - margin, win$End + margin)
        if (limits[1] < max(dt$Time) && limits[2] > min(dt$Time)) plot <- plot + coord_cartesian(xlim = limits)
      }
      plot + labs(subtitle = subtitle, caption = paste(units_text(row$Field),
        "\nScoring uses the cell’s local prestimulus baseline. The threshold line is converted to plotted units.")) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0))
    })
    output$methods <- renderText({
      x <- active_counts()
      valid <- x$Evaluable > 0
      role_text <- switch(population(), primary = "all mapped cells for the primary stimulus",
        control = "all mapped cells for KCl", restricted = "primary stimulus calls among KCl responders")
      processing <- unique(vapply(x$Field, units_text, ""))
      details <- vapply(seq_len(nrow(x)), function(i) {
        r <- x[i, ]
        field_note <- sprintf("%s (gap %g s)", presentation()$label(r$Field), r$Max_gap_s)
        notes <- row_notes(r)
        if (nzchar(notes)) paste(field_note, notes, sep = " · ") else field_note
      }, "")
      paste(paste(processing, collapse = " "), "We scored", role_text, "using the following rule.",
        paste0(rule_line(), "."), "For each field, the responding fraction was the number with a sustained response divided by evaluable cells.",
        sprintf("The pooled percentage used summed counts. The mean of field percentages weighted each of the %d evaluable fields equally (%d fields processed).",
          sum(valid), nrow(x)), paste(details, collapse = "; "))
    })
    export_rows <- function(which) {
      rows <- results()[[which]]
      control <- protocol()[protocol()$Role == "Control", , drop = FALSE]
      if (which == "calls") {
        counts <- results()$counts
        i <- match(rows$Field, counts$Field)
        for (column in c("KCl_effective_end_s", "KCl_window_warning", "KCl_field_reason"))
          rows[[column]] <- counts[[column]][i]
      }
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
