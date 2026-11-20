responder_colors <- c("Responded" = "#177465", "Did not respond" = "#925223")

# Result snapshots contain ordinary copies so later data.table edits cannot alter an applied run.
responder_copy <- function(x) unserialize(serialize(x, NULL))

responder_app_version <- function() {
  if (exists("SIMPLECA_VERSION", inherits = TRUE)) return(get("SIMPLECA_VERSION", inherits = TRUE))
  if (file.exists("DESCRIPTION")) return(unname(read.dcf("DESCRIPTION", fields = "Version")[1]))
  "unavailable"
}

responder_object_hash <- function(x) {
  path <- tempfile("responder-state-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 2, compress = FALSE)
  unname(tools::md5sum(path))
}

responder_recording_metadata <- function(snapshot) {
  fields <- names(snapshot$dts)
  meta <- snapshot$group_meta
  if (is.null(meta)) meta <- data.frame(key = fields, file = snapshot$files$name,
    label = fields, condition = "", unit_id = "", stringsAsFactors = FALSE)
  for (name in c("file", "label", "condition", "unit_id")) if (!name %in% names(meta)) meta[[name]] <- ""
  meta <- meta[match(fields, meta$key), , drop = FALSE]
  meta$key <- fields
  meta$condition[is.na(meta$condition)] <- ""
  meta$unit_id[is.na(meta$unit_id)] <- ""
  meta
}

# Pool fields within an explicitly identified experimental unit, then summarize units equally.
# Missing IDs are reported as unassigned fields; a cell or field is never invented as a replicate.
responder_unit_results <- function(counts, metadata) {
  rows <- as.data.frame(counts)
  at <- match(rows$Field, metadata$key)
  rows$Condition <- metadata$condition[at]
  rows$Experimental_unit_ID <- metadata$unit_id[at]
  rows$Condition[is.na(rows$Condition)] <- ""
  rows$Experimental_unit_ID[is.na(rows$Experimental_unit_ID)] <- ""
  assigned <- nzchar(trimws(rows$Experimental_unit_ID))
  units <- rows[assigned, , drop = FALSE] |>
    dplyr::group_by(Condition, Experimental_unit_ID, Role, Stimulus, Population) |>
    dplyr::summarise(Fields = dplyr::n_distinct(Field), Responded = sum(Responded),
      Nonresponders = sum(Nonresponders), Evaluable = sum(Evaluable),
      Excluded = sum(Excluded), Unscorable = sum(Unscorable), .groups = "drop") |>
    dplyr::mutate(Percent_responding = ifelse(Evaluable > 0, 100 * Responded / Evaluable, NA_real_))
  summary <- units |>
    dplyr::group_by(Condition, Role, Stimulus, Population) |>
    dplyr::summarise(Experimental_units = sum(is.finite(Percent_responding)),
      Mean_unit_percent = if (any(is.finite(Percent_responding))) mean(Percent_responding, na.rm = TRUE) else NA_real_,
      SD_unit_percent = stats::sd(Percent_responding, na.rm = TRUE),
      SEM_unit_percent = if (sum(is.finite(Percent_responding)) > 1L)
        stats::sd(Percent_responding, na.rm = TRUE) / sqrt(sum(is.finite(Percent_responding))) else NA_real_,
      .groups = "drop")
  list(units = as.data.frame(units), summary = as.data.frame(summary),
    unassigned_fields = unique(rows$Field[!assigned]))
}

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
      conditionalPanel("output.applied_control === true", ns = ns,
        radioButtons(ns("population"), "Population", c("Stimulus" = "primary", "Control" = "control",
          "Stimulus among control responders" = "restricted"), inline = TRUE)),
      tags$div(id = ns("defaults_notice"), class = "alert alert-info", role = "status", hidden = NA),
      uiOutput(ns("protocol_feedback")), uiOutput(ns("applied_status")),
      tags$button(type = "button", class = "btn btn-default btn-sm", "Edit scoring settings",
        onclick = sprintf("var e=document.getElementById('%s'); e.open=true; e.scrollIntoView(); e.querySelector('summary').focus();", settings_id)))),
    conditionalPanel("output.protocol_ok === true", ns = ns,
      fluidRow(theme_box("Response distribution",
        radioButtons(ns("distribution_scale"), NULL, c("Peak / threshold" = "margin", "Peak local rise" = "rise"),
          selected = "margin", inline = TRUE),
        textOutput(ns("distribution_note")), uiOutput(ns("distribution_view")))),
      fluidRow(theme_box("Population traces",
        fluidRow(column(6, selectInput(ns("trace_recording"), "Show recordings", c("All recordings" = "__all__"))),
          column(6, checkboxInput(ns("trace_full"), "Full recording", FALSE))),
        radioButtons(ns("trace_mode"), "Trace view", c("Individual traces" = "individual", "Mean ± SEM" = "mean", "Both" = "both"), inline = TRUE),
        checkboxGroupInput(ns("trace_status"), "Show response classes",
          choices = c("Responded", "Did not respond"), selected = c("Responded", "Did not respond"), inline = TRUE),
        textOutput(ns("population_label")), uiOutput(ns("population_plot")),
        p(class = "text-muted", "Each panel is one uploaded recording. Teal solid lines are responders; orange dashed lines are nonresponders. Means and SEM are calculated within each recording and response class. Gray shading marks the local baseline; pale blue marks scoring. Hover for recording and cell details; click a trace to select it below."),
        p(class = "text-muted", "Traces use each cell’s local prestimulus signal. Cell review uses the normalized input scale (ΔF/F₀ or ΔR/R₀). Visibility controls do not change counts or exports."),
        tags$details(tags$summary("Selected recording heatmap by response and onset"), textOutput(ns("heatmap_label")),
          plotOutput(ns("population_heatmap"), height = "360px"))))
    ),
    fluidRow(theme_box("Results by field", uiOutput(ns("cards")),
      tags$details(tags$summary("Cell count definitions"),
        p("Evaluable includes responders and nonresponders. Excluded cells failed loading normalization. Unscorable cells lack the observations or local baseline needed for a decision.")),
      conditionalPanel("output.protocol_ok === true", ns = ns,
        browser_download_button(ns("download_counts"), "Download field counts"),
        browser_download_button(ns("download_calls"), "Download cell calls"),
        browser_download_button(ns("download_bundle"), "Download complete analysis (ZIP)"))),
      uiOutput(ns("unit_summary")),
      conditionalPanel("output.protocol_ok === true", ns = ns,
        tags$details(tags$summary("Stimulus-window measurements"),
          p("Local peak and signed AUC use the applied response window. Latency is measured from the recorded stimulus onset. Missing measurements remain missing."),
          DT::DTOutput(ns("stimulus_metrics"))))),
    fluidRow(theme_box("Recording and scoring settings",
      tags$details(id = settings_id, tags$summary(tags$strong("Scoring settings"), " · ",
          textOutput(ns("rule_summary"), inline = TRUE)),
        uiOutput(ns("data_info")), plotOutput(ns("overview"), height = "235px"),
        fluidRow(column(6, textInput(ns("protocol_name"), "Protocol name", "Current SimpleCa sample rule")),
          column(6, selectInput(ns("validation_status"), "Protocol validation status",
            c("Provisional — validate for this acquisition" = "provisional", "User defined" = "user_defined",
              "Validated by the user" = "validated_by_user")))),
        fluidRow(column(8, selectInput(ns("preset"), "Starting preset", c(
          "Current SimpleCa: 10%, 3 SD, 2 samples" = "legacy",
          "Slow evoked candidate: 10%, 3 SD, 1 second (provisional)" = "slow"))),
          column(4, actionButton(ns("load_preset"), "Load selected preset"))),
        fileInput(ns("import_protocol"), "Import response protocol (JSON)", accept = c(".json", "application/json")),
        selectInput(ns("contract_version"), "Scoring contract", c(
          "Legacy: unchanged sample rule and bounded endpoint clipping" = "legacy",
          "Version 2: explicit timing and complete response windows" = "explicit"), "legacy"),
        p("The legacy contract reproduces earlier SimpleCa decisions. Select Version 2 for elapsed time, independent baseline/onset, timing overrides, or a generic control. Version 2 requires complete scoring windows and does not clip them silently."),
        p("Presets are starting rules, not universal biological cutoffs. Use controls to choose the rule before comparing experimental groups. Applying a protocol records its exact settings."),
        p("Suggested times come from the recording. Confirm stimulus onset, the independent baseline end, and the scoring interval against your acquisition protocol."),
        fluidRow(column(4, textInput(ns("stimulus"), "Stimulus name", "", placeholder = "e.g. Capsaicin")),
          column(4, conditionalPanel("input.contract_version === 'explicit'", ns = ns, scoring_number("onset", "Stimulus onset (s)"))),
          column(4, conditionalPanel("input.contract_version === 'explicit'", ns = ns, scoring_number("baseline_end", "Local baseline ends (s)")))),
        fluidRow(column(4, scoring_number("baseline", "Baseline duration (s)")),
          column(4, scoring_number("start", "Scoring window start (s)")),
          column(4, scoring_number("end", "Scoring window end (s)"))),
        actionButton(ns("reset_windows"), "Use recording defaults"),
        fluidRow(column(4, numericInput(ns("floor"), "Minimum local fractional rise", .10, min = 0, step = .01)),
          column(4, numericInput(ns("noise"), "Baseline SD multiplier", 3, min = 0, step = .1)),
          column(4, selectInput(ns("duration_mode"), "Persistence rule",
            c("Consecutive acquired samples (existing rule)" = "samples", "Elapsed sampled duration" = "seconds"), "samples"))),
        fluidRow(column(4, numericInput(ns("frames"), "Minimum consecutive samples", 2, min = 1, step = 1)),
          column(4, conditionalPanel("input.duration_mode === 'seconds'", ns = ns,
            numericInput(ns("duration_s"), "Minimum sampled duration (s)", 1, min = 0, step = .1))),
          column(4, checkboxInput(ns("auto_gap"), "Automatic gap limit per field", TRUE),
            conditionalPanel("!input.auto_gap", ns = ns,
              numericInput(ns("gap"), "Maximum sample gap (s)", 1, min = 0, step = .1)))),
        tags$details(tags$summary("Baseline requirements"),
          numericInput(ns("min_baseline_samples"), "Minimum baseline samples", 3, min = 3, step = 1),
          numericInput(ns("noise_floor"), "Minimum local noise SD (fraction)", 0, min = 0, step = .001),
          numericInput(ns("expected_interval_s"), "Expected acquisition interval (s; blank uses observed times)", NA_real_, min = 0, step = .1),
          p("Three samples is the structural minimum, not assurance of a reliable noise estimate. A zero noise floor retains the existing mean/SD rule.")),
        checkboxInput(ns("control"), "Include a positive control", FALSE),
        conditionalPanel("input.control", ns = ns,
          fluidRow(column(4, textInput(ns("control_name"), "Control name", "KCl")),
            column(4, conditionalPanel("input.contract_version === 'explicit'", ns = ns, numericInput(ns("control_onset"), "Control onset (s; blank uses scoring start)", NA_real_))),
            column(4, conditionalPanel("input.contract_version === 'explicit'", ns = ns, numericInput(ns("control_baseline_end"), "Control baseline ends (s; blank uses onset)", NA_real_)))),
          fluidRow(column(4, conditionalPanel("input.contract_version === 'explicit'", ns = ns, numericInput(ns("control_baseline"), "Control baseline duration (s; blank uses primary)", NA_real_))),
            column(4, numericInput(ns("control_start"), "Control scoring start (s)", NA_real_)),
            column(4, numericInput(ns("control_end"), "Control scoring end (s)", NA_real_))),
          p("Control response defines a separately reported population; it is not automatically a universal viability requirement.")),
        tags$details(tags$summary("Per-recording timing overrides"),
          p("Save an explicit override when stimulus delivery differs between recordings. Overrides do not change the response rule. Recording keys must match the loaded data."),
          fluidRow(column(6, selectInput(ns("override_field"), "Recording", character())),
            column(6, selectInput(ns("override_role"), "Window", c("Stimulus" = "Stimulus")))),
          actionButton(ns("load_override_defaults"), "Fill editor from current draft window"),
          fluidRow(column(4, numericInput(ns("override_baseline_start"), "Baseline start (s)", NA_real_)),
            column(4, numericInput(ns("override_baseline_end"), "Baseline end (s)", NA_real_)),
            column(4, numericInput(ns("override_onset"), "Stimulus onset (s)", NA_real_))),
          fluidRow(column(6, numericInput(ns("override_start"), "Scoring start (s)", NA_real_)),
            column(6, numericInput(ns("override_end"), "Scoring end (s)", NA_real_))),
          actionButton(ns("save_override"), "Save timing override"),
          actionButton(ns("remove_override"), "Remove selected override"),
          actionButton(ns("clear_overrides"), "Clear all timing overrides"),
          tableOutput(ns("timing_overrides"))),
        p("A response meets the larger of the amplitude floor and the SD-based threshold. Duration is the time from the first to last qualifying acquired sample; missing values and excessive gaps cannot bridge a response. A one-second rule can reject brief real transients. The automatic gap limit is 1.5 times each field’s median frame interval."),
        uiOutput(ns("settings_feedback")),
        actionButton(ns("apply_protocol"), "Apply analysis settings", class = "btn-primary"),
        conditionalPanel("output.protocol_ok === true", ns = ns,
          browser_download_button(ns("download_protocol"), "Export applied protocol (JSON)")),
        p(class = "text-muted", "Edits remain a draft until Apply. Displayed calls, plots, methods, and downloads always use the same applied run.")))),
    conditionalPanel("output.protocol_ok === true", ns = ns,
      fluidRow(theme_box("Cell review",
        selectInput(ns("review_recording"), "Recording to review", character()),
        selectInput(ns("status"), "Filter cells", c("All", "Near threshold", "Flagged for review", "Responded", "Did not respond", "Excluded", "Unscorable")),
        DT::DTOutput(ns("calls")),
        actionButton(ns("previous"), "Previous cell"), actionButton(ns("next_cell"), "Next cell"),
        checkboxInput(ns("full"), "Show full recording", FALSE),
        tags$p(`aria-live` = "polite", textOutput(ns("review_label"), inline = TRUE)),
        uiOutput(ns("cell_diagnostics")),
        plotOutput(ns("trace"), height = "365px"))),
      fluidRow(theme_box("Methods", tags$details(tags$summary("Methods for this analysis"),
        textOutput(ns("methods")), tags$button(type = "button", class = "btn btn-default", "Copy methods",
          onclick = sprintf("var e=document.getElementById('%s'); if(navigator.clipboard) navigator.clipboard.writeText(e.textContent);", ns("methods"))))))
    ),
    tags$style(HTML(".responder-panels {display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,300px),1fr));gap:14px;margin:16px 0}.responder-panel {border:1px solid #dce3e8;border-radius:6px;min-width:0;overflow:hidden}.responder-panel-heading {padding:12px;border-bottom:1px solid #edf0f4}.responder-panel-heading strong {font-size:16px}.responder-panel-heading .btn {float:right}.responder-file {overflow-wrap:anywhere;color:#65717d;font-size:12px}.responder-panel-note {padding:8px 12px;margin:0}.responder-panel .shiny-html-output {min-width:0}")),
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
        var contract=document.getElementById(m.prefix+'contract_version');
        var legacy=!contract || contract.value==='legacy';
        var required=legacy?['start','end','baseline']:keys;
        var complete=function(v){return required.every(function(k){return finite(v[k]);});};
        var bounds=m.bounds;
        var fits=function(v) {
          if (!bounds || !complete(v)) return false;
          var baselineEnd=legacy?v.start:v.baseline_end, onset=legacy?v.start:v.onset;
          var endLimit=legacy?bounds.end_with_gap:bounds.end;
          var tol=8*Number.EPSILON*Math.max(1,Math.abs(baselineEnd-v.baseline),
            Math.abs(v.start),Math.abs(v.end),Math.abs(bounds.start),Math.abs(bounds.end));
          return v.baseline>0 && v.start<v.end && v.start<bounds.end &&
            baselineEnd-v.baseline>=bounds.start-tol && baselineEnd<=onset && onset<=v.start && v.end<=endLimit+tol;
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
      if (length(values)) {
        values <- c(values, onset = unname(values["start"]), baseline_end = unname(values["start"]))
        session$sendCustomMessage(session$ns("defaults"), list(values = as.list(values),
          bounds = as.list(responder_bounds(rv$dts, if (isFALSE(input$auto_gap)) input$gap else NULL)),
          reset = reset, prefix = session$ns("")))
      }
    }
    observeEvent(list(rv$dts, rv$baseline_frames), push_defaults())
    observeEvent(input$reset_windows, push_defaults(TRUE))
    timing_overrides <- reactiveVal(data.frame())
    imported_protocol <- reactiveVal(NULL)
    imported_controls <- reactiveVal(NULL)
    analysis_message <- reactiveVal(NULL)
    applied_run <- reactiveVal(NULL)
    run_sequence <- 0L
    finite_or <- function(value, fallback) {
      if (length(value) == 1L && is.finite(value)) value else fallback
    }
    form_values <- reactive({
      control <- isTRUE(input$control)
      contract <- input$contract_version %||% "legacy"
      start <- input$start
      onset <- if (contract == "legacy") start else input$onset %||% start
      baseline_end <- if (contract == "legacy") start else input$baseline_end %||% onset
      cstart <- input$control_start
      conset <- if (contract == "legacy") cstart else finite_or(input$control_onset, cstart)
      list(contract_version = contract,
        name = input$protocol_name %||% "Current SimpleCa sample rule",
        stimulus = input$stimulus, start = start, end = input$end,
        baseline = input$baseline, onset = onset, baseline_end = baseline_end,
        floor = input$floor, noise = input$noise, frames = input$frames,
        duration_mode = input$duration_mode %||% "samples", duration_s = input$duration_s %||% 1,
        gap = if (isTRUE(input$auto_gap)) NULL else input$gap,
        min_baseline_samples = input$min_baseline_samples %||% 3L,
        noise_floor = input$noise_floor %||% 0,
        expected_interval_s = if (length(input$expected_interval_s) == 1L && is.finite(input$expected_interval_s)) input$expected_interval_s else NULL,
        validation_status = input$validation_status %||% "provisional",
        control = control, control_name = if (control) input$control_name %||% "KCl" else NULL,
        control_start = if (control) cstart else NULL,
        control_end = if (control) input$control_end else NULL,
        control_onset = if (control) conset else NULL,
        control_baseline_end = if (control) { if (contract == "legacy") cstart else finite_or(input$control_baseline_end, conset) } else NULL,
        control_baseline = if (control) { if (contract == "legacy") input$baseline else finite_or(input$control_baseline, input$baseline) } else NULL)
    })
    draft_protocol <- reactive({
      v <- form_values()
      if (length(v$stimulus) != 1L || is.na(v$stimulus) || !nzchar(trimws(v$stimulus))) stop("Enter a stimulus name.")
      if (length(v$start) != 1L || length(v$end) != 1L || !is.finite(v$start) || !is.finite(v$end))
        stop("Enter the scoring window start and end in seconds.")
      imported <- imported_protocol()
      if (!is.null(imported) && isTRUE(all.equal(v, imported_controls(), check.attributes = FALSE))) return(imported)
      if (!is.null(imported) && nrow(imported) > 1L) {
        per_window <- intersect(c("Floor_local_fraction", "SD_multiplier", "Duration_mode", "Min_duration_s",
          "Consecutive_samples", "Min_baseline_samples", "Noise_floor_local_fraction", "Expected_interval_s", "Max_gap_s"), names(imported))
        different <- per_window[vapply(imported[per_window], function(x) length(unique(x)) > 1L, logical(1))]
        if (length(different)) stop(paste0("This imported protocol has different per-window rules (",
          paste(different, collapse = ", "), "). Apply it unchanged, or load a preset to intentionally start a unified rule before editing. No hidden control criteria have been changed."))
      }
      if (v$contract_version == "legacy") {
        same <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b)))
        if (v$duration_mode != "samples" || !same(v$onset, v$start) || !same(v$baseline_end, v$start) ||
            v$min_baseline_samples != 3 || v$noise_floor != 0 || !is.null(v$expected_interval_s) ||
            (v$control && (tolower(trimws(v$control_name)) != "kcl" || !same(v$control_onset, v$control_start) ||
             !same(v$control_baseline_end, v$control_start) || !same(v$control_baseline, v$baseline))))
          stop("These settings require the Version 2 scoring contract. Legacy mode keeps the baseline immediately before scoring, uses samples, and supports the original KCl control.")
        p <- responder_protocol(v$stimulus, v$start, v$end, v$baseline, v$floor, v$noise, v$frames,
          v$gap, v$control, v$control_start, v$control_end)
        p$Protocol_version <- 1L
        p$Protocol_name <- v$name
        return(p)
      }
      windows <- data.frame(Role = "Stimulus", Stimulus = v$stimulus,
        Baseline_start_s = v$baseline_end - v$baseline, Baseline_end_s = v$baseline_end,
        Stimulus_onset_s = v$onset, Start_s = v$start, End_s = v$end)
      if (v$control) windows <- rbind(windows, data.frame(Role = "Control", Stimulus = v$control_name,
        Baseline_start_s = v$control_baseline_end - v$control_baseline,
        Baseline_end_s = v$control_baseline_end, Stimulus_onset_s = v$control_onset,
        Start_s = v$control_start, End_s = v$control_end))
      responder_protocol_v2(v$name, windows, floor = v$floor, noise = v$noise,
        duration_mode = v$duration_mode, duration_s = v$duration_s, frames = v$frames,
        gap = v$gap, min_baseline_samples = v$min_baseline_samples, noise_floor = v$noise_floor,
        validation_status = v$validation_status, expected_interval_s = v$expected_interval_s)
    })
    protocol_error <- reactive({
      tryCatch({
        p <- draft_protocol()
        overrides <- timing_overrides()
        if (nrow(overrides) && any(!overrides$Field %in% fields()))
          stop("Timing overrides reference recordings that are not loaded. Remove or remap those overrides before applying.")
        for (key in fields()) responder_protocol_for_field(p, key, overrides)
        NULL
      }, error = function(e) conditionMessage(e))
    })
    # Only scientific inputs invalidate a run. Presentation metadata stays freely editable.
    data_fingerprint <- reactive({
      meta <- rv$group_meta
      scientific_meta <- if (!is.null(meta)) meta[order(meta$key), intersect(c("key", "condition", "unit_id"), names(meta)), drop = FALSE] else NULL
      responder_object_hash(list(dts = rv$dts, baselines = rv$baselines, files = rv$files,
        baseline_frames = rv$baseline_frames, input_data_mode = rv$input_data_mode,
        signal_kind = rv$signal_kind, normalized_units = rv$normalized_units,
        normalization_reference = rv$normalization_reference,
        background = rv$background_traces, quality = rv$cell_quality, metadata = scientific_meta))
    })
    observeEvent(data_fingerprint(), {
      run <- applied_run()
      if (!is.null(run) && !identical(run$data_fingerprint, data_fingerprint())) {
        applied_run(NULL)
        analysis_message("The data, preprocessing, or experimental-unit metadata changed. Apply the analysis settings again to create a new run.")
      }
    }, ignoreInit = TRUE)
    applied_ready <- reactive(!is.null(applied_run()) && identical(applied_run()$data_fingerprint, data_fingerprint()))
    protocol <- reactive({ req(applied_ready()); applied_run()$protocol })
    results <- reactive({ req(applied_ready()); applied_run()$results })
    result_data <- reactive({ req(applied_ready()); applied_run()$snapshot$dts })
    dirty <- reactive({
      if (!applied_ready()) return(FALSE)
      if (!is.null(protocol_error())) return(TRUE)
      !identical(responder_protocol_hash(draft_protocol(), timing_overrides()), applied_run()$protocol_hash)
    })
    observeEvent(input$apply_protocol, {
      tryCatch({
        if (!length(fields())) stop("Process at least one recording before applying a response protocol.")
        err <- protocol_error()
        if (!is.null(err)) stop(err)
        p <- responder_copy(draft_protocol())
        overrides <- responder_copy(timing_overrides())
        snapshot <- responder_copy(list(dts = rv$dts, baselines = rv$baselines,
          files = rv$files, baseline_frames = rv$baseline_frames, input_data_mode = rv$input_data_mode,
          signal_kind = rv$signal_kind %||% "fluorescence", signal_label = rv$signal_label %||% "ΔF/F₀",
          normalized_units = rv$normalized_units %||% "fraction",
          normalization_reference = rv$normalization_reference %||% "fixed",
          background_traces = rv$background_traces, group_meta = rv$group_meta,
          processing_manifest = rv$processing_manifest, cell_quality = rv$cell_quality))
        by_field <- lapply(names(snapshot$dts), function(key) {
          cells <- names(snapshot$baselines[[key]])
          if (!length(cells) && !is.null(snapshot$cell_quality))
            cells <- snapshot$cell_quality$Cell[snapshot$cell_quality$Field == key]
          result <- responder_field_results(snapshot$dts[[key]], cells, key, p,
            cell_quality = snapshot$cell_quality, overrides = overrides)
          if (responder_protocol_version(p) == 1L) result$calls <- responder_legacy_annotations(
            snapshot$dts[[key]], cells, key, p, cell_quality = snapshot$cell_quality, legacy_result = result)
          result
        })
        computed <- list(calls = do.call(rbind, lapply(by_field, "[[", "calls")),
          counts = do.call(rbind, lapply(by_field, "[[", "counts")))
        hash <- responder_protocol_hash(p, overrides)
        run_sequence <<- run_sequence + 1L
        run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"), "-", substr(hash, 1, 8), "-", substr(data_fingerprint(), 1, 8), "-", run_sequence)
        applied_run(list(id = run_id, created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          protocol = p, overrides = overrides, protocol_hash = hash,
          data_fingerprint = data_fingerprint(), snapshot = snapshot, results = responder_copy(computed),
          units = responder_unit_results(computed$counts, responder_recording_metadata(snapshot))))
        analysis_message(NULL)
      }, error = function(e) analysis_message(conditionMessage(e)))
    }, ignoreInit = TRUE)
    output$protocol_ok <- reactive(applied_ready())
    output$applied_control <- reactive(applied_ready() && any(protocol()$Role == "Control"))
    outputOptions(output, "protocol_ok", suspendWhenHidden = FALSE)
    outputOptions(output, "applied_control", suspendWhenHidden = FALSE)
    output$protocol_feedback <- renderUI({
      message <- protocol_error()
      if (!is.null(message)) div(class = "alert alert-warning", role = "status", paste("Draft:", message))
    })
    output$settings_feedback <- renderUI({
      msg <- analysis_message() %||% protocol_error()
      if (!is.null(msg)) div(class = "alert alert-warning", role = "status", msg)
    })
    output$applied_status <- renderUI({
      if (!applied_ready()) return(div(class = "alert alert-info", role = "status",
        analysis_message() %||% "Choose the scoring settings, then Apply to calculate responses."))
      run <- applied_run()
      div(class = if (dirty()) "alert alert-warning" else "alert alert-success", role = "status",
        if (dirty()) tags$strong("Unapplied changes. ") else tags$strong("Applied analysis. "),
        sprintf("Displayed results and downloads use %s · run %s.", run$protocol$Protocol_name[1] %||% "response protocol", run$id))
    })
    observeEvent(input$load_preset, {
      slow <- identical(input$preset, "slow")
      imported_protocol(NULL); imported_controls(NULL)
      updateTextInput(session, "protocol_name", value = if (slow) "Slow evoked response candidate" else "Current SimpleCa sample rule")
      updateNumericInput(session, "floor", value = .10)
      updateNumericInput(session, "noise", value = 3)
      updateNumericInput(session, "frames", value = 2)
      updateNumericInput(session, "duration_s", value = 1)
      updateNumericInput(session, "min_baseline_samples", value = 3)
      updateNumericInput(session, "noise_floor", value = 0)
      updateSelectInput(session, "duration_mode", selected = if (slow) "seconds" else "samples")
      updateSelectInput(session, "contract_version", selected = if (slow) "explicit" else "legacy")
      updateNumericInput(session, "expected_interval_s", value = NA_real_)
      updateSelectInput(session, "validation_status", selected = "provisional")
      analysis_message("Preset loaded into the draft. Confirm the windows and apply when ready.")
    })
    # The imported original is retained when its displayed controls are unchanged,
    # including version-1 sample-mode protocols. Editing creates a version-2 protocol.
    observeEvent(input$import_protocol, {
      tryCatch({
        p <- responder_import_protocol(input$import_protocol$datapath)
        timing_overrides(attr(p, "field_overrides") %||% data.frame())
        get <- function(row, key, fallback) if (key %in% names(row)) row[[key]][1] else fallback
        primary <- p[p$Role == "Stimulus", , drop = FALSE]
        control <- p[p$Role == "Control", , drop = FALSE]
        onset <- get(primary, "Stimulus_onset_s", primary$Start_s)
        bend <- get(primary, "Baseline_end_s", primary$Start_s)
        bstart <- get(primary, "Baseline_start_s", bend - primary$Baseline_s)
        vals <- list(contract_version = if (responder_protocol_version(p) == 1L) "legacy" else "explicit",
          name = get(primary, "Protocol_name", "Imported response protocol"),
          stimulus = primary$Stimulus, start = primary$Start_s, end = primary$End_s,
          baseline = bend - bstart, onset = onset, baseline_end = bend,
          floor = primary$Floor_local_fraction, noise = primary$SD_multiplier,
          frames = primary$Consecutive_samples, duration_mode = get(primary, "Duration_mode", "samples"),
          duration_s = get(primary, "Min_duration_s", 1), gap = if (is.na(primary$Max_gap_s)) NULL else primary$Max_gap_s,
          min_baseline_samples = get(primary, "Min_baseline_samples", 3L), noise_floor = get(primary, "Noise_floor_local_fraction", 0),
          expected_interval_s = if (is.finite(get(primary, "Expected_interval_s", NA_real_))) primary$Expected_interval_s else NULL,
          validation_status = get(primary, "Validation_status", "provisional"), control = nrow(control) > 0,
          control_name = NULL, control_start = NULL, control_end = NULL, control_onset = NULL,
          control_baseline_end = NULL, control_baseline = NULL)
        if (!is.finite(vals$duration_s)) vals$duration_s <- 1
        if (nrow(control)) {
          vals$control_name <- control$Stimulus
          vals$control_start <- control$Start_s; vals$control_end <- control$End_s
          vals$control_onset <- get(control, "Stimulus_onset_s", control$Start_s)
          vals$control_baseline_end <- get(control, "Baseline_end_s", control$Start_s)
          vals$control_baseline <- vals$control_baseline_end - get(control, "Baseline_start_s", control$Start_s - control$Baseline_s)
        }
        imported_protocol(p); imported_controls(vals)
        for (key in c("stimulus", "control_name")) updateTextInput(session, key, value = vals[[key]] %||% "KCl")
        updateTextInput(session, "protocol_name", value = vals$name)
        for (key in c("start", "end", "baseline", "onset", "baseline_end", "floor", "noise", "frames", "duration_s", "min_baseline_samples", "noise_floor",
                      "control_start", "control_end", "control_onset", "control_baseline_end", "control_baseline", "expected_interval_s"))
          updateNumericInput(session, key, value = vals[[key]] %||% NA_real_)
        updateSelectInput(session, "duration_mode", selected = vals$duration_mode)
        updateSelectInput(session, "contract_version", selected = vals$contract_version)
        updateSelectInput(session, "validation_status", selected = vals$validation_status)
        updateCheckboxInput(session, "auto_gap", value = is.null(vals$gap))
        if (!is.null(vals$gap)) updateNumericInput(session, "gap", value = vals$gap)
        updateCheckboxInput(session, "control", value = vals$control)
        analysis_message("Protocol imported into the draft. Check recording keys and timing, then Apply.")
      }, error = function(e) analysis_message(paste("Protocol import:", conditionMessage(e))))
    })
    observe({
      keys <- union(fields() %||% character(), timing_overrides()$Field %||% character())
      labels <- presentation()$label(keys)
      labels[is.na(labels)] <- paste("Not loaded:", keys[is.na(labels)])
      choices <- stats::setNames(keys, labels)
      current <- isolate(input$override_field)
      updateSelectInput(session, "override_field", choices = choices,
        selected = if (length(current) && current %in% keys) current else keys[1])
      roles <- if (isTRUE(input$control)) c("Stimulus", "Control") else "Stimulus"
      current_role <- isolate(input$override_role)
      updateSelectInput(session, "override_role", choices = roles,
        selected = if (length(current_role) && current_role %in% roles) current_role else "Stimulus")
    })
    observeEvent(list(input$override_field, input$override_role, input$import_protocol, input$load_override_defaults), {
      p <- tryCatch(draft_protocol(), error = function(e) NULL)
      if (is.null(p) || !length(input$override_field)) return()
      row <- tryCatch(responder_protocol_for_field(p, input$override_field, timing_overrides()), error = function(e) NULL)
      if (is.null(row)) return()
      row <- row[row$Role == (input$override_role %||% "Stimulus"), , drop = FALSE]
      if (!nrow(row)) return()
      if (!"Baseline_start_s" %in% names(row)) row$Baseline_start_s <- row$Start_s - row$Baseline_s
      if (!"Baseline_end_s" %in% names(row)) row$Baseline_end_s <- row$Start_s
      if (!"Stimulus_onset_s" %in% names(row)) row$Stimulus_onset_s <- row$Start_s
      mapping <- c(baseline_start = "Baseline_start_s", baseline_end = "Baseline_end_s", onset = "Stimulus_onset_s", start = "Start_s", end = "End_s")
      for (key in names(mapping)) updateNumericInput(session, paste0("override_", key), value = row[[mapping[[key]]]])
    }, ignoreNULL = FALSE)
    observeEvent(input$save_override, {
      tryCatch({
        p <- draft_protocol()
        if (!length(input$override_field) || !input$override_field %in% fields()) stop("Choose a loaded recording before saving an override.")
        row <- data.frame(Field = input$override_field, Role = input$override_role,
          Baseline_start_s = input$override_baseline_start, Baseline_end_s = input$override_baseline_end,
          Stimulus_onset_s = input$override_onset, Start_s = input$override_start, End_s = input$override_end)
        old <- timing_overrides()
        if (nrow(old)) old <- old[!(old$Field == row$Field & old$Role == row$Role), , drop = FALSE]
        next_overrides <- if (nrow(old)) dplyr::bind_rows(old, row) else row
        responder_protocol_for_field(p, row$Field, next_overrides)
        timing_overrides(as.data.frame(next_overrides))
        analysis_message("Timing override saved to the draft. Apply to use it.")
      }, error = function(e) analysis_message(conditionMessage(e)))
    })
    observeEvent(input$remove_override, {
      old <- timing_overrides()
      if (nrow(old) && length(input$override_field) == 1L && length(input$override_role) == 1L)
        timing_overrides(old[!(old$Field == input$override_field & old$Role == input$override_role), , drop = FALSE])
    })
    observeEvent(input$clear_overrides, timing_overrides(data.frame()))
    output$timing_overrides <- renderTable({
      if (nrow(timing_overrides())) timing_overrides() else data.frame(Note = "No recording-specific overrides")
    }, digits = 3)
    control_name <- reactive({
      if (!applied_ready()) return("Control")
      p <- protocol(); x <- p$Stimulus[p$Role == "Control"]
      if (length(x)) x[1] else "Control"
    })
    observe({
      req(applied_ready())
      current <- isolate(input$population) %||% "primary"
      updateRadioButtons(session, "population", choices = stats::setNames(c("primary", "control", "restricted"),
        c("Stimulus", control_name(), paste("Stimulus among", control_name(), "responders"))), selected = current)
    })
    population <- reactive(if (applied_ready() && any(protocol()$Role == "Control")) input$population %||% "primary" else "primary")
    role <- reactive(if (population() == "control") "Control" else "Stimulus")
    active_counts <- reactive({
      x <- results()$counts
      subset_name <- if (population() == "restricted") paste0(control_name(), "-positive cells") else "Cells in field"
      x[x$Role == role() & x$Population == subset_name, , drop = FALSE]
    })
    active_calls <- reactive({
      all <- results()$calls
      x <- all[all$Role == role(), , drop = FALSE]
      if (population() == "restricted") {
        eligible <- all[all$Role == "Control" & all$Status == "Responded", c("Field", "Cell")]
        x <- dplyr::semi_join(x, eligible, by = c("Field", "Cell"))
      }
      x$Reason <- signal_display_text(x$Reason, applied_run()$snapshot$signal_kind)
      # URI-encoded components keep identifiers unique and safe in the browser table.
      encode <- function(s) vapply(s, utils::URLencode, "", reserved = TRUE, repeated = TRUE, USE.NAMES = FALSE)
      x$Key <- if (nrow(x)) paste(encode(x$Field), encode(x$Cell), x$Role, sep = ":") else character()
      x
    })
    rule_line <- reactive({
      if (!applied_ready()) return("Choose the stimulus and scoring window, then Apply")
      p <- protocol(); p <- p[p$Role == role(), , drop = FALSE]
      mode <- p$Duration_mode %||% "samples"
      persistence <- if (mode[1] == "seconds") sprintf("%g s sampled span and at least %d samples", p$Min_duration_s, p$Consecutive_samples)
        else sprintf("%d consecutive sample%s", p$Consecutive_samples, if (p$Consecutive_samples == 1) "" else "s")
      sprintf("%s · base window %g–%g s · %g s local baseline · larger of %g%% or %g SD · %s · %s%s",
        p$Stimulus, p$Start_s, p$End_s, p$Baseline_s, 100 * p$Floor_local_fraction, p$SD_multiplier,
        persistence, if (is.na(p$Max_gap_s)) "automatic gap" else sprintf("%g s gap limit", p$Max_gap_s),
        if (nrow(applied_run()$overrides)) " · explicit recording overrides applied" else "")
    })
    output$rule_summary <- renderText(rule_line())
    row_notes <- function(x) {
      notes <- c(x$Window_warning, x$Field_reason)
      if (grepl("-positive cells$", x$Population)) {
        warning <- x$Control_window_warning %||% x$KCl_window_warning %||% ""
        if (nzchar(warning)) notes <- c(notes, paste(control_name(), warning))
      }
      if (!x$Evaluable && !nzchar(x$Field_reason)) notes <- c(notes, "No evaluable cells")
      paste(unique(notes[nzchar(notes)]), collapse = " · ")
    }
    output$headline <- renderUI({
      req(applied_ready())
      x <- active_counts()
      s <- responder_summary(x)
      n <- s$totals
      notes <- vapply(seq_len(nrow(x)), function(i) row_notes(x[i, ]), "")
      tags$div(
        tags$strong(style = "font-size:2.3rem", if (is.finite(s$pooled)) sprintf("%.1f%% responding", s$pooled) else "No evaluable cells"),
        p(sprintf("%d of %d evaluable cells%s%s", n["Responded"], n["Evaluable"],
          if (nrow(x) > 1L) " · pooled across fields" else "",
          if (population() == "restricted") paste0(" · among ", control_name(), " responders") else "")),
        p(sprintf("%d responded · %d did not respond · %d excluded · %d unscorable",
          n["Responded"], n["Nonresponders"], n["Excluded"], n["Unscorable"])),
        if (nrow(x) > 1L && s$n) p(sprintf("Mean of field percentages %.1f%%%s · %d of %d fields evaluable",
          s$mean, if (s$n > 1) sprintf(" ± %.1f percentage points (SD)", s$sd) else "", s$n, nrow(x))),
        p(rule_line()),
        if (any(nzchar(notes))) tags$ul(class = "alert alert-warning", lapply(which(nzchar(notes)), function(i)
          tags$li(presentation()$label(x$Field[i]), " · ", notes[i]))))
    })
    output$cards <- renderUI({
      ok <- applied_ready()
      counts <- if (ok) active_counts()
      view <- presentation()
      rows <- lapply(intersect(view$levels, fields()), function(name) {
        cells <- if (ok) {
          x <- counts[counts$Field == name, , drop = FALSE]
          lapply(c(x$Responded, x$Nonresponders, x$Evaluable, x$Excluded, x$Unscorable,
            if (x$Evaluable) sprintf("%.1f%%", x$Percent_responding) else "—", row_notes(x)), tags$td)
        } else tags$td("Apply scoring settings")
        tags$tr(class = if (identical(name, field())) "info", `data-field` = name, `data-target` = session$ns("field"),
          onclick = "Shiny.setInputValue(this.dataset.target,this.dataset.field,{priority:'event'})",
          tags$td(tags$button(type = "button", class = "btn btn-link", view$label(name))), cells)
      })
      headings <- if (ok) c("Responded", "Did not respond", "Evaluable", "Excluded", "Unscorable", "% responding", "Notes") else "Status"
      div(class = "table-responsive", tags$table(class = "table table-hover",
        tags$thead(tags$tr(lapply(c("Field of view", headings), tags$th))), tags$tbody(rows)))
    })
    # The overview is explicitly a draft preview; result traces use applied windows below.
    preview_windows <- reactive({
      interval <- function(role, start, end, baseline_end, baseline, onset) {
        if (length(start) && length(end) && is.finite(start) && is.finite(end) && end > start)
          data.frame(Role = role, Start = start, End = end,
            Baseline_start = finite_or(baseline_end, start) - finite_or(baseline, 0),
            Baseline_end = finite_or(baseline_end, start), Stimulus_onset = finite_or(onset, start))
      }
      v <- form_values()
      rbind(interval("Stimulus", v$start, v$end, v$baseline_end, v$baseline, v$onset),
        if (v$control) interval("Control", v$control_start, v$control_end,
          v$control_baseline_end, v$control_baseline, v$control_onset))
    })
    trace_plot <- function(time, signal, title, windows = preview_windows(), signal_label = rv$signal_label %||% "ΔF/F₀") {
      plot <- ggplot(data.frame(Time = time, Signal = signal), aes(Time, Signal)) +
        labs(x = "Time (s)", y = signal_label, title = title) + theme_minimal(base_size = 12)
      if (NROW(windows)) {
        baseline <- data.frame(Role = "Local baseline", Start = windows$Baseline_start, End = windows$Baseline_end)
        baseline <- baseline[is.finite(baseline$Start) & is.finite(baseline$End) & baseline$End > baseline$Start, ]
        shading <- rbind(windows[, c("Role", "Start", "End"), drop = FALSE], baseline)
        plot <- plot + geom_rect(data = shading, aes(xmin = Start, xmax = End, fill = Role),
          ymin = -Inf, ymax = Inf, inherit.aes = FALSE, alpha = 0.22) +
          scale_fill_manual("Window", values = c("Local baseline" = "#92A0B5", Stimulus = "#42A995", Control = "#D7A047")) +
          geom_vline(data = windows, aes(xintercept = Stimulus_onset), colour = "#5F6978", linetype = "dotdash", linewidth = .4)
      }
      plot + geom_line(colour = "#244365", linewidth = 0.65, na.rm = TRUE)
    }
    overview_data <- reactive({
      dt <- rv$dts[[field()]]
      signal <- if (ncol(dt) > 1L) rowMeans(as.data.frame(dt)[-1], na.rm = TRUE) else rep(NA_real_, nrow(dt))
      data.frame(time = dt$Time, signal = signal)
    })
    output$overview <- renderPlot({
      data <- overview_data()
      validate(need(any(is.finite(data$signal)), "No valid normalized traces in this recording. Apply the protocol to inspect excluded and unscorable counts."))
      trace_plot(data$time, data$signal, paste(presentation()$label(field()), "· draft timing preview · mean cell trace"))
    })
    picked <- reactiveVal("")
    near_cutoff <- function(x) is.finite(x$Margin) & x$Margin >= .8 & x$Margin <= 1.2
    matches_filter <- function(x) {
      status <- input$status %||% "All"
      switch(status, All = rep(TRUE, nrow(x)), `Near threshold` = near_cutoff(x),
        `Flagged for review` = if ("QC_flags" %in% names(x)) !is.na(x$QC_flags) & nzchar(x$QC_flags) else rep(FALSE, nrow(x)),
        x$Status == status)
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
          paste0("Baseline mean (", applied_run()$snapshot$signal_label, ")"), "Baseline SD (local)", "Reason", "Key"),
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
    select_trace_key <- function(key, allowed_fields = fields()) {
      if (length(key) != 1L || is.na(key)) return(invisible(FALSE))
      x <- evaluable()
      row <- x[x$Key == key & x$Field %in% allowed_fields, , drop = FALSE]
      if (nrow(row) != 1L) return(invisible(FALSE))
      if (!matches_filter(row)) updateSelectInput(session, "status", selected = "All")
      selected_field(row$Field)
      picked(row$Key)
      invisible(TRUE)
    }
    observeEvent(point_event(), {
      if (select_trace_key(point_event()$key))
        session$sendCustomMessage(session$ns("review"), session$ns("calls"))
    })
    trace_bundles <- reactive({
      rows <- evaluable()
      stats::setNames(lapply(fields(), function(key)
        responder_trace_bundle(result_data()[[key]], rows[rows$Field == key, , drop = FALSE])), fields())
    })
    field_traces <- reactive({
      g <- trace_bundles()[[field()]]
      validate(need(nrow(g$rows) > 0L, "No evaluable cells for this field and population."))
      g
    })
    active_windows <- function(row) {
      end <- if (is.finite(row$Effective_end_s)) row$Effective_end_s else row$End_s
      data.frame(Role = if (row$Role == "Control") "Control" else "Stimulus", Start = row$Start_s, End = end,
        Baseline_start = row$Baseline_start_s %||% (row$Start_s - row$Baseline_s),
        Baseline_end = row$Baseline_end_s %||% row$Start_s,
        Stimulus_onset = row$Stimulus_onset_s %||% row$Start_s)
    }
    ordered_fields <- reactive(as.character(intersect(presentation()$levels, fields())))
    trace_fields <- reactive({
      keys <- ordered_fields()
      chosen <- input$trace_recording %||% "__all__"
      if (chosen %in% keys) chosen else keys
    })
    recording_choices <- reactive(stats::setNames(ordered_fields(), presentation()$label(ordered_fields())))
    observe({
      choices <- recording_choices()
      current <- isolate(input$trace_recording)
      updateSelectInput(session, "trace_recording", choices = c("All recordings" = "__all__", choices),
        selected = if (length(current) && current %in% choices) current else "__all__")
    })
    # Selecting a reviewed field must not resend the previous plot selection
    # before the browser has acknowledged a Focus update.
    observe({
      updateSelectInput(session, "review_recording", choices = recording_choices(), selected = field())
    })
    observeEvent(input$review_recording, {
      if (input$review_recording %in% fields()) selected_field(input$review_recording)
    })
    observeEvent(input$focus_recording, {
      if (input$focus_recording %in% fields()) {
        updateSelectInput(session, "trace_recording", selected = input$focus_recording)
        selected_field(input$focus_recording)
      }
    })
    trace_statuses <- reactive(if (is.null(input$trace_status)) character() else input$trace_status)
    trace_limits <- reactive({
      x <- active_counts()
      responder_trace_limits(trace_bundles()[trace_fields()], x[x$Field %in% trace_fields(), ], isTRUE(input$trace_full))
    })
    panel_id <- function(key) paste0("population_trace_", paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(key)))), collapse = ""))
    output$population_label <- renderText({
      x <- evaluable(); x <- x[x$Field %in% trace_fields(), , drop = FALSE]
      n <- length(trace_fields())
      sprintf("%d %s · showing %d of %d evaluable cells · shared axes",
        n, if (n == 1L) "recording" else "recordings", sum(x$Status %in% trace_statuses()), nrow(x))
    })
    output$heatmap_label <- renderText(presentation()$label(field()))
    output$population_plot <- renderUI({
      view <- presentation(); counts <- active_counts(); bundles <- trace_bundles()
      div(class = "responder-panels", lapply(trace_fields(), function(key) {
        r <- counts[counts$Field == key, , drop = FALSE]; g <- bundles[[key]]
        file <- rv$group_meta$file[match(key, rv$group_meta$key)]
        if (!length(file) || is.na(file)) file <- key
        note <- row_notes(r)
        shown <- sum(g$rows$Status %in% trace_statuses())
        div(class = "responder-panel", `data-recording-key` = key,
          div(class = "responder-panel-heading",
            tags$button(type = "button", class = "btn btn-default btn-xs", "Focus",
              onclick = sprintf("Shiny.setInputValue('%s',%s,{priority:'event'})", session$ns("focus_recording"), jsonlite::toJSON(key, auto_unbox = TRUE))),
            tags$strong(view$label(key)), div(class = "responder-file", file),
            p(if (r$Evaluable > 0) sprintf("%.1f%% responding · %d of %d evaluable", r$Percent_responding, r$Responded, r$Evaluable) else "No evaluable cells"),
            div(sprintf("%d responded · %d did not respond · %d excluded · %d unscorable", r$Responded, r$Nonresponders, r$Excluded, r$Unscorable))),
          if (nzchar(note)) p(class = "responder-panel-note text-warning", role = "status", note),
          if (!nrow(g$rows)) p(class = "responder-panel-note", role = "status", "No evaluable cells for this recording and population.")
          else if (!shown) p(class = "responder-panel-note", role = "status", "Choose a response class to display.")
          else plotly::plotlyOutput(session$ns(panel_id(key)), height = if (length(trace_fields()) == 1L) "380px" else "290px"),
          p(class = "responder-panel-note text-muted", sprintf("Showing %d of %d evaluable cells", shown, nrow(g$rows))))
      }))
    })
    panel_observers <- list()
    observeEvent(fields(), {
      removed <- setdiff(names(panel_observers), fields())
      for (key in removed) {
        panel_observers[[key]]$destroy()
        # The old htmlwidget may still be bound while its parent UI is hidden.
        # A renderUI(NULL) payload crashes that binding and interrupts the
        # remaining Shiny updates. Retire it with the same binding's validation.
        output[[panel_id(key)]] <- plotly::renderPlotly({ req(FALSE) })
        panel_observers[[key]] <<- NULL
      }
      for (key in setdiff(fields(), names(panel_observers))) local({
        recording_key <- key; id <- panel_id(key); source <- session$ns(id)
        drawn <- reactiveVal(FALSE)
        output[[id]] <- plotly::renderPlotly({
          req(recording_key %in% trace_fields())
          g <- trace_bundles()[[recording_key]]
          req(nrow(g$rows) > 0L, any(g$rows$Status %in% trace_statuses()))
          r <- active_counts(); r <- r[r$Field == recording_key, , drop = FALSE]
          p <- responder_trace_plot(g, r, presentation()$label(recording_key), trace_limits(),
            input$trace_mode %||% "individual", trace_statuses(), picked(), source)
          drawn(TRUE)
          p
        })
        panel_observers[[recording_key]] <<- observeEvent({
          req(drawn())
          plotly::event_data("plotly_click", source = source, priority = "event")
        }, {
          event <- plotly::event_data("plotly_click", source = source, priority = "event")
          if (!identical(input$trace_mode, "mean") && recording_key %in% trace_fields())
            select_trace_key(event$key, recording_key)
        })
      })
    }, ignoreNULL = FALSE)
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
      snapshot <- applied_run()$snapshot
      label <- snapshot$signal_label
      if (snapshot$input_data_mode %in% c("dff0", "dff0_percent", "drr0", "drr0_percent"))
        return(paste0("Uploaded ", label, " normalized to fractions; original raw baseline QC is unavailable."))
      sprintf("%s uses loading frames %d–%d%s.", label, snapshot$baseline_frames[1], snapshot$baseline_frames[2],
        if (key %in% names(snapshot$background_traces)) " after per-frame background subtraction" else "")
    }
    output$review_label <- renderText({
      row <- selected_call()
      paste(presentation()$label(row$Field), row$Cell, row$Stimulus, sep = " · ")
    })
    output$cell_diagnostics <- renderUI({
      row <- selected_call()
      flags <- row$QC_flags %||% ""
      if (is.na(flags)) flags <- ""
      tagList(
        if (nzchar(flags)) p(class = "text-warning", paste("Review flags:", flags, "Flags alone do not exclude a cell.")),
        if ("Raw_QC_available" %in% names(row) && !isTRUE(row$Raw_QC_available))
          p("Raw baseline brightness/noise QC is unavailable for this input."),
        p(paste0("Baseline samples: ", row$Baseline_samples %||% "unknown", "; response samples: ", row$Response_samples %||% "unknown", ".")),
        if ("Response_latency_s" %in% names(row) && is.finite(row$Response_latency_s))
          p(sprintf("Response latency %.3g s; rule confirmed at recording time %.3g s.", row$Response_latency_s, row$Confirmation_s)))
    })
    output$trace <- renderPlot({
      row <- selected_call()
      dt <- result_data()[[row$Field]]
      validate(need(row$Cell %in% names(dt), row$Reason))
      win <- active_windows(row)
      y <- dt[[row$Cell]]
      plot <- trace_plot(dt$Time, y, paste(row$Cell, "·", row$Stimulus, "·", row$Status), win, applied_run()$snapshot$signal_label)
      subtitle <- row$Reason
      if (is.finite(row$Threshold_increase)) {
        label <- sprintf("Threshold %.3g (%s)", row$Threshold_increase,
          if (row$Threshold_source == "Floor") "floor" else sprintf("%g SD", row$SD_multiplier))
        refs <- data.frame(label = c("Local baseline", label), y = row$Baseline_mean +
          (1 + row$Baseline_mean) * c(0, row$Threshold_increase))
        refs$start <- c(win$Baseline_start, win$Start)
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
        limits <- c(min(win$Baseline_start, win$Stimulus_onset, win$Start) - margin, win$End + margin)
        if (limits[1] < max(dt$Time) && limits[2] > min(dt$Time)) plot <- plot + coord_cartesian(xlim = limits)
      }
      plot + labs(subtitle = subtitle, caption = paste(units_text(row$Field),
        "\nScoring uses the cell’s local prestimulus baseline. The threshold line is converted to plotted units.")) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0))
    })
    methods_text <- reactive({
      x <- active_counts()
      valid <- x$Evaluable > 0
      role_text <- switch(population(), primary = "all mapped cells for the primary stimulus",
        control = paste("all mapped cells for", control_name()), restricted = paste("primary stimulus calls among", control_name(), "responders"))
      processing <- unique(vapply(x$Field, units_text, ""))
      details <- vapply(seq_len(nrow(x)), function(i) {
        r <- x[i, ]
        field_note <- sprintf("%s: baseline %g–%g s, stimulus onset %g s, scoring %g–%g s, gap limit %g s",
          presentation()$label(r$Field), r$Baseline_start_s %||% (r$Start_s - r$Baseline_s),
          r$Baseline_end_s %||% r$Start_s, r$Stimulus_onset_s %||% r$Start_s,
          r$Start_s, r$Effective_end_s, r$Max_gap_s)
        notes <- row_notes(r)
        if (nzchar(notes)) paste(field_note, notes, sep = " · ") else field_note
      }, "")
      paste(paste(processing, collapse = " "), "We scored", role_text, "using the following rule.",
        paste0(rule_line(), "."), "For each field, the responding fraction was the number with a sustained response divided by evaluable cells.",
        sprintf("The pooled percentage used summed counts. The mean of field percentages weighted each of the %d evaluable fields equally (%d fields processed).",
          sum(valid), nrow(x)),
        "Valid flat traces remained in the denominator. Quality warnings did not remove cells. Excluded and unscorable cells were reported separately.",
        "Experimental-unit summaries pooled fields within each explicitly assigned unit and weighted evaluable units equally; unassigned recordings were omitted from unit summaries.",
        paste(details, collapse = "; "),
        sprintf("Analysis run %s; protocol hash %s; scoring contract %s; measurement annotations %s.",
          applied_run()$id, applied_run()$protocol_hash, if (responder_protocol_version(protocol()) == 1L) "legacy-1" else responder_engine_version(), responder_engine_version()))
    })
    output$methods <- renderText(methods_text())
    output$unit_summary <- renderUI({
      req(applied_ready())
      u <- applied_run()$units
      rows <- u$summary
      active <- active_counts()
      rows <- rows[rows$Role == role() & rows$Population == active$Population[1], , drop = FALSE]
      tagList(tags$h4("Experimental-unit summary"),
        if (length(u$unassigned_fields)) p(class = "text-warning", sprintf(
          "%d recording(s) lack an experimental unit ID and are excluded from unit summaries. Enter IDs under Load Data → Recording details, then reapply.", length(u$unassigned_fields))),
        if (!nrow(rows)) p("No identified experimental units are available. Cell counts and field summaries remain descriptive.") else
          tags$ul(lapply(seq_len(nrow(rows)), function(i) {
            r <- rows[i, ]
            label <- if (nzchar(r$Condition)) r$Condition else "Unspecified condition"
            if (!r$Experimental_units) tags$li(paste0(label, ": no evaluable experimental units.")) else
              tags$li(sprintf("%s: %d evaluable experimental unit(s); mean %.1f%% responding%s.",
                label, r$Experimental_units, r$Mean_unit_percent,
                if (r$Experimental_units > 1) sprintf(" ± %.1f percentage points (SD)", r$SD_unit_percent) else ""))
          })),
        p(class = "text-muted", "Fields are pooled within each assigned unit. Means and SD between units weight each unit equally. The assignment defines the unit; the software does not infer biological independence."))
    })
    export_rows <- function(which) {
      req(applied_ready())
      run <- applied_run(); snapshot <- run$snapshot
      rows <- if (which %in% c("units", "unit_summary")) run$units[[if (which == "units") "units" else "summary"]] else results()[[which]]
      if (which == "metrics") {
        rows <- results()$calls
        columns <- c("Field", "Cell", "Role", "Stimulus", "Status", "Reason", "QC_flags", "Window_peak_local_fraction", "Window_AUC_fraction_s",
          "Peak_s", "Peak_latency_s", "Onset_s", "Confirmation_s", "Response_latency_s", "Run_duration_s", "Run_samples",
          "Stimulus_onset_s", "Start_s", "End_s", "Effective_end_s", "Baseline_start_s", "Baseline_end_s", "Baseline_samples", "Response_samples",
          "Window_AUC_start_s", "Window_AUC_end_s", "Window_AUC_covered_s", "Window_AUC_coverage_fraction", "AUC_method", "Eligible", "Measurements_available", "Response_window_complete")
        rows <- rows[, intersect(columns, names(rows)), drop = FALSE]
      }
      if (which == "qc") {
        rows <- results()$calls
        columns <- c("Field", "Cell", "Role", "Stimulus", "Status", "Reason", "QC_flags", "Raw_QC_available", "Raw_F0", "Raw_baseline_SD", "F0", "Baseline_SD_raw",
          "Baseline_samples", "Response_samples", "Baseline_mean", "Baseline_SD", "Floor_threshold", "Noise_threshold", "Threshold_source",
          "Field_reason", "Window_warning")
        rows <- rows[, intersect(columns, names(rows)), drop = FALSE]
      }
      if ("Field" %in% names(rows)) {
        metadata <- responder_recording_metadata(snapshot)
        at <- match(rows$Field, metadata$key)
        rows$Source_file <- metadata$file[at]
        rows$Recording_label <- metadata$label[at]
        rows$Condition <- metadata$condition[at]
        rows$Experimental_unit_ID <- metadata$unit_id[at]
      }
      if (which %in% c("calls", "counts")) {
        rows$Control_start_s <- vapply(rows$Field, function(key) {
          p <- responder_protocol_for_field(run$protocol, key, run$overrides)
          control <- p[p$Role == "Control", , drop = FALSE]
          if (nrow(control)) control$Start_s[1] else NA_real_
        }, numeric(1))
        rows$Control_end_s <- vapply(rows$Field, function(key) {
          p <- responder_protocol_for_field(run$protocol, key, run$overrides)
          control <- p[p$Role == "Control", , drop = FALSE]
          if (nrow(control)) control$End_s[1] else NA_real_
        }, numeric(1))
        # Retain established KCl aliases only when that is the actual control.
        kcl <- any(run$protocol$Role == "Control" & tolower(run$protocol$Stimulus) == "kcl")
        rows$KCl_start_s <- if (kcl) rows$Control_start_s else NA_real_
        rows$KCl_end_s <- if (kcl) rows$Control_end_s else NA_real_
      }
      # Canonical names are modality neutral; do not label ratio measurements as fluorescence.
      rows <- rows[, setdiff(names(rows), c("Window_peak_local_dFF", "Window_AUC_local_dFF_s")), drop = FALSE]
      for (column in intersect(c("Response_reference", "Reason", "Field_reason"), names(rows)))
        rows[[column]] <- signal_display_text(rows[[column]], snapshot$signal_kind)
      rows$Input_mode <- rep(snapshot$input_data_mode, nrow(rows))
      rows$Signal_kind <- rep(snapshot$signal_kind, nrow(rows))
      rows$Normalized_units <- rep("fraction", nrow(rows))
      rows$Normalization_reference <- rep(snapshot$normalization_reference, nrow(rows))
      rows$F0_start_frame <- rep(snapshot$baseline_frames[1], nrow(rows))
      rows$F0_end_frame <- rep(snapshot$baseline_frames[2], nrow(rows))
      rows$Analysis_run_ID <- rep(run$id, nrow(rows))
      rows$Protocol_hash <- rep(run$protocol_hash, nrow(rows))
      rows$Engine_version <- rep(if (responder_protocol_version(run$protocol) == 1L) "legacy-1" else responder_engine_version(), nrow(rows))
      rows$App_version <- rep(responder_app_version(), nrow(rows))
      revision <- Sys.getenv("SIMPLECA_SOURCE_SHA", unset = "")
      rows$Source_revision <- rep(if (nzchar(revision)) revision else "unavailable", nrow(rows))
      rows$Scoring_contract_version <- rep(responder_protocol_version(run$protocol), nrow(rows))
      rows
    }
    output$stimulus_metrics <- DT::renderDT({
      rows <- export_rows("metrics")
      columns <- intersect(c("Field", "Cell", "Stimulus", "Status", "Window_peak_local_fraction", "Window_AUC_fraction_s", "Response_latency_s", "Peak_latency_s", "QC_flags"), names(rows))
      labels <- c(Field = "Recording", Cell = "Cell", Stimulus = "Stimulus", Status = "Status",
        Window_peak_local_fraction = "Peak local rise", Window_AUC_fraction_s = "Signed AUC (fraction·s)",
        Response_latency_s = "Response latency (s)", Peak_latency_s = "Peak latency (s)", QC_flags = "Quality flags")
      DT::datatable(rows[, columns, drop = FALSE], rownames = FALSE, colnames = unname(labels[columns]),
        options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
    })
    analysis_manifest <- function() {
      run <- applied_run(); req(!is.null(run))
      list(analysis_run_id = run$id, created_at_utc = run$created_at, protocol_hash = run$protocol_hash,
        engine_version = if (responder_protocol_version(run$protocol) == 1L) "legacy-1" else responder_engine_version(),
        annotation_engine_version = responder_engine_version(), app_version = responder_app_version(),
        source_revision = if (nzchar(Sys.getenv("SIMPLECA_SOURCE_SHA", unset = ""))) Sys.getenv("SIMPLECA_SOURCE_SHA") else NA_character_,
        scoring_contract_version = responder_protocol_version(run$protocol), data_fingerprint = run$data_fingerprint,
        signal_kind = run$snapshot$signal_kind, normalized_units = "fraction",
        normalization_reference = run$snapshot$normalization_reference, input_mode = run$snapshot$input_data_mode,
        denominator = "Responded / (Responded + Did not respond); excluded and unscorable are reported separately",
        unit_summary = "Counts pooled within explicitly assigned experimental units; equal weighting between evaluable units",
        unassigned_recording_keys = run$units$unassigned_fields,
        qc_policy = "Warning flags do not exclude cells; invalid normalization and unscorable windows are separate statuses",
        timing_overrides = run$overrides, recording_metadata = responder_recording_metadata(run$snapshot),
        processing_manifest = run$snapshot$processing_manifest)
    }
    write_analysis_bundle <- function(file) {
      req(applied_ready())
      run <- applied_run()
      directory <- tempfile("responder-export-")
      dir.create(directory)
      on.exit(unlink(directory, recursive = TRUE), add = TRUE)
      paths <- character()
      write_csv <- function(name, rows) {
        path <- file.path(directory, name); data.table::fwrite(rows, path, na = "NA")
        paths <<- c(paths, path)
      }
      for (kind in c("counts", "calls", "metrics", "qc", "units", "unit_summary"))
        write_csv(paste0(kind, ".csv"), export_rows(kind))
      protocol_path <- file.path(directory, "response_protocol.json")
      responder_export_protocol(run$protocol, protocol_path, overrides = run$overrides)
      paths <- c(paths, protocol_path)
      manifest_path <- file.path(directory, "analysis_manifest.json")
      jsonlite::write_json(analysis_manifest(), manifest_path, pretty = TRUE, auto_unbox = TRUE, na = "null", dataframe = "rows")
      paths <- c(paths, manifest_path)
      if (!is.null(run$snapshot$processing_manifest)) write_csv("processing_manifest.csv", run$snapshot$processing_manifest)
      if (!is.null(run$snapshot$cell_quality)) write_csv("loading_quality.csv", run$snapshot$cell_quality)
      for (key in names(run$snapshot$dts)) write_csv(paste0("normalized_", match(key, names(run$snapshot$dts)), "_", make.names(key), ".csv"), run$snapshot$dts[[key]])
      methods_path <- file.path(directory, "methods.txt")
      writeLines(c(paste("Analysis run", run$id), methods_text()), methods_path)
      paths <- c(paths, methods_path)
      write_zip_archive(file, paths)
    }
    export_base <- reactive(if (length(fields()) > 1) sprintf("%d_fields", length(fields())) else export_base_name(rv))
    register_exports <- function(inputs, target_session) {
      suffixes <- c(counts = "in_vitro_counts", calls = "in_vitro_cell_calls", metrics = "stimulus_metrics",
        qc = "response_quality", units = "experimental_units", unit_summary = "unit_summary")
      for (kind in names(suffixes)) local({
        key <- kind; suffix <- suffixes[[key]]
        register_browser_download(inputs, target_session, paste0("download_", key),
          function() build_export_filename(rv, suffix, base = export_base()),
          function(file) data.table::fwrite(export_rows(key), file, na = "NA"))
      })
      register_browser_download(inputs, target_session, "download_protocol",
        function() build_export_filename(rv, "response_protocol", ext = "json", base = export_base()),
        function(file) { req(applied_ready()); responder_export_protocol(protocol(), file, overrides = applied_run()$overrides) })
      register_browser_download(inputs, target_session, "download_bundle",
        function() build_export_filename(rv, "response_analysis", ext = "zip", base = export_base()),
        write_analysis_bundle)
    }
    register_exports(input, session)
    list(register_exports = register_exports, ready = applied_ready,
      applied_run = applied_run, dirty = dirty, export_rows = export_rows)
  })
}
