# End-to-end browser smoke test: upload -> process -> plots -> data/figure
# downloads. This runs against a real local Shiny session in CI.
required <- c("chromote", "processx", "data.table")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Browser smoke dependencies missing: ", paste(missing, collapse = ", "))
browser_mode <- Sys.getenv("SIMPLECA_BROWSER_MODE", "shiny")
if (!(browser_mode %in% c("shiny", "shinylive"))) {
  stop("SIMPLECA_BROWSER_MODE must be 'shiny' or 'shinylive'.")
}
if (identical(browser_mode, "shinylive") && !file.exists("_shinylive/index.html")) {
  stop("Build _shinylive before running the Shinylive browser smoke test.")
}

port <- httpuv::randomPort()
fixture <- tempfile(fileext = ".csv")
time <- seq(0, by = 0.1, length.out = 60)
pulse <- c(rep(0, 20), seq(0.1, 1, by = 0.1), seq(0.9, 0, by = -0.1), rep(0, 20))
data.table::fwrite(data.frame(Time = time, Cell1 = 100 * (1 + pulse)), fixture)

server_expr <- if (identical(browser_mode, "shinylive")) {
  sprintf(
    "httpuv::runStaticServer('_shinylive', host='127.0.0.1', port=%d, browse=FALSE)",
    port
  )
} else {
  sprintf(
    "options(simpleca.verbose=FALSE); shiny::runApp('.', host='127.0.0.1', port=%d, launch.browser=FALSE)",
    port
  )
}
server <- processx::process$new(
  file.path(R.home("bin"), "Rscript"), c("-e", server_expr),
  wd = normalizePath("."), stdout = "|", stderr = "|", cleanup = TRUE
)
on.exit({
  if (server$is_alive()) server$kill()
  unlink(fixture)
}, add = TRUE)

browser <- chromote::ChromoteSession$new()
on.exit(browser$close(), add = TRUE)

await_cdp <- function(promise, timeout = 10) {
  browser$wait_for(chromote:::promise_timeout(promise, timeout))
}
evaluate <- function(js) {
  await_cdp(browser$Runtime$evaluate(js, returnByValue = TRUE, wait_ = FALSE))$result$value
}
evaluate_remote <- function(js) {
  await_cdp(browser$Runtime$evaluate(js, returnByValue = FALSE, wait_ = FALSE))$result
}
in_app <- function(code) {
  context <- if (identical(browser_mode, "shinylive")) {
    paste(
      "var frame=document.querySelector('iframe.app-frame');",
      "if(!frame){return false;}",
      "var w=frame.contentWindow; var d=frame.contentDocument;"
    )
  } else {
    "var w=window; var d=document;"
  }
  sprintf("(function(){%s if(!w||!d){return false;} %s})()", context, code)
}
wait_until <- function(js, label, timeout = 45) {
  deadline <- Sys.time() + timeout
  repeat {
    value <- tryCatch(isTRUE(evaluate(js)), error = function(e) FALSE)
    if (value) return(invisible(TRUE))
    if (Sys.time() >= deadline) {
      output <- tryCatch(server$read_output_lines(), error = function(e) character())
      errors <- tryCatch(server$read_error_lines(), error = function(e) character())
      dom_state <- tryCatch(evaluate(in_app(
        "return JSON.stringify(['load_data-upload_feedback','load_data-process_feedback','load_data-results_bar'].reduce(function(x,id){var e=d.getElementById(id);x[id]=e?e.innerText:'<missing>';return x;},{}));"
      )), error = function(e) "<browser unavailable>")
      server_lines <- tail(c(output, errors), 80)
      stop(
        "Browser smoke timed out waiting for ", label,
        "\nDOM state: ", dom_state,
        "\nServer output (tail):\n", paste(server_lines, collapse = "\n")
      )
    }
    Sys.sleep(0.2)
  }
}

url <- sprintf("http://127.0.0.1:%d", port)
deadline <- Sys.time() + 30
repeat {
  navigated <- tryCatch({
    await_cdp(browser$Page$navigate(url, wait_ = FALSE), timeout = 5)
    TRUE
  }, error = function(e) FALSE)
  if (navigated) break
  if (Sys.time() >= deadline) stop("Shiny server did not start at ", url)
  Sys.sleep(0.2)
}
startup_timeout <- if (identical(browser_mode, "shinylive")) 300 else 45
wait_until(in_app("return d.readyState === 'complete' && !!w.Shiny;"), "application load", startup_timeout)
wait_until(in_app("return !!d.getElementById('load_data-data_files');"), "file input")

# Record custom-message downloads without writing to the CI runner's Downloads
# folder. The Blob still has to be created successfully before click() occurs.
invisible(evaluate(in_app(
  "w.__simplecaDownloads=[]; w.HTMLAnchorElement.prototype.click=function(){if(this.download){w.__simplecaDownloads.push(this.download);}}; return true;"
)))

invisible(await_cdp(browser$DOM$getDocument(wait_ = FALSE)))
file_input <- evaluate_remote(in_app("return d.getElementById('load_data-data_files');"))
stopifnot(!is.null(file_input$objectId))
node <- await_cdp(browser$DOM$requestNode(objectId = file_input$objectId, wait_ = FALSE))
stopifnot(node$nodeId > 0)
invisible(await_cdp(browser$DOM$setFileInputFiles(
  files = list(normalizePath(fixture)), nodeId = node$nodeId, wait_ = FALSE
)))
wait_until(in_app("return d.getElementById('load_data-upload_feedback').innerText.indexOf('1 file ready') >= 0;"), "upload staging")
invisible(evaluate(in_app("d.getElementById('load_data-load_btn').click(); return true;")))
wait_until(
  in_app("return d.getElementById('load_data-results_bar').innerText.indexOf('processing complete') >= 0 && d.getElementById('load_data-results_bar').innerText.indexOf('60 timepoints') >= 0;"),
  "processed results"
)

# A baseline edit immediately followed by Process must survive the slider's
# asynchronous echo back from the browser (no success-then-stale transition).
invisible(evaluate(in_app(paste(
  "w.$('#load_data-pp_baseline_end').val(12).trigger('change');",
  "d.getElementById('load_data-load_btn').click(); return true;"
))))
wait_until(in_app(paste(
  "var frames=w.Shiny.shinyapp.$inputValues['load_data-pp_baseline_frames'];",
  "return frames && frames[1] === 12 &&",
  "d.getElementById('load_data-results_bar').innerText.indexOf('processing complete') >= 0;"
)), "processing after immediate baseline edit")
Sys.sleep(2)
stopifnot(isTRUE(evaluate(in_app(paste(
  "return d.getElementById('load_data-results_bar').innerText.indexOf('processing complete') >= 0 &&",
  "d.getElementById('load_data-process_status').innerText.indexOf('Settings changed') < 0;"
)))))

plot_targets <- c(
  time = "#time_course-timecourse_plot img",
  heatmap = "#heatmap-heatmap_plot img",
  metrics = "#metrics-metrics_plot img"
)
for (target in names(plot_targets)) {
  invisible(evaluate(in_app(sprintf("w.$('a[href=\"#shiny-tab-%s\"]').click(); return true;", target))))
  wait_until(
    in_app(sprintf("return !!d.querySelector('%s');", plot_targets[[target]])),
    paste(target, "plot")
  )
}

invisible(evaluate(in_app("w.$('a[href=\"#shiny-tab-data_export\"]').click(); return true;")))
wait_until(in_app("return !!d.getElementById('data_export-download_cell_metrics');"), "data export tab")
invisible(evaluate(in_app("d.getElementById('data_export-download_cell_metrics').click(); return true;")))
wait_until(in_app("return w.__simplecaDownloads.some(function(x){return /cell_metrics.*[.]csv$/.test(x);});"), "metrics CSV download")

invisible(evaluate(in_app("w.$('a[href=\"#shiny-tab-data_export\"]').click(); return true;")))
invisible(evaluate(in_app("w.$('a[data-value=\"Figure Export\"]').tab('show'); return true;")))
wait_until(in_app("return !!d.getElementById('data_export-dl_timecourse_plot');"), "figure export controls")
invisible(evaluate(in_app("d.getElementById('data_export-dl_timecourse_plot').click(); return true;")))
wait_until(in_app("return w.__simplecaDownloads.some(function(x){return /timecourse_plot.*[.]png$/.test(x);});"), "figure download")

cat(browser_mode, "browser upload, immediate baseline edit, plots, CSV download, and figure download passed\n")

source("tests/browser-baseline-sync.R")

source("tests/browser-time-validation.R")
