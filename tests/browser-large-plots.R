# Same accumulated browser/webR session as the normal workflow regressions.
# Exercise the reported sequence: short upload, long replacement, large plots.
invisible(evaluate(in_app(paste(
  "w.$('a[href=\"#shiny-tab-load\"]').click();",
  "w.Shiny.setInputValue('load_data-clear_files',Date.now(),{priority:'event'});",
  "w.$('#load_data-upload_mode input[value=single]').click();",
  "var bg=d.getElementById('load_data-has_background');if(bg.checked)bg.click();",
  "var log=d.getElementById('time_course-tc_log_y');if(log.checked)log.click();",
  "w.__stressPreviousPlots={};['time_course-timecourse_plot','heatmap-heatmap_plot'].forEach(function(id){",
  "var img=d.querySelector('#'+id+' img');w.__stressPreviousPlots[id]=img?img.src:'';});return true;"
))))
wait_until(in_app(paste(
  "var input=d.getElementById('load_data-data_files');",
  "return input && !input.multiple && d.getElementById('load_data-process_status').textContent.includes('Waiting for files.');"
)), "single uploader reset")
stress_upload <- function(path, filename) {
  input <- evaluate_remote(in_app("return d.getElementById('load_data-data_files');"))
  node <- await_cdp(browser$DOM$requestNode(objectId = input$objectId, wait_ = FALSE))
  invisible(await_cdp(browser$DOM$setFileInputFiles(files = list(normalizePath(path)),
    nodeId = node$nodeId, wait_ = FALSE)))
  wait_until(in_app(sprintf("return d.getElementById('load_data-upload_feedback').textContent.includes('%s');", filename)),
    paste("uploaded", filename), timeout = 120)
}
stress_dir <- tempfile("stress-")
dir.create(stress_dir)
short_path <- file.path(stress_dir, "short.csv")
large_path <- file.path(stress_dir, "large.csv")
data.table::fwrite(data.frame(Time = 0:4, Cell1 = 101:105), short_path)
stress_upload(short_path, "short.csv")
wait_until(in_app(paste(
  "var r=w.$('#load_data-pp_baseline_frames').data('ionRangeSlider').result;",
  "return r.from===1 && r.to===20 && d.getElementById('load_data-upload_feedback').textContent.includes('will be skipped');"
)), "short file preserves baseline with a visible warning")

y <- 100 * (1 + .5 * exp(-((0:2999 - 1500) / 80)^2))
large <- data.table::as.data.table(setNames(rep(list(y), 1000), sprintf("Cell%04d", 1:1000)))
large[, Time := 0:2999]
data.table::setcolorder(large, c("Time", setdiff(names(large), "Time")))
data.table::fwrite(large, large_path)
rm(large)
stress_upload(large_path, "large.csv")
wait_until(in_app(paste(
  "var r=w.$('#load_data-pp_baseline_frames').data('ionRangeSlider').result;",
  "return r.from===1 && r.to===20 && w.$('#load_data-pp_baseline_frames').data('ionRangeSlider').options.max===3000;"
)), "long replacement retains baseline 1-20")
invisible(evaluate(in_app("d.getElementById('load_data-load_btn').click();return true;")))
wait_until(in_app(paste(
  "var b=d.getElementById('load_data-results_bar');",
  "return b && b.textContent.includes('1000 cells with metrics');"
)), "1000-cell recording processed", timeout = 180)

invisible(evaluate(in_app("d.querySelector('input[name=\"time_course-plot_type_toggle\"][value=Static]').click();return true;")))
for (target in list(c("time", "time_course-timecourse_plot"), c("heatmap", "heatmap-heatmap_plot"))) {
  invisible(evaluate(in_app(sprintf("w.$('a[href=\"#shiny-tab-%s\"]').click();return true;", target[1]))))
  wait_until(in_app(sprintf(paste(
    "var e=d.getElementById('%s'),img=e&&e.querySelector('img');",
    "return img && img.src!==w.__stressPreviousPlots[e.id] && img.complete && img.naturalWidth>0 && !e.classList.contains('recalculating') &&",
    "!e.classList.contains('shiny-output-error');"), target[2])),
    paste("large", target[1], "plot"), timeout = 180)
  cat(browser_mode, "1000 x 3000", target[1], "plot rendered\n")
}
invisible(evaluate(in_app(paste(
  "w.$('a[href=\"#shiny-tab-time\"]').click();",
  "d.querySelector('input[name=\"time_course-plot_type_toggle\"][value=Interactive]').click();return true;"
))))
wait_until(in_app(paste(
  "var e=d.getElementById('time_course-timecourse_plotly');",
  "var p=e && (e.classList.contains('js-plotly-plot')?e:e.querySelector('.js-plotly-plot'));",
  # ggplotly combines same-style cell lines into one trace with NA breaks.
  # Verify the actual cell identities, not the number of drawing objects.
  "if(!p || !p.data)return false;var cells=new Set();p.data.forEach(function(t){",
  "(Array.isArray(t.text)?t.text:[t.text]).forEach(function(v){",
  "var m=String(v||'').match(/Cell: (Cell[0-9]{4})/);if(m)cells.add(m[1]);});});",
  "return cells.size===1000 && !e.classList.contains('recalculating') &&",
  "!e.classList.contains('shiny-output-error');"
)), "large interactive time course", timeout = 180)
stopifnot(isTRUE(evaluate(in_app(paste(
  "return !d.querySelector('#shiny-tab-time .shiny-output-error:not(.shiny-output-error-validation)') &&",
  "!d.querySelector('#shiny-tab-heatmap .shiny-output-error:not(.shiny-output-error-validation)');"
)))))
switch_tab("data_export")
invisible(evaluate(in_app("w.$('a[data-value=\"Figure Export\"]').tab('show');return true;")))
wait_until(in_app("var b=d.getElementById('data_export-dl_metrics_csv');return b && b.offsetWidth>0;"), "large metrics export ready")
metrics <- read_export("data_export-dl_metrics_csv", "metrics")
stopifnot(nrow(metrics) == 1000L, all(abs(metrics$Peak_dFF0 - .5) < 1e-10))
unlink(stress_dir, recursive = TRUE)
cat(browser_mode, "short-file replacement and 1000 x 3000 static/interactive plots passed\n")
