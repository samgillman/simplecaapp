# Two independent uploads deliberately share their basename and cell headers.
switch_tab <- function(name) {
  # The download probe intercepts native anchor.click(). Trigger the
  # dashboard's navigation handler and wait for its actual pane to open.
  invisible(evaluate(in_app(sprintf("w.$('a[href=\"#shiny-tab-%s\"]').click();return true;", name))))
  wait_until(in_app(sprintf("return d.getElementById('shiny-tab-%s').classList.contains('active');", name)),
    paste(name, "tab selected"))
}
switch_tab("load")
invisible(evaluate(in_app(
  "d.querySelector('input[name=\"load_data-upload_mode\"][value=\"multi\"]').click();return true;"
)))
wait_until(in_app("return !!d.querySelector('#load_data-data_files[multiple]');"), "multi-file input")
upload_recordings <- function(paths) {
  el <- evaluate_remote(in_app("return d.getElementById('load_data-data_files');"))
  node <- await_cdp(browser$DOM$requestNode(objectId = el$objectId, wait_ = FALSE))
  invisible(await_cdp(browser$DOM$setFileInputFiles(files = as.list(normalizePath(paths)),
    nodeId = node$nodeId, wait_ = FALSE)))
}
registry_fixtures <- vapply(seq_len(3), function(i) {
  dir <- tempfile()
  dir.create(dir)
  path <- file.path(dir, if (i <= 2) "recording.csv" else "third.csv")
  data.table::fwrite(data.frame(Time = (0:39)/2, Cell = 100 + i * (raw - 100)), path)
  path
}, character(1))
upload_recordings(registry_fixtures[1:2])
wait_until(in_app("return d.querySelectorAll('.recording-editor').length===2;"), "duplicate recordings staged")
invisible(evaluate(in_app("d.getElementById('load_data-load_btn').click();return true;")))
wait_for_batch <- function(n) {
  wait_until(in_app(sprintf(paste(
    "var bar=d.querySelector('#load_data-results_bar .results-bar-stats');",
    "return bar && Number(bar.querySelector('b').textContent)===%d &&",
    "bar.textContent.includes('processing complete');"
  ), n)), paste(n, "recordings processed"))
}
wait_for_batch(2L)
registry_exports <- function() {
  switch_tab("data_export")
  invisible(evaluate(in_app("w.$('a[data-value=\"Figure Export\"]').tab('show');return true;")))
  wait_until(in_app(paste(
    "var m=d.getElementById('data_export-dl_metrics_csv'), c=d.getElementById('data_export-download_counts');",
    "return m && m.classList.contains('shiny-bound-input') && c && !c.disabled;"
  )), "exports ready for the processed recordings")
  list(metrics = read_export("data_export-dl_metrics_csv", "metrics"),
       counts = read_export("data_export-download_counts", "in_vitro_counts"))
}
original <- registry_exports()
stopifnot(setequal(original$metrics$Group, c("recording", "recording_1")),
          length(unique(original$metrics$Peak_dFF0)) == 2L)
switch_tab("load")
edit_display <- function(field, value) {
  invisible(evaluate(in_app(sprintf(paste(
    "var row=d.querySelectorAll('.recording-editor')[1];row.open=true;",
    "var e=row.querySelector('[data-field=%s]');e.value='%s';",
    "e.dispatchEvent(new w.Event('change',{bubbles:true}));return true;"
  ), field, value))))
}
edit_display("label", "Capsaicin_B")
edit_display("order", "1")
wait_until(in_app("return d.querySelector('.recording-editor [data-field=label]').value==='Capsaicin_B';"),
  "promoted recording retains its new name")
switch_tab("in_vitro")
wait_until(in_app("return d.querySelector('#in_vitro-cards tbody button').textContent==='Capsaicin_B';"),
  "renamed recording is the first responder row")
stopifnot(identical(original, registry_exports()))
switch_tab("load")
upload_recordings(registry_fixtures[3])
wait_until(in_app("return d.querySelectorAll('.recording-editor').length===3;"), "third recording staged")
invisible(evaluate(in_app("d.getElementById('load_data-load_btn').click();return true;")))
wait_for_batch(3L)
expanded <- registry_exports()
stopifnot(identical(original$metrics, expanded$metrics[expanded$metrics$Group != "third", ]),
          identical(original$counts, expanded$counts[expanded$counts$Field != "third", ]))
switch_tab("in_vitro")
wait_until(in_app("return d.querySelector('#in_vitro-cards tbody button').textContent==='Capsaicin_B';"),
  "metadata survives adding and processing another recording")
unlink(dirname(registry_fixtures), recursive = TRUE)
cat(browser_mode, "recording edits preserved metric exports and responder counts\n")
