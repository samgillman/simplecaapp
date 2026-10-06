# Sourced by browser-smoke.R in the same real Shiny/Shinylive browser session.
# Synthetic 40-frame signed recording from the live-review reproduction.
raw <- c(rep(c(99, 101), 5), rep(100, 10),
  100 * (1 + c(.05, .10, .15, .20, .15, .10, .05)), rep(100, 13))
baseline_fixture <- tempfile(fileext = ".csv")
data.table::fwrite(data.frame(Time = 0:39, CellA = raw, CellB = raw), baseline_fixture)
invisible(evaluate(in_app("w.$('a[href=\"#shiny-tab-load\"]').click(); return true;")))
file_input <- evaluate_remote(in_app("return d.getElementById('load_data-data_files');"))
node <- await_cdp(browser$DOM$requestNode(objectId = file_input$objectId, wait_ = FALSE))
invisible(await_cdp(browser$DOM$setFileInputFiles(
  files = list(normalizePath(baseline_fixture)), nodeId = node$nodeId, wait_ = FALSE
)))
wait_until(in_app("return w.$('#load_data-pp_baseline_frames').data('ionRangeSlider').options.max === 40;"), "40-frame upload")
# Finish upload-driven bounds updates before testing user edits.
Sys.sleep(1)

# Capture actual generated CSV contents, without replacing app export handlers.
invisible(evaluate(in_app(paste(
  "w.__baselineResultsVersion=0;w.$(d).on('shiny:value.baselineResults',function(e){",
  "if(e.name==='load_data-results_bar')w.__baselineResultsVersion++;});",
  "w.__baselineFiles=[]; var create=w.URL.createObjectURL; var blobs={};",
  "w.URL.createObjectURL=function(blob){var url=create.call(this,blob);blobs[url]=blob;return url;};",
  "var click=w.HTMLAnchorElement.prototype.click;",
  "w.HTMLAnchorElement.prototype.click=function(){",
  "if(this.download && /[.]csv$/.test(this.download) && blobs[this.href]){",
  "var name=this.download;blobs[this.href].text().then(function(text){w.__baselineFiles.push({name:name,text:text});});}",
  "return click.call(this);};",
  # Expose the real feedback race reliably instead of depending on runner speed.
  # Old code emits slider and numeric updates after each edit; delayed delivery
  # makes an earlier server echo arrive after a newer user action.
  "w.$(d).on('shiny:updateinput.baselineLatency',function(e){",
  "if(e.target.id.indexOf('pp_baseline')>=0){e.preventDefault();",
  "var el=e.target,binding=e.binding,message=e.message;",
  "w.setTimeout(function(){binding.receiveMessage(el,message);},400);}});return true;"
))))

pointer_position <- function(selector) {
  evaluate(in_app(sprintf(paste(
    "var e=d.querySelector('%s');e.scrollIntoView({block:'center'});",
    "var r=e.getBoundingClientRect();",
    "var f=typeof frame==='undefined'?{x:0,y:0}:frame.getBoundingClientRect();",
    "return {x:r.x+r.width/2+f.x,y:r.y+r.height/2+f.y};"
  ), selector)))
}
mouse <- function(type, position) {
  invisible(await_cdp(browser$Input$dispatchMouseEvent(type = type,
    x = position$x, y = position$y, button = "left", clickCount = 1, wait_ = FALSE)))
}
process_click <- function() {
  invisible(evaluate(in_app("w.__baselineClickVersion=w.__baselineResultsVersion;return true;")))
  position <- pointer_position("#load_data-load_btn")
  mouse("mousePressed", position)
  mouse("mouseReleased", position)
}
edit_bound <- function(bound, value, process = TRUE) {
  invisible(evaluate(in_app(sprintf(paste(
    "var e=d.getElementById('load_data-pp_baseline_%s');",
    "e.focus();e.select();return true;"
  ), bound))))
  invisible(await_cdp(browser$Input$insertText(text = as.character(value), wait_ = FALSE)))
  if (process) process_click()
}
controls_match <- function(start, end) in_app(sprintf(paste(
  "var r=w.$('#load_data-pp_baseline_frames').data('ionRangeSlider').result;",
  "return r.from===%d && r.to===%d &&",
  "+d.getElementById('load_data-pp_baseline_start').value===%d &&",
  "+d.getElementById('load_data-pp_baseline_end').value===%d;"
), start, end, start, end))
assert_processed <- function(start, end) {
  wait_until(controls_match(start, end), "matching baseline controls")
  complete <- in_app(paste(
    "return w.__baselineResultsVersion>w.__baselineClickVersion && d.getElementById('load_data-results_bar').innerText.replace(/\\s+/g,' ').indexOf('2 cells')>=0 &&",
    "d.getElementById('load_data-results_bar').innerText.indexOf('processing complete')>=0 &&",
    "d.getElementById('load_data-process_status').innerText.indexOf('Settings changed')<0;"
  ))
  wait_until(complete, "stable two-cell results")
  # Sample beyond multiple 400 ms updates and 250 ms input debounce intervals.
  for (i in 1:10) {
    Sys.sleep(.2)
    if (!isTRUE(evaluate(controls_match(start, end))) || !isTRUE(evaluate(complete))) {
      stop("Baseline did not stay committed: ", evaluate(in_app(paste(
        "return JSON.stringify({status:d.getElementById('load_data-process_status').innerText,",
        "results:d.getElementById('load_data-results_bar').innerText,",
        "frames:w.Shiny.shinyapp.$inputValues['load_data-pp_baseline_frames'],",
        "resultVersion:w.__baselineResultsVersion,clickVersion:w.__baselineClickVersion});"
      ))))
    }
  }
}
read_export <- function(button, name) {
  invisible(evaluate(in_app(sprintf(
    "w.__baselineFiles=[];d.getElementById('%s').click();return true;", button))))
  wait_until(in_app(sprintf(
    "return w.__baselineFiles.some(function(f){return f.name.indexOf('%s')>=0;});", name)), name)
  contents <- evaluate(in_app(sprintf(
    "return w.__baselineFiles.find(function(f){return f.name.indexOf('%s')>=0;}).text;", name)))
  data.table::fread(text = contents)
}
assert_committed <- function(start, end) {
  invisible(evaluate(in_app("w.$('a[href=\"#shiny-tab-data_export\"]').click();w.$('a[data-value=\"Figure Export\"]').tab('show');return true;")))
  wait_until(in_app("var e=d.getElementById('data_export-dl_manifest_csv');return !!e && e.classList.contains('shiny-bound-input') && e.getBoundingClientRect().height>0;"), "current export controls")
  Sys.sleep(.2)
  manifest <- read_export("data_export-dl_manifest_csv", "processing_manifest")
  stopifnot(as.integer(manifest$value[manifest$field == "baseline_start_frame"]) == start,
    as.integer(manifest$value[manifest$field == "baseline_end_frame"]) == end)
  metrics <- read_export("data_export-dl_metrics_csv", "metrics")
  expected_peak <- (max(raw[(end + 1):length(raw)]) - mean(raw[start:end])) / mean(raw[start:end])
  stopifnot(nrow(metrics) == 2, all(abs(metrics$Peak_dFF0 - expected_peak) < 1e-8),
    all(metrics$Time_to_Peak == 23))
  if (start == 1 && end == 10) stopifnot(all(abs(metrics$FWHM - 4) < 1e-8))
  cat(browser_mode, "committed baseline", start, end, "and exported metrics verified\n")
  invisible(evaluate(in_app("w.$('a[href=\"#shiny-tab-load\"]').click();return true;")))
}

edit_bound("end", 40)
wait_until(in_app("return d.getElementById('load_data-process_status').innerText.indexOf('Baseline must end before the final frame')>=0;"), "full-record baseline rejection")
# The reported failure: rapid 40 -> 10, with a real Process click after each edit.
for (end in c(40, 10)) {
  edit_bound("end", end)
  Sys.sleep(.08)
}
assert_processed(1, 10)
assert_committed(1, 10)
# Repeated changes in both directions, including an end inside the pulse so the
# exported normalization changes measurably if an old baseline was committed.
for (end in c(22, 10, 22, 10)) {
  edit_bound("end", end)
  assert_processed(1, end)
  assert_committed(1, end)
}
for (start in c(4, 1)) {
  edit_bound("start", start)
  assert_processed(start, 10)
  assert_committed(start, 10)
}

# Drag each slider handle continuously in both directions. Controls must follow
# throughout the drag; rebuilding the slider on every change breaks this test.
for (handle in c("to", "from")) {
  origin <- pointer_position(paste0("#load_data-baseline_controls .irs-handle.", handle))
  mouse("mousePressed", origin)
  for (dx in c(10, 25, 40, 25, 10)) {
    mouse("mouseMoved", list(x = origin$x + dx, y = origin$y))
    Sys.sleep(.08)
  }
  mouse("mouseReleased", list(x = origin$x + 10, y = origin$y))
  frames <- evaluate(in_app(paste(
    "var r=w.$('#load_data-pp_baseline_frames').data('ionRangeSlider').result;",
    "return [r.from,r.to];"
  )))
  stopifnot(if (handle == "to") frames[[2]] > 10 else frames[[1]] > 1)
  process_click()
  assert_processed(frames[[1]], frames[[2]])
  assert_committed(frames[[1]], frames[[2]])
}
# A real edit after processing must still invalidate and clear the result bar.
edit_bound("end", 12, process = FALSE)
wait_until(in_app(paste(
  "return d.getElementById('load_data-process_status').innerText.indexOf('Settings changed')>=0 &&",
  "d.getElementById('load_data-results_bar').innerText.indexOf('processing complete')<0;"
)), "genuine edit invalidates results")
process_click()
start <- as.integer(evaluate(in_app("return +d.getElementById('load_data-pp_baseline_start').value;")))
assert_processed(start, 12)
assert_committed(start, 12)
invisible(evaluate(in_app("w.$(d).off('shiny:updateinput.baselineLatency');return true;")))
unlink(baseline_fixture)
cat(browser_mode, "baseline synchronization, delayed updates, dragging, committed CSVs, and invalidation passed\n")
