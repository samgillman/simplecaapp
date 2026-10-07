# Continue the real-browser session after the baseline-control regression.
# This synthetic recording has half-second sampling and one duplicated Time.
time_fixture <- tempfile(fileext = ".csv")
time <- (0:39) / 2
time[15] <- time[14]
data.table::fwrite(data.frame(Time = time, CellA = raw, CellB = raw), time_fixture)
file_input <- evaluate_remote(in_app("return d.getElementById('load_data-data_files');"))
node <- await_cdp(browser$DOM$requestNode(objectId = file_input$objectId, wait_ = FALSE))
invisible(await_cdp(browser$DOM$setFileInputFiles(
  files = list(normalizePath(time_fixture)), nodeId = node$nodeId, wait_ = FALSE
)))
wait_until(in_app(sprintf(
  "return d.getElementById('load_data-upload_feedback').innerText.indexOf('%s')>=0;",
  basename(time_fixture))), "duplicate-Time upload")
# Uploading another file retains the current valid window; choose 1-10 explicitly.
Sys.sleep(1)
edit_bound("start", 1, process = FALSE)
stopifnot(evaluate(in_app("return +d.getElementById('load_data-pp_sampling_rate').value;")) == 1)
edit_bound("end", 10)
wait_until(in_app(paste(
  "var status=d.getElementById('load_data-process_status').innerText;",
  "return status.indexOf('finite, strictly increasing')>=0 && status.indexOf('Advanced Options')>=0 &&",
  "d.getElementById('load_data-results_bar').innerText.indexOf('processing complete')<0;"
)), "invalid-Time rejection and persistent recovery guidance")
cat(browser_mode, "invalid Time rejected with persistent recovery guidance\n")

# Use the existing, explicit generation workflow and enter the acquisition rate.
invisible(evaluate(in_app("d.querySelector('[data-accordion-id=\"load_data-advanced_opts\"] .accordion-header').click();return true;")))
wait_until(in_app(paste(
  "var e=d.querySelector('select[id^=\"load_data-time_mode_\"]');",
  "return !!e && !!e.selectize && d.getElementById('load_data-column_mapping_ui').innerText.indexOf('automatic processing is blocked')>=0;"
)), "invalid-Time mapping controls")
invisible(evaluate(in_app("w.__timeModeControl=d.querySelector('select[id^=\"load_data-time_mode_\"]');return true;")))
invisible(evaluate(in_app("var e=d.getElementById('load_data-pp_sampling_rate');e.focus();e.select();return true;")))
invisible(await_cdp(browser$Input$insertText(text = "2", wait_ = FALSE)))
invisible(evaluate(in_app("w.$('#load_data-pp_sampling_rate').trigger('change');return true;")))
wait_until(in_app("return w.Shiny.shinyapp.$inputValues['load_data-pp_sampling_rate:shiny.number']===2;"), "confirmed 2 Hz rate")
invisible(evaluate(in_app("d.querySelector('select[id^=\"load_data-time_mode_\"]').selectize.setValue('generated');return true;")))
wait_until(in_app("return d.getElementById('load_data-column_mapping_ui').innerText.indexOf('Generating Time from row number at 2 Hz')>=0;"), "explicit generated Time selection")
stopifnot(isTRUE(evaluate(in_app("return w.__timeModeControl===d.querySelector('select[id^=\"load_data-time_mode_\"]') && w.__timeModeControl.value==='generated';"))))
process_click()
assert_processed(1, 10)

invisible(evaluate(in_app("w.$('a[href=\"#shiny-tab-data_export\"]').click();w.$('a[data-value=\"Figure Export\"]').tab('show');return true;")))
wait_until(in_app("var e=d.getElementById('data_export-dl_manifest_csv');return !!e && e.classList.contains('shiny-bound-input') && e.getBoundingClientRect().height>0;"), "current time-validation export controls")
Sys.sleep(.2)
manifest <- read_export("data_export-dl_manifest_csv", "processing_manifest")
stopifnot(as.numeric(manifest$value[manifest$field == "sampling_rate_hz"]) == 2)
metrics <- read_export("data_export-dl_metrics_csv", "metrics")
stopifnot(nrow(metrics) == 2, all(abs(metrics$Peak_dFF0 - .2) < 1e-8),
  all(abs(metrics$Time_to_Peak - 11.5) < 1e-8),
  all(abs(metrics$AUC - .4025) < 1e-8), all(abs(metrics$Rise_Time - 1.6) < 1e-8),
  all(abs(metrics$FWHM - 2) < 1e-8))
cat(browser_mode, "invalid Time blocked; explicit 2 Hz recovery and exported timing metrics passed\n")

# Inspect numeric labels in the browser-rendered interactive version of the
# same ggplot; unit tests also inspect the static plot's trained scale.
invisible(evaluate(in_app(paste(
  "w.$('a[href=\"#shiny-tab-time\"]').click();",
  "w.__logPlotVersion=0;w.$(d).on('shiny:value.logTicks',function(e){",
  "if(e.name==='time_course-timecourse_plotly')w.__logPlotVersion++;});",
  "d.getElementById('time_course-tc_log_y').click();",
  "d.querySelector('input[name=\"time_course-plot_type_toggle\"][value=\"Interactive\"]').click();return true;"
))))
tick_labels <- "Array.from(d.querySelectorAll('#time_course-timecourse_plotly .ytick text')).map(function(e){return e.textContent;})"
wait_until(in_app(paste0(
  "var ticks=", tick_labels, ";return w.__logPlotVersion>0 && ticks.length>=2 && ",
  "ticks.every(function(t){return t.trim()!=='' && Number.isFinite(Number(t)) && Number(t)>0;});"
)), "automatic Log10 numeric tick labels")
invisible(evaluate(in_app(paste0("w.__automaticLogLabels=JSON.stringify(", tick_labels, ");return true;"))))
invisible(evaluate(in_app(paste(
  "w.__previousLogVersion=w.__logPlotVersion;",
  "w.$('#time_course-tc_y_breaks').val('0.02,0.05,0.1').trigger('change');return true;"
))))
wait_until(in_app(paste0(
  "var ticks=", tick_labels, ";return w.__logPlotVersion>w.__previousLogVersion && ",
  "JSON.stringify(ticks.map(Number))===JSON.stringify([0.02,0.05,0.1]);"
)), "custom Log10 numeric tick labels")
invisible(evaluate(in_app(paste(
  "w.__previousLogVersion=w.__logPlotVersion;",
  "w.$('#time_course-tc_y_breaks').val('').trigger('change');return true;"
))))
wait_until(in_app(paste0(
  "var ticks=", tick_labels, ";return w.__logPlotVersion>w.__previousLogVersion && ",
  "JSON.stringify(ticks)===w.__automaticLogLabels;"
)), "restored automatic ticks after clearing custom ticks")
invisible(evaluate(in_app("w.$(d).off('shiny:value.logTicks');return true;")))
unlink(time_fixture)
cat(browser_mode, "automatic/custom/restored Log10 tick labels passed\n")
