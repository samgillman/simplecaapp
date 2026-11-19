# A changing background must disappear from the normalized trace and cell count.
switch_tab("load")
invisible(evaluate(in_app(
  "d.querySelector('input[name=\"load_data-upload_mode\"][value=\"single\"]').click();return true;"
)))
wait_until(in_app("return !!d.querySelector('#load_data-data_files:not([multiple])');"), "single-file uploader")
path <- tempfile(fileext = ".csv")
bg <- 20 + seq_len(40)
signal <- c(rep(100, 24), rep(125, 8), rep(100, 8))
data.table::fwrite(data.frame(Time = 0:39, Cell1 = signal + bg, BG = bg), path)
upload_recordings(path)
wait_until(in_app("return d.querySelectorAll('.recording-editor').length===1;"), "background fixture")
invisible(evaluate(in_app("d.getElementById('load_data-has_background').click();return true;")))
wait_until(in_app(paste(
  "var el=d.querySelector('#load_data-background_ui select');",
  "return el && el.selectedOptions.length===1 && el.selectedOptions[0].value==='BG';"
)), "named background selected automatically")
invisible(evaluate(in_app("d.getElementById('load_data-load_btn').click();return true;")))
wait_for_batch(1L)
switch_tab("data_export")
invisible(evaluate(in_app("w.$('a[data-value=\"Figure Export\"]').tab('show');return true;")))
wait_until(in_app(paste(
  "var e=d.getElementById('data_export-dl_metrics_csv');",
  "return e && e.classList.contains('shiny-bound-input') && e.offsetHeight>0;"
)), "background-corrected exports")
metrics <- read_export("data_export-dl_metrics_csv", "metrics")
manifest <- read_export("data_export-dl_manifest_csv", "processing_manifest")
stopifnot(nrow(metrics) == 1L, abs(metrics$Peak_dFF0 - .25) < 1e-9,
          identical(manifest$value[manifest$field == "background_correction"],
                    "background_roi_mean_subtracted_per_frame_before_f0"))
unlink(path)
cat(browser_mode, "background ROI selection, corrected peak, cell count and manifest passed\n")
