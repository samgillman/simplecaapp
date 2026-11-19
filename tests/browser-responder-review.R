# End the browser workflow with a known denominator and enough cells to page.
switch_tab("load")
path <- tempfile(fileext = ".csv")
rises <- c(rep(.15, 185), rep(.11, 6), rep(.09, 19))
rises[1] <- .40  # An isolated marker for the pointer test.
signals <- 100 * (1 + outer(0:39 %in% 23:28, rises))
colnames(signals) <- sprintf("Cell%03d", seq_along(rises))
data.table::fwrite(data.frame(Time = 0:39, signals, BG = 0), path)
upload_recordings(path)
wait_until(in_app(sprintf("return d.getElementById('load_data-upload_feedback').innerText.includes('%s');",
  basename(path))), "210-cell recording staged")
# Background selection is per upload. Select the blank ROI for this fixture too.
invisible(evaluate(in_app("var b=d.getElementById('load_data-has_background');if(!b.checked)b.click();return true;")))
wait_until(in_app("var e=d.querySelector('#load_data-background_ui select');return e && Array.from(e.selectedOptions).some(o=>o.value==='BG');"),
  "blank ROI excluded")
invisible(evaluate(in_app("d.getElementById('load_data-load_btn').click();return true;")))
wait_for_batch(1L)
switch_tab("in_vitro")
start <- Sys.time()
invisible(evaluate(in_app(paste(
  "var control=d.getElementById('in_vitro-control');if(control.checked)control.click();",
  "w.$('#in_vitro-start').val(20).trigger('change');w.$('#in_vitro-end').val(40).trigger('change');",
  "w.$('#in_vitro-baseline').val(5).trigger('change');return true;"
))))
wait_until(in_app(paste(
  "var el=d.getElementById('in_vitro-headline');return el.innerText.includes('91.0%') &&",
  "el.innerText.includes('191 of 210') && el.innerText.includes('clipped');"
)), "191/210 responders with clipped endpoint")
cat(browser_mode, "210-cell protocol to headline (seconds)", as.numeric(difftime(Sys.time(), start, units = "secs")), "\n")
wait_until(in_app("return d.getElementById('in_vitro-distribution_note').innerText.includes('25 of 210');"),
  "borderline distribution count")
invisible(evaluate(in_app("d.getElementById('in_vitro-status').selectize.setValue('Near threshold');return true;")))
wait_until(in_app("var td=d.querySelector('#in_vitro-calls tbody td');return !!td && w.$(td.closest('table')).DataTable().rows().count()===25;"),
  "near-threshold review rows")
set_viewport(420, 840)
wait_until(in_app("var p=d.getElementById('in_vitro-population_plot');return p.offsetWidth>0 && p.offsetWidth<=w.innerWidth;"),
  "population traces fit compact viewport")
start <- Sys.time()
click_distribution_point()
wait_until(in_app(paste(
  "return d.getElementById('in_vitro-status').value==='All' &&",
  "d.getElementById('in_vitro-review_label').innerText.includes('Cell001');"
)), "point selection reconciles a conflicting filter")
cat(browser_mode, "210-cell point to review label (seconds)", as.numeric(difftime(Sys.time(), start, units = "secs")), "\n")
set_viewport(1440, 900)
unlink(path)

# A typed baseline survives within the session, but must be checked against
# the new clock before it can replace a data-derived suggestion.
invisible(evaluate(in_app(paste(
  "d.getElementById('in_vitro-reset_windows').click();return true;"
))))
wait_until(in_app("return d.getElementById('in_vitro-start').value==='19' && d.getElementById('in_vitro-baseline').value==='19';"), "original recording defaults")
invisible(evaluate(in_app(paste(
  "var e=d.getElementById('in_vitro-baseline');e.value=20;",
  "e.dispatchEvent(new w.Event('input',{bubbles:true}));w.$(e).trigger('change');return true;"
))))
for (panel in c("in_vitro-distribution_view", "in_vitro-population_plot")) {
  invisible(evaluate(in_app(sprintf("d.getElementById('%s').scrollIntoView({block:'center'});return true;", panel))))
  wait_until(in_app(sprintf("return d.getElementById('%s').textContent.includes('No evaluable cells');", panel)),
    paste(panel, "shows empty-population guidance"))
}
switch_tab("load")
path <- tempfile(fileext = ".csv")
signal <- 100 * (1 + outer(0:299 >= 25, c(rep(.2, 94), rep(0, 6))))
colnames(signal) <- sprintf("Cell%03d", 1:100)
data.table::fwrite(data.frame(Time = 0:299, signal, BG = 0), path)
upload_recordings(path)
wait_until(in_app(sprintf("return d.getElementById('load_data-upload_feedback').innerText.includes('%s');", basename(path))),
  "new 300-second recording staged")
wait_until(in_app("var e=d.querySelector('#load_data-background_ui select');return e && Array.from(e.selectedOptions).some(o=>o.value==='BG');"),
  "new blank background selected")
edit_bound("end", 20)
wait_for_batch(1L)
switch_tab("in_vitro")
wait_until(in_app(paste(
  "return d.getElementById('in_vitro-start').value==='19' && d.getElementById('in_vitro-end').value==='299' &&",
  "d.getElementById('in_vitro-baseline').value==='19' &&",
  "!d.getElementById('in_vitro-defaults_notice').hidden &&",
  "d.getElementById('in_vitro-headline').innerText.includes('94 of 100 evaluable cells');"
)), "incompatible retained baseline resets with an explicit notice")
recovered <- read_export("in_vitro-download_counts", "in_vitro_counts")
stopifnot(recovered$Percent_responding == 94, recovered$Start_s == 19,
  recovered$End_s == 299, recovered$Baseline_s == 19,
  all(is.na(recovered$Field_reason) | recovered$Field_reason == ""))
# Valid edits, including an allowed clipped endpoint, survive reprocessing.
invisible(evaluate(in_app(paste(
  "Object.entries({start:25,end:300,baseline:20}).forEach(function(kv){",
  "var e=d.getElementById('in_vitro-'+kv[0]);e.value=kv[1];",
  "e.dispatchEvent(new w.Event('input',{bubbles:true}));w.$(e).trigger('change');});return true;"
))))
wait_until(in_app("return d.getElementById('in_vitro-headline').innerText.includes('clipped');"), "allowed clipped endpoint")
switch_tab("load")
process_click()
wait_for_batch(1L)
switch_tab("in_vitro")
wait_until(in_app(paste(
  "return d.getElementById('in_vitro-start').value==='25' && d.getElementById('in_vitro-end').value==='300' &&",
  "d.getElementById('in_vitro-baseline').value==='20' && d.getElementById('in_vitro-defaults_notice').hidden;"
)), "compatible edits survive reprocessing")
# Clearing a field deliberately leaves the protocol incomplete.
invisible(evaluate(in_app(paste(
  "var e=d.getElementById('in_vitro-start');e.value='';",
  "e.dispatchEvent(new w.Event('input',{bubbles:true}));w.$(e).trigger('change');return true;"
))))
switch_tab("load")
process_click()
wait_for_batch(1L)
switch_tab("in_vitro")
wait_until(in_app("return d.getElementById('in_vitro-start').value==='' && d.getElementById('in_vitro-protocol_feedback').innerText.includes('start');"),
  "a deliberately cleared window stays blank")
unlink(path)
cat(browser_mode, "incompatible retained times reset, compatible times persist, and empty results remain usable\n")
