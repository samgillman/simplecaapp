# End the browser workflow with a known denominator and enough cells to page.
switch_tab("load")
path <- tempfile(fileext = ".csv")
rises <- c(rep(.15, 185), rep(.11, 6), rep(.09, 19))
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
invisible(evaluate(in_app(paste(
  "var p=d.getElementById('in_vitro-distribution');var tr=p.data.find(t=>t.customdata && t.customdata.length===210);",
  "p.emit('plotly_click',{points:[{customdata:tr.customdata.find(k=>k.includes(':Cell001:'))}]});return true;"
))))
wait_until(in_app(paste(
  "return d.getElementById('in_vitro-status').value==='All' &&",
  "d.getElementById('in_vitro-review_label').innerText.includes('Cell001');"
)), "point selection reconciles a conflicting filter")
cat(browser_mode, "210-cell point to review label (seconds)", as.numeric(difftime(Sys.time(), start, units = "secs")), "\n")
set_viewport(1440, 900)
unlink(path)
