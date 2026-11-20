# Multi-recording plots keep duplicate cell names distinct, including in webR.
switch_tab("load")
invisible(evaluate(in_app(paste(
  "w.Shiny.setInputValue('load_data-clear_files',Date.now(),{priority:'event'});",
  "d.querySelector('input[name=\"load_data-upload_mode\"][value=multi]').click();",
  "var bg=d.getElementById('load_data-has_background');if(bg.checked)bg.click();return true;"
))))
# Switching upload modes replaces the input asynchronously. The empty status
# can arrive first; uploading to that old single-file input loses the batch.
wait_until(in_app(paste(
  "return !!d.querySelector('#load_data-data_files[multiple]') &&",
  "d.getElementById('load_data-process_status').innerText.includes('Waiting');"
)), "empty multi-file uploader ready")
paths <- vapply(1:3, function(k) {
  folder <- tempfile(); dir.create(folder); path <- file.path(folder, if (k < 3) "compare.csv" else "short.csv")
  time <- (0:59) * if (k == 3) .5 else 1
  data.table::fwrite(data.frame(Time = time, Cell001 = 100 * (1 + (time >= 22 & time <= 26) * (.2 + k * .2)),
    Cell002 = 100), path)
  path
}, character(1))
upload_recordings(paths)
wait_until(in_app("return d.querySelectorAll('.recording-editor').length===3;"), "three recordings staged")
process_click(); wait_for_batch(3L); switch_tab("in_vitro")
invisible(evaluate(in_app("d.getElementById('in_vitro-reset_windows').click();return true;")))
wait_until(in_app("return d.getElementById('in_vitro-start').value!=='' && d.getElementById('in_vitro-end').value==='29.5';"), "recording defaults reset")
invisible(evaluate(in_app(paste(
  "w.$('#in_vitro-stimulus').val('Comparison').trigger('change');",
  "Object.entries({start:20,end:30,baseline:5,onset:20,baseline_end:20}).forEach(kv=>w.$('#in_vitro-'+kv[0]).val(kv[1]).trigger('change'));return true;"
))))
apply_response_protocol()
wait_until(in_app(paste(
  "var p=d.querySelectorAll('#in_vitro-population_plot .responder-panel');",
  "return p.length===3 && Array.from(p).every(e=>e.innerText.includes('50.0%') && e.querySelector('.js-plotly-plot'))",
  "&& p[2].innerText.includes('clipped');"
)), "recording panels with separate counts and clipped endpoint")
original_counts <- read_export("in_vitro-download_counts", "in_vitro_counts")
invisible(evaluate(in_app("d.querySelectorAll('#in_vitro-population_plot button')[1].click();return true;")))
wait_until(in_app("return d.querySelectorAll('#in_vitro-population_plot .responder-panel').length===1 && d.getElementById('in_vitro-trace_recording').value==='compare_1';"),
  "Focus selects the second duplicate filename")
invisible(evaluate(in_app(paste(
  "d.querySelector('input[name=\"in_vitro-trace_mode\"][value=mean]').click();return true;"
))))
wait_until(in_app(paste(
  "var p=d.querySelector('#in_vitro-population_plot .js-plotly-plot');",
  "return p && p.data && !p.classList.contains('recalculating') && p.data.every(t=>t.meta.kind==='summary'||t.meta.kind==='sem');"
)), "summary mode contains no cell-selection curves")
invisible(evaluate(in_app(paste(
  "d.querySelector('input[name=\"in_vitro-trace_mode\"][value=individual]').click();",
  "d.getElementById('in_vitro-status').selectize.setValue('Did not respond');return true;"
))))
wait_until(in_app(paste(
  "var p=d.querySelector('#in_vitro-population_plot .js-plotly-plot');",
  "if(!p || !p.data || p.classList.contains('recalculating') || !p.data.some(t=>t.meta.kind==='cells'))return false;",
  "p.scrollIntoView({block:'center'});return p._fullLayout.width<=p.clientWidth+1;"
)), "individual curves rendered")
# Native pointer input on a real trajectory sample, with hover before click.
xy <- evaluate(in_app(paste(
  "var p=d.querySelector('#in_vitro-population_plot .js-plotly-plot'),t=p.data.find(t=>t.meta.kind==='cells');",
  "var peak=Math.max(...t.y.filter(v=>v!==null)),i=t.y.indexOf(peak),r=p.getBoundingClientRect(),l=p._fullLayout;",
  "var f=typeof frame==='undefined'?{x:0,y:0}:frame.getBoundingClientRect();",
  "w.__traceHovered=false;p.once('plotly_hover',()=>w.__traceHovered=true);",
  "return {x:r.x+l.xaxis._offset+l.xaxis.l2p(t.x[i])+f.x,y:r.y+l.yaxis._offset+l.yaxis.l2p(t.y[i])+f.y};"
)))
invisible(await_cdp(browser$Input$dispatchMouseEvent(type = "mouseMoved", x = xy$x, y = xy$y, wait_ = FALSE)))
wait_until(in_app("return w.__traceHovered;"), "trajectory hover")
mouse("mousePressed", xy); mouse("mouseReleased", xy)
wait_until(in_app(paste(
  "var label=d.getElementById('in_vitro-review_label').innerText;",
  "return d.getElementById('in_vitro-status').value==='All' && label.includes('compare_1') && label.includes('Cell001');"
)), "trajectory selects recording and cell and reconciles the filter")
invisible(evaluate(in_app("d.getElementById('in_vitro-trace_recording').selectize.setValue('__all__');return true;")))
wait_until(in_app("return d.querySelectorAll('#in_vitro-population_plot .responder-panel').length===3;"), "comparison restored")
set_viewport(420, 840)
wait_until(in_app(paste(
  "return Array.from(d.querySelectorAll('#in_vitro-population_plot .responder-panel')).every(e=>",
  "e.getBoundingClientRect().right<=w.innerWidth+1);"
)), "recording panels fit narrow viewport")
set_viewport(1440, 900)
stopifnot(identical(original_counts, read_export("in_vitro-download_counts", "in_vitro_counts")))
unlink(dirname(paths), recursive = TRUE)
cat(browser_mode, "recording panels, Focus, native trajectory click and unchanged counts passed\n")
