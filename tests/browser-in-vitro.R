# Reuse the two-cell, 2 Hz recording and inspect the exported responder decisions.
invisible(evaluate(in_app(paste(
  "w.$('a[href=\"#shiny-tab-in_vitro\"]').click();",
  "return true;"
))))
wait_until(in_app(paste(
  "return !!d.querySelector('#in_vitro-overview img') && d.getElementById('in_vitro-settings').open &&",
  "d.getElementById('in_vitro-protocol_feedback').innerText.includes('Enter a stimulus name') &&",
  "d.getElementById('in_vitro-start').value==='4.5' && d.getElementById('in_vitro-end').value==='19.5';"
)), "unscored recording with suggested 2 Hz window and blank stimulus")
invisible(evaluate(in_app(paste(
  "w.$('#in_vitro-stimulus').val('Capsaicin').trigger('change');",
  "Object.entries({start:10,end:13,baseline:3}).forEach(function(entry){",
  "var e=d.getElementById('in_vitro-'+entry[0]);e.value=entry[1];",
  "e.dispatchEvent(new w.Event('input',{bubbles:true}));w.$(e).trigger('change');});return true;"
))))
wait_until(in_app("return d.getElementById('in_vitro-headline').innerText.includes('2 of 2 evaluable cells');"),
           "in vitro responder counts")
wait_until(in_app(paste(
  "return !!d.querySelector('#in_vitro-population_plot img') &&",
  "!!d.querySelector('#in_vitro-distribution .point') && d.getElementById('in_vitro-review_label').innerText.includes('CellA');"
)), "distribution, group traces and first reviewed cell")
invisible(evaluate(in_app("d.getElementById('in_vitro-next_cell').click();return true;")))
wait_until(in_app("return d.getElementById('in_vitro-review_label').innerText.includes('CellB');"), "next cell")
# Use Chrome pointer input on the SVG marker, including the hover step that
# Plotly uses to identify the nearest data point.
click_distribution_point <- function() {
  xy <- pointer_position("#in_vitro-distribution .scatterlayer .point")
  invisible(await_cdp(browser$Input$dispatchMouseEvent(type = "mouseMoved",
    x = xy$x, y = xy$y, wait_ = FALSE)))
  mouse("mousePressed", xy)
  mouse("mouseReleased", xy)
}
click_distribution_point()
wait_until(in_app("return d.getElementById('in_vitro-review_label').innerText.includes('CellA');"), "distribution point selects cell")
invisible(evaluate(in_app(paste(
  "d.getElementById('in_vitro-control').click();",
  "w.$('#in_vitro-control_start').val(16.5).trigger('change');",
  "w.$('#in_vitro-control_end').val(20).trigger('change');return true;"
))))
wait_until(in_app("return d.getElementById('in_vitro-headline').innerText.includes('2 of 2');"), "KCl protocol ready")
invisible(evaluate(in_app("d.querySelector('input[name=\"in_vitro-population\"][value=restricted]').click();return true;")))
wait_until(in_app("return d.getElementById('in_vitro-cards').innerText.includes('No KCl responders') && d.getElementById('in_vitro-headline').innerText.includes('clipped');"),
           "empty KCl-positive denominator and clipped control warning")
invisible(evaluate(in_app("d.querySelector('input[name=\"in_vitro-population\"][value=primary]').click();return true;")))
wait_until(in_app(paste(
  "var table=d.getElementById('in_vitro-calls'), trace=d.getElementById('in_vitro-trace');",
  "return table && trace && trace.querySelector('img') &&",
  "trace.getBoundingClientRect().top>=table.getBoundingClientRect().bottom;"
)), "selected cell trace below the full-width review table")
counts <- read_export("in_vitro-download_counts", "in_vitro_counts")
calls <- read_export("in_vitro-download_calls", "in_vitro_cell_calls")
stopifnot(identical(as.numeric(counts$Percent_responding), c(100, 0, NA_real_)),
          nrow(calls) == 4L, sum(calls$Status == "Responded") == 2L,
          all(calls$Threshold_source == "Floor"), all(counts$KCl_effective_end_s == 19.5),
          all(calls$Peak_s[calls$Role == "Stimulus"] == 11.5),
          all(is.na(calls$Onset_s[calls$Role == "Control"])))
cat(browser_mode, "in vitro counts, KCl denominator and cell calls passed\n")

# These controls are accessible directly on Data & Export, whatever pill is selected.
invisible(evaluate(in_app("w.$('a[href=\"#shiny-tab-data_export\"]').click();return true;")))
wait_until(in_app("var b=d.getElementById('data_export-download_counts');return b && !b.disabled && b.offsetWidth>0;"),
           "responder exports on Data & Export")
export_counts <- read_export("data_export-download_counts", "in_vitro_counts")
export_calls <- read_export("data_export-download_calls", "in_vitro_cell_calls")
stopifnot(identical(counts, export_counts), identical(calls, export_calls),
          "Floor_local_fraction" %in% names(calls), "Floor_local_fraction" %in% names(counts),
          all(calls$Floor_local_fraction == .1), all(counts$Floor_local_fraction == .1))
