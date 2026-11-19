# Reuse the two-cell, 2 Hz recording and inspect the exported responder decisions.
invisible(evaluate(in_app(paste(
  "w.$('a[href=\"#shiny-tab-in_vitro\"]').click();",
  "return true;"
))))
wait_until(in_app("return !!d.querySelector('#in_vitro-overview img') && d.getElementById('in_vitro-cards').innerText.includes('Enter scoring windows');"),
           "recording visible before a protocol is entered")
invisible(evaluate(in_app(paste(
  "w.$('#in_vitro-start').val(10).trigger('change');",
  "w.$('#in_vitro-end').val(13).trigger('change');",
  "w.$('#in_vitro-baseline').val(3).trigger('change');return true;"
))))
wait_until(in_app("return d.getElementById('in_vitro-cards').innerText.includes('2/2 (100.0%)');"),
           "in vitro responder counts")
invisible(evaluate(in_app(paste(
  "d.getElementById('in_vitro-control').click();",
  "w.$('#in_vitro-control_start').val(16.5).trigger('change');",
  "w.$('#in_vitro-control_end').val(19.5).trigger('change');return true;"
))))
wait_until(in_app("return d.getElementById('in_vitro-cards').innerText.includes('Unavailable');"),
           "empty KCl-positive denominator")
counts <- read_export("in_vitro-download_counts", "in_vitro_counts")
calls <- read_export("in_vitro-download_calls", "in_vitro_cell_calls")
stopifnot(identical(as.numeric(counts$Percent_responding), c(100, 0, NA_real_)),
          nrow(calls) == 4L, sum(calls$Status == "Responded") == 2L)
cat(browser_mode, "in vitro counts, KCl denominator and cell calls passed\n")
