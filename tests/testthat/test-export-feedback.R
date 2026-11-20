test_that("default Arial plots and scientific labels export as valid PDFs", {
  plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x, y)) +
    ggplot2::geom_line() + ggplot2::theme_classic(base_family = "Arial") +
    ggplot2::labs(title = "Calcium response", y = if (capabilities("cairo")) "ΔF/F₀" else "dF/F0")
  path <- tempfile(fileext = ".pdf")
  on.exit(unlink(path), add = TRUE)
  expect_silent(save_plot_file(path, plot, 6, 4, format = "pdf"))
  expect_gt(file.info(path)$size, 100)
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  expect_identical(rawToChar(bytes[1:5]), "%PDF-")
  expect_match(rawToChar(tail(bytes, 30)), "%%EOF", fixed = TRUE)
  expect_identical(plot$theme$text$family, "Arial")
})

test_that("download failures report missing, empty and partial files without sending them", {
  notices <- saves <- list()
  session <- list(
    sendNotification = function(type, message) notices[[length(notices) + 1L]] <<- message,
    sendCustomMessage = function(type, message) saves[[length(saves) + 1L]] <<- message
  )
  writers <- list(
    function(path) invisible(NULL),
    function(path) file.create(path),
    function(path) { writeLines("partial", path); stop("deliberate writer failure") }
  )
  for (writer in writers) shiny::withReactiveDomain(session, browser_download(session, "test.pdf", writer))
  expect_length(saves, 0)
  expect_length(notices, 3)
  expect_match(as.character(notices[[1]]$html), "No file content")
  expect_match(as.character(notices[[2]]$html), "No file content")
  expect_match(as.character(notices[[3]]$html), "deliberate writer failure")
  shiny::withReactiveDomain(session, browser_download(session, "good.csv", function(path) writeLines("a,b\n1,2", path)))
  expect_length(saves, 1)
  expect_identical(saves[[1]]$filename, "good.csv")
  expect_match(rawToChar(jsonlite::base64_dec(saves[[1]]$b64)), "a,b\n1,2", fixed = TRUE)
  expect_length(notices, 3)
})

test_that("Excel parsing errors survive processing and valid openpyxl workbooks retain values", {
  bad <- tempfile(fileext = ".xlsx")
  on.exit(unlink(bad), add = TRUE)
  writeLines("not a workbook", bad)
  expect_error(safe_read(bad), "Excel read failed:")
  good <- file.path(repo_root, "tests", "fixtures", "openpyxl-pulse.xlsx")
  files <- data.frame(name = c("bad.xlsx", "good.xlsx"), datapath = c(bad, good))
  state <- build_processed_state(files, list(baseline_frames = c(1, 20), sampling_rate = 1))
  expect_equal(nrow(state$metrics), 1)
  expect_equal(state$metrics$Peak_dFF0, .5)
  expect_equal(state$dts[[1]]$Time, 0:59)
  expect_match(state$skipped_details$reason, "Excel read failed:")
  expect_false(grepl("fewer than two rows", state$skipped_details$reason))
  expect_error(build_processed_state(files[1, ], list()), "Excel read failed:")
})

test_that("diverging heatmap breaks contain exact zero at fractional intervals", {
  for (limit in c(.3, .6, .7, 1.3)) {
    b <- compute_heatmap_scale(limit, .1, min_value = -limit)$breaks
    expect_true(any(b == 0))
    expect_false(any(abs(b) < 1e-12 & b != 0))
    expect_equal(b, -rev(b))
  }
})

test_that("table formatting keeps exact data and non-display values", {
  data <- data.frame(AUC = c(-2.99e-14, -.01, 0, NA_real_))
  original <- DT::datatable(data)
  formatted <- format_display_round(original, columns = "AUC", digits = 4)
  expect_identical(formatted$x$data, original$x$data)
  renderer <- as.character(formatted$x$options$columnDefs[[1]]$render)
  expect_match(renderer, "type==='display'", fixed = TRUE)
  expect_match(renderer, "value.slice(1)", fixed = TRUE)
})

test_that("axis parsing reports rejected entries without rejecting valid negatives", {
  expect_equal(parse_axis_breaks("-5, 0, .5")$values, c(-5, 0, .5))
  expect_length(parse_axis_breaks("")$invalid, 0)
  expect_length(parse_axis_breaks(NULL)$values, 0)
  parsed <- parse_axis_breaks("abc, 2, Inf, 0, -1", positive = TRUE)
  expect_equal(parsed$values, 2)
  expect_equal(parsed$invalid, c("abc", "Inf", "0", "-1"))
})
