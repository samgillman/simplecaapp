# Tests for the data-loading helpers in R/utils.R

test_that("ensure_time_first finds, moves, and renames the time column", {
  dt <- data.table::data.table(Cell1 = 1:3, `time (s)` = c(0, 1, 2))
  out <- ensure_time_first(dt)
  expect_equal(names(out)[1], "Time")
  expect_equal(out$Time, c(0, 1, 2))
  expect_identical(attr(out, "time_info")$source, "time")
})

test_that("ensure_time_first converts Frame using sampling rate", {
  dt2 <- data.table::data.table(frame = 1:3, Cell1 = 4:6)
  out2 <- ensure_time_first(dt2, sampling_rate = 2)
  expect_equal(names(out2), c("Time", "Cell1"))
  expect_equal(out2$Time, c(0, 0.5, 1))
  expect_identical(attr(out2, "time_info")$source, "frame")
})

test_that("ensure_time_first recognizes an unnamed sequential frame index", {
  dt <- data.table::data.table(V1 = 1:4, Mean1 = c(100, 101, 102, 103))
  out <- ensure_time_first(dt, sampling_rate = 2)

  expect_equal(names(out), c("Time", "Mean1"))
  expect_equal(out$Time, c(0, 0.5, 1, 1.5))
  expect_identical(attr(out, "time_info")$source, "inferred_frame")
})

test_that("a non-sequential trace named V1 is not consumed as Frame", {
  dt <- data.table::data.table(V1 = c(100, 101, 103), Mean1 = c(80, 82, 85))
  out <- ensure_time_first(dt, sampling_rate = 2)

  expect_equal(names(out), c("Time", "V1", "Mean1"))
  expect_equal(out$Time, c(0, 0.5, 1))
  expect_equal(out$V1, dt$V1)
  expect_identical(attr(out, "time_info")$source, "generated_missing")
})

test_that("ensure_time_first generates Time without consuming a cell trace", {
  dt <- data.table::data.table(
    CellA = c(100, 101, 103),
    CellB = c(200, 205, 210)
  )
  out <- ensure_time_first(dt, sampling_rate = 4)

  expect_equal(names(out), c("Time", "CellA", "CellB"))
  expect_equal(out$Time, c(0, 0.25, 0.5))
  expect_equal(out$CellA, dt$CellA)
  expect_identical(attr(out, "time_info")$source, "generated_missing")
})

test_that("ensure_time_first replaces invalid Time instead of dropping traces", {
  dt <- data.table::data.table(
    Time = c(0, 2, 1),
    CellA = c(10, 11, 12)
  )
  out <- ensure_time_first(dt, sampling_rate = 2)

  expect_equal(out$Time, c(0, 0.5, 1))
  expect_equal(out$CellA, dt$CellA)
  expect_identical(attr(out, "time_info")$source, "generated_invalid_time")
})

test_that("explicit Time and Frame mappings are validated and respected", {
  dt <- data.table::data.table(
    Seconds = c(0, 0.5, 1),
    Image = 10:12,
    CellA = c(100, 101, 102)
  )

  as_time <- ensure_time_first(dt, time_col = "Seconds", sampling_rate = 20)
  expect_equal(as_time$Time, c(0, 0.5, 1))
  expect_identical(attr(as_time, "time_info")$source, "time")

  as_frame <- ensure_time_first(dt, frame_col = "Image", sampling_rate = 2)
  expect_equal(as_frame$Time, c(0, 0.5, 1))
  expect_identical(attr(as_frame, "time_info")$source, "frame")

  bad <- data.table::data.table(Seconds = c(0, 2, 1), CellA = 1:3)
  expect_error(
    ensure_time_first(bad, time_col = "Seconds"),
    "strictly increasing"
  )
})

test_that("column mappings exclude metadata before processing", {
  dt <- data.table::data.table(
    Time = c(10, 20, 30),
    CellA = c(100, 101, 102),
    CellB = c(200, 201, 202),
    Label = c("a", "b", "c")
  )

  generated <- apply_column_mapping(
    dt,
    mapping = list(
      time_mode = "generated",
      excluded_columns = "CellB"
    ),
    sampling_rate = 2
  )

  expect_equal(names(generated), c("Time", "CellA"))
  expect_equal(generated$Time, c(0, 0.5, 1))
  expect_identical(attr(generated, "time_info")$source, "generated_selected")

  expect_error(
    apply_column_mapping(
      dt,
      mapping = list(
        time_mode = "time",
        time_column = "Time",
        excluded_columns = c("CellA", "CellB")
      )
    ),
    "No numeric trace columns"
  )
})

test_that("coerce_numeric_dt converts columns and drops non-numeric ones", {
  dt <- data.table::data.table(
    Time = c("0", "1", "2"),
    CellA = c("1.5", "2.5", "3.5"),
    Junk = c("a", "b", "c")
  )
  out <- coerce_numeric_dt(dt)
  expect_equal(names(out), c("Time", "CellA"))
  expect_type(out$Time, "double")
  expect_equal(out$CellA, c(1.5, 2.5, 3.5))
})

test_that("safe_read round-trips a CSV file", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  data.table::fwrite(data.table::data.table(Time = c(0, 0.1), C1 = c(1, 2)), f)

  out <- safe_read(f)
  expect_equal(nrow(out), 2)
  expect_equal(names(out), c("Time", "C1"))
})

test_that("time-course Y range includes all enabled plot layers", {
  summary_df <- data.frame(
    mean_dFF0 = c(0, 2),
    sem_dFF0 = c(0.2, 0.2)
  )
  trace_df <- data.frame(dFF0 = c(-1, 1, 30))

  with_traces <- timecourse_visible_y_range(summary_df, trace_df)
  summary_only <- timecourse_visible_y_range(
    summary_df, trace_df, show_traces = FALSE
  )

  expect_equal(with_traces, c(-1, 30))
  expect_equal(summary_only, c(-0.2, 2.2))
  expect_gte(max(compute_even_y_breaks(range(c(0, with_traces)), expand = TRUE)), 30)
})

test_that("a cleared time-course title means no plot title", {
  expect_null(optional_plot_title(NULL))
  expect_null(optional_plot_title(""))
  expect_null(optional_plot_title("   "))
  expect_identical(optional_plot_title("Custom title"), "Custom title")
})

test_that("censored width summaries distinguish exact values from lower bounds", {
  expect_identical(
    format_timecourse_metric_display("FWHM", 0, NaN, NaN, 29, 29),
    "Not estimable — 0/29 exact; 29/29 right-censored"
  )
  expect_identical(
    format_timecourse_metric_display("FWHM_Lower_Bound", 29, 142.7338, 5.22286, 29, 29),
    "142.7 ± 5.223 (n=29 censored)"
  )
  expect_identical(
    format_timecourse_metric_display("FWHM", 15, 167.2, 6.586, 52, 37),
    "167.2 ± 6.586 (n=15 exact; 37 censored)"
  )
})

test_that("to_long pivots cells and tags group identifiers", {
  dt <- data.table::data.table(Time = c(0, 1), A = c(1, 2), B = c(3, 4))
  long <- to_long(dt, "g")
  expect_equal(nrow(long), 4)
  expect_setequal(names(long), c("Time", "Cell", "dFF0", "Group", "Cell_ID"))
  expect_true(all(long$Group == "g"))
  expect_setequal(as.character(unique(long$Cell)), c("A", "B"))
})

test_that("default_group_colors returns one named color per group", {
  cols <- default_group_colors(c("ctrl", "treat"))
  expect_named(cols, c("ctrl", "treat"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}", cols)))
  expect_length(default_group_colors(character(0)), 0)
})

test_that("format_timepoint_context reports per-file dimensions", {
  expect_equal(
    format_timepoint_context(list(a = data.frame(x = 1:600))),
    "600 timepoints"
  )
  expect_equal(
    format_timepoint_context(list(
      a = data.frame(x = 1:600),
      b = data.frame(x = 1:600)
    )),
    "600 timepoints/file"
  )
  expect_equal(
    format_timepoint_context(list(
      a = data.frame(x = 1:500),
      b = data.frame(x = 1:600)
    )),
    "500\u2013600 timepoints/file"
  )
})

test_that("write_zip_archive produces a standard ZIP that round-trips", {
  dir <- file.path(tempdir(), "ziptest")
  dir.create(dir, showWarnings = FALSE)

  f_text <- file.path(dir, "group_a_processed.csv")
  writeLines(c("Time,Mean1,Mean2", "1,0.5123,0.witness", "2,0.6,0.7"), f_text)
  f_bin <- file.path(dir, "group_b_processed.csv")
  set.seed(42)
  writeBin(as.raw(sample(0:255, 50000, replace = TRUE)), f_bin)
  f_empty <- file.path(dir, "empty.csv")
  file.create(f_empty)

  zipf <- file.path(dir, "out.zip")
  write_zip_archive(zipf, c(f_text, f_bin, f_empty))
  expect_true(file.exists(zipf))
  # ZIP local-header magic
  expect_identical(readBin(zipf, "raw", 4), as.raw(c(0x50, 0x4b, 0x03, 0x04)))

  # Round-trip through R's independent internal unzip implementation
  out <- file.path(dir, "extract")
  unlink(out, recursive = TRUE)
  extracted <- utils::unzip(zipf, exdir = out)
  expect_length(extracted, 3)
  for (orig in c(f_text, f_bin, f_empty)) {
    got <- file.path(out, basename(orig))
    n <- file.info(orig)$size
    expect_identical(
      readBin(got, "raw", n = max(n, 1)),
      readBin(orig, "raw", n = max(n, 1)),
      info = basename(orig)
    )
  }
})
