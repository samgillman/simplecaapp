# Tests for filename/export helpers and small utilities in R/utils.R

test_that("sanitize_filename_component strips unsafe characters", {
  expect_equal(sanitize_filename_component("My File (1)"), "My_File_1")
  expect_equal(sanitize_filename_component("ok-name_2"), "ok-name_2")
  expect_equal(sanitize_filename_component(NULL, fallback = "fb"), "fb")
  expect_equal(sanitize_filename_component("###", fallback = "fb"), "fb")
  expect_equal(sanitize_filename_component(12.5), "12_5")
})

test_that("build_export_filename composes base, parts, date, and extension", {
  rv <- list(files = data.frame(name = "exp 1.csv", stringsAsFactors = FALSE))
  fn <- build_export_filename(rv, parts = c("metrics", "3_cells"), ext = "csv")
  expect_match(fn, "^exp_1_metrics_3_cells_\\d{4}-\\d{2}-\\d{2}\\.csv$")

  fn_nodate <- build_export_filename(rv, parts = "x", ext = "png", include_date = FALSE)
  expect_equal(fn_nodate, "exp_1_x.png")

  # No files loaded: falls back to "data"
  fn_default <- build_export_filename(list(), ext = "csv", include_date = FALSE)
  expect_equal(fn_default, "data.csv")
})

test_that("compute_auto_y_step picks sensible steps", {
  expect_equal(compute_auto_y_step(c(0, 10)), 2.5)
  expect_equal(compute_auto_y_step(c(0, 1)), 0.25)
  # Degenerate ranges fall back to 0.5
  expect_equal(compute_auto_y_step(c(2, 2)), 0.5)
  expect_equal(compute_auto_y_step(c(0, Inf)), 0.5)
})

test_that("compute_even_y_breaks keeps every tick interval equal", {
  breaks <- compute_even_y_breaks(c(0, 1.1))
  expect_equal(breaks, seq(0, 1.1, length.out = 5))
  expect_equal(diff(breaks), rep(0.275, 4))
  expect_equal(range(breaks), c(0, 1.1))

  # The step is based on the span, not the absolute distance from zero.
  expect_equal(compute_even_y_breaks(c(10, 11)), seq(10, 11, by = 0.25))
})

test_that("compute_even_y_breaks can extend to nice outer bounds", {
  breaks <- compute_even_y_breaks(c(-0.1, 0.9), expand = TRUE)
  expect_equal(breaks, seq(-0.25, 1, by = 0.25))
  expect_equal(length(unique(round(diff(breaks), 12))), 1)

  expect_equal(compute_even_y_breaks(c(2, 2)), 2)
  expect_length(compute_even_y_breaks(c(0, Inf)), 0)
})

test_that("has_data distinguishes empty from usable objects", {
  expect_false(has_data(NULL))
  expect_false(has_data(data.frame()))
  expect_false(has_data(list()))
  expect_true(has_data(data.frame(a = 1)))
  expect_true(has_data(list(1)))
})

test_that("multi-file uploads export under a group-count base, not file 1's name", {
  rv <- list(files = data.frame(name = c("a.csv", "b.csv", "c.csv"), stringsAsFactors = FALSE))
  fn <- build_export_filename(rv, parts = "cell_metrics", ext = "csv", include_date = FALSE)
  expect_equal(fn, "3_groups_cell_metrics.csv")

  # Single file keeps the original-name base
  rv1 <- list(files = data.frame(name = "a.csv", stringsAsFactors = FALSE))
  expect_equal(
    build_export_filename(rv1, parts = "cell_metrics", ext = "csv", include_date = FALSE),
    "a_cell_metrics.csv"
  )
})

test_that("base override names per-group downloads after their own file", {
  rv <- list(files = data.frame(name = c("a.csv", "b.csv"), stringsAsFactors = FALSE))
  fn <- build_export_filename(rv, parts = "processed", ext = "csv",
                              include_date = FALSE, base = "b")
  expect_equal(fn, "b_processed.csv")
})
