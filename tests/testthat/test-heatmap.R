test_that("heatmap scale intervals can be automatic or user-defined", {
  automatic <- compute_heatmap_scale(1.75, interval = 0)
  expect_true(automatic$automatic)
  expect_equal(automatic$step, 0.25)
  expect_equal(automatic$upper, 1.75)
  expect_equal(automatic$breaks, seq(0, 1.75, by = 0.25))

  custom <- compute_heatmap_scale(1.75, interval = 0.5)
  expect_false(custom$automatic)
  expect_equal(custom$step, 0.5)
  expect_equal(custom$upper, 2)
  expect_equal(custom$breaks, c(0, 0.5, 1, 1.5, 2))

  exact <- compute_heatmap_scale(2, interval = 0.5)
  expect_equal(exact$upper, 2)
  expect_equal(exact$breaks, c(0, 0.5, 1, 1.5, 2))
})

test_that("heatmap scale intervals reject pathological break counts", {
  expect_error(
    compute_heatmap_scale(10, interval = 0.001),
    "Color scale interval is too small",
    fixed = TRUE
  )
})

test_that("the heatmap plot applies the requested color scale interval", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("data.table")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("purrr")
  skip_if_not_installed("ggplot2")

  suppressPackageStartupMessages({
    library(shiny)
    library(dplyr)
    library(ggplot2)
  })

  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_heatmap.R"), envir = module_env)

  rv <- shiny::reactiveValues(
    dts = list(dataset = data.table::data.table(
      Time = 0:2,
      Cell1 = c(0, 1.75, 0.5)
    )),
    groups = "dataset",
    files = data.frame(name = "dataset.csv")
  )

  shiny::testServer(module_env$mod_heatmap_server, args = list(rv = rv), {
    session$setInputs(
      hm_sort = "orig",
      hm_palette = "plasma",
      hm_scale_interval = 0.5,
      hm_title = "Dataset",
      hm_center_title = TRUE,
      hm_x_label = "Time (s)",
      hm_y_label = "Cell",
      hm_base_font_size = 14,
      hm_bold_labels = TRUE,
      hm_font = "Arial"
    )
    session$flushReact()

    plot <- session$getReturned()$plot()
    fill_scale <- plot$scales$get_scales("fill")

    expect_equal(fill_scale$limits, c(0, 2))
    expect_equal(fill_scale$breaks, c(0, 0.5, 1, 1.5, 2))
  })
})
