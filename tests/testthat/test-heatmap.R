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

test_that("heatmap scales preserve negative values around zero", {
  signed <- compute_heatmap_scale(1.2, min_value = -0.6, interval = 0.5)
  expect_true(signed$diverging)
  expect_equal(signed$lower, -1.5)
  expect_equal(signed$upper, 1.5)
  expect_equal(signed$breaks, seq(-1.5, 1.5, by = 0.5))
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

test_that("heatmap keeps negative values and sorts on post-baseline peaks", {
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
  baseline_spike <- c(5, rep(0, 8), 0.5, rep(0, 20))
  response_first <- c(rep(0, 7), 1.2, rep(0, 21), -0.6)
  rv <- shiny::reactiveValues(
    dts = list(dataset = data.table::data.table(
      Time = seq(0, by = 0.1, length.out = 30),
      BaselineSpike = baseline_spike,
      ResponseFirst = response_first
    )),
    baseline_frames = c(1, 5), groups = "dataset",
    files = data.frame(name = "dataset.csv")
  )

  shiny::testServer(module_env$mod_heatmap_server, args = list(rv = rv), {
    session$setInputs(
      hm_sort = "tpeak", hm_palette = "plasma", hm_scale_interval = 0.5,
      hm_title = "Dataset", hm_center_title = TRUE,
      hm_x_label = "Time (s)", hm_y_label = "Cell",
      hm_base_font_size = 14, hm_bold_labels = TRUE, hm_font = "Arial"
    )
    session$flushReact()

    plot <- session$getReturned()$plot()
    expect_equal(min(plot$data$Value, na.rm = TRUE), -0.6)
    expect_equal(unique(plot$data$Cell_Label[plot$data$Cell == 1]), "ResponseFirst")
    fill_scale <- plot$scales$get_scales("fill")
    expect_equal(fill_scale$limits, c(-5, 5))
  })
})
