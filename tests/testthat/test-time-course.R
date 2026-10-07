# Regression tests for time-course plot controls.

test_that("clearing the title removes it from the rendered time-course plot", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggplot2")

  suppressPackageStartupMessages({
    library(shiny)
    library(dplyr)
    library(ggplot2)
  })

  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_time_course.R"), envir = module_env)

  rv <- shiny::reactiveValues(
    summary = data.frame(
      Time = 0:2,
      mean_dFF0 = c(0, 1, 0.5),
      sem_dFF0 = rep(0.1, 3),
      Group = "dataset"
    ),
    long = NULL,
    metrics = data.frame(Group = "dataset", Peak_dFF0 = 1),
    groups = "dataset",
    colors = c(dataset = "#000000"),
    files = data.frame(name = "dataset.csv")
  )

  shiny::testServer(module_env$mod_time_course_server, args = list(rv = rv), {
    session$setInputs(
      tc_title = "",
      tc_show_traces = FALSE,
      tc_show_avg_line = TRUE,
      tc_show_ribbon = FALSE,
      tc_line_color = "#000000",
      tc_line_width = 2,
      tc_bold_labels = TRUE,
      tc_x = "Time (s)",
      tc_y = "dFF0",
      tc_base_font_size = 14,
      tc_font = "Arial",
      tc_theme = "classic",
      tc_legend_pos = "auto",
      tc_log_y = FALSE,
      tc_limits = FALSE,
      tc_x_breaks = "",
      tc_y_breaks = "",
      tc_tick_format = "number"
    )
    session$flushReact()

    expect_null(session$getReturned()$plot()$labels$title)

    session$setInputs(tc_title = "Custom title")
    session$flushReact()
    expect_identical(session$getReturned()$plot()$labels$title, "Custom title")
  })
})

test_that("single-cell time courses keep the mean line when SEM is unavailable", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggplot2")
  suppressPackageStartupMessages({
    library(shiny)
    library(dplyr)
    library(ggplot2)
  })

  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_time_course.R"), envir = module_env)
  rv <- shiny::reactiveValues(
    summary = data.frame(
      Time = 0:2, mean_dFF0 = c(0, 1, 0.5), sem_dFF0 = NA_real_,
      sd_dFF0 = NA_real_, n_cells = 1L, Group = "dataset"
    ),
    long = data.frame(
      Time = 0:2, dFF0 = c(0, 1, 0.5), Cell = "Cell1",
      Group = "dataset", Cell_ID = "dataset_Cell1"
    ),
    metrics = data.frame(Group = "dataset", Peak_dFF0 = 1),
    groups = "dataset", colors = c(dataset = "#000000"),
    files = data.frame(name = "dataset.csv")
  )

  shiny::testServer(module_env$mod_time_course_server, args = list(rv = rv), {
    session$setInputs(
      tc_title = "", tc_show_traces = TRUE, tc_trace_transparency = 50,
      tc_show_avg_line = TRUE, tc_show_ribbon = TRUE,
      tc_line_color = "#000000", tc_line_width = 2,
      tc_bold_labels = TRUE, tc_x = "Time (s)", tc_y = "dFF0",
      tc_base_font_size = 14, tc_font = "Arial", tc_theme = "classic",
      tc_legend_pos = "auto", tc_log_y = FALSE, tc_limits = FALSE,
      tc_x_breaks = "", tc_y_breaks = "", tc_tick_format = "number"
    )
    session$flushReact()

    plot <- session$getReturned()$plot()
    expect_false(any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomRibbon"), logical(1))))
    expect_true(any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1))))
    expect_false(grepl("No valid finite values", paste(plot$labels, collapse = " "), fixed = TRUE))
  })
})

test_that("time-course summary explains an all-censored width result", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("ggplot2")

  suppressPackageStartupMessages({
    library(shiny)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
  })

  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_time_course.R"), envir = module_env)
  rv <- shiny::reactiveValues(
    summary = data.frame(
      Time = 0:2,
      mean_dFF0 = c(0, 1, 1),
      sem_dFF0 = rep(0.1, 3),
      Group = "dataset"
    ),
    long = NULL,
    metrics = data.frame(
      Group = rep("dataset", 2),
      Peak_dFF0 = c(1, 1.2),
      FWHM = c(NA_real_, NA_real_),
      FWHM_Censored = c(TRUE, TRUE),
      FWHM_Lower_Bound = c(3, 4),
      Half_Width = c(NA_real_, NA_real_)
    ),
    groups = "dataset",
    colors = c(dataset = "#000000"),
    files = data.frame(name = "dataset.csv")
  )

  shiny::testServer(module_env$mod_time_course_server, args = list(rv = rv), {
    session$flushReact()
    markup <- paste(as.character(output$tc_summary_table), collapse = " ")
    expect_match(markup, "Not estimable — 0/2 exact; 2/2 right-censored", fixed = TRUE)
    expect_match(markup, "3.5 ± 0.5 (n=2 censored)", fixed = TRUE)
    expect_match(markup, "remained above half-maximum", fixed = TRUE)
  })
})


test_that("log time courses retain automatic and deliberate custom tick labels", {
  suppressPackageStartupMessages({
    library(shiny)
    library(dplyr)
    library(ggplot2)
  })
  module_env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "mod_time_course.R"), envir = module_env)
  values <- c(.001, .005, .01, .05, .1, .2)
  rv <- shiny::reactiveValues(
    summary = data.frame(Time = 0:5, mean_dFF0 = values, sem_dFF0 = 0,
      Group = "dataset"),
    long = NULL, metrics = data.frame(Group = "dataset", Peak_dFF0 = .2),
    groups = "dataset", colors = c(dataset = "#000000"),
    files = data.frame(name = "dataset.csv")
  )
  shiny::testServer(module_env$mod_time_course_server, args = list(rv = rv), {
    session$setInputs(tc_show_traces = FALSE, tc_show_avg_line = TRUE,
      tc_show_ribbon = FALSE, tc_log_y = TRUE, tc_y_breaks = "",
      tc_limits = FALSE, tc_tick_format = "number")
    for (blank in c("", "   ")) {
      session$setInputs(tc_y_breaks = blank)
      panel <- suppressWarnings(ggplot_build(session$getReturned()$plot()))$layout$panel_params[[1]]$y
      ticks <- is.finite(panel$get_breaks())
      expect_gte(sum(ticks), 2L)
      expect_true(all(nzchar(panel$get_labels()[ticks])))
      expect_true(all(is.finite(as.numeric(panel$get_labels()[ticks]))))
    }
    session$setInputs(tc_y_breaks = "0, 0.01, 0.05, 0.1, 0.2, -1")
    panel <- suppressWarnings(ggplot_build(session$getReturned()$plot()))$layout$panel_params[[1]]$y
    expect_equal(panel$get_breaks(), log10(c(.01, .05, .1, .2)))
    expect_equal(as.numeric(panel$get_labels()), c(.01, .05, .1, .2))
    session$setInputs(tc_tick_format = "percent")
    panel <- suppressWarnings(ggplot_build(session$getReturned()$plot()))$layout$panel_params[[1]]$y
    expect_true(all(grepl("%", panel$get_labels(), fixed = TRUE)))
    session$setInputs(tc_y_breaks = "", tc_limits = TRUE, tc_ymin = .01, tc_ymax = .2)
    panel <- suppressWarnings(ggplot_build(session$getReturned()$plot()))$layout$panel_params[[1]]$y
    expect_gte(sum(is.finite(panel$get_breaks())), 2L)
    # Nonpositive samples have no finite log transform. Expect the normal
    # ggplot/plotly warnings, without clipping or recentering the source data.
    signed <- c(-.01, 0, .01, .05, .1, .2)
    suppressWarnings({
      rv$summary$mean_dFF0 <- signed
      session$flushReact()
      panel <- ggplot_build(session$getReturned()$plot())$layout$panel_params[[1]]$y
    })
    expect_gte(sum(is.finite(panel$get_breaks())), 2L)
    expect_equal(rv$summary$mean_dFF0, signed)
  })
})
