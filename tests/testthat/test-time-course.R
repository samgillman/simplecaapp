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
