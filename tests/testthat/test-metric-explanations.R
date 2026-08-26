library(shiny)
library(htmltools)

source(file.path(repo_root, "R", "components.R"))
source(file.path(repo_root, "R", "metrics_explanation_content.R"))
source(file.path(repo_root, "R", "mod_metrics_explained.R"))

explanation_markup <- function(metric) {
  as.character(get_metric_explanation_content(metric, shiny::NS("explain")))
}

test_that("metric explanations use scientifically bounded terminology", {
  time_text <- explanation_markup("time_to_peak")
  auc_text <- explanation_markup("auc")
  rate_text <- explanation_markup("ca_entry_rate")
  width_text <- explanation_markup("fwhm")

  expect_match(time_text, "time coordinate", fixed = TRUE)
  expect_match(time_text, "not a stimulus-to-peak latency", fixed = TRUE)
  expect_match(auc_text, "signed net integral", fixed = TRUE)
  expect_match(auc_text, "values below zero contribute negatively", ignore.case = TRUE)
  expect_match(rate_text, "fluorescence-kinetics metric", fixed = TRUE)
  expect_match(rate_text, "not a direct measurement of calcium influx", fixed = TRUE)
  expect_match(width_text, "Derived Half-Width", fixed = TRUE)
  expect_match(width_text, "not a separately measured peak-to-crossing interval", fixed = TRUE)
})

test_that("explanation peak lookup follows the stored post-baseline peak", {
  trace <- data.frame(
    Time = 0:5,
    dFF0 = c(3, 0, 0.2, 0.6, 1.0, 0.2)
  )

  expect_identical(which.max(trace$dFF0), 1L)
  expect_identical(metric_explanation_peak_index(trace, peak_time = 4), 5L)
})

test_that("baseline explanation counts only observed values in the selected frames", {
  trace <- data.frame(
    Time = 10:15,
    dFF0 = c(NA_real_, -0.1, 0.1, 0.5, 1, 0)
  )
  details <- metric_explanation_baseline_details(trace, c(1, 3))

  expect_identical(details$indices, 1:3)
  expect_identical(details$observed_n, 2L)
  expect_equal(details$observed_values, c(-0.1, 0.1))
})

test_that("AUC explanation counts the same adjacent observed intervals as metrics", {
  trace <- data.frame(
    Time = 0:4,
    dFF0 = c(0, 1, NA_real_, 2, 1)
  )
  details <- metric_explanation_auc_details(trace)

  expect_identical(details$valid_pairs, c(TRUE, FALSE, FALSE, TRUE))
  expect_identical(details$interval_count, 2L)
  expect_equal(details$mean_interval, 1)
})

test_that("user-facing metric labels describe derived quantities", {
  expect_match(metric_label("Calcium_Entry_Rate"), "10–90% Rise Rate", fixed = TRUE)
  expect_match(metric_title("Calcium_Entry_Rate"), "ΔF/F₀ Rise Rate", fixed = TRUE)
  expect_match(metric_label("Half_Width"), "Derived Half-Width", fixed = TRUE)
  expect_match(metric_label("AUC"), "Signed Net AUC", fixed = TRUE)
})

test_that("unsupported interpretations are absent from metric UI sources", {
  files <- c(
    "metrics_explanation_content.R", "mod_metrics_explained.R",
    "mod_metrics.R", "mod_time_course.R", "plot_controls.R"
  )
  source_text <- paste(vapply(
    file.path(repo_root, "R", files),
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  ), collapse = "\n")

  expect_false(grepl("Strong cumulative response", source_text, fixed = TRUE))
  expect_false(grepl("Response latency:", source_text, fixed = TRUE))
  expect_false(grepl("Calcium Entry Rate =", source_text, fixed = TRUE))
  expect_false(grepl("Half Width (HWHM)", source_text, fixed = TRUE))
})
