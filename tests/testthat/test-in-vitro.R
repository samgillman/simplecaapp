source(file.path(repo_root, "R", "responders.R"))

vitro_test_protocol <- function() {
  responder_protocol("Capsaicin", 5, 8, 2, 0.1, 3, 2, 0.6, TRUE, 12, 15)
}

test_that("field counts include flat cells and explicitly track invalid and missing traces", {
  time <- seq(0, 15, 0.5)
  dt <- data.frame(Time = time, Both = 0, Spike = 0, Flat = 0, KCl_only = 0, Missing = 0)
  dt$Both[time %in% c(5, 5.5, 12, 12.5)] <- 0.1
  dt$KCl_only[time %in% c(14.5, 15)] <- 0.2
  dt$Spike[time == 6] <- 1
  dt$Missing[time == 6] <- NA_real_
  dt$Missing[time %in% c(12, 12.5)] <- 1
  result <- responder_field_results(dt, c(names(dt)[-1], "InvalidF0"), "Coverslip A", vitro_test_protocol())
  counts <- result$counts
  expect_equal(counts$Registered, c(6, 6, 3))
  expect_equal(counts$Responded, c(1, 3, 1))
  expect_equal(counts$Evaluable, c(4, 5, 2))
  expect_equal(counts$Percent_responding, c(25, 60, 50))
  expect_equal(counts$Excluded, c(1, 1, 0))
  expect_equal(counts$Unscorable, c(1, 0, 1))
  expect_equal(result$calls$Status[result$calls$Cell == "Spike"], rep("Did not respond", 2))
  expect_equal(result$calls$Status[result$calls$Cell == "InvalidF0"], rep("Excluded", 2))
})

test_that("local elevation, baseline variation and gaps affect only the relevant window", {
  time <- seq(0, 15, 0.5)
  dt <- data.frame(Time = time, Drift = rep(1, length(time)), Noisy = 0)
  dt$Noisy[time >= 3 & time < 5] <- c(-0.1, 0.1, -0.1, 0.1)
  dt$Noisy[time >= 5 & time <= 8] <- 0.15
  r <- responder_field_results(dt, c("Drift", "Noisy"), "A", vitro_test_protocol())
  expect_equal(r$counts$Responded, c(0, 0, 0))
  expect_true(is.na(r$counts$Percent_responding[3]))
  expect_gt(r$calls$Threshold_increase[r$calls$Role == "Stimulus" & r$calls$Cell == "Noisy"], 0.15)
  gap <- dt[time != 5.5, ]
  rg <- responder_field_results(gap, "Drift", "A", vitro_test_protocol())
  expect_equal(rg$counts$Evaluable, c(0, 1, 0))
  expect_true(is.na(rg$counts$Percent_responding[1]))
  short <- dt[time < 8, ]
  rs <- responder_field_results(short, "Drift", "A", vitro_test_protocol())
  expect_equal(rs$counts$Unscorable, c(1, 1, 0))
  expect_error(responder_protocol("KCl", 1, 2, 3, .1, 3, 1, 1, TRUE, 10, 12), "KCl")
  expect_error(responder_protocol("A", 10, 12, 5, .1, 3, 1, 1, TRUE, 14, 17), "Separate")
  expect_error(responder_protocol("A", NA_real_, 12, 5, .1, 3, 2, 1), "start")
})

test_that("real processing retains invalid F0 names and the module refreshes with data and settings", {
  library(shiny)
  source(file.path(repo_root, "R", "mod_in_vitro.R"), local = TRUE)
  time <- seq(0, 15, 0.5)
  raw <- data.table::data.table(Time = time, Cell1 = 100 + 40 * (time >= 5 & time <= 8),
                               Cell2 = 100, InvalidF0 = 0)
  files <- data.frame(name = c("field.csv", "field.csv"), datapath = c("a", "b"))
  state <- build_processed_state(files, list(baseline_frames = c(1, 4)),
    read_fun = function(path) { x <- data.table::copy(raw); if (path == "b") x$Cell2 <- x$Cell1; x })
  rv <- do.call(reactiveValues, state[processed_state_fields])
  testServer(mod_in_vitro_server, args = list(rv = rv), {
    session$setInputs(stimulus = "Capsaicin", start = 5, end = 8, baseline = 2, floor = .1,
      noise = 3, frames = 2, gap = .6, control = FALSE, field = "field")
    expect_equal(results()$counts$Percent_responding, c(50, 100))
    expect_equal(results()$counts$Excluded, c(1, 1))
    expect_equal(export_rows("calls")$Source_file, rep("field.csv", 6))
    library(ggplot2)
    session$setInputs(control = TRUE, control_start = 12, control_end = 15)
    windows <- trace_plot(time, raw$Cell1, "Preview")$layers[[1]]$data
    expect_equal(windows$Start[windows$Role == "Local baseline"], c(3, 10))
    expect_equal(windows$End[windows$Role == "Local baseline"], c(5, 12))
    session$setInputs(control = FALSE)
    session$setInputs(field = "field_1", cell = "Cell2")
    expect_equal(field(), "field_1")
    clear_processed_state(rv)
    session$flushReact()
    commit_processed_state(rv, state)
    session$flushReact()
    expect_equal(field(), "field_1")
    expect_equal(nrow(overview_data()), length(time))
    session$setInputs(status = "Excluded")
    expect_true(all(review_rows()$Status == "Excluded"))
    session$setInputs(floor = 1)
    expect_equal(results()$counts$Percent_responding, c(0, 0))
    session$setInputs(start = 9)
    expect_error(results(), "start")
    clear_processed_state(rv)
    session$flushReact()
    expect_null(fields())
    expect_error(results(), class = "shiny.silent.error")
  })
})

test_that("automatic timing accommodates slow acquisitions and explains failures", {
  assess <- function(time, start, end, baseline = 12, gap = NULL) {
    p <- responder_protocol("Stimulus", start, end, baseline, .1, 3, 2, gap)
    dt <- data.frame(Time = time, Cell = ifelse(time >= start, .12, 0))
    responder_field_results(dt, "Cell", "Field", p)$calls
  }
  for (interval in c(2, 3.03, 1.03)) {
    call <- assess((0:40) * interval, 15, 25)
    expect_equal(call$Status, "Responded")
    expect_equal(call$Max_gap_s, 1.5 * interval)
  }
  expect_match(assess(0:40, 5, 15)$Reason, "span")
  expect_match(assess(0:40, 12, 18, 2)$Reason, "three prestimulus")
  expect_match(assess(0:40, 12, 12.2)$Reason, "Too few response")
  expect_match(assess((0:40)[-16], 12, 20)$Reason, "gap exceeds")
})

test_that("a late 12 percent rise survives bleaching in both input modes", {
  files <- data.frame(name = "field.csv", datapath = "test")
  t <- 0:50
  fluorescence <- ifelse(t < 20, 100, 66)
  fluorescence[t %in% c(10, 11, 40, 41)] <- fluorescence[t %in% c(10, 11, 40, 41)] * 1.12
  p <- responder_protocol("Capsaicin", 10, 15, 5, .1, 3, 2, NULL, TRUE, 40, 45)
  for (mode in c("raw_fluorescence", "dff0")) {
    f <- if (mode == "dff0") fluorescence / 100 - 1 else fluorescence
    state <- build_processed_state(files, list(input_data_mode = mode, baseline_frames = c(1, 4)),
      read_fun = function(path) data.table::data.table(Time = t, A = f, Blank = NA_real_))
    r <- responder_field_results(state$dts[[1]], "A", "Field", p)
    expect_equal(r$calls$Peak_increase, c(.12, .12))
    expect_equal(r$calls$Status, rep("Responded", 2))
    expect_match(state$import_audit[[1]]$note, "Blank")
  }
  invalid <- responder_field_results(data.frame(Time = t, A = -1), "A", "Field", p)
  expect_true(all(invalid$calls$Status == "Unscorable"))
})
