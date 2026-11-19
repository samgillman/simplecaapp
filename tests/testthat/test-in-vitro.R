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
  expect_equal(rs$counts$Unscorable, c(0, 1, 0))
  expect_equal(rs$counts$Effective_end_s[1], 7.5)
  expect_match(rs$counts$Field_reason[3], "KCl control: Recording")
  expect_match(r$counts$Field_reason[3], "No KCl responders")
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
    # Each field has one cell in at least one response class (undefined SEM).
    expect_type(output$population_plot$src, "character")
    expect_type(output$trace$src, "character")
    expect_match(output$headline$html, "3 of 4 evaluable cells")
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

test_that("time evidence distinguishes sustained rises, tied peaks, noise and single spikes", {
  t <- seq(0, 8, .5)
  dt <- data.frame(Time = t, Run = 0, Spike = 0, Noisy = 0)
  dt$Run[t >= 5] <- c(0, .12, .14, .13, 0, .25, .25)
  dt$Spike[t == 7.5] <- .25
  dt$Noisy[t >= 3 & t < 5] <- c(-.1, .1, -.1, .1)
  p <- responder_protocol("A", 5, 8, 2, .1, 3, 2, NULL)
  x <- responder_field_results(dt, names(dt)[-1], "A", p)$calls
  expect_equal(x$Status, c("Responded", "Did not respond", "Did not respond"))
  expect_equal(x$Threshold_source, c("Floor", "Floor", "Baseline noise"))
  expect_equal(x$Floor_threshold, rep(.1, 3))
  expect_equal(x$Noise_threshold, c(0, 0, 3 * sd(c(-.1,.1,-.1,.1))))
  expect_equal(x$Margin[1:2], c(2.5, 2.5))
  expect_equal(x$Onset_s, c(5.5, NA, NA))
  expect_equal(x$Run_start_s, x$Onset_s)
  expect_equal(x$Run_end_s, c(6.5, NA, NA))
  expect_equal(x$Peak_s, c(7.5, 7.5, 5))
  p$Floor_local_fraction <- x$Noise_threshold[3]
  tie <- responder_field_results(dt, "Noisy", "A", p)$calls
  expect_equal(tie$Threshold_source, "Floor")
})

test_that("only a right endpoint within the resolved gap can be clipped", {
  assess <- function(end, step = 1, gap = NULL) {
    t <- seq(0, 299, step)
    p <- responder_protocol("Stimulus", 20, end, 10, .1, 3, 2, gap)
    responder_field_results(data.frame(Time = t, A = .2 * (t >= 20)), c("A", "invalid"), "F", p)
  }
  expect_false(assess(299)$counts$Window_clipped)
  for (end in c(300, 300.5)) {
    r <- assess(end)
    expect_equal(r$counts$Responded, 1)
    expect_equal(r$counts$Effective_end_s, 299)
    expect_equal(r$calls$End_s, rep(end, 2))
    expect_match(r$counts$Window_warning, "clipped")
    expect_equal(r$counts$Field_reason, "")
  }
  for (res in list(assess(302), assess(300, .5), assess(300, 1, .9))) {
    expect_false(res$counts$Window_clipped)
    expect_true(is.na(res$counts$Effective_end_s))
    expect_equal(c(res$counts$Unscorable, res$counts$Excluded), c(1,1))
    expect_match(res$counts$Field_reason, "span")
  }
  expect_true(assess(299.5, .5)$counts$Window_clipped)
  expect_true(assess(300, 1, 1)$counts$Window_clipped)
})

test_that("suggestions use observed clocks and field summaries preserve denominators", {
  first <- data.frame(Time = seq(0, 22, .5))
  second <- data.frame(Time = 5:39)
  expect_equal(responder_defaults(list(first), c(1,20)), c(start=9.5, end=22, baseline=9.5))
  expect_equal(responder_defaults(list(first, second), c(1,20)), c(start=24, end=22, baseline=9.5))
  expect_equal(responder_defaults(list(second), c(3,9)), c(start=13, end=39, baseline=6))
  x <- data.frame(Responded=c(1,9,0), Nonresponders=c(1,1,0), Evaluable=c(2,10,0),
    Excluded=0, Unscorable=c(0,0,1), Percent_responding=c(50,90,NA))
  s <- responder_summary(x)
  expect_equal(unname(s$pooled), 100 * 10/12)
  expect_equal(c(s$mean, s$sd, s$n), c(70, sd(c(50,90)), 2))
  expect_true(is.na(responder_summary(x[3, ])$pooled))
  m <- cbind(c(1, 2, NA), c(3, NA, NA), c(2, 2, 2))
  st <- responder_trace_summary(1:3, m, c("Responded","Responded","Did not respond"))
  st <- st[st$Status == "Responded", ]
  expect_equal(st$Mean, c(2,2,NA))
  expect_equal(st$SEM, c(1,NA,NA))
  expect_equal(st$N, c(2,1,0))
})
