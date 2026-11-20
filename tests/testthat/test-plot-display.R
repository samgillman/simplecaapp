test_that("large heatmaps bound their tiles and retain every cell and full-data scale", {
  y <- rep(0, 3000)
  y[1500] <- 12
  y[1501] <- -6
  dt <- data.table::as.data.table(setNames(rep(list(y), 500), sprintf("Cell%03d", 1:500)))
  dt[, Time := 0:2999]
  data.table::setcolorder(dt, c("Time", setdiff(names(dt), "Time")))
  before <- data.table::copy(dt)
  display <- heatmap_display_field(dt, "field", "orig")
  expect_true(display$reduced)
  expect_lte(nrow(display$data), 100000)
  expect_equal(length(unique(display$data$Cell_Label)), 500L)
  expect_equal(display$range, c(-6, 12))
  expect_equal(range(c(display$data$Time_Min, display$data$Time_Max)), c(0, 2999))
  expect_equal(dt, before)
})

test_that("time-bin means preserve empty intervals and missing cells on irregular time", {
  dt <- data.table::data.table(Time = c(0, 1, 2, 8, 9, 10),
    A = c(0, 2, 4, 6, 8, 10), B = c(NA, NA, NA, 10, 10, 10))
  display <- heatmap_display_field(dt, "field", "orig", max_tiles = 8L)
  expect_equal(display$bins, 4L)
  expect_equal(display$data$Value[display$data$Cell_Label == "A"], c(2, NA, NA, 8))
  expect_equal(display$data$Value[display$data$Cell_Label == "B"], c(NA, NA, NA, 10))
  # Sorting uses the actual post-baseline peak, not its diluted bin mean.
  dt <- data.table::data.table(Time = 0:99, narrow = c(rep(0, 51), 10, rep(0, 48)), broad = rep(2, 100))
  display <- heatmap_display_field(dt, "field", "amp", c(1, 2), max_tiles = 20)
  expect_identical(display$data$Cell_Label[1], "narrow")
})

test_that("trace reduction retains peaks, troughs, endpoints and all recording-cell pairs", {
  y <- rep(0, 3000)
  y[1500] <- 12
  y[1501] <- -6
  long <- data.frame(Time = rep(0:2999, 1000), dFF0 = rep(y, 1000),
    Group = rep(c("A", "B"), each = 1500000),
    Cell = rep(rep(sprintf("Cell%03d", 1:500), 2), each = 3000))
  display <- timecourse_display_data(long)
  expect_true(attr(display, "display_reduced"))
  expect_lte(nrow(display), 100000L)
  expect_equal(attr(display, "source_cells"), 1000L)
  peaks <- tapply(display$dFF0, interaction(display$Group, display$Cell), max)
  troughs <- tapply(display$dFF0, interaction(display$Group, display$Cell), min)
  expect_true(all(peaks == 12))
  expect_true(all(troughs == -6))
  expect_true(all(tapply(display$Time, interaction(display$Group, display$Cell), min) == 0))
  expect_true(all(tapply(display$Time, interaction(display$Group, display$Cell), max) == 2999))
  expect_equal(nrow(long), 3000000L)
  expect_equal(long$dFF0[1500:1501], c(12, -6))
})

test_that("small traces remain exact and reduced missing bins remain gaps", {
  long <- data.frame(Time = 0:99, dFF0 = sin(0:99), Group = "A", Cell = "Cell1")
  long$dFF0[40] <- NA_real_
  exact <- timecourse_display_data(long)
  expect_false(attr(exact, "display_reduced"))
  expect_equal(exact$Time, long$Time)
  expect_equal(exact$dFF0, long$dFF0)
  reduced <- timecourse_display_data(long, max_points = 20)
  expect_true(all(is.na(reduced$dFF0[reduced$Time >= 20 & reduced$Time < 40])))
  expect_lte(nrow(reduced), 20L)
})
