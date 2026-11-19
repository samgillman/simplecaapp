source(file.path(repo_root, "R", "responders.R"))

plot_fixture <- function(key = "a", scale = 1) {
  dt <- data.frame(Time = 0:15, Cell = c(rep(1, 5), rep(1 + .4 * scale, 4), rep(1, 7)), Flat = 1)
  r <- responder_field_results(dt, c("Cell", "Flat"), key, responder_protocol("A", 5, 8, 3, .1, 3, 2, NULL))
  r$calls$Key <- paste(r$calls$Field, r$calls$Cell, r$calls$Role, sep = ":")
  list(dt = dt, calls = r$calls, counts = r$counts, bundle = responder_trace_bundle(dt, r$calls))
}

test_that("trace bundles use local units and retain per-recording identities", {
  a <- plot_fixture(); b <- plot_fixture("b", 2)
  expect_equal(unname(a$bundle$local[6, match("Cell", a$bundle$rows$Cell)]), .2)
  expect_equal(unname(b$bundle$local[6, match("Cell", b$bundle$rows$Cell)]), .4)
  expect_length(intersect(a$bundle$rows$Key, b$bundle$rows$Key), 0)
  expect_true(all(is.na(a$bundle$summary$SEM)))
  limits <- responder_trace_limits(list(a$bundle, b$bundle), rbind(a$counts, b$counts))
  expect_gt(limits$y[2], .4)
  expect_equal(responder_trace_limits(list(a$bundle), a$counts, TRUE)$x, c(0, 15))
  xy <- responder_trace_points(c(0, 1, 4, 5), c(0, NA, 1, 2), 1.5)
  expect_equal(xy$x, c(0, 1, NA, 4, 5))
  expect_equal(xy$y, c(0, NA, NA, 1, 2))
})

test_that("individual trajectories keep keys and summary curves cannot select cells", {
  a <- plot_fixture(); g <- a$bundle
  build <- function(mode, statuses = c("Responded", "Did not respond")) plotly::plotly_build(
    responder_trace_plot(g, a$counts, "<field>", responder_trace_limits(list(g), a$counts), mode, statuses))$x$data
  individual <- build("individual")
  expect_length(individual, 2)
  expect_setequal(unlist(lapply(individual, function(t) unique(t$key))), g$rows$Key)
  expect_true(all(vapply(individual, function(t) identical(t$connectgaps, FALSE), logical(1))))
  expect_true(all(vapply(individual, function(t) all(grepl("&lt;field&gt;", t$text)), logical(1))))
  means <- build("mean")
  expect_true(all(vapply(means, function(t) is.null(t$key), logical(1))))
  expect_false(any(vapply(means, function(t) identical(t$meta$kind, "sem"), logical(1))))
  expect_length(build("individual", "Responded"), 1)
  expect_equal(a$counts$Percent_responding, 50)
})

test_that("recording clicks select the right repeated cell and preserve scientific counts", {
  library(shiny)
  source(file.path(repo_root, "R", "mod_in_vitro.R"), local = TRUE)
  a <- plot_fixture()
  files <- data.frame(name = c("same.csv", "same.csv"), datapath = c("a", "b"))
  state <- build_processed_state(files, list(baseline_frames = c(1, 4)), read_fun = function(path) data.table::as.data.table(a$dt))
  rv <- do.call(reactiveValues, state[processed_state_fields])
  sent <- new.env(parent = emptyenv())
  mock <- MockShinySession$new()
  mock$sendInputMessage <- function(inputId, message) sent[[inputId]] <- message
  testServer(mod_in_vitro_server, args = list(rv = rv), session = mock, {
    session$setInputs(stimulus = "A", start = 5, end = 8, baseline = 3, floor = .1,
      noise = 3, frames = 2, auto_gap = TRUE, control = FALSE,
      trace_status = c("Responded", "Did not respond"), trace_mode = "individual")
    expect_length(trace_fields(), 2)
    expect_type(output$calls, "character")
    before <- export_rows("calls")
    expect_match(output$population_plot$html, "50.0% responding", fixed = TRUE)
    expect_match(output$population_plot$html, "same.csv", fixed = TRUE)
    x <- evaluable(); key <- x$Key[x$Field == "same_1" & x$Cell == "Cell"]
    expect_true(select_trace_key(key))
    expect_equal(selected_call()$Field, "same_1")
    expect_equal(selected_call()$Cell, "Cell")
    expect_length(trace_fields(), 2)
    expect_false(select_trace_key(key, "same"))
    expect_false(select_trace_key("stale:key:Stimulus"))
    session$setInputs(trace_recording = "__all__")
    selected_field("same"); session$flushReact()
    session$setInputs(focus_recording = "same_1")
    expect_equal(sent$trace_recording$value, "same_1")
    expect_equal(field(), "same_1")
    session$setInputs(trace_status = "Responded", trace_recording = "same_1", trace_mode = "both")
    expect_equal(trace_fields(), "same_1")
    expect_equal(export_rows("calls"), before)
    expect_match(output$population_label, "showing 1 of 2")
    widget <- jsonlite::fromJSON(output[[panel_id("same_1")]], simplifyVector = FALSE)
    expect_true(length(widget$x$data) > 0L)
    session$setInputs(trace_status = character())
    expect_match(output$population_plot$html, "Choose a response class")
    session$setInputs(baseline = 6)
    expect_match(output$population_plot$html, "No evaluable cells")
    session$setInputs(baseline = 3, trace_status = "Responded")
    expect_match(output$population_plot$html, "plotly")
    # Replacing a batch must not send a renderUI(NULL) payload to a plotly
    # binding that remains mounted in a hidden tab until its parent refreshes.
    replacement <- build_processed_state(data.frame(name = "new.csv", datapath = "c"),
      list(baseline_frames = c(1, 4)), read_fun = function(path) data.table::as.data.table(a$dt))
    commit_processed_state(rv, replacement)
    session$flushReact()
    expect_error(output[[panel_id("same_1")]], class = "shiny.silent.error")
    expect_match(output$population_plot$html, "new.csv", fixed = TRUE)
    expect_match(output$cards$html, ">new</button>", fixed = TRUE)
  })
})

test_that("individual paths retain missing samples and acquisition gaps", {
  a <- plot_fixture(); g <- a$bundle
  r <- g$rows[1, ]; r$Cell <- "Other"; r$Key <- "a:Other:Stimulus"
  g$rows <- rbind(g$rows, r)
  g$local <- cbind(g$local, g$local[, 1])
  g$local[12, 1] <- NA_real_
  g$time[13:16] <- g$time[13:16] + 4
  p <- plotly::plotly_build(responder_trace_plot(g, a$counts, "a",
    responder_trace_limits(list(g), a$counts)))$x$data[[1]]
  expect_equal(as.character(p$key), g$rows$Key[1])
  expect_gte(sum(is.na(p$y)), 2L)
  expect_true(all(diff(p$x[is.finite(p$x)]) > 0))
})
