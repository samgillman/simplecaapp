test_that("moving and annotating a duplicate upload preserves identities and analytic values", {
  files <- data.frame(name = c("recording.csv", "recording.csv", "third_R2.csv"),
    datapath = c("a", "b", "c"))
  read <- function(path) {
    amp <- match(path, files$datapath)
    data.table::data.table(Time = pulse_time(), Cell = 100 * (1 + amp * make_pulse_trace()))
  }
  batch <- files[1:2, ]
  before <- build_processed_state(batch, read_fun = read)
  meta <- before$group_meta
  id <- meta$upload_id[2]
  expect_equal(meta$key, c("recording", "recording_1"))
  expect_equal(meta$n_cells, c(1L, 1L))
  for (edit in list(c("label", "Capsaicin_B"), c("condition", "Stimulated"),
                    c("unit_id", "Prep A"), c("color", "#334488"), c("order", "1"))) {
    meta <- edit_recording(meta, id, edit[1], edit[2])
  }
  after <- build_processed_state(batch, read_fun = read, group_meta = meta)
  for (field in c("long", "metrics", "summary", "raw_traces", "dts", "baselines")) {
    expect_identical(after[[field]], before[[field]])
  }
  expect_identical(after$groups, rev(before$groups))
  expect_equal(after$colors[["recording_1"]], "#334488")
  view <- recording_view(after)
  expect_identical(view$label(view$levels), c("Capsaicin_B", "recording"))
  mapping <- jsonlite::fromJSON(after$processing_manifest$value[
    after$processing_manifest$field == "recording_registry"])
  expect_equal(mapping$unit_id, c("", "Prep A"))
  expect_equal(mapping$condition, c("", "Stimulated"))
  expanded <- build_processed_state(files, read_fun = read, group_meta = after$group_meta)
  expect_identical(expanded$groups, c(after$groups, "third_R2"))
  expect_identical(expanded$metrics[1:2, ], before$metrics)
  expect_equal(tail(expanded$group_meta$condition, 1), "")
  retained <- recording_registry(files[-1, ], expanded$group_meta)
  expect_equal(retained$key, c("recording_1", "third_R2"))
  expect_equal(retained$label[1], "Capsaicin_B")
  expect_identical(edit_recording(retained, id, "color", "invalid"), retained)
  expect_identical(edit_recording(retained, id, "order", "3"), retained)
})

test_that("live metadata edits keep the successful processing state and its timestamp", {
  library(shiny)
  source(file.path(repo_root, "R", "mod_load_data.R"), local = TRUE)
  paths <- vapply(1:3, function(i) {
    path <- tempfile(fileext = ".csv")
    data.table::fwrite(data.frame(Time = pulse_time(), Cell = 100 + i * make_pulse_trace()), path)
    path
  }, character(1))
  on.exit(unlink(paths), add = TRUE)
  files <- data.frame(name = c("test.csv", "test.csv", "third.csv"), type = "text/csv",
    size = file.info(paths)$size, datapath = paths)
  rv <- reactiveValues()
  testServer(mod_load_data_server, args = list(rv = rv), {
    session$setInputs(upload_mode = "multi", pp_sampling_rate = 10, pp_baseline_frames = c(1, 20))
    session$setInputs(data_files = files[1:2, ])
    session$setInputs(load_btn = 1)
    before <- rv$metrics
    timestamp <- function() rv$processing_manifest$value[rv$processing_manifest$field == "processed_at_utc"]
    original_time <- timestamp()
    id <- rv$group_meta$upload_id[2]
    session$setInputs(recording_edit = list(id = id, field = "label", value = "Sample B"))
    session$setInputs(recording_edit = list(id = id, field = "order", value = "1"))
    expect_identical(process_state(), "success")
    expect_identical(rv$metrics, before)
    expect_identical(timestamp(), original_time)
    expect_identical(rv$groups, c("test_1", "test"))
    expect_match(rv$processing_manifest$value[rv$processing_manifest$field == "recording_registry"], "Sample B")
    session$setInputs(data_files = files[3, ])
    session$setInputs(load_btn = 2)
    expect_identical(recording_view(rv)$labels, c("Sample B", "test", "third"))
    expect_equal(rv$metrics[1:2, ], before)
  })
})
