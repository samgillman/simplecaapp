# Full application startup smoke test used by CI.
app_env <- new.env(parent = globalenv())
app <- source("app.R", local = app_env)$value

stopifnot(
  inherits(app, "shiny.appobj"),
  is.function(app$serverFuncSource()),
  !is.null(app$httpHandler)
)

cat("SimpleCa app object initialized successfully\n")
