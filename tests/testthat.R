# Run the unit test suite from the repository root:
#   Rscript tests/testthat.R
library(testthat)

results <- test_dir("tests/testthat", stop_on_failure = TRUE)
result_table <- as.data.frame(results)
if (any(result_table$skipped)) {
  skipped <- result_table$test[result_table$skipped]
  stop("Unexpected skipped tests: ", paste(skipped, collapse = "; "))
}
