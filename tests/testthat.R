# Run the unit test suite from the repository root:
#   Rscript tests/testthat.R
library(testthat)

test_dir("tests/testthat", stop_on_failure = TRUE)
