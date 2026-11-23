# Cell-by-cell check of the response engine against an independent calculation,
# for recordings kept outside the repository. Run from the repository root:
#   Rscript tests/reference-recordings.R <reference directory>
# The directory has the layout of tests/fixtures/response-protocols/reference-recordings
# (see tests/testthat/helper-reference-recordings.R). Needs base R and jsonlite only.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(args[1])) stop("Give one reference directory.")
for (file in c("utils.R", "response_protocols.R", "responders.R")) source(file.path("R", file))
source(file.path("tests", "testthat", "helper-reference-recordings.R"))

check <- reference_recordings_check(args[1])
cat("Response engine", responder_engine_version(), "on", R.version.string, "\n")
print(check$summary, row.names = FALSE)
if (nrow(check$differences)) {
  print(check$differences, row.names = FALSE)
  stop(nrow(check$differences), " cell comparisons differ from the expected calls.")
}
cat(sprintf("All %d cell comparisons agree with the expected calls.\n", sum(check$summary$Cells)))
