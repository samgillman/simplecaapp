# Auto-sourced by testthat before the tests run.
# Locates the repo root relative to this directory so the suite works whether
# it is started from the repo root (Rscript tests/testthat.R) or via test_dir().
repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(repo_root, "R", "utils.R"))

# Synthetic ground-truth trace used across metric tests.
#
# Sampling: dt = 0.1 s, frame i is at time (i - 1) * 0.1.
# - Frames 1-20:  baseline (0 by default; callers may substitute noise)
# - Frames 21-30: linear rise 0.1, 0.2, ..., 1.0  (peak = 1.0 at frame 30, t = 2.9)
# - Frames 31-40: linear decay 0.9, 0.8, ..., 0.0
# - Frames 41-60: flat 0
#
# Exact expectations (piecewise-linear, so trapezoid/interpolation are exact):
#   Peak_dFF0 = 1.0, Time_to_Peak = 2.9
#   Time to 10% = 2.0, 25% = 2.15, 50% = 2.4, 75% = 2.65, 90% = 2.8
#   Rise_Time (10-90%) = 0.8, Calcium_Entry_Rate = 0.8 * 1.0 / 0.8 = 1.0
#   FWHM = 3.4 - 2.4 = 1.0, Half_Width = 0.5
#   AUC = rise triangle (0.5) + decay triangle (0.5) = 1.0
make_pulse_trace <- function(baseline_vals = rep(0, 20)) {
  stopifnot(length(baseline_vals) == 20)
  c(baseline_vals, seq(0.1, 1.0, by = 0.1), seq(0.9, 0.0, by = -0.1), rep(0, 20))
}

pulse_time <- function(n = 60) seq(0, by = 0.1, length.out = n)
