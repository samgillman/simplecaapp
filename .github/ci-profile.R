# R profile used by the GitHub Actions workflows (via R_PROFILE_USER).
#
# Pointing R_PROFILE_USER here does two jobs at once:
#  1. It replaces the repository's .Rprofile, so renv does not activate on CI
#     (renv would redirect .libPaths to an empty project library and hide the
#     packages the workflow installs).
#  2. It sets the CRAN mirror from the RSPM env var provided by
#     r-lib/actions/setup-r (use-public-rspm), which normally lives in the
#     user profile that the repository .Rprofile would shadow.
options(repos = c(CRAN = Sys.getenv("RSPM", "https://cloud.r-project.org")))
