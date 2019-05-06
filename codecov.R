library("covr")
covr::codecov(type = "tests", quiet = FALSE)

#' to run locally:
#'
#' Sys.setenv(NOT_CRAN = "true"); covr::report(covr::package_coverage(type = "tests", quiet = FALSE))
