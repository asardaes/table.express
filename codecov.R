library("covr")
covr::codecov(type = "tests", quiet = FALSE)

#' to run locally:
#'
#' devtools::test_coverage(quiet=F)
