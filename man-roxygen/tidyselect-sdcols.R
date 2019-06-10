#' @details
#'
#' Additionally, `.SDcols` supports [tidyselect::select_helpers], with the caveat that the
#' expression is evaluated eagerly, i.e. with the currently captured `data.table`. Consider using
#' [chain()] to explicitly capture intermediate results as actual `data.table`s.
