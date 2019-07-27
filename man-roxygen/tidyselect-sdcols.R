#' @details
#'
#' Additionally, `.SDcols` supports:
#'
#' - [tidyselect::select_helpers]
#' - A predicate using the `.COL` pronoun that should return a single logical when `.COL` is
#'   replaced by a *column* of the data.
#' - A formula using `.` or `.x` instead of the aforementioned `.COL`.
#'
#' The caveat is that the expression is evaluated eagerly, i.e. with the currently captured
#' `data.table`. Consider using [chain()] to explicitly capture intermediate results as actual
#' `data.table`s.
