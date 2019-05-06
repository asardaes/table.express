#' @importFrom dplyr filter
#' @export
#'
dplyr::filter

#' Filter rows
#'
#' Aliases for [where-table.express].
#'
#' @rdname filter-table.express
#' @name filter-table.express
#' @export
#'
#' @template data-arg
#' @param ... See [where-table.express].
#' @param .preserve Ignored.
#'
filter.ExprBuilder <- function(.data, ..., .preserve) {
    where.ExprBuilder(.data, ...)
}

#' @rdname filter-table.express
#' @export
#' @importFrom rlang caller_env
#'
filter.data.table <- function(.data, ..., .preserve) {
    where.ExprBuilder(ExprBuilder$new(.data, rlang::caller_env()), ...)
}
