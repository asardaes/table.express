#' @importFrom dplyr filter
#' @export
#'
dplyr::filter

#' Filter rows
#'
#' @rdname filter-table.express
#' @name filter-table.express
#' @export
#'
#' @template data-arg
#' @param ... See [where-table.express].
#' @param .preserve Ignored.
#'
#' @details
#'
#' The [ExprBuilder] method is an alias for [where-table.express].
#'
#' The [data.table::data.table-class] method works eagerly like [dplyr::filter()].
#'
#' @template docu-examples
#'
filter.ExprBuilder <- function(.data, ..., .preserve) {
    where.ExprBuilder(.data, ...)
}

#' @rdname filter-table.express
#' @export
#' @importFrom rlang caller_env
#'
filter.data.table <- function(.data, ...) {
    eb <- ExprBuilder$new(.data)
    lazy_ans <- filter.ExprBuilder(eb, ...)
    end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env())
}
