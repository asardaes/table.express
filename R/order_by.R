#' Order by clause
#'
#' Clause for ordering rows.
#'
#' @export
#'
#' @param .data Something resembling a `data.frame`.
#' @param ... Arguments for the specific methods.
#'
order_by <- function(.data, ...) { UseMethod("order_by") }

#' Order by clause
#'
#' Clause for ordering rows of a [data.table::data.table-class].
#'
#' @rdname order_by-table.express
#' @name order_by-table.express
#' @export
#' @importFrom rlang enquos
#' @importFrom rlang expr
#' @importFrom rlang quo_squash
#'
#' @template data-arg
#' @param ... The columns to sort by.
#' @param .collapse Ignored.
#' @param .parse See [where-table.express].
#'
order_by.ExprBuilder <- function(.data, ..., .collapse, .parse = FALSE) {
    next_builder <- if (is.null(.data$where)) .data else chain(.data)

    dots <- lapply(rlang::enquos(...), to_expr, .parse = .parse)
    e <- rlang::expr(
        base::evalq(where(next_builder, order(!!!dots)))
    )

    e <- rlang::quo_squash(e)
    base::eval(e)
}

#' @rdname order_by-table.express
#' @export
#' @importFrom rlang caller_env
#'
order_by.data.table <- function(.data, ...) {
    order_by.ExprBuilder(ExprBuilder$new(.data, rlang::caller_env()), ...)
}
