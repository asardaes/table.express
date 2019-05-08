#' Order by clause
#'
#' Clause for ordering rows.
#'
#' @rdname order_by-table.express
#' @name order_by-table.express
#' @export
#'
#' @param .data The input data.
#' @param ... Arguments for the specific methods.
#'
order_by <- function(.data, ...) { UseMethod("order_by") }


#' @rdname order_by-table.express
#' @export
#' @importFrom rlang enexprs
#' @importFrom rlang expr
#'
#' @param .collapse Ignored.
#' @param .parse See [where-table.express].
#'
order_by.ExprBuilder <- function(.data, ..., .collapse, .parse = FALSE) {
    dots <- lapply(rlang::enexprs(...), to_expr, .parse = .parse)
    e <- rlang::expr(
        base::evalq(where(.data$chain(), order(!!!dots)))
    )

    base::eval(e)
}
