#' Order by clause
#'
#' Clause for ordering rows.
#'
#' @rdname order_by-table.express
#' @name order_by-table.express
#' @export
#'
#' @param .data The input data.
#' @template generic-dots
#'
order_by <- function(.data, ...) { UseMethod("order_by") }

#' @rdname order_by-table.express
#' @export
#' @importFrom rlang expr
#'
#' @param .collapse Ignored. See details.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' The [ExprBuilder] method dispatches to [where-table.express], but doesn't forward the `.collapse`
#' argument.
#'
order_by.ExprBuilder <- function(.data, ..., .collapse,
                                 .parse = getOption("table.express.parse", FALSE), .chain = TRUE)
{
    dots <- parse_dots(.parse, ...)

    e <- rlang::expr(
        base::evalq(where(.data, order(!!!dots), .chain = !!.chain))
    )

    base::eval(e)
}
