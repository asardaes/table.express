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
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     order_by(-cyl, gear)
#'
order_by.ExprBuilder <- function(.data, ..., .collapse,
                                 .parse = getOption("table.express.parse", FALSE),
                                 .chain = getOption("table.express.chain", TRUE))
{
    where.ExprBuilder(.data, order(!!!parse_dots(.parse, ...)), .parse = FALSE, .chain = .chain)
}
