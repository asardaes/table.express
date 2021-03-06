#' Start expression
#'
#' Start building an expression.
#'
#' @export
#'
#' @param .data Optionally, something to capture for the expression.
#' @template generic-dots
#'
start_expr <- function(.data, ...) {
    UseMethod("start_expr")
}

#' @rdname start_expr
#' @export
#' @param .verbose Whether to print more information during the expression-building process.
#'
#' @details
#'
#' The [data.table::data.table-class] method returns an [ExprBuilder] instance.
#'
#' @template docu-examples
#'
start_expr.data.table <- function(.data, ..., .verbose = getOption("table.express.verbose", FALSE)) {
    ExprBuilder$new(.data, verbose = .verbose)
}
