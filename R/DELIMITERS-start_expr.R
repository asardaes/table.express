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
#'
#' @details
#'
#' The [data.table::data.table-class] method returns an [ExprBuilder] instance.
#'
#' @template docu-examples
#'
start_expr.data.table <- function(.data, ...) {
    ExprBuilder$new(.data)
}
