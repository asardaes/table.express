#' Chain
#'
#' Build a chain of similar objects/operations.
#'
#' @export
#'
#' @param .data Object to be chained.
#' @template generic-dots
#'
chain <- function(.data, ...) { UseMethod("chain") }

#' @rdname chain
#' @export
#'
#' @details
#'
#' The chaining for [ExprBuilder] is equivalent to calling [end_expr()] followed by [start_expr()].
#' The ellipsis (`...`) is passed to both functions.
#'
chain.ExprBuilder <- function(.data, ...) {
    start_expr(end_expr.ExprBuilder(.data, ...), ...)
}
