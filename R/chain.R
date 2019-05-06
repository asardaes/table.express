#' Chain anything
#'
#' Build a chain of similar objects/operations.
#'
#' @export
#'
#' @param .data Object to be chained.
#' @param ... Arguments for the specific methods.
#'
chain <- function(.data, ...) { UseMethod("chain") }

#' @rdname chain
#' @export
#'
#' @details
#'
#' The [ExprBuilder] method is equivalent to calling [end_expr()] and then [start_expr()] again.
#'
chain.ExprBuilder <- function(.data, ...) {
    start_expr(end_expr(.data, ...))
}
