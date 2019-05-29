#' Chain
#'
#' Build a chain of similar objects/operations.
#'
#' @export
#'
#' @param .data Object to be chained.
#' @template generic-dots
#'
chain <- function(.data, ...) {
    UseMethod("chain")
}

#' @rdname chain
#' @export
#' @importFrom rlang caller_env
#'
#' @param .parent_env See [end_expr()].
#'
#' @details
#'
#' The chaining for [ExprBuilder] is equivalent to calling [end_expr()] followed by [start_expr()].
#' The ellipsis (`...`) is passed to both functions.
#'
#' @template docu-examples
#'
chain.ExprBuilder <- function(.data, ..., .parent_env = rlang::caller_env()) {
    start_expr(end_expr.ExprBuilder(.data, ..., .parent_env = .parent_env), ...)
}
