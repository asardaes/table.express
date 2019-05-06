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
#' Adds a new frame to the call chain, i.e. another call to [`[`][base::Extract].
#'
chain.ExprBuilder <- function(.data, ...) {
    ExprBuilder$new(.data)
}
