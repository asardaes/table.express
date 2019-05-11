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
#' The [ExprBuilder] method adds another frame to the query.
#'
chain.ExprBuilder <- function(.data, ...) {
    .data$chain()
}
