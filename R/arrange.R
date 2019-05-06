#' @importFrom dplyr arrange
#' @export
#'
dplyr::arrange

#' Arrange rows
#'
#' Aliases for [order_by-table.express].
#'
#' @rdname arrange-table.express
#' @name arrange-table.express
#' @export
#'
#' @template data-arg
#' @param ... See [order_by-table.express].
#'
arrange.ExprBuilder <- function(.data, ...) {
    order_by.ExprBuilder(.data, ...)
}
