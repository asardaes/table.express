#' @importFrom dplyr arrange
#' @export
#'
dplyr::arrange

#' Arrange rows
#'
#' Alias for [order_by-table.express].
#'
#' @rdname arrange-table.express
#' @name arrange-table.express
#' @export
#'
#' @template data-arg
#' @param ... See [order_by-table.express].
#'
#' @template docu-examples
#'
arrange.ExprBuilder <- function(.data, ...) {
    order_by.ExprBuilder(.data, ...)
}

#' @rdname arrange-table.express
#' @export
#' @importFrom rlang caller_env
#'
arrange.data.table <- function(.data, ...) {
    eb <- ExprBuilder$new(.data)
    lazy_ans <- order_by.ExprBuilder(eb, ...)
    try_delegate("arrange", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
}
