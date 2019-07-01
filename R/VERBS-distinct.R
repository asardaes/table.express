#' @importFrom dplyr distinct
#' @export
#'
dplyr::distinct

#' Rows with distinct combinations of columns
#'
#' @rdname distinct-table.express
#' @name distinct-table.express
#' @export
#' @importFrom rlang expr
#'
#' @template data-arg
#' @param ... Which columns to check for uniqueness.
#' @template parse-arg
#'
#' @template docu-examples
#'
distinct.ExprBuilder <- function(.data, ..., .parse = getOption("table.express.parse", FALSE)) {
    .data <- .data$chain_if_set(".select", ".by")
    cols <- parse_dots(.parse, ...)

    .data$set_by(rlang::expr(list(!!!cols)), FALSE)
    .data$set_select(rlang::expr(.SD[1L]), FALSE)

    .data
}
