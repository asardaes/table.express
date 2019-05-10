#' Append expressions to the frame
#'
#' Add named expressions to the [data.table::data.table-class] frame.
#'
#' @export
#' @importFrom rlang enexprs
#'
#' @param .data An [ExprBuilder] instance.
#' @param ... Expressions to add to the frame.
#' @param .parse See [where-table.express].
#'
frame_append <- function(.data, ..., .parse = FALSE) {
    .data$appends <- lapply(rlang::enexprs(...), to_expr, .parse = .parse)
    .data
}
