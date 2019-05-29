#' Append expressions to the frame
#'
#' Add named expressions for the [data.table::data.table-class] frame.
#'
#' @export
#'
#' @template data-arg
#' @param ... Expressions to add to the frame.
#' @template parse-arg
#'
#' @examples
#'
#' data.table::data.table() %>%
#'     start_expr %>%
#'     frame_append(anything = "goes")
#'
frame_append <- function(.data, ..., .parse = getOption("table.express.parse", FALSE)) {
    .data$appends <- parse_dots(.parse, ...)
    .data
}
