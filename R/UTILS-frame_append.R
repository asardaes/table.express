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
frame_append <- function(.data, ..., .parse = FALSE) {
    .data$appends <- parse_dots(.parse, ...)
    .data
}