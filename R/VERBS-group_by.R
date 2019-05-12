#' @importFrom dplyr group_by
#' @export
#'
dplyr::group_by

#' Group by clause
#'
#' Aggregations for a [data.table::data.table-class].
#'
#' @rdname group_by-table.express
#' @name group_by-table.express
#' @export
#' @importFrom rlang expr
#' @importFrom rlang quo_squash
#'
#' @template data-arg
#' @param ... Clause for grouping on columns. The `by` inside the `data.table`'s frame.
#' @template parse-arg
#'
#' @details
#'
#' Everything in `...` will be wrapped in a call to `list`.
#'
group_by.ExprBuilder <- function(.data, ..., .parse = FALSE) {
    clause <- parse_dots(.parse, ...)
    .data$set_by(rlang::quo_squash(rlang::expr(list(!!!clause))))
}
