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
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     group_by(cyl, gear)
#'
group_by.ExprBuilder <- function(.data, ..., .parse = getOption("table.express.parse", FALSE)) {
    clause <- parse_dots(.parse, ...)
    .data$set_by(rlang::quo_squash(rlang::expr(list(!!!clause))))
}
