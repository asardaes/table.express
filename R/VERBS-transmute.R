#' @importFrom dplyr transmute
#' @export
#'
dplyr::transmute

#' Compute new columns
#'
#' Compute and keep only new columns.
#'
#' @rdname transmute-table.express
#' @name transmute-table.express
#' @export
#' @importFrom rlang expr
#'
#' @template data-arg
#' @param ... Clauses for transmuting columns. For `j` inside the `data.table`'s frame.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' Everything in `...` is wrapped in a call to `list`.
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     transmute(ans = mpg * 2)
#'
transmute.ExprBuilder <- function(.data, ...,
                                  .parse = getOption("table.express.parse", FALSE),
                                  .chain = getOption("table.express.chain", TRUE))
{
    clauses <- parse_dots(.parse, ...)
    if (length(clauses) == 0L) return(.data)

    .data$set_select(rlang::expr(list(!!!clauses)), .chain)
}
