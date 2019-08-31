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
#' @importFrom rlang warn
#'
#' @template data-arg
#' @param ... Clauses for transmuting columns. For `j` inside the `data.table`'s frame.
#' @param .enlist See details.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' Everything in `...` is wrapped in a call to `list` by default. If only one expression is given,
#' you can set `.enlist` to `FALSE` to skip the call to `list`.
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     transmute(ans = mpg * 2)
#'
transmute.ExprBuilder <- function(.data, ..., .enlist = TRUE,
                                  .parse = getOption("table.express.parse", FALSE),
                                  .chain = getOption("table.express.chain", TRUE))
{
    clauses <- parse_dots(.parse, ...)
    if (length(clauses) == 0L) return(.data)

    if (.enlist) {
        .data$set_j(rlang::expr(list(!!!clauses)), .chain)
    }
    else {
        if (length(clauses) > 1L) {
            rlang::warn("Only 1 expression is allowed for .enlist = FALSE, ignoring all but first.")
        }

        .data$set_j(clauses[[1L]], .chain)
    }
}

#' @rdname transmute-table.express
#' @export
#' @importFrom rlang caller_env
#'
#' @param .parent_env See [end_expr()]
#'
transmute.EagerExprBuilder <- function(.data, ..., .parent_env = rlang::caller_env()) {
    end_expr.ExprBuilder(transmute.ExprBuilder(.data, ...), .parent_env = .parent_env)
}

#' @rdname transmute-table.express
#' @export
#' @importFrom rlang caller_env
#'
transmute.data.table <- function(.data, ...) {
    eb <- ExprBuilder$new(.data)
    lazy_ans <- transmute.ExprBuilder(eb, ...)
    try_delegate("transmute", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
}
