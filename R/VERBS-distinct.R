#' @importFrom dplyr distinct
#' @export
#'
dplyr::distinct

#' Rows with distinct combinations of columns
#'
#' @rdname distinct-table.express
#' @name distinct-table.express
#' @export
#' @importFrom rlang as_string
#' @importFrom rlang enexpr
#' @importFrom rlang expr
#'
#' @template data-arg
#' @param ... Which columns to use to determine uniqueness.
#' @param .keep See details below.
#' @param .n Indices of rows to return *for each* unique combination of the chosen columns. See
#'   details.
#' @template parse-arg
#'
#' @details
#'
#' If `.keep = TRUE` (the default), the columns not mentioned in `...` are also kept. However, if
#' a new column is created in one of the expressions therein, `.keep` can also be set to a character
#' vector containing the names of *all* the columns that should be in the result in addition to the
#' ones mentioned in `...`. See the examples.
#'
#' The value of `.n` is only relevant when `.keep` is *not* `FALSE`. It is used to subset `.SD` in
#' the built `data.table` expression. For example, we could get 2 rows per combination by setting
#' `.n` to `1:2`, or get the last row instead of the first by using `.N`. If more than one index is
#' used, and not enough rows are found, some rows will have `NA`. Do note that, at least as of
#' version 1.12.2 of `data.table`, only expressions with single indices are internally optimized.
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' # compare with .keep = TRUE
#' data.table::as.data.table(mtcars) %>%
#'     distinct(amvs = am + vs, .keep = names(mtcars))
#'
distinct.ExprBuilder <- function(.data, ..., .keep = TRUE, .n = 1L,
                                 .parse = getOption("table.express.parse", FALSE)) {
    cols <- parse_dots(.parse, ...)

    if (isTRUE(.keep) || is.character(.keep)) {
        .data <- .data$chain_if_set(".j", ".by")
        .data$set_by(rlang::expr(list(!!!cols)), FALSE)
        .data$set_j(rlang::expr(.SD[!!rlang::enexpr(.n)]), FALSE)

        if (is.character(.keep)) {
            frame_append(.data, .SDcols = !!.keep)
        }
    }
    else {
        cols <- sapply(cols, rlang::as_string)
        .data <- .data$set_j(rlang::expr(unique(.SD)), TRUE)
        frame_append(.data, .SDcols = !!cols)
    }

    .data
}

#' @rdname distinct-table.express
#' @export
#' @importFrom rlang caller_env
#'
distinct.data.table <- function(.data, ...) {
    eb <- ExprBuilder$new(.data)
    lazy_ans <- distinct.ExprBuilder(eb, ...)
    end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env())
}
