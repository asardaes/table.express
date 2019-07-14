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
#' @importFrom rlang expr
#'
#' @template data-arg
#' @param ... Which columns to use to determine uniqueness.
#' @param .keep See details below.
#' @template parse-arg
#'
#' @details
#'
#' If `.keep = TRUE` (the default), the columns not mentioned in `...` are also kept. However, if
#' a new column is created in one of the expressions therein, `.keep` can also be set to a character
#' vector containing the names of *all* the columns that should be in the result in addition to the
#' ones mentioned in `...`. See the examples.
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' # compare with .keep = TRUE
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     distinct(amvs = am + vs, .keep = names(mtcars)) %>%
#'     end_expr
#'
distinct.ExprBuilder <- function(.data, ..., .keep = TRUE,
                                 .parse = getOption("table.express.parse", FALSE)) {
    cols <- parse_dots(.parse, ...)

    if (isTRUE(.keep) || is.character(.keep)) {
        .data <- .data$chain_if_set(".select", ".by")
        .data$set_by(rlang::expr(list(!!!cols)), FALSE)
        .data$set_select(rlang::expr(.SD[1L]), FALSE)

        if (is.character(.keep)) {
            frame_append(.data, .SDcols = !!.keep)
        }
    }
    else {
        cols <- sapply(cols, rlang::as_string)
        .data <- .data$set_select(rlang::expr(unique(.SD)), TRUE)
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
