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
#' @param .keep_all Whether to also keep columns that are not mentioned in `...`.
#' @param .keep_old This should be changed to `TRUE` if a new column is created in `...` and the
#'   columns used to create it should also be kept. Has no effect if `.keep_all` is `FALSE`. See
#'   examples.
#' @template parse-arg
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     distinct(amvs = am + vs, .keep_old = TRUE) %>%
#'     end_expr
#'
distinct.ExprBuilder <- function(.data, ..., .keep_all = TRUE, .keep_old = FALSE,
                                 .parse = getOption("table.express.parse", FALSE)) {
    cols <- parse_dots(.parse, ...)

    if (.keep_all) {
        .data <- .data$chain_if_set(".select", ".by")
        .data$set_by(rlang::expr(list(!!!cols)), FALSE)
        .data$set_select(rlang::expr(.SD[1L]), FALSE)

        if (.keep_old) {
            # TODO: won't work if pronoun is chained afterwards
            frame_append(.data, .SDcols = names(.DT_))
        }
    }
    else {
        cols <- sapply(cols, rlang::as_string)
        .data <- .data$set_select(rlang::expr(unique(.SD)), TRUE)
        frame_append(.data, .SDcols = !!cols)
    }

    .data
}
