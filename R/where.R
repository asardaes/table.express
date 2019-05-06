#' Where clause
#'
#' Clause for subsetting rows.
#'
#' @export
#'
#' @param .data Something resembling a `data.frame`.
#' @param ... Arguments for the specific methods.
#'
where <- function(.data, ...) { UseMethod("where") }

#' Where clause
#'
#' Clause for subsetting rows of a [data.table::data.table-class].
#'
#' @rdname where-table.express
#' @name where-table.express
#' @export
#' @importFrom rlang caller_env
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang new_quosure
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_squash
#'
#' @template data-arg
#' @param ... Clause for subsetting. The `i` inside the `data.table`'s frame. For the `data.table`
#'   method, this can also extra parameters from the `ExprBuilder` method.
#' @param .collapse A boolean function which will be used to "concatenate" all conditions in `...`.
#' @param .parse If you want/need to provide strings in `...`, set this to `TRUE` to call
#'   [rlang::parse_expr()] on each one.
#'
where.ExprBuilder <- function(.data, ..., .collapse = `&`, .parse = FALSE) {
    clause <- rlang::enquos(...)

    if (length(clause) == 0L) {
        return(.data)
    }
    else {
        if (.parse)
            first_where <- to_expr(clause[[1L]], .parse = .parse)
        else
            first_where <- rlang::quo_squash(clause[[1L]])

        if (length(clause) == 1L) {
            clause <- first_where
        }
        else {
            .collapse <- rlang::quo_get_expr(rlang::enquo(.collapse))
            clause <- squash_expr(clause[-1L], first_where, .collapse, .parse = .parse)
        }
    }

    .data$where <- rlang::new_quosure(clause, rlang::caller_env())
    .data
}
