#' Where clause
#'
#' Clause for subsetting rows.
#'
#' @rdname where-table.express
#' @name where-table.express
#' @export
#'
#' @param .data The input data.
#' @param ... Arguments for the specific methods.
#'
where <- function(.data, ...) { UseMethod("where") }

#' @rdname where-table.express
#' @export
#' @importFrom rlang enexprs
#' @importFrom rlang enquo
#' @importFrom rlang quo_get_expr
#'
#' @param .collapse A boolean function which will be used to "concatenate" all conditions in `...`.
#' @param .parse If you want/need to provide strings in `...`, set this to `TRUE` to call
#'   [rlang::parse_expr()] on each one.
#' @template chain-arg
#'
where.ExprBuilder <- function(.data, ..., .collapse = `&`, .parse = FALSE, .chain = TRUE) {
    clause <- rlang::enexprs(...)

    if (length(clause) == 0L) {
        return(.data)
    }

    first_where <- to_expr(clause[[1L]], .parse = .parse)

    if (length(clause) == 1L) {
        clause <- first_where
    }
    else {
        .collapse <- rlang::quo_get_expr(rlang::enquo(.collapse))
        clause <- squash_expr(clause[-1L], first_where, .collapse, .parse = .parse)
    }

    .data$set_where(clause, .chain)
}
