#' Where clause
#'
#' Clause for subsetting rows.
#'
#' @rdname where-table.express
#' @name where-table.express
#' @export
#'
#' @param .data The input data.
#' @template generic-dots
#'
where <- function(.data, ...) {
    UseMethod("where")
}

#' @rdname where-table.express
#' @export
#' @importFrom rlang enexprs
#' @importFrom rlang enquo
#' @importFrom rlang quo_get_expr
#'
#' @param .collapse A boolean function which will be used to "concatenate" all conditions in `...`.
#' @template parse-arg
#' @template chain-arg
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     where(vs == 0, am == 1)
#'
where.ExprBuilder <- function(.data, ..., .collapse = `&`,
                              .parse = getOption("table.express.parse", FALSE),
                              .chain = getOption("table.express.chain", TRUE))
{
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
        clause <- reduce_expr(clause[-1L], first_where, .collapse, .parse = .parse)
    }

    .data$set_where(clause, .chain)
}
