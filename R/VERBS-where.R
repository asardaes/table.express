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
#' @importFrom rlang enquo
#' @importFrom rlang is_missing
#' @importFrom rlang quo_get_expr
#'
#' @param which Passed to [data.table::data.table].
#' @param .collapse A boolean function which will be used to "concatenate" all conditions in `...`.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' For [ExprBuilder], the expressions in `...` can call [nest_expr()], and are eagerly nested if
#' they do.
#'
#' The [data.table::data.table-class] method is **lazy**, so it expects another verb to follow
#' *afterwards*.
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
where.ExprBuilder <- function(.data, ..., which, .collapse = `&`,
                              .parse = getOption("table.express.parse", FALSE),
                              .chain = getOption("table.express.chain", TRUE))
{
    clause <- parse_dots(.parse, ...)

    if (length(clause) == 0L) {
        return(.data)
    }

    clause <- .data$seek_and_nestroy(clause)

    first_where <- clause[[1L]]

    if (length(clause) == 1L) {
        clause <- first_where
    }
    else {
        .collapse <- rlang::quo_get_expr(rlang::enquo(.collapse))
        clause <- reduce_expr(clause[-1L], first_where, .collapse, .parse = .parse)
    }

    .data <- .data$set_i(clause, .chain)
    if (!rlang::is_missing(which)) {
        frame_append(.data, which = !!which, .parse = FALSE)
    }

    .data
}

#' @rdname where-table.express
#' @export
#'
#' @examples
#'
#' data.table::as.data.table(mtcars) %>%
#'     where(vs == 0) %>%
#'     transmute(mpg = round(mpg))
#'
where.data.table <- function(.data, ...) {
    eb <- EagerExprBuilder$new(.data)
    where.ExprBuilder(eb, ...)
}
