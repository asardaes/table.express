#' @importFrom dplyr mutate
#' @export
#'
dplyr::mutate

#' Add or update columns
#'
#' Add or update columns of a [data.table::data.table-class], possibly by reference using
#' [`:=`][data.table::set].
#'
#' @rdname mutate-table.express
#' @name mutate-table.express
#' @export
#' @importFrom rlang enexprs
#' @importFrom rlang expr
#' @importFrom rlang quo_squash
#'
#' @template data-arg
#' @param ... Mutation clauses.
#' @param .parse See [where-table.express].
#' @param .by_ref Whether to update the data.table by reference or not.
#'
mutate.ExprBuilder <- function(.data, ..., .parse = FALSE, .by_ref = TRUE) {
    clauses <- lapply(rlang::enexprs(..., .named = TRUE), to_expr, .parse = .parse)

    .data$select <- rlang::quo_squash(rlang::expr(
        `:=`(!!!clauses)
    ))

    .data$by_ref <- .by_ref
    .data
}
