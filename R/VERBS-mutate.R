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
#' @importFrom rlang expr
#' @importFrom rlang quo_squash
#' @importFrom rlang warn
#'
#' @template data-arg
#' @param ... Mutation clauses.
#' @template parse-arg
#' @template chain-arg
#' @param .unquote_names Passed to [rlang::enexprs()]. Set to `FALSE` if you want to pass the single
#'   [`:=`][data.table::set] expression.
#'
mutate.ExprBuilder <- function(.data, ..., .parse = FALSE, .chain = TRUE, .unquote_names = TRUE) {
    clauses <- parse_dots(.parse, ..., .named = TRUE, .unquote_names = .unquote_names)

    if (.unquote_names) {
        clause <- rlang::quo_squash(rlang::expr(
            `:=`(!!!clauses)
        ))
    }
    else {
        if (length(clauses) > 1L) {
            rlang::warn(paste("Only one expression can be provided in '...' for .unquote_names = FALSE,",
                              "ignoring all but first."))
        }

        clause <- clauses[[1L]]
    }

    .data$set_select(clause, .chain)
}