#' @importFrom dplyr select
#' @export
#'
dplyr::select

#' Select clause
#'
#' Select or compute over columns of a [data.table::data.table-class].
#'
#' @rdname select-table.express
#' @name select-table.express
#' @export
#' @importFrom rlang expr
#' @importFrom rlang exprs
#' @importFrom rlang is_call
#'
#' @template data-arg
#' @param ... Clause for selectin/computing on columns. The `j` inside the `data.table`'s frame.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' If `length(...) == 1L` and the expression is a call to `:` (e.g. a:c), then it will be taken as
#' the single expression for the select clause, otherwise everything in `...` will be wrapped
#' in a call to [base::list()].
#'
select.ExprBuilder <- function(.data, ..., .parse = FALSE, .chain = TRUE) {
    clause <- parse_dots(.parse, ...)

    if (length(clause) == 1L && rlang::is_call(clause[[1L]], ":")) {
        clause <- clause[[1L]]
    }
    else {
        clause <- rlang::expr(list(!!!clause))
    }

    .data$set_select(clause, .chain)
}
