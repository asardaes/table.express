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
#' @importFrom rlang enexprs
#' @importFrom rlang expr
#' @importFrom rlang exprs
#' @importFrom rlang is_call
#'
#' @template data-arg
#' @param ... Clause for selectin/computing on columns. The `j` inside the `data.table`'s frame.
#' @param with Passed to [data.table::data.table-class]'s [`[`][data.table::data.table-package]
#'   method.
#' @template chain-arg
#'
#' @details
#'
#' Everything in `...` will be wrapped in a call to:
#'
#' - [base::list()] if `with = TRUE`
#' - [base::c()] otherwise
#'
select.ExprBuilder <- function(.data, ..., with = TRUE, .chain = TRUE) {
    clause <- rlang::enexprs(..., .unquote_names = FALSE)

    if (length(clause) == 1L && rlang::is_call(clause[[1L]], ":")) {
        clause <- clause[[1L]]
    }
    else if (with) {
        clause <- rlang::expr(list(!!!clause))
    }
    else {
        clause <- rlang::exprs(c(!!!clause), with = FALSE)
    }

    .data$set_select(clause, .chain)
}
