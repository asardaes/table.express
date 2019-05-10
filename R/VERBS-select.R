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
#'
#' @template data-arg
#' @param ... Clause for selectin/computing on columns. The `j` inside the `data.table`'s frame. For
#'   the `data.table` method, this can also extra parameters from the `ExprBuilder` method.
#' @param with Passed to [data.table::data.table-class]'s [`[`][data.table::data.table-package]
#'   method.
#'
#' @details
#'
#' If the length of `...` > 1, everything therein will be wrapped in a call to:
#'
#' - `list` if `with = TRUE`
#' - `c` otherwise
#'
select.ExprBuilder <- function(.data, ..., with = TRUE) {
    clause <- rlang::enexprs(..., .unquote_names = FALSE)

    if (length(clause) > 1L) {
        if (with) {
            squashed <- rlang::expr(list(!!!clause))
            clause <- rlang::exprs(!!squashed)
        }
        else {
            squashed <- rlang::expr(c(!!!clause))
            clause <- rlang::exprs(!!squashed, with = FALSE)
        }
    }
    else if (!with) {
        clause$with <- FALSE
    }

    .data$select <- clause
    .data
}
