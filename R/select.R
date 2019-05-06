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
#' @importFrom rlang enquos
#' @importFrom rlang expr
#' @importFrom rlang quo_get_env
#' @importFrom rlang quo_set_env
#' @importFrom rlang quos
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
    clause <- rlang::enquos(...)

    if (length(clause) > 1L) {
        if (with)
            squashed <- rlang::expr(list(!!!clause))
        else
            squashed <- rlang::expr(c(!!!clause))

        .data$select <- sapply(rlang::quos(!!squashed, with = !!with),
                               rlang::quo_set_env,
                               rlang::quo_get_env(clause[[1L]]))
    }
    else {
        .data$select <- rlang::enquos(..., with = with)
    }

    .data
}
