#' @importFrom dplyr transmute
#' @export
#'
dplyr::transmute

#' Add columns and keep only the result
#'
#' Add columns of a [data.table::data.table-class], keeping only the resulting columns.
#'
#' @rdname transmute-table.express
#' @name transmute-table.express
#' @export
#' @importFrom data.table as.data.table
#' @importFrom rlang as_data_mask
#' @importFrom rlang call2
#' @importFrom rlang enexprs
#' @importFrom rlang eval_tidy
#'
#' @param .data A [data.table::data.table-class].
#' @param ... Transmutation clauses.
#' @template parse-arg
#' @param .unquote_names See details.
#'
#' @details
#'
#' This is more of a convenience helper which never modifies the input by reference. It does *not*
#' operate on an [ExprBuilder] instance to make it clear that the input data must be an actual
#' `data.table` and not an expression. It should work like [dplyr::transmute()].
#'
#' The ellipsis is passed to [rlang::enexprs()], and its `.named` argument is set to `TRUE`. Since
#' the resulting expressions are evaluated with [rlang::eval_tidy()], `.unquote_names` cannot be
#' `FALSE`, so its value is checked by this function before proceeding.
#'
transmute.data.table <- function(.data, ..., .parse = FALSE, .unquote_names = TRUE) {
    stopifnot(.unquote_names)

    data_mask <- rlang::as_data_mask(.data)
    expressions <- lapply(rlang::enexprs(..., .named = TRUE), to_expr, .parse = .parse)
    ans <- Map(expressions, names(expressions), f = function(e, nm) {
        dots <- list(nm, e)
        call <- rlang::call2(`<-`, !!!dots)
        rlang::eval_tidy(call, data = data_mask)
    })

    data.table::as.data.table(ans)
}
