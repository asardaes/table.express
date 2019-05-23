#' End and evaluate expression
#'
#' Finish the expression building process and evaluate it.
#'
#' @export
#'
#' @param .data The expression.
#' @template generic-dots
#'
end_expr <- function(.data, ...) {
    UseMethod("end_expr")
}

#' @rdname end_expr
#' @export
#' @importFrom rlang caller_env
#'
#' @param .by_ref Whether to update the `data.table` by reference or not.
#' @param .parent_env Optionally, the enclosing environment of the expression's evaluation
#'   environment.
#'
#' @details
#'
#' The [ExprBuilder] method returns a [data.table::data.table-class].
#'
end_expr.ExprBuilder <- function(.data, ..., .by_ref = TRUE, .parent_env) {
    if (missing(.parent_env)) {
        .data$eval(rlang::caller_env(), .by_ref)
    }
    else {
        .data$eval(.parent_env, .by_ref)
    }
}