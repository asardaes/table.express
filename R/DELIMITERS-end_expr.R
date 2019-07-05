#' End and evaluate expression
#'
#' Finish the expression-building process and evaluate it.
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
#' @importFrom rlang is_missing
#'
#' @param .by_ref If `FALSE`, [data.table::copy()] is used before evaluation.
#' @param .parent_env Optionally, the *enclosing* environment of the expression's evaluation
#'   environment. Defaults to the caller environment.
#'
#' @details
#'
#' The [ExprBuilder] method returns a [data.table::data.table-class].
#'
#' @template docu-examples
#'
end_expr.ExprBuilder <- function(.data, ..., .by_ref = TRUE, .parent_env) {
    if (rlang::is_missing(.parent_env)) {
        .data$eval(rlang::caller_env(), .by_ref)
    }
    else {
        .data$eval(.parent_env, .by_ref)
    }
}

