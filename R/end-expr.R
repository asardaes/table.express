#' End and evaluate expression
#'
#' Finish the expression building process and evaluate it.
#'
#' @export
#'
#' @param .data The expression.
#' @param ... Arguments for the specific methods.
#'
end_expr <- function(.data, ...) { UseMethod("end_expr") }

#' @rdname end_expr
#' @export
#' @importFrom rlang caller_env
#'
#' @param .parent_env Optionally, the enclosing environment of the expression's environment.
#'
#' @details
#'
#' The [ExprBuilder] method should return a [data.table::data.table-class].
#'
end_expr.ExprBuilder <- function(.data, ..., .parent_env) {
    if (missing(.data)) {
        force(.parent_env)
        function(.data) { .data$eval(.parent_env) } # nocov
    }
    else if (missing(.parent_env)) {
        .data$eval(rlang::caller_env())
    }
    else {
        .data$eval(.parent_env)
    }
}
