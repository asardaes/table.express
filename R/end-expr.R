#' End and evaluate expression
#'
#' Helper to evaluate the expression created by [ExprBuilder] and get the resulting
#' [data.table::data.table-class].
#'
#' @export
#' @importFrom rlang caller_env
#'
#' @param .expr_builder The [ExprBuilder] instance.
#' @param .parent_env Optionally, the enclosing environment of the expression's environment.
#'
end_expr <- function(.expr_builder, .parent_env) {
    if (missing(.expr_builder)) {
        force(.parent_env)
        function(.expr_builder) { .expr_builder$eval(.parent_env) } # nocov
    }
    else if (missing(.parent_env)) {
        .expr_builder$eval(rlang::caller_env())
    }
    else {
        .expr_builder$eval(.parent_env)
    }
}
