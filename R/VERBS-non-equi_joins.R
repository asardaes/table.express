#' Non-equi joins
#'
#' See also [data.table::data.table].
#'
#' @rdname non-equi_joins
#' @name non-equi_joins
#'
#' @param x An [ExprBuilder] instance.
#' @param y A [data.table::data.table-class].
#' @param ... Expressions for the `on` part of the join.
#' @param nomatch See [data.table::data.table].
#' @param mult See [data.table::data.table].
#'
NULL

#' @importFrom rlang abort
#' @importFrom rlang call2
#' @importFrom rlang call_args
#' @importFrom rlang call_name
#' @importFrom rlang is_call
#'
comp_switcheroo <- function(on) {
    lapply(on, function(e) {
        if (!rlang::is_call(e)) {
            return(e)
        }

        comp <- switch(rlang::call_name(e),
                       "==" = "==",
                       "<" = ">",
                       "<=" = ">=",
                       ">" = "<",
                       ">=" = "<=",
                       # default
                       rlang::abort("The 'on' expressions must be variables or comparisons."))

        args <- rev(rlang::call_args(e))
        rlang::call2(comp, !!!args)
    })
}
