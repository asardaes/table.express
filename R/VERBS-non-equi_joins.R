#' Non-equi joins
#'
#' Left and right non-equi joins.
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
#' @details
#'
#' Non-equi joins are similar to normal joins but support comparisons (`<`, `>`, `<=`, and `>=`) in
#' the `on` expression. We can loosely divide them into left and right non-equi joins, `lne_join`
#' and `rne_join` respectively. Their differences are the rows in the result, and the values
#' returned in a given column that is part of the comparison. Check the "Joining verbs" vignette for
#' more information.
#'
#' @seealso
#'
#' [joins], [data.table::data.table], [dplyr::join]
#'
NULL

#' @importFrom rlang abort
#' @importFrom rlang call2
#' @importFrom rlang call_args
#' @importFrom rlang call_name
#' @importFrom rlang is_call
#'
comp_switcheroo <- function(on) {
    lapply(unname(on), function(e) {
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
