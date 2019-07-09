#' @importFrom dplyr left_join
#' @export
#'
dplyr::left_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang caller_env
#' @importFrom rlang maybe_missing
#'
#' @examples
#'
#' # creates new data.table
#' lhs %>%
#'     start_expr %>%
#'     left_join(rhs, x) %>%
#'     end_expr
#'
left_join.ExprBuilder <- function(x, y, ..., nomatch, mult, roll, rollends, .parent_env) {
    if (missing(y)) {
        y <- end_expr.ExprBuilder(x, .parent_env = rlang::maybe_missing(.parent_env))
    }

    if (missing(.parent_env)) {
        .parent_env <- rlang::caller_env()
    }

    x <- x$chain("pronoun", y, .parent_env)

    on <- parse_dots(TRUE, ...)
    on <- name_comp_switcheroo(on)

    join_extras <- list(
        nomatch = rlang::maybe_missing(nomatch),
        mult = rlang::maybe_missing(mult),
        roll = rlang::maybe_missing(roll),
        rollends = rlang::maybe_missing(rollends)
    )

    leftright_join(x, on, join_extras)
}
