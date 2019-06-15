#' @importFrom dplyr left_join
#' @export
#'
dplyr::left_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexprs
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
left_join.ExprBuilder <- function(x, y, ..., nomatch, mult, roll, rollends) {
    on <- rlang::enexprs(...)
    on <- name_switcheroo(on)

    join_extras <- list(
        nomatch = rlang::maybe_missing(nomatch),
        mult = rlang::maybe_missing(mult),
        roll = rlang::maybe_missing(roll),
        rollends = rlang::maybe_missing(rollends)
    )

    eb <- x$chain("pronoun", y)
    leftright_join(eb, on, join_extras)
}
