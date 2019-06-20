#' @importFrom dplyr right_join
#' @export
#'
dplyr::right_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang enexprs
#' @importFrom rlang maybe_missing
#'
#' @examples
#'
#' # creates new data.table
#' lhs %>%
#'     start_expr %>%
#'     right_join(rhs, x) %>%
#'     end_expr
#'
right_join.ExprBuilder <- function(x, y, ..., nomatch, mult, roll, rollends) {
    y <- rlang::enexpr(y)
    on <- lapply(rlang::enexprs(...), to_expr, .parse = TRUE)

    join_extras <- list(
        nomatch = rlang::maybe_missing(nomatch),
        mult = rlang::maybe_missing(mult),
        roll = rlang::maybe_missing(roll),
        rollends = rlang::maybe_missing(rollends)
    )

    x <- x$set_where(y, TRUE)
    leftright_join(x, on, join_extras)
}
