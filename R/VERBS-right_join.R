#' @importFrom dplyr right_join
#' @export
#'
dplyr::right_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang maybe_missing
#'
#' @param which If `TRUE`, return the row numbers that matched in `x` instead of the result of the
#'   join.
#'
#' @examples
#'
#' # creates new data.table
#' lhs %>%
#'     right_join(rhs, x)
#'
right_join.ExprBuilder <- function(x, y, ..., which, nomatch, mult, roll, rollends) {
    y <- x$seek_and_nestroy(list(rlang::enexpr(y)))[[1L]]
    on <- parse_dots(TRUE, ...)

    join_extras <- list(
        nomatch = rlang::maybe_missing(nomatch),
        mult = rlang::maybe_missing(mult),
        roll = rlang::maybe_missing(roll),
        rollends = rlang::maybe_missing(rollends),
        which = rlang::maybe_missing(which)
    )

    x <- x$set_i(y, TRUE)
    leftright_join(x, on, join_extras)
}

#' @rdname joins
#' @export
#' @importFrom rlang caller_env
#'
right_join.data.table <- function(x, ..., allow = FALSE, .expr = FALSE) {
    eb <- if (.expr) EagerExprBuilder$new(x) else ExprBuilder$new(x)
    lazy_ans <- right_join.ExprBuilder(eb, ...)

    if (allow) {
        frame_append(lazy_ans, allow.cartesian = TRUE)
    }

    if (.expr) {
        lazy_ans
    }
    else {
        try_delegate("right_join", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
    }
}
