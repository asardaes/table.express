#' @importFrom dplyr right_join
#' @export
#'
dplyr::right_join

#' @include UTILS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang is_missing
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
right_join.ExprBuilder <- function(x, y, ..., allow = FALSE, which, nomatch, mult, roll, rollends, .selecting, .framing) {
    y <- x$seek_and_nestroy(list(rlang::enexpr(y)))[[1L]]

    on <- if (assume_dplyr_join(...)) {
        dplyr_by_to_dots(...)
    }
    else {
        parse_dots(TRUE, ...)
    }

    join_extras <- list(
        nomatch = rlang::maybe_missing(nomatch),
        mult = rlang::maybe_missing(mult),
        roll = rlang::maybe_missing(roll),
        rollends = rlang::maybe_missing(rollends),
        which = rlang::maybe_missing(which)
    )

    x <- x$set_i(y, TRUE)
    eb <- leftright_join(x, on, join_extras)

    if (allow) {
        frame_append(eb, allow.cartesian = TRUE)
    }

    j <- extract_expressions(rlang::enexpr(.selecting), FALSE)
    if (!rlang::is_missing(j)) {
        eb$set_j(j, FALSE)
    }

    appends <- extract_expressions(rlang::enexpr(.framing), TRUE)
    if (!rlang::is_missing(appends)) {
        frame_append(eb, !!!appends)
    }

    eb

}

#' @rdname joins
#' @export
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#'
right_join.data.table <- function(x, y, ..., allow = FALSE, .expr = FALSE, .selecting, .framing) {
    eb <- if (.expr) EagerExprBuilder$new(x) else ExprBuilder$new(x)

    lazy_ans <- right_join.ExprBuilder(eb, !!rlang::enexpr(y), ..., allow = allow,
                                       .selecting = !!rlang::enexpr(.selecting),
                                       .framing = !!rlang::enexpr(.framing))

    if (.expr) {
        lazy_ans
    }
    else {
        try_delegate("right_join", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
    }
}
