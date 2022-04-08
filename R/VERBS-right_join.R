#' @importFrom dplyr right_join
#' @export
#'
dplyr::right_join

#' @include UTILS-joins.R
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
#' @importFrom rlang call2
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#' @importFrom rlang is_missing
#' @importFrom rlang sym
#'
right_join.data.table <- function(x, y, ..., allow = FALSE, .expr = FALSE, .selecting, .framing) {
    eb <- if (.expr) EagerExprBuilder$new(x) else ExprBuilder$new(x)
    y_expr <- rlang::enexpr(y)
    assume_dplyr <- assume_dplyr_join(...)

    lazy_ans <- if (assume_dplyr) {
        y_expr <- list(y_expr)
        expr <- rlang::call2("right_join.ExprBuilder", rlang::sym("eb"), !!!y_expr, !!!dplyr_by_to_dots(...))
        base::eval(expr)
    }
    else {
        right_join.ExprBuilder(eb, !!y_expr, ...)
    }

    if (allow) {
        frame_append(lazy_ans, allow.cartesian = TRUE)
    }

    j <- extract_expressions(rlang::enexpr(.selecting), FALSE)
    if (!rlang::is_missing(j)) {
        lazy_ans$set_j(j, FALSE)
    }

    appends <- extract_expressions(rlang::enexpr(.framing), TRUE)
    if (!rlang::is_missing(appends)) {
        frame_append(lazy_ans, !!!appends)
    }

    if (.expr) {
        lazy_ans
    }
    else {
        try_delegate("right_join", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
    }
}
