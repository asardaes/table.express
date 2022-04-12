#' @importFrom dplyr left_join
#' @export
#'
dplyr::left_join

#' @include UTILS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang caller_env
#' @importFrom rlang maybe_missing
#'
#' @examples
#'
#' # creates new data.table
#' lhs %>%
#'     left_join(rhs, x)
#'
left_join.ExprBuilder <- function(x, y, ..., nomatch, mult, roll, rollends, .parent_env, .to_eager = FALSE) {
    if (missing(y)) {
        y <- end_expr.ExprBuilder(x, .parent_env = rlang::maybe_missing(.parent_env))
    }

    if (missing(.parent_env)) {
        .parent_env <- rlang::caller_env()
    }

    x <- x$chain("pronoun", y, .parent_env, .to_eager)

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

#' @rdname joins
#' @export
#' @importFrom rlang caller_env
#'
left_join.data.table <- function(x, y, ..., allow = FALSE, .expr = FALSE) {
    eb <- start_expr.data.table(x)
    lazy_ans <- left_join.ExprBuilder(eb, y, ..., .to_eager = TRUE)

    if (allow) {
        frame_append(lazy_ans, allow.cartesian = TRUE)
    }

    if (.expr) {
        lazy_ans
    }
    else {
        try_delegate("left_join", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
    }
}
