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

#' @rdname joins
#' @export
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#'
left_join.data.table <- function(x, y, ..., allow = FALSE, .expr = FALSE) {
    x_expr <- rlang::enexpr(x)

    if (missing(y)) {
        y <- x
    }

    eb <- if (.expr) EagerExprBuilder$new(y) else ExprBuilder$new(y)
    lazy_ans <- right_join.ExprBuilder(eb, y = !!x_expr, ...)

    if (allow) {
        frame_append(lazy_ans, allow.cartesian = TRUE)
    }

    if (.expr) {
        lazy_ans
    }
    else {
        end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env())
    }
}
