#' @importFrom dplyr inner_join
#' @export
#'
dplyr::inner_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexpr
#'
#' @examples
#'
#' lhs %>%
#'     inner_join(rhs, x)
#'
inner_join.ExprBuilder <- function(x, y, ...) {
    y <- rlang::enexpr(y)
    on <- parse_dots(TRUE, ...)

    y <- x$seek_and_nestroy(list(y))[[1L]]
    x <- x$set_i(y, TRUE)
    frame_append(x, nomatch = NULL, mult = "all")

    if (length(on) > 0L) {
        frame_append(x, on = list(!!!on))
    }

    x
}

#' @rdname joins
#' @export
#' @importFrom rlang caller_env
#'
inner_join.data.table <- function(x, ..., .expr = FALSE) {
    eb <- if (.expr) EagerExprBuilder$new(x) else ExprBuilder$new(x)
    lazy_ans <- inner_join.ExprBuilder(eb, ...)

    if (.expr) {
        lazy_ans
    }
    else {
        try_delegate("inner_join", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
    }
}
