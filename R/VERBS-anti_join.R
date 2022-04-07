#' @importFrom dplyr anti_join
#' @export
#'
dplyr::anti_join

#' @include UTILS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang expr
#'
#' @examples
#'
#' rhs %>%
#'     anti_join(lhs, x, v)
#'
anti_join.ExprBuilder <- function(x, y, ...) {
    y <- rlang::enexpr(y)
    y <- x$seek_and_nestroy(list(y))[[1L]]
    y <- rlang::expr(`!`(`!!`(y)))

    on <- parse_dots(TRUE, ...)

    x <- x$set_i(y, TRUE)

    if (length(on) > 0L) {
        frame_append(x, on = list(!!!on))
    }

    x
}

#' @rdname joins
#' @export
#' @importFrom rlang caller_env
#'
anti_join.data.table <- function(x, ..., .expr = FALSE) {
    eb <- if (.expr) EagerExprBuilder$new(x) else ExprBuilder$new(x)
    lazy_ans <- anti_join.ExprBuilder(eb, ...)

    if (.expr) {
        lazy_ans
    }
    else {
        try_delegate("anti_join", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
    }
}
