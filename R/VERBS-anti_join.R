#' @importFrom dplyr anti_join
#' @export
#'
dplyr::anti_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang expr
#'
#' @examples
#'
#' rhs %>%
#'     start_expr %>%
#'     anti_join(lhs, x, v) %>%
#'     end_expr
#'
anti_join.ExprBuilder <- function(x, y, ...) {
    y <- rlang::enexpr(y)
    y <- x$seek_and_nestroy(list(y))[[1L]]
    y <- rlang::expr(`!`(`!!`(y)))

    on <- parse_dots(TRUE, ...)

    x <- x$set_where(y, TRUE)

    if (length(on) > 0L) {
        frame_append(x, on = list(!!!on))
    }

    x
}
