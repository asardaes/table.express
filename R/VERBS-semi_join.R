#' @importFrom dplyr semi_join
#' @export
#'
dplyr::semi_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang expr
#'
#' @examples
#'
#' # keep only columns from lhs
#' lhs %>%
#'     start_expr %>%
#'     semi_join(rhs, x) %>%
#'     end_expr
#'
semi_join.ExprBuilder <- function(x, y, ...) {
    on <- parse_dots(TRUE, ...)

    x <- x$chain_if_set(".where", ".select")
    x <- x$set_where(rlang::enexpr(y), FALSE)
    x <- x$set_select(rlang::expr(unique(.SD)), FALSE)

    frame_append(x, nomatch = NULL, .parse = FALSE)
    if (length(on) > 0L) {
        frame_append(x, on = list(!!!on), .parse = FALSE)
    }

    x
}
