#' @importFrom dplyr inner_join
#' @export
#'
dplyr::inner_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang enexprs
#'
#' @examples
#'
#' lhs %>%
#'     start_expr %>%
#'     inner_join(rhs, x) %>%
#'     end_expr
#'
inner_join.ExprBuilder <- function(x, y, ...) {
    y <- rlang::enexpr(y)
    on <- lapply(rlang::enexprs(...), to_expr, .parse = TRUE)

    where.ExprBuilder(x, !!y) %>%
        frame_append(on = list(!!!on), nomatch = NULL, mult = "all")
}
