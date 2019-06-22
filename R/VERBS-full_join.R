#' @importFrom dplyr full_join
#' @export
#'
dplyr::full_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang as_string
#' @importFrom rlang enexprs
#' @importFrom rlang maybe_missing
#'
#' @details
#'
#' The `full_join` method is really a wrapper for `data.table::merge` that specifies `all = TRUE`.
#' The expression in `x` gets evaluated, merged with `y`, and the result is captured in a new
#' [ExprBuilder]. Useful in case you want to keep building expressions after the merge.
#'
full_join.ExprBuilder <- function(x, y, ..., sort = TRUE, allow = TRUE, .parent_env) {
    x <- end_expr.ExprBuilder(x, .parent_env = rlang::maybe_missing(.parent_env))
    if (missing(y)) {
        y <- x
    }

    on <- sapply(rlang::enexprs(...), rlang::as_string)

    by_x <- by_y <- NULL

    if (length(on) > 0L) {
        by_x <- names(on)
        by_y <- unname(on)
        zchars <- !nzchar(by_x)

        if (any(zchars)) {
            by_x[zchars] <- by_y[zchars]
        }
    }

    ans <- base::merge(x, y, all = TRUE, by.x = by_x, by.y = by_y, sort = sort, allow.cartesian = allow)
    ExprBuilder$new(ans)
}
