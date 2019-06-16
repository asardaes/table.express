#' @include VERBS-non-equi_joins.R
#' @rdname non-equi_joins
#' @export
#'
rne_join <- function(x, y, ...) {
    UseMethod("rne_join")
}

#' @include VERBS-non-equi_joins.R
#' @rdname non-equi_joins
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang enexprs
#' @importFrom rlang maybe_missing
#'
rne_join.ExprBuilder <- function(x, y, ..., nomatch, mult) {
    y <- rlang::enexpr(y)
    on <- lapply(rlang::enexprs(...), to_expr, .parse = TRUE)

    join_extras <- list(
        nomatch = rlang::maybe_missing(nomatch),
        mult = rlang::maybe_missing(mult)
    )

    if (length(on) > 0L) {
        frame_append(x, on = list(!!!on))
    }

    x$set_where(y, TRUE)
    frame_append(x, !!!join_extras, .ignore_empty = "all")
}
