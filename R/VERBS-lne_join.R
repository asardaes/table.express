#' @include VERBS-non-equi_joins.R
#' @rdname non-equi_joins
#' @export
#'
lne_join <- function(x, y, ...) {
    UseMethod("lne_join")
}

#' @include VERBS-non-equi_joins.R
#' @rdname non-equi_joins
#' @export
#' @importFrom rlang enexprs
#' @importFrom rlang maybe_missing
#'
lne_join.ExprBuilder <- function(x, y, ..., nomatch, mult) {
    x <- x$chain("pronoun", y)

    on <- lapply(rlang::enexprs(...), to_expr, .parse = TRUE)
    on <- comp_switcheroo(on)

    join_extras <- list(
        nomatch = rlang::maybe_missing(nomatch),
        mult = rlang::maybe_missing(mult)
    )

    if (length(on) > 0L) {
        frame_append(x, on = list(!!!on))
    }

    frame_append(x, !!!join_extras, .ignore_empty = "all")
}
