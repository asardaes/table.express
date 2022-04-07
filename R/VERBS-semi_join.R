#' @importFrom dplyr semi_join
#' @export
#'
dplyr::semi_join

#' @include UTILS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang expr
#' @importFrom rlang exprs
#'
#' @param .eager For `semi_join`. If `TRUE`, it uses [nest_expr()] to build an expression like
#'   [this](https://stackoverflow.com/a/18971223/5793905) instead of the default one. This uses the
#'   captured `data.table` eagerly, so use [chain()] when needed. The default is lazy.
#'
#' @examples
#'
#' # keep only columns from lhs
#' lhs %>%
#'     semi_join(rhs, x)
#'
semi_join.ExprBuilder <- function(x, y, ..., allow = FALSE, .eager = FALSE) {
    y <- rlang::enexpr(y)
    on <- parse_dots(TRUE, ...)

    if (.eager) {
        where_expr <- rlang::exprs(nest_expr(
            .parse = FALSE,
            .end = FALSE,
            inner_join(!!y, !!!on),
            frame_append(which = TRUE, allow.cartesian = !!allow),
            end_expr,
            unique
        ))

        where_clause <- x$seek_and_nestroy(where_expr)[[1L]]
        x <- x$set_i(where_clause, TRUE)
    }
    else {
        x <- x$chain_if_set(".i", ".j")
        x <- x$set_i(y, FALSE)
        x <- x$set_j(rlang::expr(unique(.SD)), FALSE)

        frame_append(x, nomatch = NULL, .parse = FALSE)
        if (length(on) > 0L) {
            frame_append(x, on = list(!!!on), .parse = FALSE)
        }
    }

    x
}

#' @rdname joins
#' @export
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#' @importFrom rlang exprs
#'
semi_join.data.table <- function(x, y, ..., allow = FALSE, .eager = FALSE) {
    eb <- ExprBuilder$new(x)
    y_expr <- rlang::enexpr(y)

    if (.eager) {
        on <- parse_dots(TRUE, ...)

        where_expr <- rlang::exprs(nest_expr(
            .parse = FALSE,
            .end = FALSE,
            inner_join(!!y_expr, !!!on),
            frame_append(which = TRUE, allow.cartesian = !!allow),
            end_expr,
            unique
        ))

        where_clause <- eb$seek_and_nestroy(where_expr)[[1L]]
        lazy_ans <- eb$set_i(where_clause, FALSE)
    }
    else {
        lazy_ans <- semi_join.ExprBuilder(eb, y = !!y_expr, ...)
    }

    try_delegate("semi_join", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
}
