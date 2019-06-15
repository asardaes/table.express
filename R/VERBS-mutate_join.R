#' @include VERBS-joins.R
#' @rdname joins
#' @export
#'
mutate_join <- function(x, y, ...) {
    UseMethod("mutate_join")
}

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang enexprs
#' @importFrom rlang maybe_missing
#' @importFrom rlang syms
#' @importFrom tidyselect scoped_vars
#'
#' @param .SDcols For `mutate_join`. See the details below.
#'
#' @details
#'
#' The [ExprBuilder] method for `mutate_join` implements the idiom described in
#' [this link](https://stackoverflow.com/a/54313203/5793905). The columns specified in `.SDcols` are
#' those that will be added to `x` from `y`. If the character vector in `.SDcols` is named, the
#' names are taken as the new column names for the values added to `x`. On the other hand, `.SDcols`
#' can be specified using [tidyselect::select_helpers].
#'
#' @examples
#'
#' # would modify lhs by reference
#' lhs %>%
#'     start_expr %>%
#'     mutate_join(rhs, x, .SDcols = c("foo", rhs.v = "v"))
#'
mutate_join.ExprBuilder <- function(x, y, ..., .SDcols, mult, roll, rollends) {
    dt <- rlang::enexpr(y)

    on <- rlang::enexprs(...)
    on <- name_switcheroo(on)

    if (is_tidyselect_call(rlang::enexpr(.SDcols))) {
        tidyselect::scoped_vars(colnames(y))
        .SDcols <- colnames(y)[.SDcols]
    }

    new_names <- name_switcheroo(.SDcols, named = FALSE, as_sym = FALSE)
    dt_cols <- rlang::syms(paste("x", unname(.SDcols), sep = "."))

    join_extras <- list(
        mult = rlang::maybe_missing(mult),
        roll = rlang::maybe_missing(roll),
        rollends = rlang::maybe_missing(rollends)
    )

    mutate.ExprBuilder(x, !!new_names := `[`(!!dt, .SD, list(!!!dt_cols), on = list(!!!on), !!!join_extras),
                       .unquote_names = FALSE, .parse = FALSE)
}
