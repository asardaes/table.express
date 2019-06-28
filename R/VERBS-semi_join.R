#' @importFrom dplyr semi_join
#' @export
#'
dplyr::semi_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom data.table setnames
#' @importFrom rlang as_string
#' @importFrom rlang enexprs
#' @importFrom rlang expr
#' @importFrom rlang maybe_missing
#' @importFrom rlang sym
#'
#' @details
#'
#' The `semi_join` method [starts a new expression][chain()] immediately.
#'
#' @examples
#'
#' # keep only columns from lhs
#' lhs %>%
#'     start_expr %>%
#'     semi_join(rhs, x) %>%
#'     end_expr
#'
semi_join.ExprBuilder <- function(x, y, ..., .parent_env) {
    on <- rlang::enexprs(...)
    on <- name_switcheroo(on)
    on_char <- sapply(unname(on), rlang::as_string)

    eb <- x$chain("pronoun", y)
    new_pronoun <- rlang::sym(eb$get_newest_pronoun())

    # avoid NOTE
    .semi_joined_names <- EBCompanion$helper_functions$.semi_joined_names

    if (length(on) > 0L) {
        frame_append(eb, on = list(!!!on))
    }

    frame_append(eb, nomatch = NULL, mult = "first")
    eb <- eb$set_select(rlang::expr(mget(.semi_joined_names(!!new_pronoun, .DT_, !!on_char))))

    DT <- end_expr.ExprBuilder(eb, .parent_env = rlang::maybe_missing(.parent_env))
    data.table::setnames(DT, sub("^i.", "", names(DT)))
    start_expr.data.table(DT)
}
