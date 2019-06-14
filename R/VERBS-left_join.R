#' @importFrom dplyr left_join
#' @export
#'
dplyr::left_join

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang enexprs
#' @importFrom rlang is_missing
#' @importFrom rlang maybe_missing
#' @importFrom rlang warn
#'
#' @param x An [ExprBuilder] instance.
#' @param y A [data.table::data.table-class].
#' @param ... Expressions for the `on` part of the join.
#' @param nomatch See [data.table::data.table].
#' @param mult See [data.table::data.table].
#' @param roll See [data.table::data.table].
#' @param rollends See [data.table::data.table].
#'
#' @examples
#'
#' # creates new data.table
#' lhs %>%
#'     start_expr %>%
#'     left_join(rhs, x) %>%
#'     end_expr
#'
left_join.ExprBuilder <- function(x, y, ..., nomatch, mult, roll, rollends) {
    on <- rlang::enexprs(...)
    on <- name_switcheroo(on)

    join_extras <- list(
        nomatch = rlang::maybe_missing(nomatch),
        mult = rlang::maybe_missing(mult),
        roll = rlang::maybe_missing(roll),
        rollends = rlang::maybe_missing(rollends)
    )

    which_missing <- sapply(join_extras, rlang::is_missing)
    if (!which_missing[1L] && is.null(join_extras$nomatch) && all(which_missing[-1L])) {
        rlang::warn("Specifying 'nomatch = NULL' but none of ['mult', 'roll', 'rollends'] is equivalent to an inner join.")
    }

    eb <- x$chain("pronoun", y)
    frame_append(eb, on = list(!!!on), !!!join_extras)
}
