#' Joining verbs
#'
#' See also [dplyr::join].
#'
#' @rdname joins
#' @name joins
#' @aliases joins-table.express
#'
#' @examples
#'
#' lhs <- data.table::data.table(x = rep(c("b", "a", "c"), each = 3),
#'                               y = c(1, 3, 6),
#'                               v = 1:9)
#'
#' rhs <- data.table::data.table(x = c("c", "b"),
#'                               v = 8:7,
#'                               foo = c(4, 2))
#'
NULL

# ==================================================================================================

#' @importFrom dplyr left_join
#' @export
#'
dplyr::left_join

# ==================================================================================================

#' @rdname joins
#' @export
#' @importFrom rlang caller_env
#' @importFrom rlang enexprs
#' @importFrom rlang enquo
#' @importFrom rlang maybe_missing
#'
#' @param x An [ExprBuilder] instance.
#' @param y A [data.table::data.table-class].
#' @param ... Expressions for the `on` part of the join.
#' @param nomatch See [data.table::data.table].
#' @param mult See [data.table::data.table].
#' @param roll See [data.table::data.table].
#' @param rollends See [data.table::data.table].
#' @param .adding A possibly named character vector specifying which columns from `y` are part of
#'   the join.
#'
#' @details
#'
#' TBD
#'
#' @examples
#'
#' # creates new data.table
#' lhs %>%
#'     start_expr %>%
#'     left_join(rhs, x) %>%
#'     end_expr
#'
#' # would modify lhs by reference
#' lhs %>%
#'     start_expr %>%
#'     left_join(rhs, x, .adding = c("foo", rhs.v = "v"))
#'
left_join.ExprBuilder <- function(x, y, ..., nomatch, mult, roll, rollends, .adding) {
    y <- rlang::enquo(y)
    on <- rlang::enexprs(...)
    adding <- rlang::maybe_missing(.adding)
    join_extras <- list(
        nomatch = rlang::maybe_missing(nomatch),
        mult = rlang::maybe_missing(mult),
        roll = rlang::maybe_missing(roll),
        rollends = rlang::maybe_missing(rollends)
    )

    x$join("left", y, on, adding, join_extras, parent_env = rlang::caller_env())
}
