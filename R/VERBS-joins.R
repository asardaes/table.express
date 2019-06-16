#' Joining verbs
#'
#' Two-table joins. Non-equi joins have their own [documentation entry][non-equi_joins]. Check the
#' "Joining verbs" vignette too.
#'
#' @rdname joins
#' @name joins
#'
#' @param x An [ExprBuilder] instance.
#' @param y A [data.table::data.table-class].
#' @param ... Expressions for the `on` part of the join.
#' @param nomatch See [data.table::data.table].
#' @param mult See [data.table::data.table].
#' @param roll See [data.table::data.table].
#' @param rollends See [data.table::data.table].
#'
#' @seealso
#'
#' [non-equi_joins], [data.table::data.table], [dplyr::join]
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

#' @importFrom rlang as_string
#' @importFrom rlang syms
#'
name_switcheroo <- function(symbols) {
    chars <- unname(sapply(symbols, rlang::as_string))
    nms <- names(symbols)
    empty_names <- !nzchar(nms)

    if (any(empty_names)) {
        nms[empty_names] <- chars[empty_names]
    }

    names(nms) <- chars
    names(nms)[empty_names] <- ""
    rlang::syms(nms)
}

#' @importFrom rlang is_missing
#' @importFrom rlang warn
#'
leftright_join <- function(eb, on, join_extras) {
    which_missing <- sapply(join_extras, rlang::is_missing)
    if (!which_missing[1L] && is.null(join_extras$nomatch) && all(which_missing[-1L])) {
        rlang::warn("Specifying 'nomatch = NULL' but none of ['mult', 'roll', 'rollends'] is equivalent to an inner join.")
    }

    frame_append(eb, on = list(!!!on), !!!join_extras, .ignore_empty = "all")
}
