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

#' @importFrom rlang as_string
#' @importFrom rlang syms
#'
name_switcheroo <- function(symbols, named = TRUE, as_sym = TRUE) {
    chars <- unname(sapply(symbols, rlang::as_string))
    nms <- names(symbols)
    empty_names <- !nzchar(nms)

    if (is.null(nms)) {
        nms <- chars
    }
    else if (any(empty_names)) {
        nms[empty_names] <- chars[empty_names]
    }

    if (named) {
        names(nms) <- chars
        names(nms)[empty_names] <- ""
    }

    if (as_sym) {
        rlang::syms(nms)
    }
    else {
        nms
    }
}

#' @importFrom rlang is_missing
#' @importFrom rlang warn
#'
leftright_join <- function(eb, on, join_extras) {
    which_missing <- sapply(join_extras, rlang::is_missing)
    if (!which_missing[1L] && is.null(join_extras$nomatch) && all(which_missing[-1L])) {
        rlang::warn("Specifying 'nomatch = NULL' but none of ['mult', 'roll', 'rollends'] is equivalent to an inner join.")
    }

    frame_append(eb, on = list(!!!on), !!!join_extras)
}
