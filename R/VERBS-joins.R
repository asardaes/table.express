#' Joining verbs
#'
#' Two-table joins. Check the
#' ["Joining verbs" vignette](https://asardaes.github.io/table.express/articles/joins.html) for more
#' information.
#'
#' @rdname joins
#' @name joins
#'
#' @param x An [ExprBuilder] instance.
#' @param y A [data.table::data.table-class].
#' @param ... Expressions for the `on` part of the join.
#' @param nomatch,mult,roll,rollends See [data.table::data.table].
#' @param .parent_env See [end_expr()].
#' @param .SDcols For `mutate_join`. See the details below.
#' @param sort Passed to [data.table::merge].
#' @param allow Passed as [`data.table`][data.table::data.table]'s `allow.cartesian`.
#' @param .by_each For `mutate_join`. See the details below.
#'
#' @seealso
#'
#' [data.table::data.table], [dplyr::join]
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
name_switcheroo <- function(on) {
    chars <- unname(sapply(on, rlang::as_string))
    nms <- names(on)
    empty_names <- !nzchar(nms)

    if (any(empty_names)) {
        nms[empty_names] <- chars[empty_names]
    }

    names(nms) <- chars
    names(nms)[empty_names] <- ""
    rlang::syms(nms)
}

#' @importFrom rlang abort
#' @importFrom rlang call2
#' @importFrom rlang call_args
#' @importFrom rlang call_name
#'
comp_switcheroo <- function(on) {
    lapply(unname(on), function(e) {
        comp <- switch(rlang::call_name(e),
                       "==" = "==",
                       "<" = ">",
                       "<=" = ">=",
                       ">" = "<",
                       ">=" = "<=",
                       # default
                       rlang::abort("The 'on' expressions must be variables or comparisons."))

        args <- rev(rlang::call_args(e))
        rlang::call2(comp, !!!args)
    })
}

#' @importFrom rlang is_call
#'
name_comp_switcheroo <- function(on) {
    if (length(on) == 0L) {
        return(on)
    }

    calls <- sapply(on, rlang::is_call)
    non_calls <- !calls

    ans <- structure(rep(list(NULL), length(on)),
                     names = rep("", length(on)))

    ans[calls] <- comp_switcheroo(on[calls])
    if (any(non_calls)) {
        switched_names <- name_switcheroo(on[non_calls])
        ans[non_calls] <- switched_names
        names(ans)[non_calls] <- names(switched_names)
    }

    ans
}

#' @importFrom rlang is_call
#' @importFrom rlang is_missing
#' @importFrom rlang warn
#'
leftright_join <- function(eb, on, join_extras) {
    calls <- sapply(on, rlang::is_call)
    which_missing <- sapply(join_extras, rlang::is_missing)

    if (!which_missing[1L] && is.null(join_extras$nomatch) && all(which_missing[-1L]) && all(!calls)) {
        rlang::warn("Specifying 'nomatch = NULL' but none of ['mult', 'roll', 'rollends'] is equivalent to an inner join.")
    }

    if (length(on) > 0L) {
        frame_append(eb, on = list(!!!on))
    }

    frame_append(eb, !!!join_extras, .ignore_empty = "all")
}
