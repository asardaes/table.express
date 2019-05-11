#' @importFrom dplyr transmute
#' @export
#'
dplyr::transmute

#' Add or adjust columns and keep only them
#'
#' Add or adjust columns of a [data.table::data.table-class], keeping only the resulting columns.
#'
#' @rdname transmute-table.express
#' @name transmute-table.express
#' @export
#' @importFrom rlang enexprs
#'
#' @template data-arg
#' @param ... Transmutation clauses.
#' @param .parse See [where-table.express].
#' @param .by_ref See [mutate-table.express].
#' @template chain-arg
#'
#' @details
#'
#' This first calls [mutate] and chains a [select] operation to get only the resulting columns.
#'
transmute.ExprBuilder <- function(.data, ..., .parse = FALSE, .by_ref = FALSE, .chain = TRUE) {
    cols <- names(rlang::enexprs(..., .named = TRUE))

    .data %>%
        mutate(..., .parse = .parse, .by_ref = .by_ref, .chain = .chain) %>%
        select(!!!cols, with = FALSE, .chain = .chain)
}
