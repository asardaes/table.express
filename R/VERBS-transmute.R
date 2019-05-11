#' @importFrom dplyr transmute
#' @export
#'
dplyr::transmute

#' Add or adjust columns and keep only the result
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
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' This first calls [mutate-table.express] and chains a [select-table.express] operation to get only
#' the resulting columns.
#'
transmute.ExprBuilder <- function(.data, ..., .parse = FALSE, .chain = TRUE) {
    cols <- names(rlang::enexprs(..., .named = TRUE))

    .data %>%
        mutate(..., .parse = .parse, .chain = .chain) %>%
        select(!!!cols, with = FALSE, .chain = .chain)
}
