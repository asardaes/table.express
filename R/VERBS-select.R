#' @importFrom dplyr select
#' @export
#'
dplyr::select

#' Select clause
#'
#' Select or compute over columns of a [data.table::data.table-class].
#'
#' @rdname select-table.express
#' @name select-table.express
#' @export
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang quos
#'
#' @template data-arg
#' @param ... Clause for selecting/computing on columns. The `j` inside the `data.table`'s frame.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' If `length(...) == 1L` and the expression is a call to `:` (e.g. `a:c`) or
#' [tidyselect::select_helpers], then it will be taken as the single expression for the select
#' clause, otherwise everything in `...` will be wrapped in a call to [base::list()].
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     select(mpg:cyl)
#'
select.ExprBuilder <- function(.data, ...,
                               .parse = getOption("table.express.parse", FALSE),
                               .chain = getOption("table.express.chain", TRUE))
{
    clause <- parse_dots(.parse, ...)

    if (length(clause) == 1L && is_tidyselect_call(clause[[1L]])) {
        # just to avoid NOTE
        .transmute_matching <- EBCompanion$helper_functions$.transmute_matching

        clause <- rlang::expr(Map(
            .transmute_matching,
            .COL = .SD,
            .COLNAME = names(.SD),
            .COLNAMES = list(names(.SD)),
            .which = rlang::quos(!!clause[[1L]]),
            .how = rlang::quos(.COL)
        ))
    }
    else if (length(clause) == 1L && rlang::is_call(clause[[1L]], ":")) {
        clause <- clause[[1L]]
    }
    else {
        clause <- rlang::expr(list(!!!clause))
    }

    .data$set_select(clause, .chain)
}
