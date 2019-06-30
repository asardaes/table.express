#' @importFrom dplyr select
#' @export
#'
dplyr::select

#' Select clause
#'
#' Select columns of a [data.table::data.table-class].
#'
#' @rdname select-table.express
#' @name select-table.express
#' @export
#' @importFrom rlang as_string
#' @importFrom rlang expr
#' @importFrom rlang is_call
#'
#' @template data-arg
#' @param ... Clause for selecting columns. For `j` inside the `data.table`'s frame.
#' @param .negate Whether to negate the selection semantics and keep only columns that do *not*
#'   match what's given in `...`.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' The expressions in `...` support [tidyselect::select_helpers].
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
select.ExprBuilder <- function(.data, ..., .negate = FALSE,
                               .parse = getOption("table.express.parse", FALSE),
                               .chain = getOption("table.express.chain", TRUE))
{
    clauses <- parse_dots(.parse, ...)
    if (length(clauses) == 0L) return(.data)

    non_calls <- !sapply(clauses, rlang::is_call)
    nums <- sapply(clauses, is_num) & !nzchar(names(clauses))

    if (all(non_calls | nums)) {
        if (all(nums)) {
            if (.negate) {
                clause <- rlang::expr(!c(!!!clauses))
            }
            else {
                clause <- rlang::expr(c(!!!clauses))
            }
        }
        else {
            if (.negate) {
                clauses <- sapply(clauses, rlang::as_string)
                clause <- rlang::expr(!c(!!!clauses))
            }
            else {
                clause <- rlang::expr(list(!!!clauses))
            }
        }
    }
    else {
        # avoid NOTE
        .select_matching <- EBCompanion$helper_functions$.select_matching
        clause <- rlang::expr(.select_matching(.SD, !!!clauses, .negate = !!.negate))
    }

    .data$set_select(clause, .chain)
}

#' @importFrom rlang call_args
#' @importFrom rlang is_call
#'
is_num <- function(e) {
    if (rlang::is_call(e, ":") && all(sapply(rlang::call_args(e), is.numeric))) {
        TRUE
    }
    else {
        is.numeric(e)
    }
}
