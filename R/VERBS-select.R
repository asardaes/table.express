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
#' @importFrom rlang maybe_missing
#' @importFrom rlang missing_arg
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

        calls <- !non_calls
        if (.negate || any(nums) || (any(calls) && (
            any(sapply(clauses[calls], rlang::is_call, name = ":")) ||
            any(sapply(clauses[calls], is_tidyselect_call))
        ))) {
            needs_sd <- TRUE
        }
        else {
            needs_sd <- FALSE
        }

        .SD <- if (needs_sd) rlang::expr(.SD) else rlang::missing_arg()
        clause <- rlang::expr(.select_matching(!!rlang::maybe_missing(.SD), !!!clauses, .negate = !!.negate))
    }

    .data$set_j(clause, .chain)
}

#' @rdname select-table.express
#' @export
#' @importFrom rlang caller_env
#'
#' @param .parent_env See [end_expr()]
#'
select.EagerExprBuilder <- function(.data, ..., .parent_env = rlang::caller_env()) {
    end_expr.ExprBuilder(select.ExprBuilder(.data, ...), .parent_env = .parent_env)
}

#' @rdname select-table.express
#' @export
#' @importFrom rlang caller_env
#'
select.data.table <- function(.data, ...) {
    eb <- ExprBuilder$new(.data)
    lazy_ans <- select.ExprBuilder(eb, ...)
    end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env())
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
