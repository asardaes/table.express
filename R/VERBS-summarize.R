#' @importFrom dplyr summarize
#' @export
#'
dplyr::summarize

#' @importFrom dplyr summarise
#' @export
#'
dplyr::summarise

gforce_optimized <- c(
    "min",
    "max",
    "mean",
    "median",
    "var",
    "sd",
    "sum",
    "prod",
    "first",
    "last"
)

#' Summarize columns
#'
#' Compute summaries for columns, perhaps by group.
#'
#' @rdname summarize-table.express
#' @name summarize-table.express
#' @export
#' @importFrom rlang expr
#' @importFrom rlang is_call
#'
#' @template data-arg
#' @param ... Clauses for transmuting columns. For `j` inside the `data.table`'s frame.
#' @param .assume_optimized An optional character vector with function names that you know
#' `data.table` can optimize. This will be added to this set of known names:
#' `r paste0(gforce_optimized, collapse = ", ")`. Note that using those functions (and only those in
#' a given call to this function) will prevent the expressions from using variables created by
#' previous expressions.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' The built expression is similar to what `transmute` builds, but the function also checks that the
#' results have length 1.
#'
#' @template docu-examples
#'
summarize.ExprBuilder <- function(.data, ..., .assume_optimized = NULL,
                                  .parse = getOption("table.express.parse", FALSE),
                                  .chain = getOption("table.express.chain", TRUE))
{
    clauses <- parse_dots(.parse, ...)
    if (length(clauses) == 0L) return(.data)

    gforce_optimizable <- sapply(clauses, rlang::is_call, name = c(gforce_optimized, .assume_optimized))

    if (all(gforce_optimizable)) {
        .data$set_j(rlang::expr(list(!!!clauses)), .chain)
    }
    else {
        named_clauses <- get_named_clauses(clauses)
        call_args <- body_from_clauses(named_clauses$clauses)
        # avoid NOTE
        .validate_summaries <- EBCompanion$helper_functions$.validate_summaries
        .data$set_j(rlang::expr(.validate_summaries(!!call_args)), .chain)
    }
}

#' @rdname summarize-table.express
#' @export
#'
summarise.ExprBuilder <- summarize.ExprBuilder

#' @rdname summarize-table.express
#' @export
#' @importFrom rlang caller_env
#'
#' @param .parent_env See [end_expr()]
#'
summarize.EagerExprBuilder <- function(.data, ..., .parent_env = rlang::caller_env()) {
    end_expr.ExprBuilder(summarize.ExprBuilder(.data, ...), .parent_env = .parent_env)
}

#' @rdname summarize-table.express
#' @export
#'
summarise.EagerExprBuilder <- summarize.EagerExprBuilder

#' @rdname summarize-table.express
#' @export
#' @importFrom rlang caller_env
#'
summarize.data.table <- function(.data, ...) {
    eb <- ExprBuilder$new(.data)
    lazy_ans <- summarize.ExprBuilder(eb, ...)
    try_delegate("summarize", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
}

#' @rdname summarize-table.express
#' @export
#'
summarise.data.table <- summarize.data.table
