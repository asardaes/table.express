#' Filter subset of data
#'
#' Helper to filter rows with the same condition applied to a subset of the data.
#'
#' @export
#'
filter_sd <- function(.data, .SDcols, .how = Negate(is.na), ...) {
    UseMethod("filter_sd")
}

#' @rdname filter_sd
#' @export
#' @importFrom rlang call2
#' @importFrom rlang call_modify
#' @importFrom rlang call_standardise
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#' @importFrom rlang enquo
#' @importFrom rlang expr
#' @importFrom rlang f_rhs
#' @importFrom rlang is_formula
#' @importFrom rlang maybe_missing
#' @importFrom rlang syms
#' @importFrom rlang zap
#'
#' @template data-arg
#' @param .SDcols See [data.table::data.table] and the details here.
#' @param .how The filtering function or predicate.
#' @param ... Possibly more arguments for `.how`.
#' @param which Passed to [data.table::data.table].
#' @param .collapse See [where-table.express].
#' @template parse-arg
#' @template chain-arg
#' @param .caller_env_n Internal. Passed to [rlang::caller_env()] to find the function specified in
#'   `.how` and [standardize][rlang::call_standardise()] its call.
#'
#' @details
#'
#' This function adds/chains an `i` expression that will be evaluated by [data.table::data.table],
#' and it supports the `.COL` pronoun and lambdas as formulas. The `.how` condition is applied to
#' all `.SDcols`.
#'
#' @template tidyselect-sdcols
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     filter_sd(c("vs", "am"), ~ .x == 1)
#'
filter_sd.ExprBuilder <- function(.data, .SDcols, .how = Negate(is.na), ..., which, .collapse = `&`,
                                  .parse = getOption("table.express.parse", FALSE),
                                  .chain = getOption("table.express.chain", TRUE),
                                  .caller_env_n = 1L)
{
    .SDcols <- process_sdcols(.data, rlang::enquo(.SDcols))

    how_expr <- rlang::enexpr(.how)
    dots <- parse_dots(.parse, ...)

    if (evaled_is(rlang::enquo(.how), "function")) {
        .how <- rlang::call2(how_expr, rlang::expr(.COL))
    }
    else {
        .how <- to_expr(how_expr, .parse = .parse)
        if (rlang::is_formula(.how)) {
            .how <- rlang::f_rhs(.how)
        }
    }

    .how <- rlang::call_standardise(.how, rlang::caller_env(.caller_env_n))
    .how <- rlang::call_modify(.how, ... = rlang::zap(), !!!dots)

    clauses <- Map(substitue_col_pronoun, list(.how), rlang::syms(.SDcols))

    where(.data, !!!clauses, which = rlang::maybe_missing(which), .collapse = !!rlang::enexpr(.collapse),
          .parse = FALSE, .chain = .chain)
}

#' @rdname filter_sd
#' @export
#' @importFrom rlang caller_env
#'
#' @template expr-arg
#'
filter_sd.data.table <- function(.data, ..., .expr = FALSE) {
    eb <- if (.expr) EagerExprBuilder$new(.data) else ExprBuilder$new(.data)
    lazy_ans <- filter_sd.ExprBuilder(eb, ..., .caller_env_n = 2L)

    if (.expr) {
        lazy_ans
    }
    else {
        end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env())
    }
}

#' @importFrom rlang as_label
#' @importFrom rlang is_call
#'
substitue_col_pronoun <- function(ex, target_sym) {
    for (i in seq_along(ex)) {
        sub_ex <- ex[[i]]

        if (rlang::is_call(sub_ex)) {
            ex[[i]] <- substitue_col_pronoun(sub_ex, target_sym)
        }
        else if (rlang::as_label(sub_ex) %in% c(".COL", ".", ".x")) {
            ex[[i]] <- target_sym
        }
    }

    ex
}
