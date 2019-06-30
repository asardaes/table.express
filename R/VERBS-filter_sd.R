#' Filter subset of data
#'
#' Helper to filter rows with the same condition applied to a subset of the data.
#'
#' @export
#' @importFrom rlang call2
#' @importFrom rlang call_modify
#' @importFrom rlang call_standardise
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#' @importFrom rlang enquo
#' @importFrom rlang expr
#' @importFrom rlang syms
#' @importFrom rlang zap
#'
#' @template data-arg
#' @param .SDcols See [data.table::data.table] and the details here.
#' @param .how The filtering function or predicate.
#' @param ... Possibly more arguments for `.how`.
#' @param .collapse See [where-table.express].
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' This function adds/chains an `i` expression that will be evaluated by [data.table::data.table],
#' and it supports the `.COL` pronoun. The `.how` condition is applied to all `.SDcols`.
#'
#' @template tidyselect-sdcols
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     filter_sd(c("vs", "am"), .COL == 1)
#'
filter_sd <- function(.data, .SDcols, .how = Negate(is.na), ..., .collapse = `&`,
                      .parse = getOption("table.express.parse", FALSE),
                      .chain = getOption("table.express.chain", TRUE))
{
    .SDcols <- process_sdcols(.data, rlang::enquo(.SDcols))

    how_expr <- rlang::enexpr(.how)
    dots <- parse_dots(.parse, ...)

    if (evaled_is(rlang::enquo(.how), "function")) {
        .how <- rlang::call2(how_expr, rlang::expr(.COL))
    }
    else {
        .how <- to_expr(how_expr, .parse = .parse)
    }

    .how <- rlang::call_standardise(.how, rlang::caller_env())
    .how <- rlang::call_modify(.how, ... = rlang::zap(), !!!dots)

    clauses <- Map(substitue_col_pronoun, list(.how), rlang::syms(.SDcols))

    where(.data, !!!clauses, .collapse = !!rlang::enexpr(.collapse), .parse = FALSE, .chain = .chain)
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
        else if (rlang::as_label(sub_ex) == ".COL") {
            ex[[i]] <- target_sym
        }
    }

    ex
}
