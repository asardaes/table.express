#' Filter subset of data
#'
#' Helper to filter rows with the same condition applied to a subset of the data.
#'
#' @export
#' @importFrom rlang as_label
#' @importFrom rlang call_modify
#' @importFrom rlang call_standardise
#' @importFrom rlang enexpr
#' @importFrom rlang expr
#' @importFrom rlang quos
#' @importFrom rlang sym
#'
#' @template data-arg
#' @param .how The filtering function, function call, or expression.
#' @param ... More arguments for `.how` if it is a function or a function-call.
#' @param .SDcols See [data.table::data.table] and the details here.
#' @param .collapse See [where-table.express].
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' This function adds/chains a `where` expression that will be evaluated by
#' [data.table::data.table]. The `.how` condition is applied to all specified `.SDcols`, and it
#' supports the `.COL` pronoun.
#'
filter_sd <- function(.data, .how = Negate(is.na), ..., .SDcols, .collapse = `&`,
                      .parse = getOption("table.express.parse", FALSE), .chain = TRUE)
{
    force(.SDcols)
    dots <- parse_dots(.parse, ...)
    how_expr <- rlang::enexpr(.how)

    if (is_fun(.how)) {
        .how <- rlang::expr((!!how_expr)(.COL))
    }
    else {
        .how <- to_expr(how_expr, .parse = .parse)
    }

    .how <- rlang::call_standardise(.how)
    .how <- rlang::call_modify(.how, ... = rlang::zap(), !!!dots)

    which_col <- which(sapply(.how, function(how) { rlang::as_label(how) == ".COL" }))
    clauses <- Map(.col = .SDcols, .how = list(.how), f = function(.col, .how) {
        col_sym <- rlang::sym(.col)
        for (i in which_col) {
            .how[[i]] <- col_sym
        }
        .how
    })

    where(.data, !!!clauses, .collapse = !!rlang::enexpr(.collapse), .parse = FALSE, .chain = .chain)
}
