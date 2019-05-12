#' Transmute subset of data
#'
#' Like [transmute-table.express] but for a single call and maybe specifying `.SDcols`.
#'
#' @export
#' @importFrom rlang call_modify
#' @importFrom rlang call_standardise
#' @importFrom rlang enexpr
#' @importFrom rlang is_call
#' @importFrom rlang quos
#' @importFrom rlang zap
#'
#' @template data-arg
#' @param .how The transmuting call.
#' @param ... More arguments for `.how` if it is a function or a function-call.
#' @param .SDcols See [data.table::data.table] and the details here.
#' @template parse-arg
#'
#' @details
#'
#' Like [transmute-table.express], this function never modifies the input by reference. However,
#' this function does operate on [ExprBuilder] instances, and adds/chains a `select` expression that
#' will be evaluated by [data.table::data.table], possibly specifying the helper function
#' `.transmute_matching`, which is assigned to the final expression's evaluation environment when
#' calling [end_expr()] (i.e., [ExprBuilder]'s `eval` method).
#'
#' Said function includes two pronouns that can be used by `.how` and `.SDcols`:
#'
#' - `.COL`: the actual values of the column.
#' - `.COLNAME`: the name of the column currently being evaluated.
#'
#' Unlike a call like `DT[, (vars) := expr]`, `.SDcols` can be created dynamically with an
#' expression that evaluates to something that would be used in place of `vars`. See the examples.
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     transmute_sd(.COL * 2, .SDcols = grepl("^d", .COLNAME)) %>%
#'     end_expr
#'
transmute_sd <- function(.data, .how = identity, ..., .SDcols = names(.SD), .parse = FALSE) {
    dots <- parse_dots(.parse, ...)

    if (is_fun(.how)) {
        ans <- select(.data, lapply(.SD, !!rlang::enexpr(.how), !!!dots))

        if (!missing(.SDcols)) {
            frame_append(ans, .SDcols = !!rlang::enexpr(.SDcols))
        }
    }
    else {
        .which <- to_expr(rlang::enexpr(.SDcols), .parse = .parse)
        .how <- to_expr(rlang::enexpr(.how), .parse = .parse)

        if (rlang::is_call(.how)) {
            .how <- rlang::call_standardise(.how)
            .how <- rlang::call_modify(.how, ... = rlang::zap(), !!!dots)
        }

        # just to avoid NOTE
        .transmute_matching <- EBCompanion$helper_functions$.transmute_matching

        ans <- select(.data, Map(.transmute_matching,
                                 .COL = .SD,
                                 .COLNAME = names(.SD),
                                 .which = rlang::quos(!!.which),
                                 .how = rlang::quos(!!.how)))
    }

    ans
}
