#' Transmute subset of data
#'
#' Like [transmute-table.express] but for a single call and maybe specifying `.SDcols`.
#'
#' @export
#' @importFrom rlang call2
#' @importFrom rlang call_modify
#' @importFrom rlang call_standardise
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang quos
#' @importFrom rlang zap
#'
#' @template data-arg
#' @template transform-sd-args
#' @param .SDcols See [data.table::data.table] and the details here.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' Like [transmute-table.express], this function never modifies the input by reference. This
#' function adds/chains a `select` expression that will be evaluated by [data.table::data.table],
#' possibly specifying the helper function `.transmute_matching`, which is assigned to the final
#' expression's evaluation environment when calling [end_expr()] (i.e., [ExprBuilder]'s `eval`
#' method).
#'
#' Said function includes two pronouns that can be used by `.how` and `.SDcols`:
#'
#' - `.COL`: the actual values of the column.
#' - `.COLNAME`: the name of the column currently being evaluated.
#'
#' Unlike a call like `DT[, (vars) := expr]`, `.SDcols` can be created dynamically with an
#' expression that evaluates to something that would be used in place of `vars` *without* using the
#' captured `data.table`. See the examples here or in [table.express-package].
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     transmute_sd(.COL * 2, .SDcols = grepl("^d", .COLNAME))
#'
transmute_sd <- function(.data, .how = identity, ..., .SDcols = names(.SD),
                         .parse = getOption("table.express.parse", FALSE),
                         .chain = getOption("table.express.chain", TRUE))
{
    dots <- parse_dots(.parse, ...)
    how_expr <- rlang::enexpr(.how)

    if (is_fun(.how)) {
        .how <- rlang::call2(how_expr, rlang::expr(.COL))
    }
    else {
        .how <- to_expr(how_expr, .parse = .parse)
    }

    .which <- to_expr(rlang::enexpr(.SDcols), .parse = .parse)

    if (rlang::is_call(.how)) {
        .how <- rlang::call_standardise(.how, rlang::caller_env())
        .how <- rlang::call_modify(.how, ... = rlang::zap(), !!!dots)
    }

    # just to avoid NOTE
    .non_null <- EBCompanion$helper_functions$.non_null
    .transmute_matching <- EBCompanion$helper_functions$.transmute_matching

    clause <- rlang::expr(
        .non_null(Map(
            .transmute_matching,
            .COL = .SD,
            .COLNAME = names(.SD),
            .COLNAMES = list(names(.SD)),
            .which = rlang::quos(!!.which),
            .how = rlang::quos(!!.how)
        ))
    )

    .data$set_select(clause, .chain)
}
