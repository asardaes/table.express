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
#' @importFrom rlang enquo
#' @importFrom rlang expr
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
transmute_sd <- function(.data, .how = identity, ..., .SDcols = everything(),
                         .parse = getOption("table.express.parse", FALSE),
                         .chain = getOption("table.express.chain", TRUE))
{
    dots <- parse_dots(.parse, ...)
    how_quo <- rlang::enquo(.how)

    .how <- to_expr(rlang::enexpr(.how), .parse = .parse)
    .which <- to_expr(rlang::enexpr(.SDcols), .parse = .parse)

    all_sdcols <- identical(.which, rlang::expr(everything()))

    if (evaled_is(how_quo, "function")) {
        if (all_sdcols || evaled_is(.which, c("numeric", "character"))) {
            clause <- rlang::expr(lapply(.SD, !!.how, !!!dots))
            ans <- .data$set_select(clause, .chain)

            if (!all_sdcols) {
                frame_append(ans, .SDcols = c(!!.which), .parse = FALSE)
            }

            return(ans)
        }

        .how <- rlang::call2(.how, rlang::expr(.COL))
    }

    .how <- rlang::call_standardise(.how, rlang::caller_env())
    .how <- rlang::call_modify(.how, ... = rlang::zap(), !!!dots)

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
