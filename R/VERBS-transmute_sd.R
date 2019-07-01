#' Transmute subset of data
#'
#' Like [transmute-table.express] but for a single call and maybe specifying `.SDcols`.
#'
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang enquo
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang quo_get_env
#' @importFrom rlang quo
#' @importFrom rlang quos
#'
#' @template data-arg
#' @param .SDcols See [data.table::data.table] and the details here.
#' @template transform-sd-args
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
#' Said function supports two pronouns that can be used by `.how` and `.SDcols`:
#'
#' - `.COL`: the actual values of the column.
#' - `.COLNAME`: the name of the column currently being evaluated.
#'
#' Unlike a call like `DT[, (vars) := expr]`, `.SDcols` can be created dynamically with an
#' expression that evaluates to something that would be used in place of `vars` *without* eagerly
#' using the captured `data.table`. See the examples here or in [table.express-package].
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     transmute_sd(grepl("^d", .COLNAME), .COL * 2)
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     transmute_sd(is.numeric(.COL), .COL * 2)
#'
transmute_sd <- function(.data, .SDcols = everything(), .how = identity, ...,
                         .parse = getOption("table.express.parse", FALSE),
                         .chain = getOption("table.express.chain", TRUE))
{
    how_quo <- rlang::enquo(.how)

    how_exprs <- to_expr(rlang::enexpr(.how), .parse = .parse)
    which_expr <- to_expr(rlang::enexpr(.SDcols), .parse = .parse)

    all_sdcols <- identical(which_expr, rlang::expr(everything()))
    colon_call <- rlang::is_call(which_expr, ":")

    if (evaled_is(how_quo, "function") &&
        (all_sdcols ||
         colon_call ||
         evaled_is(rlang::enquo(.SDcols), c("numeric", "character"))))
    {
        clause <- rlang::expr(lapply(.SD, !!how_exprs, !!!parse_dots(.parse, ...)))
        ans <- .data$set_select(clause, .chain)

        if (!all_sdcols) {
            if (colon_call) {
                frame_append(ans, .SDcols = !!which_expr, .parse = FALSE)
            }
            else {
                frame_append(ans, .SDcols = c(!!which_expr), .parse = FALSE)
            }
        }

        return(ans)
    }

    hows <- standardize_calls(how_exprs, rlang::quo_get_env(how_quo), ..., .parse = .parse)

    # just to avoid NOTE
    .transmute_matching <- EBCompanion$helper_functions$.transmute_matching

    clause <- rlang::expr(
        .transmute_matching(.SD, .which = rlang::quo(!!which_expr), .hows = rlang::quos(!!!hows))
    )

    .data$set_select(clause, .chain)
}
