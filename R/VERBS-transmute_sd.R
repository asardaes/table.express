#' Transmute subset of data
#'
#' Like [transmute-table.express] but for a single call and maybe specifying `.SDcols`.
#'
#' @export
#' @importFrom rlang enquo
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang quo_get_env
#' @importFrom rlang quo_get_expr
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
    which_quo <- rlang::enquo(.SDcols)
    how_quo <- rlang::enquo(.how)

    which_expr <- to_expr(rlang::quo_get_expr(which_quo), .parse = .parse)
    how_exprs <- to_expr(rlang::quo_get_expr(how_quo), .parse = .parse)

    all_sdcols <- identical(which_expr, rlang::expr(everything()))
    colon_call <- rlang::is_call(which_expr, ":")

    if (can_combine_lapply(which_quo, how_quo)) {
        hows <- standardize_lapplys(how_exprs, ..., .parse = .parse)
        ans <- .data$set_select(hows, .chain)

        if (!all_sdcols) {
            if (colon_call) {
                frame_append(ans, .SDcols = !!which_expr, .parse = FALSE)
            }
            else {
                frame_append(ans, .SDcols = c(!!which_expr), .parse = FALSE)
            }
        }
    }
    else {
        hows <- standardize_calls(how_exprs, rlang::quo_get_env(how_quo), ..., .parse = .parse)

        # just to avoid NOTE
        .transmute_matching <- EBCompanion$helper_functions$.transmute_matching

        clause <- rlang::expr(
            .transmute_matching(.SD, .which = rlang::quo(!!which_expr), .hows = rlang::quos(!!!hows))
        )

        ans <- .data$set_select(clause, .chain)
    }

    ans
}
