#' Mutate subset of data
#'
#' Like [mutate-table.express] but possibly recycling calls.
#'
#' @export
#' @importFrom rlang call2
#' @importFrom rlang call_args
#' @importFrom rlang call_modify
#' @importFrom rlang call_standardise
#' @importFrom rlang enexpr
#' @importFrom rlang enquo
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang new_quosure
#' @importFrom rlang quo_get_env
#' @importFrom rlang quos
#' @importFrom rlang zap
#'
#' @template data-arg
#' @param .SDcols See [data.table::data.table] and the details here.
#' @param .how The function(s) or function call(s) that will perform the transformation.
#' @param ... Possibly more arguments for *all* functions/calls in `.how`.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' This function works similar to [transmute_sd()] but keeps all columns and *can* modify by
#' reference, like [mutate-table.express]. It can serve like
#' [`dplyr`'s scoped mutation variants][dplyr::mutate_all()] depending on what's given to `.SDcols`.
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
#'     mutate_sd(c("mpg", "cyl"), .COL * 2)
#'
mutate_sd <- function(.data, .SDcols, .how = identity, ...,
                      .parse = getOption("table.express.parse", FALSE),
                      .chain = getOption("table.express.chain", TRUE))
{
    .SDcols <- rlang::enquo(.SDcols)
    SDcols <- process_sdcols(.data, .SDcols)
    how_exprs <- to_expr(rlang::enexpr(.how), .parse = .parse)
    dots <- parse_dots(.parse, ...)

    if (!rlang::is_call(how_exprs, c(".", "list"))) {
        how_exprs <- list(how_exprs)
    }
    else {
        how_exprs <- rlang::call_args(how_exprs)
    }

    how_env <- rlang::quo_get_env(.SDcols)
    how <- lapply(how_exprs, function(how_expr) {
        if (evaled_is(rlang::new_quosure(how_expr, how_env), "function")) {
            how_expr <- rlang::call2(how_expr, rlang::expr(.COL))
        }

        if (rlang::is_call(how_expr)) {
            how_expr <- rlang::call_standardise(how_expr, how_env)
            how_expr <- rlang::call_modify(how_expr, ... = rlang::zap(), !!!dots)
        }

        how_expr
    })

    # just to avoid NOTE
    .mutate_matching <- EBCompanion$helper_functions$.mutate_matching

    mutate.ExprBuilder(.data, .parse = FALSE, .unquote_names = FALSE, .chain = .chain,
                       !!SDcols := .mutate_matching(.SD, !!SDcols, rlang::quos(!!!how)))
}
