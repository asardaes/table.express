#' Mutate subset of data
#'
#' Like [mutate-table.express] but possibly recycling calls.
#'
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang enquo
#' @importFrom rlang quo_get_env
#' @importFrom rlang quos
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
    how_env <- rlang::quo_get_env(.SDcols)
    how <- standardize_calls(how_exprs, how_env, ..., .parse = .parse)

    # just to avoid NOTE
    .mutate_matching <- EBCompanion$helper_functions$.mutate_matching

    mutate.ExprBuilder(.data, .parse = FALSE, .unquote_names = FALSE, .chain = .chain,
                       !!SDcols := .mutate_matching(.SD, !!SDcols, rlang::quos(!!!how)))
}
