#' Mutate subset of data
#'
#' Like [mutate-table.express] but for a single call and some `.SDcols`.
#'
#' @export
#' @importFrom rlang call2
#' @importFrom rlang call_modify
#' @importFrom rlang call_standardise
#' @importFrom rlang caller_env
#' @importFrom rlang enexpr
#' @importFrom rlang enquo
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
#' This function works similar to [transmute_sd()] but keeps all columns and *can* modify by
#' reference, like [mutate-table.express].
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
#'     mutate_sd(.COL * 2, .SDcols = c("mpg", "cyl"))
#'
mutate_sd <- function(.data, .how = identity, ..., .SDcols,
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

    if (rlang::is_call(.how)) {
        .how <- rlang::call_standardise(.how, rlang::caller_env())
        .how <- rlang::call_modify(.how, ... = rlang::zap(), !!!dots)
    }

    # just to avoid NOTE
    .mutate_matching <- EBCompanion$helper_functions$.mutate_matching

    mutate(.data, .parse = FALSE, .unquote_names = FALSE, .chain = .chain,
           !!.SDcols := Map(.mutate_matching, .COL = .SD, .how = rlang::quos(!!.how))) %>%
        frame_append(.SDcols = !!.SDcols)
}
