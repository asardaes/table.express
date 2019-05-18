#' Mutate subset of data
#'
#' Like [mutate-table.express] but for a single call and some `.SDcols`.
#'
#' @export
#' @importFrom rlang call2
#' @importFrom rlang call_modify
#' @importFrom rlang call_standardise
#' @importFrom rlang enexpr
#' @importFrom rlang expr
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
#' This function works similar to [transmute_sd()] but keeps all columns and *can* modify by
#' reference, like [mutate-table.express]. Unlike [transmute_sd()], `.SDcols` **must** be a
#' character vector.
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     mutate_sd(.COL * 2, .SDcols = c("mpg", "cyl")) %>%
#'     end_expr
#'
mutate_sd <- function(.data, .how = identity, ..., .SDcols, .parse = getOption("table.express.parse", FALSE)) {
    force(.SDcols)

    dots <- parse_dots(.parse, ...)

    if (is_fun(.how)) {
        .how <- rlang::call2(rlang::enexpr(.how), rlang::expr(.COL), !!!dots)
    }
    else {
        .how <- to_expr(rlang::enexpr(.how), .parse = .parse)

        if (rlang::is_call(.how)) {
            .how <- rlang::call_standardise(.how)
            .how <- rlang::call_modify(.how, ... = rlang::zap(), !!!dots)
        }
    }

    # just to avoid NOTE
    .mutate_matching <- EBCompanion$helper_functions$.mutate_matching
    .non_null <- EBCompanion$helper_functions$.non_null

    mutate(.data, .parse = FALSE, .unquote_names = FALSE,
           !!.SDcols := .non_null(
               Map(.mutate_matching,
                   .COL = .SD,
                   .COLNAME = names(.SD),
                   .which = list(!!.SDcols),
                   .how = rlang::quos(!!.how))
           ))
}