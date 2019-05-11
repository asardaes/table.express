#' Transmute subset of data
#'
#' Like [transmute-table.express] but for a single function and maybe specifying `.SDcols`.
#'
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang enexprs
#'
#' @template data-arg
#' @param .fun The function to apply.
#' @param ... Further arguments for `.fun`.
#' @template parse-arg
#' @param .SDcols See [data.table::data.table] and the details.
#'
#' @details
#'
#' Providing no `.SDcols` would work like [dplyr::transmute_all()], and specifying them would be
#' similar to [dplyr::transmute_at()].
#'
transmute_sd <- function(.data, .fun, ..., .parse = FALSE, .SDcols) {
    dots <- lapply(rlang::enexprs(...), to_expr, .parse = .parse)

    .SD <- NULL
    ans <- select(.data, lapply(.SD, !!rlang::enexpr(.fun), !!!dots))

    if (!missing(.SDcols)) {
        frame_append(ans, .SDcols = !!rlang::enexpr(.SDcols))
    }

    ans
}
