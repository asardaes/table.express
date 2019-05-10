#' Transmute subset of data
#'
#' Like [transmute-table.express] but for a single function and maybe specifying `.SDcols`.
#'
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang enexprs
#'
#' @param .data An [ExprBuilder] instance.
#' @param .fun The function to apply.
#' @param ... Further arguments for `.fun`.
#' @param .parse Whether to apply [rlang::parse_expr()] to `...`.
#' @param .SDcols See [data.table::data.table].
#'
#' @details
#'
#' Providing no `.SDcols` would work like [dplyr::transmute_all()].
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
