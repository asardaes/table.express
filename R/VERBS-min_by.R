#' @rdname extrema_by
#' @export
#'
min_by <- function(.data, .col, ...) {
    UseMethod("min_by")
}

#' @rdname extrema_by
#' @export
#' @importFrom rlang expr
#' @importFrom rlang syms
#'
min_by.ExprBuilder <- function(.data, .col, ..., .some = FALSE, .chain = getOption("table.express.chain", TRUE)) {
    expressions <- lapply(rlang::syms(unname(.col)), function(col) {
        rlang::expr(!!col == min(!!col))
    })

    nested <- extrema_by(expressions, .some, ...)

    where.ExprBuilder(.data, nest_expr(.start = FALSE, .parse = FALSE, `{`(!!nested)), .parse = FALSE, .chain = .chain)
}
