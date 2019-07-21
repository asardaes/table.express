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

#' @rdname extrema_by
#' @export
#' @importFrom rlang caller_env
#'
min_by.data.table <- function(.data, .col, ..., .expr = FALSE) {
    eb <- if (.expr) EagerExprBuilder$new(.data) else ExprBuilder$new(.data)
    lazy_ans <- min_by.ExprBuilder(eb, .col, ...)

    if (.expr) {
        lazy_ans
    }
    else {
        end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env())
    }
}
