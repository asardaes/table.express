#' Nest expressions as a functional chain
#'
#' @export
#' @importFrom rlang expr
#'
#' @param ... Expressions that will be part of the functional chain.
#' @param .start Whether to add a [start_expr()] call at the beginning of the chain.
#' @param .end Whether to add an [end_expr()] call at the end of the chain.
#' @template parse-arg
#'
#' @details
#'
#' All expressions in `...` are "collapsed" with [`%>%`][magrittr::pipe], passing the
#' [ExprBuilder]'s captured `data.table` as the initial parameter. Names are silently dropped.
#'
#' The chain is evaluated eagerly and saved in the `ExprBuilder` instance to be used during final
#' expression evaluation.
#'
#' @template docu-examples
#'
nest_expr <- function(..., .start = TRUE, .end = .start, .parse = getOption("table.express.parse", FALSE)) {
    dots <- unname(parse_dots(.parse, ...))

    if (.start) {
        dots <- c(list(rlang::expr(start_expr)), dots)
    }
    if (.end) {
        dots <- c(dots, list(rlang::expr(end_expr)))
    }

    dots
}
