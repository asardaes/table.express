#' Evaluate expression
#'
#' Helper to evaluate an expression contained in an object to facilitate using
#' [`%>%`][magrittr::pipe].
#'
#' @export
#'
#' @param .data Something containing an expression that should be evaluated.
#' @param ... Arguments for the specific methods.
#'
eval_expr <- function(.data, ...) { UseMethod("eval_expr") }

#' @rdname eval_expr
#' @export
#'
#' @details
#'
#' The method for [ExprBuilder] uses [base::eval()].
#'
eval_expr.ExprBuilder <- function(.data, ...) {
    base::eval(.data$expr)
}
