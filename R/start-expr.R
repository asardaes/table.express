#' Start expression
#'
#' Helper to capture a [data.table::data.table-class] and begin building an expression for it.
#'
#' @export
#'
#' @param .DT The data.table.
#'
start_expr <- function(.DT) {
    ExprBuilder$new(.DT)
}
