#' @importFrom dplyr transmute
#' @export
#'
dplyr::transmute

#' Alias for select
#'
#' Because of the way `data.table` and the [select-table.express] method work, a
#' [dplyr::transmute()] equivalent can be specified as a `select` clause.
#'
#' @rdname transmute-table.express
#' @name transmute-table.express
#' @export
#'
#' @inheritParams select-table.express
#'
#' @inherit select-table.express details
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     transmute(ans = mpg * 2)
#'
transmute.ExprBuilder <- select.ExprBuilder
