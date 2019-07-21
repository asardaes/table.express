#' Set key to group by
#'
#' Group by setting key of the input.
#'
#' @export
#'
#' @param .data Object to be grouped and subsequently keyed.
#' @template generic-dots
#'
key_by <- function(.data, ...) {
    UseMethod("key_by")
}

#' @rdname key_by
#' @export
#' @importFrom rlang expr
#' @importFrom rlang quo_squash
#'
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' Everything in `...` will be wrapped in a call to `list`. Its contents work like Clauses for
#' grouping on columns. The `keyby` inside the [data.table::data.table] frame.
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     key_by(cyl, gear)
#'
key_by.ExprBuilder <- function(.data, ...,
                               .parse = getOption("table.express.parse", FALSE),
                               .chain = getOption("table.express.chain", TRUE))
{
    clause <- parse_dots(.parse, ...)
    clause <- rlang::quo_squash(rlang::expr(list(!!!clause)))
    attr(clause, "key_by") <- TRUE
    .data$set_by(clause, .chain)
}

#' @rdname key_by
#' @export
#'
key_by.data.table <- function(.data, ...) {
    eb <- EagerExprBuilder$new(.data)
    key_by.ExprBuilder(eb, ...)
}
