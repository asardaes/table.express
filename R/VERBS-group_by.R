#' @importFrom dplyr group_by
#' @export
#'
dplyr::group_by

#' Grouping clauses
#'
#' Grouping by columns of a [data.table::data.table].
#'
#' @rdname group_by-table.express
#' @name group_by-table.express
#' @export
#' @importFrom rlang expr
#' @importFrom rlang quo_squash
#'
#' @template data-arg
#' @param ... Clause for grouping on columns. The `by` inside the `data.table`'s frame.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' Everything in `...` will be wrapped in a call to `list`.
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     group_by(cyl, gear)
#'
group_by.ExprBuilder <- function(.data, ...,
                                 .parse = getOption("table.express.parse", FALSE),
                                 .chain = getOption("table.express.chain", TRUE))
{
    clause <- parse_dots(.parse, ...)
    .data$set_by(rlang::quo_squash(rlang::expr(list(!!!clause))), .chain)
}

#' @rdname group_by-table.express
#' @export
#' @importFrom rlang caller_env
#' @importFrom rlang warn
#'
group_by.data.table <- function(.data, ...) {
    if (cedta(rlang::caller_env())) {
        eb <- EagerExprBuilder$new(.data)
        group_by.ExprBuilder(eb, ...)
    }
    else {
        if (isTRUE(getOption("table.express.warn.cedta", TRUE))) {
            rlang::warn(paste("[table.express] 'group_by' was called from an environment that is *not* 'data.table' aware,",
                              "dispatching to data.frame method."))
        }

        NextMethod("group_by")
    }
}
