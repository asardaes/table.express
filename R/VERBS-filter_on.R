#' Filter with secondary indices
#'
#' Helper to filter specifying the `on` part of the [data.table::data.table] query.
#'
#' @export
#'
filter_on <- function(.data, ...) {
    UseMethod("filter_on")
}

#' @rdname filter_on
#' @export
#' @importFrom rlang expr
#'
#' @template data-arg
#' @param ... Key-value pairs, maybe with empty keys if the `data.table` already has them. See
#'   details.
#' @param which,nomatch,mult See [data.table::data.table].
#' @param .negate Whether to negate the expression and search only for rows that don't contain the
#'   given values.
#' @template chain-arg
#'
#' @details
#'
#' The key-value pairs in '...' are processed as follows:
#'
#' - The names are used as `on` in the `data.table` frame. If any name is empty, `on` is left
#'   missing.
#' - The values are packed in a list and used as `i` in the `data.table` frame.
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     filter_on(cyl = 4, gear = 5)
#'
filter_on.ExprBuilder <- function(.data, ..., which = FALSE, nomatch = getOption("datatable.nomatch"), mult = "all",
                                  .negate = FALSE, .chain = getOption("table.express.chain", TRUE))
{
    key_value <- parse_dots(FALSE, ...)
    keys <- names(key_value)
    values <- unname(key_value)

    clause <- rlang::expr(list(!!!values))
    if (.negate) {
        clause <- rlang::expr(`!`(`!!`(clause)))
    }

    ans <- .data$set_i(clause, .chain)

    if (all(nzchar(keys))) {
        frame_append(ans, on = !!keys, .parse = FALSE)
    }
    if (!missing(which)) {
        frame_append(ans, which = !!which, .parse = FALSE)
    }
    if (!missing(nomatch)) {
        frame_append(ans, nomatch = !!nomatch, .parse = FALSE)
    }
    if (!missing(mult)) {
        frame_append(ans, mult = !!mult, .parse = FALSE)
    }

    ans
}

#' @rdname filter_on
#' @export
#' @importFrom rlang caller_env
#'
#' @template expr-arg
#'
filter_on.data.table <- function(.data, ..., .expr = FALSE) {
    eb <- if (.expr) EagerExprBuilder$new(.data) else ExprBuilder$new(.data)
    lazy_ans <- filter_on.ExprBuilder(eb, ...)

    if (.expr) {
        lazy_ans
    }
    else {
        end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env())
    }
}
