#' Filter with secondary indices
#'
#' Helper to filter specifying the `on` part of the [data.table::data.table] query.
#'
#' @export
#' @importFrom rlang abort
#' @importFrom rlang expr
#'
#' @template data-arg
#' @param ... Key-value pairs, see details.
#' @param nomatch,mult See [data.table::data.table].
#' @param .negate Whether to negate the expression and search only for rows that don't contain the
#'   given values.
#' @template chain-arg
#'
#' @details
#'
#' The key-value pairs in '...' are processed as follows:
#'
#' - The names are used as `on` in the `data.table` frame.
#' - The values are packed in a list and used as `i` in the `data.table` frame.
#'
#' Thus, all pairs **must** be named.
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     filter_on(cyl = 4, gear = 5)
#'
filter_on <- function(.data, ..., nomatch = getOption("datatable.nomatch"), mult = "all", .negate = FALSE,
                      .chain = getOption("table.express.chain", TRUE))
{
    key_value <- parse_dots(FALSE, ...)
    keys <- names(key_value)
    values <- unname(key_value)

    if (any(is.null(keys)) || any(!nzchar(keys))) {
        rlang::abort("All arguments in '...' must be named.")
    }

    clause <- rlang::expr(list(!!!values))
    if (.negate) {
        clause <- rlang::expr(`!`(`!!`(clause)))
    }

    ans <- .data$set_where(clause, .chain) %>%
        frame_append(on = !!keys, .parse = FALSE)

    if (!missing(nomatch)) {
        frame_append(ans, nomatch = !!nomatch, .parse = FALSE)
    }
    if (!missing(mult)) {
        frame_append(ans, mult = !!mult, .parse = FALSE)
    }

    ans
}
