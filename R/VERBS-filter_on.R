#' Filter with secondary indices
#'
#' Helper to filter specifying the `on` part of the [data.table::data.table] query.
#'
#' @export
#' @importFrom rlang abort
#'
#' @template data-arg
#' @param ... Key-value pairs, see details.
#' @param nomatch See [data.table::data.table].
#' @param mult See [data.table::data.table].
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
filter_on <- function(.data, ..., nomatch = getOption("datatable.nomatch"), mult = "all",
                      .chain = getOption("table.express.chain", TRUE))
{
    key_value <- parse_dots(FALSE, ...)
    keys <- names(key_value)
    values <- unname(key_value)

    if (any(is.null(keys)) || any(!nzchar(keys))) {
        rlang::abort("All arguments in '...' must be named.")
    }

    ans <- where(.data, list(!!!values), .parse = FALSE, .chain = .chain) %>%
        frame_append(on = !!keys, .parse = FALSE)

    if (!missing(nomatch)) {
        frame_append(ans, nomatch = !!nomatch, .parse = FALSE)
    }
    if (!missing(mult)) {
        frame_append(ans, mult = !!mult, .parse = FALSE)
    }

    ans
}
