#' Find rows with extrema in specific columns
#'
#' Find rows with maxima/minima in given columns.
#'
#' @name extrema_by
#' @rdname extrema_by
#' @export
#'
#' @template data-arg
#' @param .col A character vector indicating the columns that will be searched for extrema.
#' @param ... Optionally, columns to group by, either as characters or symbols.
#' @param .some If `TRUE` the rows where *any* of the columns specified in `.col` have extrema are
#'   returned.
#' @template chain-arg
#'
#' @details
#'
#' These verbs implement the idiom shown [here](https://stackoverflow.com/q/16573995/5793905) by
#' leveraging [nest_expr()]. The whole nested expression is assigned to `i` in the `data.table`'s
#' frame. It is probably a good idea to use this on a frame that has no other frames preceding it
#' in the current expression, given that [nest_expr()] uses the captured `data.table`, so consider
#' using [chain()] when needed.
#'
#' Several columns can be specified in `.col`, and depending on the value of `.some`, the rows with
#' all or some extrema are returned, using `&` or `|` respectively. Depending on your data, using
#' more than one column might not make sense, resulting in an empty `data.table`.
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     max_by("mpg", "vs") %>%
#'     end_expr
#'
max_by <- function(.data, .col, ...) {
    UseMethod("max_by")
}

#' @rdname extrema_by
#' @export
#' @importFrom rlang expr
#' @importFrom rlang syms
#'
max_by.ExprBuilder <- function(.data, .col, ..., .some = FALSE, .chain = getOption("table.express.chain", TRUE)) {
    expressions <- lapply(rlang::syms(unname(.col)), function(col) {
        rlang::expr(!!col == max(!!col))
    })

    nested <- extrema_by(expressions, .some, ...)

    where.ExprBuilder(.data, nest_expr(.start = FALSE, .parse = FALSE, `{`(!!nested)), .parse = FALSE, .chain = .chain)
}
