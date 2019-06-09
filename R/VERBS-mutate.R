#' @importFrom dplyr mutate
#' @export
#'
dplyr::mutate

#' Add or update columns
#'
#' Add or update columns of a [data.table::data.table-class], possibly by reference using
#' [`:=`][data.table::set].
#'
#' @rdname mutate-table.express
#' @name mutate-table.express
#' @export
#' @importFrom rlang call_args
#' @importFrom rlang enquos
#' @importFrom rlang expr
#' @importFrom rlang new_quosure
#' @importFrom rlang quo_get_env
#' @importFrom rlang quo_squash
#' @importFrom rlang warn
#'
#' @template data-arg
#' @param ... Mutation clauses.
#' @param .unquote_names Passed to [rlang::enexprs()]. Set to `FALSE` if you want to pass the single
#'   [`:=`][data.table::set] expression.
#' @template parse-arg
#' @template chain-arg
#'
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     mutate(mpg_squared = mpg ^ 2)
#'
mutate.ExprBuilder <- function(.data, ..., .unquote_names = TRUE,
                               .parse = getOption("table.express.parse", FALSE),
                               .chain = getOption("table.express.chain", TRUE))
{
    clauses <- parse_dots(.parse, ..., .named = TRUE, .unquote_names = .unquote_names)
    if (length(clauses) == 0L) return(.data)

    if (.unquote_names) {
        which <- names(clauses)
        clause <- rlang::quo_squash(rlang::expr(
            `:=`(!!!clauses)
        ))
    }
    else {
        if (length(clauses) > 1L) {
            rlang::warn(paste("Only one expression can be provided in '...' for .unquote_names = FALSE,",
                              "ignoring all but first."))
        }

        clause <- clauses[[1L]]

        # seems ugly
        env <- rlang::quo_get_env(rlang::enquos(..., .unquote_names = FALSE)[[1L]])
        which <- rlang::new_quosure(rlang::call_args(clause)[[1L]], env)
    }

    ans <- .data$set_select(clause, .chain)
    .data$tidy_select(which, "union")
    ans
}
