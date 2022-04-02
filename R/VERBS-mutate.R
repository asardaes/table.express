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
#' @include pkg.R
#' @importFrom rlang call_args
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang quo_squash
#' @importFrom rlang warn
#'
#' @template data-arg
#' @param ... Mutation clauses.
#' @eval sequential_arg_doc()
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
mutate.ExprBuilder <- function(.data, ..., .sequential = FALSE, .unquote_names = TRUE,
                               .parse = getOption("table.express.parse", FALSE),
                               .chain = getOption("table.express.chain", TRUE))
{
    clauses <- parse_dots(.parse, ..., .named = !.sequential, .unquote_names = .unquote_names)

    if (length(clauses) == 0L) {
        return(.data)
    }
    else if (.sequential) {
        if (!.unquote_names && length(clauses) > 1L) {
            rlang::warn(paste("Only one expression can be provided in '...' for .unquote_names = FALSE,",
                              "ignoring all but first."))

            clauses <- clauses[1L]
        }
        if (rlang::is_call(clauses[[1L]], ":=")) {
            clauses <- rlang::call_args(clauses[[1L]])
        }
        if (rlang::is_call(clauses[[1L]], "c") && length(clauses) == 2L) {
            # c(...) = list(...) form
            clause_names <- unlist(rlang::call_args(clauses[[1L]]))
            clauses <- rlang::call_args(clauses[[2L]])
            names(clauses) <- clause_names
        }

        named_clauses <- get_named_clauses(clauses)
        clause_rhs <- body_from_clauses(named_clauses$clauses, FALSE)
        clause <- rlang::expr(`:=`(!!named_clauses$clause_names, !!clause_rhs))
    }
    else if (.unquote_names) {
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
    }

    .data$set_j(clause, .chain)
}

#' @rdname mutate-table.express
#' @export
#' @importFrom rlang caller_env
#'
#' @param .parent_env See [end_expr()]
#'
mutate.EagerExprBuilder <- function(.data, ..., .parent_env = rlang::caller_env()) {
    end_expr.ExprBuilder(mutate.ExprBuilder(.data, ...), .parent_env = .parent_env)
}

#' @rdname mutate-table.express
#' @export
#' @importFrom rlang caller_env
#'
mutate.data.table <- function(.data, ...) {
    eb <- ExprBuilder$new(.data)
    lazy_ans <- mutate.ExprBuilder(eb, ...)
    try_delegate("mutate", end_expr.ExprBuilder(lazy_ans, .parent_env = rlang::caller_env()))
}
