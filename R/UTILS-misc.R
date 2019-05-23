#' @importFrom magrittr %>%
#' @export
#'
magrittr::`%>%`

#' @importFrom magrittr %T>%
#' @export
#'
magrittr::`%T>%`

#' @importFrom rlang is_expression
#' @importFrom rlang is_quosure
#' @importFrom rlang parse_expr
#' @importFrom rlang quo
#' @importFrom rlang quo_squash
#'
to_expr <- function(obj, .parse = FALSE) {
    if (.parse && is.character(obj)) {
        rlang::parse_expr(obj)
    }
    else if (rlang::is_quosure(obj) || rlang::is_expression(obj)) {
        rlang::quo_squash(obj)
    }
    else {
        rlang::quo(!!obj)
    }
}

#' @importFrom rlang enexprs
#'
parse_dots <- function(.parse = FALSE, ..., .named = FALSE, .unquote_names = TRUE) {
    lapply(rlang::enexprs(..., .named = .named, .unquote_names = .unquote_names), to_expr, .parse = .parse)
}

#' @importFrom rlang expr
#' @importFrom rlang quo_squash
#'
reduce_expr <- function(quosures, init, op, ..., .parse = FALSE) {
    Reduce(x = quosures, init = init, f = function(current, new) {
        if (is.list(new))
            new <- lapply(new, to_expr, .parse = .parse)
        else
            new <- to_expr(new, .parse = .parse)

        if (is.list(new))
            rlang::quo_squash(rlang::expr((!!op)(!!current, !!!new)))
        else
            rlang::quo_squash(rlang::expr((!!op)(!!current, !!new)))
    })
}

#' is_fun
#'
#' Like [rlang::is_function()] but doesn't throw if the input is maybe something to be quoted.
#'
#' @keywords internal
#' @importFrom rlang is_function
#'
#' @param obj Anything really.
#'
is_fun <- function(obj) {
    isTRUE(try(rlang::is_function(obj), silent = TRUE))
}

#' @importFrom rlang eval_tidy
#'
process_sdcols <- function(.data, sdcols_quo) {
    e <- to_expr(sdcols_quo)
    if (is_tidyselect_call(e)) {
        .data$tidy_select(e)
    }
    else {
        rlang::eval_tidy(sdcols_quo)
    }
}

# Must be expresssion!
#
#' @importFrom rlang is_call
#' @importFrom tidyselect vars_select_helpers
#'
is_tidyselect_call <- function(expression) {
    rlang::is_call(expression, names(tidyselect::vars_select_helpers))
}
