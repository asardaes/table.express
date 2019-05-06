#' @importFrom magrittr %>%
#' @export
#'
magrittr::`%>%`

#' @importFrom rlang abort
#' @importFrom rlang as_name
#' @importFrom rlang expr
#' @importFrom rlang is_expression
#' @importFrom rlang is_missing
#' @importFrom rlang is_quosure
#' @importFrom rlang parse_expr
#' @importFrom rlang quo_squash
#'
to_expr <- function(obj, .parse = FALSE) {
    if (rlang::is_missing(obj))
        rlang::expr()
    else if (.parse || is.character(obj)) {
        if (!is.character(obj)) obj <- rlang::as_name(obj)
        rlang::parse_expr(obj)
    }
    else if (rlang::is_quosure(obj) || rlang::is_expression(obj))
        rlang::quo_squash(obj)
    else
        rlang::abort("Could not parse received 'obj' to expression.",
                     "table.express.invalid_argument_class_error",
                     obj = obj)
}

#' @importFrom rlang expr
#' @importFrom rlang is_quosure
#' @importFrom rlang quo_squash
#'
squash_expr <- function(quosures, init, op, ..., .parse = FALSE) {
    Reduce(x = quosures, init = init, f = function(current, new) {
        if (is.list(new))
            new <- lapply(new, to_expr, .parse = .parse)
        else if (.parse)
            new <- to_expr(new, .parse = .parse)
        else if (rlang::is_quosure(new))
            new <- rlang::quo_squash(new)

        if (is.list(new))
            rlang::expr((!!op)(!!current, !!!new))
        else
            rlang::expr((!!op)(!!current, !!new))
    })
}
