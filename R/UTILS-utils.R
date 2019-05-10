#' @importFrom magrittr %>%
#' @export
#'
magrittr::`%>%`

#' @importFrom rlang expr
#' @importFrom rlang is_expression
#' @importFrom rlang is_missing
#' @importFrom rlang is_quosure
#' @importFrom rlang parse_expr
#' @importFrom rlang quo
#' @importFrom rlang quo_squash
#'
to_expr <- function(obj, .parse = FALSE) {
    if (rlang::is_missing(obj)) {
        rlang::expr()
    }
    else if (.parse) {
        rlang::parse_expr(obj)
    }
    else if (rlang::is_quosure(obj) || rlang::is_expression(obj)) {
        rlang::quo_squash(obj)
    }
    else {
        rlang::quo(!!obj)
    }
}

#' @importFrom rlang expr
#' @importFrom rlang quo_squash
#'
squash_expr <- function(quosures, init, op, ..., .parse = FALSE) {
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
