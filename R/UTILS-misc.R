#' @importFrom rlang is_expression
#' @importFrom rlang is_quosure
#' @importFrom rlang parse_expr
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
        obj
    }
}

#' @importFrom rlang enexprs
#'
parse_dots <- function(.parse = FALSE, ..., .named = FALSE, .ignore_empty = "trailing", .unquote_names = TRUE) {
    lapply(rlang::enexprs(..., .named = .named, .ignore_empty = .ignore_empty, .unquote_names = .unquote_names),
           to_expr,
           .parse = .parse)
}

#' @importFrom rlang expr
#' @importFrom rlang is_missing
#'
reduce_expr <- function(expressions, init, op, ..., .parse = FALSE) {
    # lengths() function was introduced in R 3.2.0
    lengths <- sapply(expressions, length)

    if (identical(lengths, 1L) && rlang::is_missing(expressions[[1L]][[1L]])) {
        init
    }
    else {
        Reduce(x = expressions, init = init, f = function(current, new) {
            if (is.list(new)) {
                new <- lapply(new, to_expr, .parse = .parse)
                rlang::expr((!!op)(!!current, !!!new))
            }
            else {
                new <- to_expr(new, .parse = .parse)
                rlang::expr((!!op)(!!current, !!new))
            }
        })
    }
}

#' @importFrom methods is
#' @importFrom rlang eval_tidy
#'
evaled_is <- function(obj_quo, classes) {
    evaled <- try(rlang::eval_tidy(obj_quo), silent = TRUE)
    if (inherits(evaled, "try-error")) {
        return(FALSE)
    }

    ans <- sapply(classes, function(cl) { methods::is(evaled, cl) })
    any(ans)
}

#' @importFrom rlang abort
#' @importFrom rlang as_label
#' @importFrom rlang eval_tidy
#' @importFrom rlang is_call
#' @importFrom rlang is_logical
#' @importFrom rlang quo
#'
process_sdcols <- function(.data, .sdcols_quo) {
    .sdcols_expr <- to_expr(.sdcols_quo)

    if (is_tidyselect_call(.sdcols_expr) || rlang::is_call(.sdcols_expr, ":")) {
        .data$tidy_select(.sdcols_expr)
    }
    else if (uses_pronouns(.sdcols_expr, ".COL")) {
        # https://github.com/r-lib/covr/issues/377
        .f_ <- function(.COL) {
            ans <- base::eval(.sdcols_expr)
            if (!rlang::is_logical(ans, n = 1L)) {
                rlang::abort(paste0("The evaluation of {",
                                    rlang::as_label(.sdcols_expr),
                                    "} did not result in a single logical."))
            }

            ans
        }

        .data$tidy_select(rlang::quo(as.logical(.DT_[, lapply(.SD, .f_)])))
    }
    else {
        rlang::eval_tidy(.sdcols_quo)
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

#' @importFrom rlang as_label
#' @importFrom rlang is_call
#'
uses_pronouns <- function(ex, pronouns) {
    if (!rlang::is_call(ex)) {
        return(FALSE)
    }

    uses <- FALSE

    for (i in seq_along(ex)) {
        sub_ex <- ex[[i]]

        if (rlang::is_call(sub_ex)) {
            uses <- uses_pronouns(sub_ex, pronouns)
        }
        else if (rlang::as_label(sub_ex) %in% pronouns) {
            uses <- TRUE
        }

        if (uses) break
    }

    uses
}

#' @importFrom rlang as_string
#' @importFrom rlang call_args
#'
select_with_colon <- function(.names, .expr) {
    .args <- rlang::call_args(.expr)

    .ij <- sapply(.args, function(.arg) {
        if (is.numeric(.arg)) {
            .arg
        }
        else {
            which(rlang::as_string(.arg) == .names)[1L]
        }
    })

    .names[.ij[1L] : .ij[2L]]
}

#' @importFrom rlang call_args
#' @importFrom rlang is_call
#' @importFrom rlang new_quosure
#' @importFrom rlang quo_get_env
#' @importFrom rlang quo_get_expr
#'
can_combine_lapply <- function(which_quo, how_quo) {
    simple_num <- evaled_is(which_quo, c("numeric", "character"))
    simple_call <- rlang::is_call(rlang::quo_get_expr(which_quo), c(":", "everything"))

    if (!simple_num && !simple_call) {
        FALSE
    }
    else if (evaled_is(how_quo, "function")) {
        TRUE
    }
    else if (rlang::is_call(rlang::quo_get_expr(how_quo), c(".", "list"))) {
        env <- rlang::quo_get_env(how_quo)
        all(sapply(rlang::call_args(how_quo), function(how_expr) {
            if (rlang::is_call(how_expr)) {
                FALSE
            }
            else {
                one_quo <- rlang::new_quosure(how_expr, env)
                evaled_is(one_quo, "function")
            }
        }))
    }
    else {
        FALSE
    }
}

#' @importFrom rlang as_string
#' @importFrom rlang call_args
#' @importFrom rlang expr
#' @importFrom rlang is_call
#'
standardize_lapplys <- function(.exprs, ..., .parse) {
    .dots <- parse_dots(.parse, ...)

    if (!rlang::is_call(.exprs, c(".", "list"))) {
        .exprs <- list(.exprs)
    }
    else {
        .exprs <- rlang::call_args(.exprs)
        zchars <- !nzchar(names(.exprs))
        names(.exprs)[zchars] <- sapply(.exprs[zchars], rlang::as_string)
    }

    .ans <- lapply(.exprs, function(.expr) {
        rlang::expr(lapply(.SD, !!.expr, !!!.dots))
    })

    rlang::expr(c(!!!.ans))
}

#' @importFrom rlang call2
#' @importFrom rlang call_args
#' @importFrom rlang call_modify
#' @importFrom rlang call_standardise
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang new_quosure
#' @importFrom rlang zap
#'
standardize_calls <- function(.exprs, .env, ..., .parse) {
    .dots <- parse_dots(.parse, ...)

    if (!rlang::is_call(.exprs, c(".", "list"))) {
        .exprs <- list(.exprs)
    }
    else {
        .exprs <- rlang::call_args(.exprs)
    }

    lapply(.exprs, function(.expr) {
        if (evaled_is(rlang::new_quosure(.expr, .env), "function")) {
            .expr <- rlang::call2(.expr, rlang::expr(.COL))
        }

        if (rlang::is_call(.expr)) {
            .expr <- rlang::call_standardise(.expr, .env)
            .expr <- rlang::call_modify(.expr, ... = rlang::zap(), !!!.dots)
        }

        .expr
    })
}
