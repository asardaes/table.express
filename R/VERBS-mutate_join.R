#' @include VERBS-joins.R
#' @rdname joins
#' @export
#'
mutate_join <- function(x, y, ...) {
    UseMethod("mutate_join")
}

#' @include VERBS-joins.R
#' @rdname joins
#' @export
#' @importFrom rlang as_string
#' @importFrom rlang call_args
#' @importFrom rlang enexpr
#' @importFrom rlang enexprs
#' @importFrom rlang enquo
#' @importFrom rlang eval_tidy
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang maybe_missing
#' @importFrom rlang missing_arg
#' @importFrom rlang new_quosure
#' @importFrom rlang quo_get_env
#' @importFrom rlang sym
#' @importFrom rlang syms
#' @importFrom tidyselect scoped_vars
#'
#' @section Mutating join:
#'
#'   The [ExprBuilder] method for `mutate_join` implements the idiom described in [this
#'   link](https://stackoverflow.com/a/54313203/5793905). The columns specified in `.SDcols` are
#'   those that will be added to `x` from `y`. The specification can be done by:
#'
#'   - Using [tidyselect::select_helpers].
#'   - Passing a character vector. If the character is named, the names are taken as the new column
#'     names for the values added to `x`.
#'   - A list, using [base::list()] or `.()`, containing:
#'     - Column names, either as characters or symbols.
#'     - Named calls expressing how the column should be summarized/modified before adding it to
#'       `x`.
#'
#'   The last case mentioned above is useful when the join returns many rows from `y` for each row
#'   in `x`, so they can be summarized while joining. The value of `by` in the join depends on what
#'   is passed to `.by_each`:
#'
#'   - If `NULL` (the default), `by` is set to `.EACHI` if a call is detected in any of the
#'     expressions from the list in `.SDcols`
#'   - If `TRUE`, `by` is always set to `.EACHI`
#'   - If `FALSE`, `by` is never set to `.EACHI`
#'
#' @examples
#'
#' # would modify lhs by reference
#' lhs %>%
#'     start_expr %>%
#'     mutate_join(rhs, x, .SDcols = c("foo", rhs.v = "v"))
#'
#' # would modify rhs by reference, summarizing 'y' before adding it.
#' rhs %>%
#'     start_expr %>%
#'     mutate_join(lhs, x, .SDcols = .(y = mean(y)))
#'
mutate_join.ExprBuilder <- function(x, y, ..., .SDcols, mult, roll, rollends,
                                    allow = FALSE, .by_each = NULL, .parent_env)
{
    y_missing <- missing(y)
    if (y_missing) {
        x <- chain.ExprBuilder(x, .parent_env = rlang::maybe_missing(.parent_env))
        dt <- rlang::sym(".DT_")
    }
    else {
        dt <- rlang::enexpr(y)
    }

    on <- lapply(rlang::enexprs(...), to_expr, .parse = TRUE)
    on <- name_comp_switcheroo(on)

    sd_expr <- rlang::enexpr(.SDcols)
    sd_quo <- rlang::enquo(.SDcols)
    sd_env <- rlang::quo_get_env(sd_quo)

    .EACHI <- FALSE

    if (is_tidyselect_call(sd_expr)) {
        tidyselect::scoped_vars(colnames(y))
        new_names <- colnames(y)[.SDcols]
        dt_cols <- rlang::syms(paste("x", new_names, sep = "."))
    }
    else {
        if (evaled_is(sd_quo, "character")) {
            sd_cols <- rlang::eval_tidy(sd_quo)
        }
        else if (!rlang::is_call(sd_expr, c("c", "list", "."))) {
            sd_cols <- list(sd_expr)
        }
        else {
            sd_cols <- rlang::call_args(sd_expr)
        }

        new_names <- sd_cols_names(sd_cols)
        dt_cols <- lapply(sd_cols, function(sd_col) {
            sd_quo <- rlang::new_quosure(sd_col, env = sd_env)

            if (evaled_is(sd_quo, "character")) {
                ans <- rlang::eval_tidy(sd_quo)
                if (!grepl("^\\.", ans)) {
                    ans <- paste("x",ans, sep = ".")
                }
                rlang::sym(ans)
            }
            else if (!rlang::is_call(sd_col)) {
                ans <- rlang::as_string(sd_col)
                if (!grepl("^\\.", ans)) {
                    ans <- paste("x",ans, sep = ".")
                }
                rlang::sym(ans)
            }
            else {
                .EACHI <<- TRUE
                sd_col
            }
        })

        names(dt_cols) <- new_names
    }

    join_extras <- list()
    if (!missing(mult))     join_extras$mult <- mult
    if (!missing(roll))     join_extras$roll <- roll
    if (!missing(rollends)) join_extras$rollends <- rollends
    if (!missing(allow))    join_extras$allow.cartesian <- allow

    on_expr <- rlang::missing_arg()
    if (length(on) > 0L) {
        on_expr <- rlang::expr(list(!!!on))
    }

    if (isTRUE(.by_each) || (is.null(.by_each) && .EACHI)) {
        rhs_expr <- rlang::expr(`[`(!!dt,
                                    .SD,
                                    list(!!!dt_cols),
                                    by = .EACHI,
                                    on = !!rlang::maybe_missing(on_expr),
                                    !!!join_extras))

        dt_cols <- rlang::syms(names(dt_cols))
        rhs_expr <- rlang::expr(`[`(!!rhs_expr,
                                    !!rlang::missing_arg(),
                                    list(!!!dt_cols)))
    }
    else {
        rhs_expr <- rlang::expr(`[`(!!dt,
                                    .SD,
                                    list(!!!dt_cols),
                                    on = !!rlang::maybe_missing(on_expr),
                                    !!!join_extras))
    }

    ans <- mutate.ExprBuilder(x, !!new_names := !!rhs_expr, .unquote_names = FALSE, .parse = FALSE)
    if (y_missing) {
        ans <- chain.ExprBuilder(ans, .parent_env = rlang::maybe_missing(.parent_env))
    }

    ans
}

#' @importFrom rlang as_string
#' @importFrom rlang is_call
#'
sd_cols_names <- function(sd_cols) {
    counter <- 1L
    possible_defaults <- unname(sapply(sd_cols, function(sd_col) {
        if (rlang::is_call(sd_col)) {
            counter <- counter
            counter <<- counter + 1L
            ans <- paste0("V", counter)
        }
        else {
            ans <- rlang::as_string(sd_col)
        }

        if (ans %in% c(".N", ".I", ".GRP")) {
            ans <- sub(".", "", ans)
        }

        ans
    }))

    new_names <- names(sd_cols)

    if (is.null(new_names)) {
        new_names <- possible_defaults
    }

    zchars <- !nzchar(new_names)
    if (any(zchars)) {
        new_names[zchars] <- possible_defaults[zchars]
    }

    new_names
}
