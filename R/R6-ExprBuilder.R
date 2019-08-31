#' Frame expression builder
#'
#' Build an expression that will be used inside a [data.table::data.table-class]'s frame. This
#' shouldn't be used directly.
#'
#' @docType class
#' @export
#' @importFrom data.table copy
#' @importFrom data.table is.data.table
#' @importFrom R6 R6Class
#' @importFrom rlang abort
#' @importFrom rlang as_label
#' @importFrom rlang dots_list
#' @importFrom rlang call_name
#' @importFrom rlang caller_env
#' @importFrom rlang current_env
#' @importFrom rlang env_get_list
#' @importFrom rlang eval_tidy
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang is_syntactic_literal
#' @importFrom rlang list2
#' @importFrom rlang maybe_missing
#' @importFrom rlang new_data_mask
#' @importFrom rlang new_environment
#' @importFrom rlang parse_expr
#' @importFrom rlang quo
#' @importFrom rlang sym
#' @importFrom rlang warn
#' @importFrom tidyselect scoped_vars
#' @importFrom tidyselect vars_select_helpers
#'
#' @field appends Extra expressions that go at the end.
#' @field expr The final expression that can be evaluated with [base::eval()] or
#'   [rlang::eval_bare()].
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`initialize(DT, dt_pronouns = list(), .verbose)`}{Constructor that receives a
#'     [data.table::data.table-class] in `DT`. The `dt_pronouns` parameter is used internally when
#'     chaining for joins.}
#'   \item{`set_j(value, chain_if_needed)`}{Set the `j` clause expression(s), starting a new frame
#'     if the current one already has said expression set.}
#'   \item{`set_i(value, chain_if_needed)`}{Like `set_j` but for the `i` clause.}
#'   \item{`set_by(value, chain_if_needed)`}{Set the by clause expression.}
#'   \item{`chain(type = "frame", dt)`}{By default, start a new expression with the current one as
#'     its parent. If `type = "pronoun"`, `dt` is used to start a new expression that joins the
#'     current one.}
#'   \item{`eval(parent_env, by_ref, ...)`}{Evaluate the final expression with `parent_env` as the
#'     enclosing environment. If `by_ref = FALSE`, [data.table::copy()] is called before. The
#'     ellipsis' contents are assigned to the expression's evaluation environment.}
#'   \item{`tidy_select(select_expr)`}{Evaluate a `tidyselect` call using the currently captured
#'     table.}
#'   \item{`print(...)`}{Prints the built `expr`.}
#' }
#'
ExprBuilder <- R6::R6Class(
    "ExprBuilder",
    public = list(
        initialize = function(DT, dt_pronouns = list(), nested = list(),
                              verbose = getOption("table.express.verbose", FALSE))
        {
            if (data.table::is.data.table(DT)) {
                private$.DT <- DT
            }
            else {
                rlang::abort("Received 'DT' is not a data.table.",
                             "table.express.invalid_argument_class_error",
                             DT = DT)
            }

            private$.dt_pronouns = dt_pronouns
            private$.nested = nested
            private$.verbose = verbose

            invisible()
        },

        set_i = function(value, chain_if_needed) {
            private$.process_clause("i", value, chain_if_needed)
        },

        set_j = function(value, chain_if_needed) {
            private$.process_clause("j", value, chain_if_needed)
        },

        set_by = function(value, chain_if_needed) {
            private$.process_clause("by", value, chain_if_needed)
        },

        chain = function(type = "frame", next_dt, parent_env) {
            type <- match.arg(type, c("frame", "pronoun"))
            switch(
                type,
                frame = {
                    other <- ExprBuilder$new(private$.DT, private$.dt_pronouns, private$.nested, private$.verbose)
                    private$.insert_child(other)
                    if (private$.verbose) { # nocov start
                        cat("Starting new frame.\n")
                    } # nocov end

                    other
                },
                pronoun = {
                    dt <- self$eval(parent_env, TRUE)
                    dt_pronoun <- paste0(".DT_", length(private$.dt_pronouns), "_")
                    next_pronouns <- c(private$.dt_pronouns, rlang::list2(!!dt_pronoun := dt))

                    if (private$.verbose) { # nocov start
                        cat("Starting new expression, nesting previous .DT_ pronoun.\n")
                    } # nocov end

                    eb <- ExprBuilder$new(next_dt, next_pronouns, private$.nested, private$.verbose)
                    eb$set_i(rlang::sym(dt_pronoun), FALSE)
                }
            )
        },

        chain_if_set = function(...) {
            clause_values <- rlang::env_get_list(private, c(...))
            if (any(sapply(clause_values, Negate(is.null)))) {
                self$chain()
            }
            else {
                self
            }
        },

        seek_and_nestroy = function(.exprs) {
            .DT_ <- private$.DT
            .env <- rlang::caller_env(2L)
            .verbose <- private$.verbose

            lapply(.exprs, function(.expr) {
                if (rlang::is_call(.expr) && isTRUE(rlang::call_name(.expr) == "nest_expr")) {
                    .nested_exprs <- rlang::eval_tidy(.expr, env = .env)
                    .functional_chain <- reduce_expr(.nested_exprs, rlang::expr(.DT_), rlang::expr(`%>%`))

                    if (.verbose) { # nocov start
                        cat("Nesting the result of evaluating the following functional chain:\n")
                        print(.functional_chain)
                    } # nocov end

                    .env <- rlang::new_environment(list(.DT_ = .DT_), parent = .env)
                    .ans <- rlang::eval_tidy(.functional_chain, env = .env)

                    .nest_pronoun <- paste0(".NEST_", length(private$.nested), "_")
                    private$.nested <- c(private$.nested, rlang::list2(!!.nest_pronoun := .ans))

                    rlang::sym(.nest_pronoun)
                }
                else {
                    .expr
                }
            })
        },

        eval = function(parent_env, by_ref, ...) {
            .DT_ <- if (by_ref) {
                if (private$.verbose) { # nocov start
                    cat("Using captured data.table for evaluation.\n")
                } # nocov end

                private$.DT
            }
            else {
                if (private$.verbose) { # nocov start
                    cat("Copying data.table before evaluation.\n")
                } # nocov end

                data.table::copy(private$.DT)
            }

            is_chain <- !is.null(private$.parent) | !is.null(private$.child)

            if (private$.selected_eagerly && is_chain && EBCompanion$chain_select_count(self) > 1L) {
                rlang::warn(paste("Current expression used the cpatured data.table eagerly,",
                                  "but has more than one 'j' clause.",
                                  "Consider using 'chain' first."))
            }

            private$.eval(parent_env, .DT_ = .DT_, ...)
        },

        tidy_select = function(select_expr) {
            if (private$.verbose) { # nocov start
                cat("In {", EBCompanion$get_top_call(), "}, using captured data.table eagerly to evaluate:\n", sep = "")
                print(select_expr)
            } # nocov end

            private$.selected_eagerly <- TRUE

            if (rlang::is_call(select_expr, ":")) {
                select_with_colon(names(private$.DT), select_expr)
            }
            else {
                tidyselect::scoped_vars(names(private$.DT))

                .data_mask <- rlang::new_environment(list(.DT_ = private$.DT))
                .data_mask <- rlang::new_data_mask(.data_mask)

                names(private$.DT)[rlang::eval_tidy(select_expr, .data_mask)]
            }
        },

        print = function(...) {
            print(self$expr)
            invisible(self)
        }
    ),
    active = list(
        # value should always be a list of 0 or more expressions
        appends = function(value) {
            if (missing(value)) return(private$.appends)

            private$.appends <- c(private$.appends, value)

            if (private$.verbose) { # nocov start
                cat("Expression after ", EBCompanion$get_top_call(), ":\n", sep = "")
                print(self)
            } # nocov end

            invisible()
        },

        expr = function(.DT_) {
            if (!missing(.DT_)) rlang::abort("The 'expr' field is read-only.")
            private$.compute_expr(rlang::expr(.DT_))
        }
    ),
    private = list(
        .DT = NULL,

        .parent = NULL,
        .child = NULL,

        .i = NULL,
        .j = NULL,
        .by = NULL,
        .appends = NULL,
        .dt_pronouns = NULL,
        .nested = NULL,

        .selected_eagerly = FALSE,
        .verbose = FALSE,

        .compute_expr = function(init) {
            root <- EBCompanion$get_root(self)
            expr_chain <- EBCompanion$get_expr_chain(root)
            reduce_expr(expr_chain, init, rlang::expr(`[`))
        },

        .process_clause = function(name, value, chain_if_needed) {
            private_name <- paste0(".", name)
            prev_clause <- get(private_name, private, inherits = FALSE)

            if (!is.null(prev_clause)) {
                if (chain_if_needed) {
                    other <- self$chain()

                    expr_set_other <- paste0("other$set_", name, "(value, chain_if_needed)")
                    expr_set_other <- rlang::parse_expr(expr_set_other)
                    base::eval(expr_set_other)

                    return(other)
                }

                rlang::warn(paste0("Replacing previous '", name, "' clause:",
                                   "\n\tprev_clause -> ", rlang::as_label(prev_clause),
                                   "\n\tnew_clause -> ", rlang::as_label(value)),
                            "table.express.clause_replacement_warning",
                            prev_clause = prev_clause)
            }

            assign(private_name, value, private)

            if (private$.verbose) { # nocov start
                cat("Expression after ", EBCompanion$get_top_call(-3L), ":\n", sep = "")
                print(self)
            } # nocov end

            self
        },

        .get_all_clauses = function() {
            expressions <- rlang::env_get_list(private, EBCompanion$clause_order, NULL)
            until <- Position(Negate(is.null), expressions, right = TRUE)
            if (is.na(until)) until <- 1L

            expressions <- expressions[1L:until]
            expressions <- lapply(expressions, function(q) {
                if (is.null(q)) q <- rlang::expr()
                rlang::maybe_missing(q)
            })

            if (".by" %in% names(expressions)) {
                .EACHI <- NULL # avoid NOTE
                which_by <- if (isTRUE(attr(expressions$.by, "key_by"))) "keyby" else "by"

                if (identical(expressions$.by, rlang::expr(list(.EACHI)))) {
                    expressions$.by <- rlang::expr(.EACHI)
                }

                names(expressions) <- sub("^.by$", which_by, names(expressions))
            }

            to_unname <- names(expressions) %in% c(".j", ".i")
            if (any(to_unname)) {
                names(expressions)[to_unname] <- ""
            }

            # keep possible NULL in extra arguments
            appends <- lapply(private$.appends, function(app) {
                if (rlang::is_syntactic_literal(app)) app <- rlang::quo(!!app)
                app
            })

            c(expressions, appends)
        },

        .insert_child = function(other) {
            stopifnot(inherits(other, "ExprBuilder"))

            root <- EBCompanion$get_root(other)
            leaf <- EBCompanion$get_leaf(other)

            EBCompanion$set_child(leaf, private$.child)
            EBCompanion$set_parent(private$.child, leaf)

            EBCompanion$set_parent(root, self)
            private$.child <- root

            invisible()
        },

        .eval = function(.parent_env, ...) {
            dots <- rlang::dots_list(
                !!!private$.dt_pronouns,
                !!!private$.nested,
                !!!EBCompanion$helper_functions,
                !!!tidyselect::vars_select_helpers,
                ...,
                .homonyms = "last"
            )

            final_expr <- self$expr

            if (private$.verbose) { # nocov start
                cat("Evaluating:\n")
                print(final_expr)
            } # nocov end

            if (cedta(.parent_env)) {
                .expr_env <- rlang::new_environment(dots, parent = .parent_env)
                rlang::eval_tidy(final_expr, env = .expr_env)
            }
            else {
                .expr_env <- rlang::new_environment(dots, parent = rlang::current_env())
                ans <- try(rlang::eval_tidy(final_expr, env = .expr_env), silent = TRUE)
                if (inherits(ans, "try-error")) {
                    rlang::abort(paste("[table.express] A 'dplyr' verb dispatched from",
                                       "a package that is *not* 'data.table' aware,",
                                       "and the workaround didn't work."),
                                 "table.express.data_table_unaware_error")
                }
                else {
                    ans
                }
            }
        }
    )
)

# ==================================================================================================
# Companion

EBCompanion <- new.env()

EBCompanion$clause_order <- c(
    ".i",
    ".j",
    ".by"
)

# --------------------------------------------------------------------------------------------------
# helper functions for expression evaluation
#
# beware of https://github.com/r-lib/rlang/issues/774
#
#' @importFrom rlang abort
#' @importFrom rlang as_function
#' @importFrom rlang as_label
#' @importFrom rlang as_string
#' @importFrom rlang call_name
#' @importFrom rlang eval_tidy
#' @importFrom rlang enexprs
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang is_formula
#' @importFrom rlang is_logical
#' @importFrom rlang new_data_mask
#' @importFrom rlang new_environment
#' @importFrom rlang quo_get_expr
#' @importFrom stats as.formula
#' @importFrom tidyselect scoped_vars
#'
EBCompanion$helper_functions <- list(
    .select_matching = function(.SD = list(), ..., .negate) {
        if (!is.null(names(.SD))) {
            tidyselect::scoped_vars(names(.SD))
        }

        .clauses <- rlang::enexprs(...)

        if (.negate) {
            .ans <- as.list(.SD)
        }
        else {
            .ans <- list()
        }

        for (.i in seq_along(.clauses)) {
            .clause <- .clauses[[.i]]
            .name <- names(.clauses[.i])
            .empty_name <- if (is.null(.name) || !nzchar(.name)) TRUE else FALSE

            if (is_tidyselect_call(.clause)) {
                if (rlang::call_name(.clause) == "everything") {
                    .clause <- setdiff(base::eval(.clause), which(names(.SD) %in% names(.ans)))

                    if (length(.clause) == 0L) {
                        next
                    }
                }
                else {
                    .clause <- base::eval(.clause)
                }
            }

            if (.empty_name && (is.numeric(.clause) || rlang::is_call(.clause, ":"))) {
                .expr <- rlang::expr(base::evalq(.SD[, !!.clause]))
                .sub_ans <- as.list(base::eval(.expr))
            }
            else {
                .sub_ans <- eval.parent(.clause)

                if (is.list(.sub_ans)) {
                    .sub_ans <- as.list(.sub_ans)
                }
                else {
                    .sub_ans <- list(.sub_ans)
                }

                if (.empty_name) {
                    try(silent = TRUE, {
                        .name <- rlang::as_string(.clause)
                    })
                }

                if (!is.null(.name) && nzchar(.name) && length(.name) == length(.sub_ans)) {
                    names(.sub_ans) <- .name
                }
            }

            if (.negate) {
                .ans <- .ans[setdiff(names(.ans), names(.sub_ans))]
            }
            else {
                .ans <- c(.ans, .sub_ans)
            }
        }

        .ans
    },

    .transmute_matching = function(.SD, .which, .hows) {
        .names <- names(.SD)
        tidyselect::scoped_vars(.names)

        .which_expr <- rlang::quo_get_expr(.which)

        if (is_tidyselect_call(.which_expr)) {
            .matches <- rlang::eval_tidy(.which)
        }
        else if (rlang::is_call(.which_expr, ":")) {
            .matches <- select_with_colon(.names, .which_expr)
        }
        else if (rlang::is_formula(.which_expr)) {
            .which_fun <- rlang::as_function(stats::as.formula(.which_expr))
            .matches <- Map(.which_fun, .SD, .names)
            .matches <- .names[unlist(.matches)]
        }
        else if (uses_pronouns(.which_expr, c(".COL", ".COLNAME"))) {
            .matches <- sapply(.names, function(.COLNAME) {
                .COL <- .SD[[.COLNAME]]

                .data_mask <- rlang::new_environment(list(.COL = .COL, .COLNAME = .COLNAME))
                .data_mask <- rlang::new_data_mask(.data_mask)

                .match <- rlang::eval_tidy(.which, .data_mask)
                if (!rlang::is_logical(.match, n = 1L)) {
                    rlang::abort(paste0("The evaluation of {",
                                        rlang::as_label(.which_expr),
                                        "} did not result in a single logical."))
                }

                .match
            })

            .matches <- .names[.matches]
        }
        else {
            .matches <- rlang::eval_tidy(.which)
        }

        if (is.integer(.matches)) {
            .matches <- .names[.matches]
        }

        .data_masks <- lapply(.matches, function(.match) {
            .COL <- .SD[[.match]]
            .data_mask <- rlang::new_data_mask(rlang::new_environment(list(.COL = .COL)))
        })

        .ans <- unlist(recursive = FALSE, lapply(.hows, function(.how) {
            .how <- unformulate(.how)

            lapply(.data_masks, function(.data_mask) {
                rlang::eval_tidy(.how, .data_mask)
            })
        }))

        if (length(.hows) == 1L) {
            .name <- names(.hows)
            .prefix <- if (nzchar(.name)) paste0(.name, ".") else ""
            names(.ans) <- paste0(.prefix, .matches)
        }
        else {
            .prefix <- Map(.hows, names(.hows), f = function(.how, .name) {
                if (nzchar(.name)) .name else rlang::call_name(.how)
            })

            names(.ans) <- as.character(t(outer(unlist(.prefix), .matches, paste, sep = ".")))
        }

        .ans
    },

    # data.table needs to see .SD in order to expose the variables in parent.frame
    .mutate_matching = function(.SD, .SDcols, .hows) {
        Map(.SDcols, .hows, list(parent.frame()), f = function(.sd_col, .how, .dt_env) {
            .COL <- mget(.sd_col, .dt_env, ifnotfound = list(NULL))
            names(.COL) <- ".COL"
            if (is.null(.COL$.COL)) {
                .COL <- list()
            }

            .how <- unformulate(.how)
            .data_mask <- rlang::new_data_mask(rlang::new_environment(.COL))
            rlang::eval_tidy(.how, .data_mask)
        })
    },

    .validating_summarize = function(...) {
        ans <- list(...)
        if (length(ans) > 0L && any(lengths(ans) > 1L)) {
            stop("All summary values must have length 1, got: [",
                 paste(names(ans), lengths(ans), sep = " of length ", collapse = ", "),
                 "]")
        }
        ans
    }
)

# --------------------------------------------------------------------------------------------------
# get_root
#
EBCompanion$get_root <- function(expr_builder) {
    parent <- EBCompanion$get_parent(expr_builder)
    if (is.null(parent))
        expr_builder
    else
        EBCompanion$get_root(parent)
}

# --------------------------------------------------------------------------------------------------
# get_leaf
#
EBCompanion$get_leaf <- function(expr_builder) {
    child <- EBCompanion$get_child(expr_builder)
    if (is.null(child))
        expr_builder
    else
        EBCompanion$get_leaf(child)
}

# --------------------------------------------------------------------------------------------------
# get_expr_chain
#
EBCompanion$get_expr_chain <- function(expr_builder, acc = list()) {
    acc <- c(acc, list(expr_builder$.__enclos_env__$private$.get_all_clauses()))
    next_builder <- EBCompanion$get_child(expr_builder)

    if (is.null(next_builder))
        acc
    else
        EBCompanion$get_expr_chain(next_builder, acc)
}

# --------------------------------------------------------------------------------------------------
# get_parent
#
EBCompanion$get_parent <- function(expr_builder) {
    expr_builder$.__enclos_env__$private$.parent
}

# --------------------------------------------------------------------------------------------------
# set_parent
#
EBCompanion$set_parent <- function(expr_builder, parent) {
    if (!is.null(expr_builder)) {
        expr_builder$.__enclos_env__$private$.parent <- parent
    }
}

# --------------------------------------------------------------------------------------------------
# get_child
#
EBCompanion$get_child <- function(expr_builder) {
    expr_builder$.__enclos_env__$private$.child
}

# --------------------------------------------------------------------------------------------------
# set_child
#
EBCompanion$set_child <- function(expr_builder, child) {
    if (!is.null(expr_builder)) {
        expr_builder$.__enclos_env__$private$.child <- child
    }
}

# --------------------------------------------------------------------------------------------------
# chain_select_count
#
EBCompanion$chain_select_count <- function(expr_builder) {
    .recursion <- function(node, count) {
        if (!is.null(node$.__enclos_env__$private$.j)) {
            count <- count + 1L
        }

        if (!is.null(EBCompanion$get_child(node))) {
            .recursion(EBCompanion$get_child(node), count)
        }
        else {
            count
        }
    }

    .recursion(EBCompanion$get_root(expr_builder), 0L)
}

# --------------------------------------------------------------------------------------------------
# get_top_call
#
#' @importFrom rlang as_label
#' @importFrom rlang call_name
#' @importFrom rlang trace_back
#'
EBCompanion$get_top_call <- function(n = -2L) {
    ns_funs <- ls(asNamespace("table.express"))
    call_stack <- rlang::trace_back()[[1L]]
    top_call <- Find(call_stack, right = TRUE, nomatch = sys.call(n), f = function(.call) {
        if (is.null(.call)) return(FALSE)

        .name <- rlang::call_name(.call)
        if (is.null(.name)) return(FALSE)

        .name %in% ns_funs
    })

    rlang::as_label(top_call)
}

lockEnvironment(EBCompanion, TRUE)
