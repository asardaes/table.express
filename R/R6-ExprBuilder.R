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
#'   \item{`set_select(value, chain_if_needed)`}{Set the select clause expression(s), starting a new
#'     frame if the current one already has said expression set.}
#'   \item{`set_where(value, chain_if_needed)`}{Like `set_select` but for the where clause.}
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
        initialize = function(DT, dt_pronouns = list(), verbose = getOption("table.express.verbose", FALSE)) {
            if (data.table::is.data.table(DT)) {
                private$.DT <- DT
            }
            else {
                rlang::abort("Received 'DT' is not a data.table.",
                             "table.express.invalid_argument_class_error",
                             DT = DT)
            }

            private$.dt_pronouns = dt_pronouns
            private$.verbose = verbose

            invisible()
        },

        set_select = function(value, chain_if_needed) {
            ans <- private$.process_clause("select", value, chain_if_needed)
            if (private$.verbose) { # nocov start
                cat("Expression after ", rlang::as_label(sys.call(-1L)), ":\n", sep = "")
                print(self)
            } # nocov end

            ans
        },

        set_where = function(value, chain_if_needed) {
            ans <- private$.process_clause("where", value, chain_if_needed)
            if (private$.verbose) { # nocov start
                cat("Expression after ", rlang::as_label(sys.call(-1L)), ":\n", sep = "")
                print(self)
            } # nocov end

            ans
        },

        set_by = function(value, chain_if_needed) {
            ans <- private$.process_clause("by", value, chain_if_needed)
            if (private$.verbose) { # nocov start
                cat("Expression after ", rlang::as_label(sys.call(-1L)), ":\n", sep = "")
                print(self)
            } # nocov end

            ans
        },

        chain = function(type = "frame", dt) {
            type <- match.arg(type, c("frame", "pronoun"))
            switch(
                type,
                frame = {
                    other <- ExprBuilder$new(private$.DT)
                    private$.insert_child(other)
                    if (private$.verbose) { # nocov start
                        cat("Starting new frame.\n")
                    } # nocov end

                    other
                },
                pronoun = {
                    dt_pronoun <- paste0(".DT_", length(private$.dt_pronouns), "_")
                    dt_expr <- private$.compute_expr(rlang::sym(dt_pronoun))
                    next_pronouns <- c(private$.dt_pronouns, rlang::list2(!!dt_pronoun := private$.DT))

                    if (private$.verbose) { # nocov start
                        cat("Starting new expression.\n")
                    } # nocov end

                    eb <- ExprBuilder$new(dt, next_pronouns)
                    eb$set_where(dt_expr, FALSE)
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

            dots <- rlang::dots_list(
                .DT_ = .DT_,
                !!!private$.dt_pronouns,
                !!!EBCompanion$helper_functions,
                !!!tidyselect::vars_select_helpers,
                ...,
                .homonyms = "last"
            )

            .expr_env <- rlang::new_environment(dots, parent = parent_env)

            final_expr <- self$expr
            if (private$.verbose) { # nocov start
                cat("Evaluating:\n")
                print(final_expr)
            } # nocov end

            final_expr <- rlang::expr(base::evalq(!!final_expr, .expr_env))

            base::eval(final_expr)
        },

        tidy_select = function(select_expr) {
            if (private$.verbose) { # nocov start
                cat("In ", rlang::as_label(sys.call(-1L)), ", using captured data.table eagerly to evaluate:\n", sep = "")
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

        get_newest_pronoun = function() {
            ans <- names(private$.dt_pronouns)
            ans[length(ans)]
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

        .select = NULL,
        .where = NULL,
        .by = NULL,
        .appends = NULL,
        .dt_pronouns = NULL,

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
                which_by <- if (isTRUE(attr(expressions$.by, "key_by"))) "keyby" else "by"
                names(expressions) <- sub("^.by$", which_by, names(expressions))
            }

            to_unname <- names(expressions) %in% c(".select", ".where")
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
        }
    )
)

# ==================================================================================================
# Companion

EBCompanion <- new.env()

EBCompanion$clause_order <- c(
    ".where",
    ".select",
    ".by"
)

# --------------------------------------------------------------------------------------------------
# helper functions for expression evaluation
#
# beware of https://github.com/r-lib/rlang/issues/774
#
#' @importFrom rlang abort
#' @importFrom rlang as_label
#' @importFrom rlang as_string
#' @importFrom rlang call_name
#' @importFrom rlang eval_tidy
#' @importFrom rlang enexprs
#' @importFrom rlang expr
#' @importFrom rlang is_call
#' @importFrom rlang is_logical
#' @importFrom rlang new_data_mask
#' @importFrom rlang new_environment
#' @importFrom rlang quo_get_expr
#' @importFrom tidyselect scoped_vars
#'
EBCompanion$helper_functions <- list(
    .select_matching = function(.SD, ..., .negate) {
        tidyselect::scoped_vars(names(.SD))
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

            .data_mask <- rlang::new_data_mask(rlang::new_environment(.COL))
            rlang::eval_tidy(.how, .data_mask)
        })
    },

    .semi_joined_names = function(x, y, on) {
        ans <- names(x)

        prepend_i <- ans %in% names(y) & !(ans %in% on)
        if (any(prepend_i)) {
            ans[prepend_i] <- paste("i", ans[prepend_i], sep = ".")
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
        if (!is.null(node$.__enclos_env__$private$.select)) {
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

lockEnvironment(EBCompanion, TRUE)
