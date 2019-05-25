#' Frame expression builder
#'
#' Build an expression that will be used inside a [data.table::data.table-class]'s frame.
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
#' @importFrom rlang is_syntactic_literal
#' @importFrom rlang list2
#' @importFrom rlang maybe_missing
#' @importFrom rlang new_environment
#' @importFrom rlang parse_expr
#' @importFrom rlang quo
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
#'   \item{`initialize(DT)`}{Constructor that receives a [data.table::data.table-class] in `DT`.}
#'   \item{`set_select(value, chain_if_needed)`}{Set the select clause expression(s), starting a new
#'     frame if the current one already has said expression set.}
#'   \item{`set_where(value, chain_if_needed)`}{Like `set_select` but for the where clause.}
#'   \item{`set_by(value)`}{Set the by clause expression.}
#'   \item{`chain()`}{Start a new expression with the current one as its parent.}
#'   \item{`eval(parent_env, by_ref, ...)`}{Evaluate the final expression with `parent_env` as the
#'     enclosing environment. If `by_ref = FALSE`, [data.table::copy()] is called before. The
#'     ellipsis' contents are assigned to the expression's evaluation environment.}
#'   \item{`print(...)`}{Prints the built `expr`.}
#' }
#'
ExprBuilder <- R6::R6Class(
    "ExprBuilder",
    public = list(
        initialize = function(DT) {
            if (data.table::is.data.table(DT))
                private$.DT <- DT
            else
                rlang::abort("Received 'DT' is not a data.table.",
                             "table.express.invalid_argument_class_error",
                             DT = DT)

            invisible()
        },

        set_select = function(value, chain_if_needed) {
            private$.process_clause("select", value, chain_if_needed)
        },

        set_where = function(value, chain_if_needed) {
            private$.process_clause("where", value, chain_if_needed)
        },

        set_by = function(value) {
            private$.process_clause("by", value, FALSE)
        },

        chain = function() {
            other <- ExprBuilder$new(private$.DT)
            private$.insert_child(other)
            other
        },

        eval = function(parent_env, by_ref, ...) {
            .DT_ <- if (by_ref) private$.DT else data.table::copy(private$.DT)

            is_chain <- !is.null(private$.parent) | !is.null(private$.child)

            if (private$.selected_eagerly && is_chain && EBCompanion$chain_select_count(self) > 1L) {
                rlang::warn(paste("Current expression chain used 'tidyselect' helpers eagerly,",
                                  "but has more than one 'j' clause.",
                                  "Consider using 'chain' first."))
            }

            dots <- rlang::dots_list(
                .DT_ = .DT_,
                !!!EBCompanion$helper_functions,
                !!!tidyselect::vars_select_helpers,
                !!!rlang::list2(...),
                .homonyms = "last"
            )

            .expr_env <- rlang::new_environment(dots, parent = parent_env)

            final_expr <- self$expr
            final_expr <- rlang::expr(base::evalq(!!final_expr, .expr_env))

            base::eval(final_expr)
        },

        tidy_select = function(select_expr) {
            private$.selected_eagerly <- TRUE
            tidyselect::scoped_vars(names(private$.DT))
            names(private$.DT)[rlang::eval_tidy(select_expr)]
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
        },

        expr = function(.DT_) {
            if (!missing(.DT_)) rlang::abort("The 'expr' field is read-only.")

            root <- EBCompanion$get_root(self)
            quo_chain <- EBCompanion$get_quo_chain(root)
            init <- rlang::expr(.DT_)
            reduce_expr(quo_chain, init, rlang::expr(`[`))
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

        .selected_eagerly = FALSE,

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

        .unlist_quosures = function() {
            quosures <- rlang::env_get_list(private, EBCompanion$clause_order, NULL)
            until <- Position(Negate(is.null), quosures, right = TRUE)
            if(is.na(until)) until <- 1L

            quosures <- quosures[1L:until]
            quosures <- lapply(quosures, function(q) {
                if (is.null(q)) q <- rlang::expr()
                rlang::maybe_missing(q)
            })

            if (".by" %in% names(quosures)) {
                which_by <- if (isTRUE(attr(quosures$.by, "key_by"))) "keyby" else "by"
                names(quosures) <- sub("^.by$", which_by, names(quosures))
            }

            to_unname <- names(quosures) %in% c(".select", ".where")
            if (any(to_unname)) {
                names(quosures)[to_unname] <- ""
            }

            # keep possible NULL in extra arguments
            appends <- lapply(private$.appends, function(app) {
                if (rlang::is_syntactic_literal(app)) app <- rlang::quo(!!app)
                app
            })

            quosures <- c(quosures, appends)
            unlist(quosures)
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
#' @importFrom rlang eval_tidy
#' @importFrom rlang new_data_mask
#' @importFrom rlang new_environment
#' @importFrom tidyselect scoped_vars
#'
EBCompanion$helper_functions <- list(
    .transmute_matching = function(.COL, .COLNAME, .COLNAMES, .which, .how) {
        tidyselect::scoped_vars(.COLNAMES)

        data_mask <- rlang::new_environment(list(.COL = .COL, .COLNAME = .COLNAME))
        data_mask <- rlang::new_data_mask(data_mask)

        condition <- rlang::eval_tidy(.which, data_mask)

        if (is.integer(condition)) {
            condition <- .COLNAMES[condition]
        }

        if (is.character(condition)) {
            condition <- .COLNAME %in% condition
        }

        if (condition) {
            rlang::eval_tidy(.how, data_mask)
        }
        else {
            NULL
        }
    },

    .mutate_matching = function(.COL, .how) {
        data_mask <- rlang::new_environment(list(.COL = .COL))
        data_mask <- rlang::new_data_mask(data_mask)
        rlang::eval_tidy(.how, data_mask)
    },

    .non_null = function(col_list) {
        col_list[!sapply(col_list, is.null)]
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
# get_quo_chain
#
EBCompanion$get_quo_chain <- function(expr_builder, acc = list()) {
    acc <- c(acc, list(expr_builder$.__enclos_env__$private$.unlist_quosures()))
    next_builder <- EBCompanion$get_child(expr_builder)

    if (is.null(next_builder))
        acc
    else
        EBCompanion$get_quo_chain(next_builder, acc)
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
# chain_has_select
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
