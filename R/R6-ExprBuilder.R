#' Frame expression builder
#'
#' Build an expression that will be used inside a [data.table::data.table-class]'s frame.
#'
#' @docType class
#' @export
#' @importFrom data.table is.data.table
#' @importFrom R6 R6Class
#' @importFrom rlang abort
#' @importFrom rlang as_label
#' @importFrom rlang expr
#' @importFrom rlang maybe_missing
#' @importFrom rlang new_environment
#' @importFrom rlang parse_expr
#' @importFrom rlang warn
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
#'   \item{`eval(env)`}{Evaluate the final expression with `env` as the enclosing environment.}
#'   \item{`print(...)`}{Prints the built `expr`.}
#' }
#'
ExprBuilder <- R6Class(
    "ExprBuilder",
    public = list(
        by_ref = TRUE,

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

        eval = function(env) {
            expr_env <- rlang::new_environment(list(.DT_ = private$.DT), parent = env)
            final_expr <- self$expr
            final_expr <- rlang::expr(base::evalq(!!final_expr, !!expr_env))
            base::eval(final_expr)
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
            init <- if (self$by_ref) rlang::expr(.DT_) else rlang::expr(data.table::copy(.DT_))
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
            quosures <- mget(EBCompanion$clause_order, private, ifnotfound = list(NULL))
            until <- Position(Negate(is.null), quosures, right = TRUE)
            if(is.na(until)) until <- 1L

            quosures <- quosures[1L:until]
            quosures <- lapply(quosures, function(q) {
                if (is.null(q)) q <- rlang::expr()
                rlang::maybe_missing(q)
            })

            names(quosures) <- sub("^.by$", "by", names(quosures))

            to_unname <- names(quosures) %in% c(".select", ".where")
            if (any(to_unname)) {
                names(quosures)[to_unname] <- ""
            }

            quosures <- c(quosures, private$.appends)
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

lockEnvironment(EBCompanion, TRUE)
