#' Frame expression builder
#'
#' Build an expression that will be used inside a [data.table::data.table-class]'s frame.
#'
#' @docType class
#' @export
#' @importFrom R6 R6Class
#' @importFrom rlang abort
#' @importFrom rlang expr
#' @importFrom rlang maybe_missing
#' @importFrom rlang new_environment
#'
#' @field where Clause for subsetting. The `i` inside the `data.table`'s frame.
#' @field select Clause for selectin/computing on columns. The `j` inside the `data.table`'s frame.
#' @field expr The final expression that can be evaluated with [base::eval()] or
#'   [rlang::eval_bare()].
#'
#' @section Methods:
#'
#' \describe{
#'   \item{`initialize(DT, caller_env, after)`}{Constructor that receives a
#'     [data.table::data.table-class] in `DT` and the caller's environment in `caller_env`.Chains
#'     can be built by passing an existing `ExprBuilder` instance in `DT`.}
#'   \item{`print(...)`}{Prints the built `expr`.}
#' }
#'
ExprBuilder <- R6Class(
    "ExprBuilder",
    public = list(
        where = NULL,
        select = NULL,

        initialize = function(DT, caller_env, after = TRUE) {
            if (inherits(DT, "ExprBuilder")) {
                private$.target_env <- ExprBuilderCompanion$get_env(DT)
                ExprBuilderCompanion$insert_child(DT, self)
            }
            else if (inherits(DT, "data.table"))
                private$.target_env  <- rlang::new_environment(list(.DT_ = DT), parent = caller_env)
            else
                rlang::abort("Received 'DT' is neither ExprBuilder nor data.table.",
                             "table.express.invalid_argument_class_error",
                             DT = DT)

            invisible()
        },

        print = function(...) {
            print(self$expr)

            invisible(self)
        }
    ),
    active = list(
        expr = function(.DT_) {
            if (!missing(.DT_)) rlang::abort("The 'expr' field is read-only.") # nocov

            root <- ExprBuilderCompanion$get_root(self)
            quo_chain <- ExprBuilderCompanion$get_quo_chain(root)
            quosures <- squash_expr(quo_chain, rlang::expr(.DT_), rlang::expr(`[`))

            rlang::expr(base::evalq(!!quosures, !!private$.target_env))
        }
    ),
    private = list(
        .target_env = NULL,
        .parent = NULL,
        .child = NULL,

        .unlist_quosures = function() {
            quosures <- mget(ExprBuilderCompanion$clause_order, self, ifnotfound = list(NULL))
            until <- Position(Negate(is.null), quosures, right = TRUE)
            if(is.na(until)) until <- 1L

            quosures <- quosures[1L:until]
            quosures <- lapply(quosures, function(q) {
                if (is.null(q)) q <- rlang::expr()
                rlang::maybe_missing(q)
            })

            unlist(unname(quosures), recursive = FALSE)
        },

        .insert_child = function(other) {
            stopifnot(inherits(other, "ExprBuilder"))

            root <- ExprBuilderCompanion$get_root(other)
            leaf <- ExprBuilderCompanion$get_leaf(other)

            ExprBuilderCompanion$set_child(leaf, private$.child)
            ExprBuilderCompanion$set_parent(private$.child, leaf)

            ExprBuilderCompanion$set_parent(root, self)
            private$.child <- root

            invisible()
        }
    )
)

# ==================================================================================================
# Companion

ExprBuilderCompanion <- new.env()

ExprBuilderCompanion$clause_order <- c(
    "where",
    "select"
)

# --------------------------------------------------------------------------------------------------
# insert_child
#
ExprBuilderCompanion$insert_child <- function(target, child) {
    target$.__enclos_env__$private$.insert_child(child)
}

# --------------------------------------------------------------------------------------------------
# get_root
#
ExprBuilderCompanion$get_root <- function(expr_builder) {
    parent <- ExprBuilderCompanion$get_parent(expr_builder)
    if (is.null(parent))
        expr_builder
    else
        ExprBuilderCompanion$get_root(parent)
}

# --------------------------------------------------------------------------------------------------
# get_leaf
#
ExprBuilderCompanion$get_leaf <- function(expr_builder) {
    child <- ExprBuilderCompanion$get_child(expr_builder)
    if (is.null(child))
        expr_builder
    else
        ExprBuilderCompanion$get_leaf(child)
}

# --------------------------------------------------------------------------------------------------
# get_quo_chain
#
ExprBuilderCompanion$get_quo_chain <- function(expr_builder, acc = list()) {
    acc <- c(acc, list(expr_builder$.__enclos_env__$private$.unlist_quosures()))
    next_builder <- ExprBuilderCompanion$get_child(expr_builder)

    if (is.null(next_builder))
        acc
    else
        ExprBuilderCompanion$get_quo_chain(next_builder, acc)
}

# --------------------------------------------------------------------------------------------------
# get_env
#
ExprBuilderCompanion$get_env <- function(expr_builder) {
    expr_builder$.__enclos_env__$private$.target_env
}

# --------------------------------------------------------------------------------------------------
# get_parent
#
ExprBuilderCompanion$get_parent <- function(expr_builder) {
    expr_builder$.__enclos_env__$private$.parent
}

# --------------------------------------------------------------------------------------------------
# set_parent
#
ExprBuilderCompanion$set_parent <- function(expr_builder, parent) {
    if (!is.null(expr_builder)) {
        expr_builder$.__enclos_env__$private$.parent <- parent
    }
}

# --------------------------------------------------------------------------------------------------
# get_child
#
ExprBuilderCompanion$get_child <- function(expr_builder) {
    expr_builder$.__enclos_env__$private$.child
}

# --------------------------------------------------------------------------------------------------
# set_child
#
ExprBuilderCompanion$set_child <- function(expr_builder, child) {
    if (!is.null(expr_builder)) {
        expr_builder$.__enclos_env__$private$.child <- child
    }
}

lockEnvironment(ExprBuilderCompanion, TRUE)
