#' Eager frame expression builder
#'
#' @description
#' Like [ExprBuilder], but eager in some regards. This shouldn't be used directly.
#'
#' @docType class
#' @export
#' @include R6-ExprBuilder.R
#' @importFrom R6 R6Class
#' @importFrom rlang abort
#'
#' @param ... Ignored.
#'
EagerExprBuilder <- R6::R6Class(
    "EagerExprBuilder",
    inherit = ExprBuilder,
    public = list(
        #' @description
        #' Constructor.
        #' @param DT A [data.table::data.table-class].
        initialize = function(DT, ...) {
            super$initialize(DT, ...)
        },

        #' @description
        #' Override to abort if chaining is attempted.
        #'
        chain = function(...) {
            rlang::abort("EagerExprBuilders cannot be chained.")
        },

        #' @description
        #' Override to abort if chaining is attempted.
        #'
        chain_if_set = function(...) {
            rlang::abort("EagerExprBuilders cannot be chained.")
        }
    )
)
