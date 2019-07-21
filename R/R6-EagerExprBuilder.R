#' Eager frame expression builder
#'
#' Like [ExprBuilder], but eager in some regards. This shouldn't be used directly.
#'
#' @docType class
#' @export
#' @include R6-ExprBuilder.R
#' @importFrom R6 R6Class
#' @importFrom rlang abort
#'
EagerExprBuilder <- R6::R6Class(
    "EagerExprBuilder",
    inherit = ExprBuilder,
    public = list(
        initialize = function(DT) {
            super$initialize(DT)
        },

        chain = function(...) {
            rlang::abort("EagerExprBuilders cannot be chained.")
        },

        chain_if_set = function(...) {
            rlang::abort("EagerExprBuilders cannot be chained.")
        }
    )
)
