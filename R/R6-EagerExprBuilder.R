#' Eager frame expression builder
#'
#' Like [ExprBuilder], but eager in some regards. This shouldn't be used directly.
#'
#' @docType class
#' @export
#' @include R6-ExprBuilder.R
#'
EagerExprBuilder <- R6::R6Class(
    "EagerExprBuilder",
    inherit = ExprBuilder,
    public = list(
        initialize = function(DT) {
            super$initialize(DT)
        }
    )
)
