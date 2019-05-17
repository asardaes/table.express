#' Bindings to use dplyr's verbs with data.table
#'
#' A set of light-weight bindings that map [dplyr::dplyr-package] verbs to an expression that can be
#' used within a [data.table::data.table-package]'s frame.
#'
#' Since this package's functionality is based on the \pkg{rlang} package, and it is still evolving,
#' breaking changes may be needed in the future.
#'
#' @author Alexis Sarda-Espinosa
#'
#' @importFrom data.table :=
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom utils globalVariables
#'
"_PACKAGE"

utils::globalVariables(c(".SD", ".COL"))
