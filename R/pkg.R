#' Bindings to use dplyr's verbs with data.table
#'
#' A set of light-weight bindings that map [dplyr::dplyr-package] verbs to an expression that can be
#' used within a [data.table::data.table-package]'s frame.
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
