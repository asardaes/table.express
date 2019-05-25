#' Building 'data.table' expressions with data manipulation verbs
#'
#' A specialization of [dplyr::dplyr-package] verbs, as well as a set of custom ones, that build
#' expressions that can be used within a [data.table::data.table-package]'s frame.
#'
#' Since this package's functionality is based on the \pkg{rlang} package, and \pkg{rlang} is still
#' evolving, breaking changes may be needed in the future.
#'
#' @author Alexis Sarda-Espinosa
#'
#' @importFrom data.table :=
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom utils globalVariables
#'
#' @example man-roxygen/example-all.R
#'
"_PACKAGE"

utils::globalVariables(c(".SD", ".COL"))
