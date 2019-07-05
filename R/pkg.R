#' Building 'data.table' expressions with data manipulation verbs
#'
#' A specialization of [`dplyr`][dplyr::dplyr-package] verbs, as well as a set of custom ones, that
#' build expressions that can be used within a [`data.table`][data.table::data.table-package]'s
#' frame.
#'
#' @author Alexis Sarda-Espinosa
#'
#' @importFrom utils globalVariables
#'
#' @note
#'
#' Since this package's functionality is based on the \pkg{rlang} package, and \pkg{rlang} is still
#' evolving, breaking changes may be needed in the future.
#'
#' This software package was developed independently of any organization or institution that is or
#' has been associated with the author.
#'
#' @example man-roxygen/example-all.R
#'
"_PACKAGE"

utils::globalVariables(c(".DT_", ".SD", ".COL"))

#' @importFrom data.table :=
#' @export
#'
data.table::`:=`

#' @importFrom magrittr %>%
#' @export
#'
magrittr::`%>%`

#' @importFrom magrittr %T>%
#' @export
#'
magrittr::`%T>%`

#' @importFrom rlang !!
#' @export
#'
rlang::`!!`

#' @importFrom rlang !!!
#' @export
#'
rlang::`!!!`
