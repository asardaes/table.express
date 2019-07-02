#' Mutate subset of data
#'
#' Like [mutate-table.express] but possibly recycling calls.
#'
#' @export
#' @importFrom rlang enexpr
#' @importFrom rlang enquo
#' @importFrom rlang new_quosure
#' @importFrom rlang quo_get_env
#' @importFrom rlang quos
#'
#' @template data-arg
#' @param .SDcols See [data.table::data.table] and the details here.
#' @template transform-sd-args
#' @param .prefix,.suffix Only relevant when `.how` is a function: add a prefix or suffix to the new
#'   column's name. If neither is missing, `.prefix` has preference.
#' @template parse-arg
#' @template chain-arg
#'
#' @details
#'
#' This function works similar to [transmute_sd()] but keeps all columns and *can* modify by
#' reference, like [mutate-table.express]. It can serve like
#' [`dplyr`'s scoped mutation variants][dplyr::mutate_all()] depending on what's given to `.SDcols`.
#'
#' @template tidyselect-sdcols
#' @template docu-examples
#'
#' @examples
#'
#' data("mtcars")
#'
#' data.table::as.data.table(mtcars) %>%
#'     start_expr %>%
#'     mutate_sd(c("mpg", "cyl"), .COL * 2)
#'
mutate_sd <- function(.data, .SDcols, .how = identity, ..., .prefix, .suffix,
                      .parse = getOption("table.express.parse", FALSE),
                      .chain = getOption("table.express.chain", TRUE))
{
    .SDcols <- rlang::enquo(.SDcols)
    SDcols <- process_sdcols(.data, .SDcols)

    how_exprs <- to_expr(rlang::enexpr(.how), .parse = .parse)
    how_quo <- rlang::new_quosure(how_exprs, rlang::quo_get_env(.SDcols))

    if (evaled_is(how_quo, "function")) {
        new_names <- SDcols

        if (!missing(.prefix)) {
            new_names <- paste0(.prefix, new_names)
        }
        else if (!missing(.suffix)) {
            new_names <- paste0(new_names, .suffix)
        }

        dots <- parse_dots(.parse, ...)

        mutate.ExprBuilder(.data, .parse = FALSE, .unquote_names = FALSE, .chain = .chain,
                           !!new_names := lapply(.SD, !!how_exprs, !!!dots)) %>%
            frame_append(.SDcols = !!SDcols)
    }
    else {
        how <- standardize_calls(how_exprs, rlang::quo_get_env(.SDcols), ..., .parse = .parse)

        if (length(how) != length(SDcols) && length(how) == 1L && !is.null(names(how))) {
            new_names <- paste(names(how), SDcols, sep = ".")
        }
        else {
            new_names <- SDcols
            how_names <- names(how)
            which_named <- nzchar(how_names)
            if (any(which_named)) {
                new_names[which_named] <- how_names[which_named]
            }
        }

        # just to avoid NOTE
        .mutate_matching <- EBCompanion$helper_functions$.mutate_matching

        mutate.ExprBuilder(.data, .parse = FALSE, .unquote_names = FALSE, .chain = .chain,
                           !!new_names := .mutate_matching(.SD, !!SDcols, rlang::quos(!!!how)))
    }
}
