context("Namespace test")

test_that("Namespace is well defined.", {
    skip_on_cran()
    skip_if(nzchar(Sys.getenv("R_COVR")), "calculating coverage")

    expect_silent({
        r_files <- list.files("../../R/", full.names = TRUE)
        r_files <- setdiff(r_files, "../../R/pkg.R")

        delimiter_re <- rex::rex(start, not("#"), "::", except_any_of("(", ")", ","), end)

        for (r_file in r_files) {
            code <- scan(r_file, "character", sep = "\n", quiet = TRUE)

            delimiters <- which(grepl("^[})]$", code) | rex::re_matches(code, delimiter_re))
            if (length(delimiters) > 1L) {
                delimiters <- Map(1L:length(delimiters), each = c(delimiters[1L], diff(delimiters)), f = rep)
                delimiters <- unlist(delimiters)

                if (length(delimiters) < length(code)) {
                    delimiters <- c(delimiters, rep(0, length(code) - length(delimiters)))
                }

                blocks <- split(code, unlist(delimiters))
                blocks[["0"]] <- NULL
            }
            else {
                blocks <- list(code)
            }

            for (i in seq_along(blocks)) {
                block <- blocks[[i]]

                imports <- rex::re_matches(block, rex::rex(
                    "@importFrom",
                    any_spaces,
                    capture(any_non_spaces, name = "package"),
                    any_spaces,
                    capture(any_non_spaces, name = "import"),
                    end
                ))

                imports <- filter(imports, complete.cases(imports))

                if (nrow(imports) == 0L) next

                functions <- rex::re_matches(block[!grepl("^#", block)], global = TRUE, pattern = rex::rex(
                    maybe(any_spaces),
                    capture(some_of(alnum, dot) %if_next_is% "::", name = "package"),
                    "::",
                    capture(except_some_of(space, ",", "(", ")"), name = "fun")
                ))

                functions <- dplyr::bind_rows(functions) %>%
                    filter(complete.cases(.))

                if (nrow(functions) == 0L) next

                functions <- functions %>%
                    filter(package != "base") %>%
                    mutate(fun = gsub("`", "", fun))

                used_but_not_imported <- dplyr::anti_join(functions, imports, by=c("package", "fun" = "import"))
                if (nrow(used_but_not_imported) > 0L) {
                    warning("[", r_file, "]<block ", i, "> uses but doesn't import:", immediate. = TRUE)
                    print(used_but_not_imported)
                }

                imported_but_not_used <- dplyr::anti_join(imports, functions, by=c("package", "import" = "fun"))
                if (nrow(imported_but_not_used) > 0L) {
                    warning("[", r_file, "]<block ", i, "> imports but doesn't use:", immediate. = TRUE)
                    print(imported_but_not_used)
                }
            }
        }
    })
})
