context("  Filtering SD plus other operations")

test_that("Filtering SD with tidyselect in a chain with several select clauses throws warning.", {
    expect_warning(regexp = "tidyselect.*chain", {
        ans <- DT %>%
            start_expr %>%
            select(everything()) %>%
            transmute(ans = sqrt(mpg)) %>%
            filter_sd(.COL > 0, .SDcols = contains("m")) %>%
            end_expr
    })
})

test_that("Using chain explicitly leads to appropriate use of tidyselect helpers with filter_sd.", {
    expected <- DT[, mpg:disp][mpg > 0, .(ans = sqrt(mpg))]

    ans <- DT %>%
        start_expr %>%
        select(mpg:disp) %>%
        chain %>%
        transmute(ans = sqrt(mpg)) %>%
        filter_sd(.COL > 0, .SDcols = contains("m")) %>%
        end_expr

    expect_identical(ans, expected)

    expected <- DT[mpg > 0 & am > 0, .(ans = sqrt(mpg))]

    ans <- DT %>%
        start_expr %>%
        select(everything()) %>%
        chain %>%
        transmute(ans = sqrt(mpg)) %>%
        filter_sd(.COL > 0, .SDcols = contains("m")) %>%
        end_expr

    expect_identical(ans, expected)
})
