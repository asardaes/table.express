context("  Filtering SD plus other operations")

test_that("Filtering SD with tidyselect in a chain with several select clauses throws warning.", {
    expect_warning(regexp = "eager.*chain", {
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

test_that("filter_sd works with other dplyr verbs as expected.", {
    expected <- data.table::copy(DT)[mpg > 0 & am > 0, ans := mpg + cyl, by = "gear"]

    ans <- DT %>%
        start_expr %>%
        filter_sd(.COL > 0, .SDcols = contains("m")) %>%
        mutate(ans = mpg + cyl) %>%
        group_by(gear) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[drat > 4 | wt > 4, .(ans = 2 * disp - mean(hp)), keyby = "gear"]

    ans <- DT %>%
        start_expr %>%
        filter_sd(.COL > 4, .SDcols = ends_with("t"), .collapse = `|`) %>%
        transmute(ans = 2 * disp - mean(hp)) %>%
        key_by(gear) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- data.table::copy(state)[area > 20000,
                                        `:=`(foo = abs(center_x - center_y),
                                             bar = sd(income) * sd(population)),
                                        by = c("region", "division")]

    ans <- state %>%
        start_expr %>%
        filter_sd(.COL > 20000, .SDcols = last_col()) %>%
        mutate(foo = abs(center_x - center_y), bar = sd(income) * sd(population)) %>%
        group_by(region, division) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)
})
