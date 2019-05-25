context("  Transmuting SD plus other operations")

test_that("transmute_sd works with other dplyr verbs as expected.", {
    expected <- DT[vs == 0,
                   lapply(.SD, function(x) { floor(x) / ceiling(max(x)) }),
                   by = .(gear, carb),
                   .SDcols = c("drat", "wt")]

    ans <- DT %>%
        start_expr %>%
        transmute_sd(function(x) { floor(x) / ceiling(max(x)) }, .SDcols = ends_with("t")) %>%
        where(vs == 0) %>%
        group_by(gear, carb) %>%
        end_expr

    expect_identical(ans, expected)
})