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

    ans <- DT %>%
        group_by(gear, carb) %>%
        where(vs == 0) %>%
        transmute_sd(function(x) { floor(x) / ceiling(max(x)) }, .SDcols = ends_with("t"))

    expect_identical(ans, expected)

    foo <- function(x) { floor(x) / ceiling(max(x)) }

    ans <- DT %>%
        start_expr %>%
        transmute_sd(foo, .SDcols = ends_with("t")) %>%
        where(vs == 0) %>%
        group_by(gear, carb) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- DT %>%
        group_by(gear, carb) %>%
        where(vs == 0) %>%
        transmute_sd(foo, .SDcols = ends_with("t"))

    expect_identical(ans, expected)
})
