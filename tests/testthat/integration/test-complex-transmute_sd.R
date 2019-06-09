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

    foo <- function(x) { floor(x) / ceiling(max(x)) }
    ans <- DT %>%
        start_expr %>%
        transmute_sd(foo, .SDcols = ends_with("t")) %>%
        where(vs == 0) %>%
        group_by(gear, carb) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Using multiple .SDcols-expressions with .COL pronoun needs explicit chaining.", {
    expect_error({
        DT %>%
            start_expr %>%
            transmute_sd(as.integer, .SDcols = all(.COL %% 1 == 0)) %>%
            transmute_sd(.COL * 2L, .SDcols = max(.COL) < 4L) %>%
            end_expr
    })

    whole <- names(DT)[sapply(DT, function(x) { all(x %% 1 == 0) })]
    desired <- names(DT)[sapply(DT, function(x) { max(x) < 4 })]
    expected <- DT[, lapply(.SD, as.integer), .SDcols = whole][, lapply(.SD, `*`, 2L), .SDcols = desired]

    ans <- DT %>%
        start_expr %>%
        transmute_sd(as.integer, .SDcols = all(.COL %% 1 == 0)) %>%
        chain %>%
        transmute_sd(.COL * 2L, .SDcols = max(.COL) < 4L) %>%
        end_expr

    expect_identical(ans, expected)
})
