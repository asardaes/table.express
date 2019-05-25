context("  Mutation chain")

test_that("Several mutations in one clause are equivalent to a chain of mutations.", {
    expected <- data.table::copy(DT)
    expected <- expected[, `:=`(foo = mpg - 3, bar = log(disp), baz = vs + am)]

    dt <- data.table::copy(DT)
    ans <- dt %>%
        start_expr %>%
        mutate(foo = mpg - 3, bar = log(disp), baz = vs + am) %>%
        end_expr

    expect_identical(ans, expected)

    dt <- data.table::copy(DT)
    ans <- dt %>%
        start_expr %>%
        mutate(foo = mpg - 3) %>%
        mutate(bar = log(disp)) %>%
        mutate(baz = vs + am) %>%
        end_expr

    expect_identical(ans, expected)

    dt <- data.table::copy(DT)
    ans <- dt %>%
        start_expr %>%
        mutate(foo = mpg - 3) %>%
        chain %>%
        mutate(bar = log(disp)) %>%
        chain %>%
        mutate(baz = vs + am) %>%
        end_expr

    expect_identical(ans, expected)
})
