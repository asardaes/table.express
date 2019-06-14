context("  right_join")

test_that("Simple right join works.", {
    expected <- lhs[rhs, on = "x"]

    ans <- lhs %>%
        start_expr %>%
        right_join(rhs, "x") %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Right join with mult works.", {
    expected <- rhs[lhs, on = "x", mult = "first"]

    ans <- rhs %>%
        start_expr %>%
        right_join(lhs, x, mult = "first") %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Rolling right joins work.", {
    expected <- paypal[website, on = .(name, purchase_time = session_start_time), roll = TRUE]

    ans <- paypal %>%
        start_expr %>%
        right_join(website, name, purchase_time = session_start_time, roll = TRUE) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- paypal[website, on = .(name, purchase_time = session_start_time), roll = Inf, rollends = TRUE]

    ans <- paypal %>%
        start_expr %>%
        right_join(website, name, purchase_time = "session_start_time", roll = Inf, rollends = TRUE) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- paypal[website, on = .(name, purchase_time = session_start_time), roll = -Inf, rollends = FALSE]

    ans <- paypal %>%
        start_expr %>%
        right_join(website, name, purchase_time = session_start_time, roll = -Inf, rollends = FALSE) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Right join with nomatch works, but expects to be combined when it is NULL.", {
    expected <- lhs[rhs, on = "x", nomatch = NULL]

    expect_warning(
        ans <- lhs %>%
            start_expr %>%
            right_join(rhs, x, nomatch = NULL) %>%
            end_expr
    )

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- lhs[rhs, on = c("x", "v"), nomatch = NULL, roll = TRUE]

    ans <- lhs %>%
        start_expr %>%
        right_join(rhs, x, v, nomatch = NULL, roll = TRUE) %>%
        end_expr

    expect_identical(ans, expected)
})
