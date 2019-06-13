context("  left_join")

test_that("Simple left join works.", {
    expected <- rhs[lhs, on = "x"]

    ans <- lhs %>%
        start_expr %>%
        left_join(rhs, "x") %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Left join with mult works.", {
    expected <- lhs[rhs, on = "x", mult = "first"]

    ans <- rhs %>%
        start_expr %>%
        left_join(lhs, x, mult = "first") %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Rolling left joins work.", {
    expected <- website[paypal, on = .(name, session_start_time = purchase_time), roll = TRUE]

    ans <- paypal %>%
        start_expr %>%
        left_join(website, name, purchase_time = session_start_time, roll = TRUE) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- website[paypal, on = .(name, session_start_time = purchase_time), roll = Inf, rollends = TRUE]

    ans <- paypal %>%
        start_expr %>%
        left_join(website, name, purchase_time = "session_start_time", roll = Inf, rollends = TRUE) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- website[paypal, on = .(name, session_start_time = purchase_time), roll = -Inf, rollends = FALSE]

    ans <- paypal %>%
        start_expr %>%
        left_join(website, name, purchase_time = session_start_time, roll = -Inf, rollends = FALSE) %>%
        end_expr

    expect_identical(ans, expected)
})
