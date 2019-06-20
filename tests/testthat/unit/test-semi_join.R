context("  Semi join")

test_that("Semi join works like in dplyr.", {
    expected <- data.table::setDT(dplyr::semi_join(rhs, lhs, by = "x"))

    ans <- rhs %>%
        start_expr %>%
        semi_join(lhs, "x") %>%
        end_expr

    expect_identical(ans, expected)

    lhs <- data.table::setkey(data.table::copy(lhs), x)

    ans <- rhs %>%
        start_expr %>%
        semi_join(lhs) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- data.table::setDT(dplyr::semi_join(paypal, website, by = c(payment_id = "session_id")))
    data.table::setkey(expected, NULL)

    ans <- paypal %>%
        start_expr %>%
        semi_join(website, payment_id = session_id) %>%
        end_expr

    expect_identical(ans, expected)
})
