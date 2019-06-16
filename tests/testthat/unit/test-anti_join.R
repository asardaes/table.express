context("  Anti join")

test_that("Anti join works.", {
    expected <- rhs[!lhs, on = "x"]
    ans <- rhs %>% start_expr %>% anti_join(lhs, "x") %>% end_expr
    expect_identical(ans, expected)

    expected <- paypal[!website, on = .(payment_id = session_id)]
    ans <- paypal %>% start_expr %>% anti_join(website, payment_id = session_id) %>% end_expr
    expect_identical(ans, expected)

    lhs <- data.table::setkey(data.table::copy(lhs), x)

    expected <- lhs[!rhs]
    ans <- lhs %>% start_expr %>% anti_join(rhs) %>% end_expr
    expect_identical(ans, expected)
})
