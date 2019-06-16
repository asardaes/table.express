context("  Inner join")

test_that("Inner join works.", {
    expected <- lhs[rhs, on = "x", nomatch = NULL]
    ans <- lhs %>% start_expr %>% inner_join(rhs, "x") %>% end_expr
    expect_identical(ans, expected)

    lhs <- data.table::setkey(data.table::copy(lhs), x)

    expected <- lhs[rhs, nomatch = NULL]
    ans <- lhs %>% start_expr %>% inner_join(rhs) %>% end_expr
    expect_identical(ans, expected)

    expected <- paypal[website, on = .(payment_id = session_id), nomatch = NULL]
    ans <- paypal %>% start_expr %>% inner_join(website, payment_id = session_id) %>% end_expr
    expect_identical(ans, expected)
})
