context("  inner_join")

test_that("Inner join works.", {
    expected <- lhs[rhs, on = "x", nomatch = NULL]
    ans <- lhs %>% start_expr %>% inner_join(rhs, "x") %>% end_expr
    expect_identical(ans, expected)

    expected <- paypal[website, on = .(payment_id = session_id), nomatch = NULL]
    ans <- paypal %>% start_expr %>% inner_join(website, payment_id = session_id) %>% end_expr
    expect_identical(ans, expected)
})
