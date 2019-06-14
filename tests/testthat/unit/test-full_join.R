context("  Full join")

test_that("Full join works.", {
    expected <- merge(lhs, rhs, by = "x", all = TRUE)
    ans <- lhs %>% start_expr %>% full_join(rhs, x) %>% end_expr
    expect_identical(ans, expected)

    expected <- merge(website, paypal, by.x = c("name", "session_id"), by.y = c("name", "payment_id"), all = TRUE)
    ans <- website %>% start_expr %>% full_join(paypal, "name", session_id = payment_id) %>% end_expr
    expect_identical(ans, expected)
})
