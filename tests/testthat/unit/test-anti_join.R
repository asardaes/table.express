context("  Anti join")

test_that("Anti join works.", {
    expected <- lhs[!rhs, on = "x"]
    ans <- lhs %>% start_expr %>% anti_join(rhs, "x") %>% end_expr
    expect_identical(ans, expected)

    expected <- paypal[!website, on = .(payment_id = session_id)]
    ans <- paypal %>% start_expr %>% anti_join(website, payment_id = session_id) %>% end_expr
    expect_identical(ans, expected)

    lhs <- data.table::setkey(data.table::copy(lhs), x)

    expected <- lhs[!rhs]
    ans <- lhs %>% start_expr %>% anti_join(rhs) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Nesting expressions in anti_join's y works.", {
    expected <- lhs[!lhs[x == "a"], on = "x"]
    ans <- lhs %>% start_expr %>% anti_join(nest_expr(filter(x == "a")), x) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Eager anti_join works.", {
    expected <- lhs[!rhs, on = "x"]
    ans <- lhs %>% anti_join(rhs, "x")
    expect_identical(ans, expected)

    expected <- lhs[!rhs, .(y, v), on = "x"]
    ans <- lhs %>% anti_join(rhs, "x", .expr = TRUE) %>% select(y, v)
    expect_identical(ans, expected)
})
