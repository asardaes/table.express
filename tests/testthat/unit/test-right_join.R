context("  Right join")

test_that("Simple right join works.", {
    expected <- lhs[rhs, on = "x"]

    ans <- lhs %>%
        start_expr %>%
        right_join(rhs, "x") %>%
        end_expr

    expect_identical(ans, expected)

    lhs <- data.table::setkey(data.table::copy(lhs), x)

    ans <- lhs %>%
        start_expr %>%
        right_join(rhs) %>%
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

test_that("Simple right non-equi join works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time)]

    ans <- website %>%
        start_expr %>%
        right_join(paypal, name, "session_start_time <= purchase_time") %>%
        end_expr

    expect_identical(ans, expected)

    ans <- website %>%
        start_expr %>%
        right_join(paypal, "name", session_start_time <= purchase_time) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Right non-equi join with nomatch works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time), nomatch = NULL]

    ans <- website %>%
        start_expr %>%
        right_join(paypal, name == name, session_start_time <= purchase_time, nomatch = NULL) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Right non-equi join with mult works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time), mult = "first"]

    ans <- website %>%
        start_expr %>%
        right_join(paypal, name, session_start_time <= purchase_time, mult = "first") %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Nesting expressions in right_join's y works.", {
    expected <- lhs[lhs[, .(v=max(v)), by=x], on=c("x","v")]

    ans <- lhs %>%
        start_expr %>%
        right_join(nest_expr(.start = FALSE, { .[, .(v = max(v)), by = x] }),
                   x, v) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Eager right_join works.", {
    expected <- rhs[lhs, on = "x"]
    ans <- rhs %>% right_join(lhs, "x")
    expect_identical(ans, expected)

    expected <- rhs[lhs, .(i.v, foo), on = "x"]
    ans <- rhs %>% right_join(lhs, "x", .expr = TRUE) %>% select(i.v, foo)
    expect_identical(ans, expected)

    expected <- lhs[lhs, on = "x", allow = TRUE]
    ans <- lhs %>% right_join(lhs, "x", allow = TRUE)
    expect_identical(ans, expected)
})
