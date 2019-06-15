context("  Right non-equi join")

test_that("Simple right non-equi join works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time)]

    ans <- website %>%
        start_expr %>%
        rne_join(paypal, name, "session_start_time <= purchase_time") %>%
        end_expr

    expect_identical(ans, expected)

    ans <- website %>%
        start_expr %>%
        rne_join(paypal, "name", session_start_time <= purchase_time) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Right non-equi join with nomatch works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time), nomatch = NULL]

    ans <- website %>%
        start_expr %>%
        rne_join(paypal, name == name, session_start_time <= purchase_time, nomatch = NULL) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Right non-equi join with mult works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time), mult = "first"]

    ans <- website %>%
        start_expr %>%
        rne_join(paypal, name, session_start_time <= purchase_time, mult = "first") %>%
        end_expr

    expect_identical(ans, expected)
})
