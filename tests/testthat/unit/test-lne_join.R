context("  Left non-equi join")

test_that("Simple left non-equi join works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time)]

    ans <- paypal %>%
        start_expr %>%
        lne_join(website, name, "purchase_time >= session_start_time") %>%
        end_expr

    expect_identical(ans, expected)

    ans <- paypal %>%
        start_expr %>%
        lne_join(website, "name", purchase_time >= session_start_time) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Left non-equi join with nomatch works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time), nomatch = NULL]

    ans <- paypal %>%
        start_expr %>%
        lne_join(website, name == name, purchase_time >= session_start_time, nomatch = NULL) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Left non-equi join with mult works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time), mult = "first"]

    ans <- paypal %>%
        start_expr %>%
        lne_join(website, name, purchase_time >= session_start_time, mult = "first") %>%
        end_expr

    expect_identical(ans, expected)
})
