context("  left_join")

test_that("Simple left join works.", {
    expected <- paypal[website, on = list(name, purchase_time = session_start_time)]

    ans <- website %>%
        start_expr %>%
        left_join(paypal, name, session_start_time = purchase_time) %>%
        end_expr

    expect_identical(ans, expected)
})

# TODO: test join_extras
