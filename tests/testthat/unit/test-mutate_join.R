context("  mutate_join")

test_that("Simple mutating join works.", {
    expected <- paypal[website, on = list(name, purchase_time = session_start_time)]

    ans <- website %>%
        start_expr %>%
        mutate_join(paypal, name, session_start_time = purchase_time, .SDcols = "payment_id") %>%
        end_expr(.by_ref = FALSE) %>%
        (data.table::setnames)("session_start_time", "purchase_time") %>%
        (data.table::setcolorder)(names(expected))

    expect_identical(ans, expected)

    ans <- website %>%
        start_expr %>%
        mutate_join(paypal, name, session_start_time = purchase_time, .SDcols = ends_with("id")) %>%
        end_expr(.by_ref = FALSE) %>%
        (data.table::setnames)("session_start_time", "purchase_time") %>%
        (data.table::setcolorder)(names(expected))

    expect_identical(ans, expected)
})

# TODO: test join_extras
