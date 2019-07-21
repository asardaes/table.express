context("  Semi join")

test_that("Semi join works.", {
    expected <- rhs[lhs, unique(.SD), on="x", nomatch=NULL]

    ans <- rhs %>%
        start_expr %>%
        semi_join(lhs, "x") %>%
        end_expr

    expect_identical(ans, expected)

    ans <- rhs %>%
        start_expr %>%
        semi_join(lhs, "x", .eager = TRUE) %>%
        end_expr

    expect_identical(ans, expected)

    rhs <- data.table::setkey(data.table::copy(rhs), x)

    ans <- rhs %>%
        start_expr %>%
        semi_join(lhs) %>%
        end_expr

    data.table::setkey(ans, NULL)
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- paypal[website, unique(.SD), on = .(payment_id = session_id), nomatch = NULL]

    ans <- paypal %>%
        start_expr %>%
        semi_join(website, payment_id = session_id) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- paypal %>%
        start_expr %>%
        semi_join(website, payment_id = session_id, .eager = TRUE) %>%
        end_expr

    expect_identical(ans, expected)

    expected <- data.table::setDT(dplyr::semi_join(as.data.frame(paypal), website, by = c(payment_id = "session_id")))
    data.table::setkey(expected, name, purchase_time)

    expect_identical(ans, expected)
})

test_that("Eager semi_join works.", {
    expected <- paypal[website, unique(.SD), on = .(payment_id = session_id), nomatch = NULL]

    ans <- paypal %>% semi_join(website, payment_id = session_id)
    expect_identical(ans, expected)

    ans <- paypal %>% semi_join(website, payment_id = session_id, .eager = TRUE)
    expect_identical(ans, expected)
})
