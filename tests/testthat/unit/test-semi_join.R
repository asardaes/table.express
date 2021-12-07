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

    expected <- data.table::as.data.table(dplyr::semi_join(as.data.frame(paypal), website, by = c(payment_id = "session_id")))
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

test_that("semi_join can delegate to data.frame method when necessary.", {
    .enclos <- rlang::env(asNamespace("rex"),
                          lhs = data.table::copy(lhs),
                          rhs = data.table::copy(rhs))

    .fn <- rlang::set_env(new_env = .enclos, function() {
        semi_join(lhs, rhs, by = "x")
    })

    expect_warning(ans <- .fn(), "table.express")
    expect_equal(ans, dplyr:::semi_join.data.frame(lhs, rhs, "x"))

    .expr <- substitute(semi_join(data.table::as.data.table(lhs), data.table::as.data.table(rhs), x), .enclos)
    ans_from_workaround <- eval(.expr, envir = asNamespace("rex"))
    expect_equal(ans_from_workaround, semi_join(lhs, rhs, x))
})
