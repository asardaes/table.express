context("  Inner join")

test_that("Inner join works.", {
    expected <- lhs[rhs, on = "x", nomatch = NULL]
    ans <- lhs %>% start_expr %>% inner_join(rhs, "x") %>% end_expr
    expect_identical(ans, expected)

    lhs <- data.table::setkey(data.table::copy(lhs), x)

    expected <- lhs[rhs, nomatch = NULL]
    ans <- lhs %>% start_expr %>% inner_join(rhs) %>% end_expr
    expect_identical(ans, expected)

    expected <- paypal[website, on = .(payment_id = session_id), nomatch = NULL]
    ans <- paypal %>% start_expr %>% inner_join(website, payment_id = session_id) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Nesting expressions in inner_join's y works.", {
    expected <- lhs[lhs[, .(v=max(v)), by=x], on=c("x","v"), nomatch=NULL]

    ans <- lhs %>%
        start_expr %>%
        inner_join(nest_expr(.start = FALSE, { .[, .(v = max(v)), by = x] }),
                   x, v) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Eager inner_join works.", {
    expected <- paypal[website, on = .(payment_id = session_id), nomatch = NULL]
    ans <- paypal %>% inner_join(website, payment_id = session_id)
    expect_identical(ans, expected)

    expected <- paypal[website, .(name, payment_id), on = .(payment_id = session_id), nomatch = NULL]
    ans <- paypal %>% inner_join(website, payment_id = session_id, .expr = TRUE) %>% select(name, payment_id)
    expect_identical(ans, expected)
})

test_that("inner_join can delegate to data.frame method when necessary.", {
    .enclos <- rlang::env(asNamespace("rex"),
                          lhs = data.table::copy(lhs),
                          rhs = data.table::copy(rhs))

    .fn <- rlang::set_env(new_env = .enclos, function() {
        inner_join(lhs, rhs, by = "x")
    })

    expect_warning(ans <- .fn(), "table.express")
    expect_equal(ans, dplyr:::inner_join.data.frame(lhs, rhs, "x"))

    .expr <- substitute(inner_join(data.table::as.data.table(lhs), data.table::as.data.table(rhs), x), .enclos)
    ans_from_workaround <- eval(.expr, envir = asNamespace("rex"))
    expect_equal(ans_from_workaround, inner_join(lhs, rhs, x))
})
