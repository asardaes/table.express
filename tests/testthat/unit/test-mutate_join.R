context("  Mutate join")

test_that("Simple mutating join works.", {
    expected <- rhs[lhs, .(x = i.x, y = i.y, v = i.v, foo = x.foo), on = "x"]

    ans <- lhs %>%
        start_expr %>%
        mutate_join(rhs, "x", .SDcols = "foo") %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    ans <- lhs %>%
        start_expr %>%
        mutate_join(rhs, x, .SDcols = last_col()) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    rhs <- data.table::setkey(data.table::copy(rhs), x)

    ans <- lhs %>%
        start_expr %>%
        mutate_join(rhs, .SDcols = last_col()) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)
})

test_that("Mutating join with mult works.", {
    expected <- lhs[rhs, .(x = i.x,v = i.v, foo = i.foo, y = x.y), on = "x", mult = "first"]

    ans <- rhs %>%
        start_expr %>%
        mutate_join(lhs, x, .SDcols = y, mult = "first") %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)
})

test_that("Rolling, mutating joins work.", {
    expected <- website[paypal, on = .(name, session_start_time = purchase_time), roll = TRUE]

    ans <- paypal %>%
        start_expr %>%
        mutate_join(website, name, purchase_time = session_start_time, .SDcols = c("session_id"), roll = TRUE) %>%
        end_expr(.by_ref = FALSE) %>%
        (data.table::setnames)("purchase_time", "session_start_time") %>%
        (data.table::setcolorder)(names(expected))


    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- website[paypal, on = .(name, session_start_time = purchase_time), roll = Inf, rollends = TRUE]

    ans <- paypal %>%
        start_expr %>%
        mutate_join(website, name, purchase_time = "session_start_time", .SDcols = list("session_id"), roll = Inf, rollends = TRUE) %>%
        end_expr(.by_ref = FALSE) %>%
        (data.table::setnames)("purchase_time", "session_start_time") %>%
        (data.table::setcolorder)(names(expected))

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- website[paypal, on = .(name, session_start_time = purchase_time), roll = -Inf, rollends = FALSE]

    ans <- paypal %>%
        start_expr %>%
        mutate_join(website, name, purchase_time = session_start_time, .SDcols = .("session_id"), roll = -Inf, rollends = FALSE) %>%
        end_expr(.by_ref = FALSE) %>%
        (data.table::setnames)("purchase_time", "session_start_time") %>%
        (data.table::setcolorder)(names(expected))

    expect_identical(ans, expected)
})

test_that("Summarizing with mutating join works.", {
    expected <- website[paypal,
                        .(purchase_time = i.purchase_time,
                          payment_id = i.payment_id,
                          session_start_time = min(x.session_start_time),
                          session_id = sum(x.session_id)),
                        on = "name",
                        by = .EACHI]

    data.table::setkey(expected, NULL)

    ans <- paypal %>%
        start_expr %>%
        mutate_join(website, name, .SDcols = .(session_start_time = min(session_start_time), session_id = sum(session_id))) %>%
        end_expr(.by_ref = FALSE) %>%
        (data.table::setkey)(NULL)

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- website[paypal,
                        .(purchase_time = i.purchase_time,
                          payment_id = i.payment_id,
                          V1 = min(x.session_start_time),
                          V2 = sum(x.session_id)),
                        on = "name",
                        by = .EACHI]

    data.table::setkey(expected, NULL)

    ans <- paypal %>%
        start_expr %>%
        mutate_join(website, name, .SDcols = .(min(session_start_time), sum(session_id))) %>%
        end_expr(.by_ref = FALSE) %>%
        (data.table::setkey)(NULL)

    expect_identical(ans, expected)
})
