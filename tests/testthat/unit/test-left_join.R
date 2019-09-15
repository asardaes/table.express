context("  Left join")

test_that("Simple left join works.", {
    expected <- rhs[lhs, on = "x"]

    ans <- lhs %>%
        start_expr %>%
        left_join(rhs, "x") %>%
        end_expr

    expect_identical(ans, expected)

    rhs <- data.table::setkey(data.table::copy(rhs), x)

    expected <- rhs[lhs]

    ans <- lhs %>%
        start_expr %>%
        left_join(rhs) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Self left join works.", {
    expected <- lhs[lhs, on = "x", allow = TRUE]

    ans <- lhs %>%
        start_expr %>%
        left_join(, "x") %>%
        frame_append(allow = TRUE) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Left join with mult works.", {
    expected <- lhs[rhs, on = "x", mult = "first"]

    ans <- rhs %>%
        start_expr %>%
        left_join(lhs, x, mult = "first") %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Rolling left joins work.", {
    expected <- website[paypal, on = .(name, session_start_time = purchase_time), roll = TRUE]

    ans <- paypal %>%
        start_expr %>%
        left_join(website, name, purchase_time = session_start_time, roll = TRUE) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- website[paypal, on = .(name, session_start_time = purchase_time), roll = Inf, rollends = TRUE]

    ans <- paypal %>%
        start_expr %>%
        left_join(website, name, purchase_time = "session_start_time", roll = Inf, rollends = TRUE) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- website[paypal, on = .(name, session_start_time = purchase_time), roll = -Inf, rollends = FALSE]

    ans <- paypal %>%
        start_expr %>%
        left_join(website, name, purchase_time = session_start_time, roll = -Inf, rollends = FALSE) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Left join with nomatch works, but expects to be combined when it is NULL.", {
    expected <- rhs[lhs, on = "x", nomatch = NULL]

    expect_warning(
        ans <- lhs %>%
            start_expr %>%
            left_join(rhs, x, nomatch = NULL) %>%
            end_expr
    )

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- rhs[lhs, on = c("x", "v"), nomatch = NULL, roll = TRUE]

    ans <- lhs %>%
        start_expr %>%
        left_join(rhs, x, v, nomatch = NULL, roll = TRUE) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Simple left non-equi join works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time)]

    ans <- paypal %>%
        start_expr %>%
        left_join(website, name, "purchase_time >= session_start_time") %>%
        end_expr

    expect_identical(ans, expected)

    ans <- paypal %>%
        start_expr %>%
        left_join(website, "name", purchase_time >= session_start_time) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Left non-equi join with nomatch works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time), nomatch = NULL]

    ans <- paypal %>%
        start_expr %>%
        left_join(website, name == name, purchase_time >= session_start_time, nomatch = NULL) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Left non-equi join with mult works.", {
    expected <- website[paypal, on = .(name, session_start_time <= purchase_time), mult = "first"]

    ans <- paypal %>%
        start_expr %>%
        left_join(website, name, purchase_time >= session_start_time, mult = "first") %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Eager left_join works.", {
    expected <- rhs[lhs, on = "x"]
    ans <- lhs %>% left_join(rhs, "x")
    expect_identical(ans, expected)

    expected <- rhs[lhs, .(i.v, foo), on = "x"]
    ans <- lhs %>% left_join(rhs, "x", .expr = TRUE) %>% select(i.v, foo)
    expect_identical(ans, expected)

    expected <- lhs[lhs, on = "x", allow = TRUE]
    ans <- lhs %>% left_join(, "x", allow = TRUE)
    expect_identical(ans, expected)
})

test_that("left_join can delegate to data.frame method when necessary.", {
    .expr <- rlang::expr((function() {
        local_lhs <- data.table::as.data.table(!!lhs)
        left_join(local_lhs, data.table::as.data.table(!!rhs), by = "x")
    })())

    expect_warning(ans <- eval(.expr, envir = asNamespace("rex")), "table.express")
    expect_equal(ans, dplyr:::left_join.data.frame(lhs, rhs, "x"))

    .expr <- rlang::expr(left_join(data.table::as.data.table(!!lhs), data.table::as.data.table(!!rhs), x))
    ans_from_workaround <- eval(.expr, envir = asNamespace("rex"))
    expect_equal(ans_from_workaround, left_join(lhs, rhs, x))
})
