context("  Chains with joins")

test_that("Visits that had a purchase afterwards, with totals per name and n_visits before purchase.", {
    expected <- paypal[website, on = .(name, purchase_time = session_start_time), nomatch = NULL, roll = Inf
                        ][, purchase_time := NULL
                          ][, num_purchases := length(unique(payment_id)), by = "name"
                            ][, num_previous_visits := .N, by = "payment_id"]

    ans <- website %>%
        start_expr %>%
        left_join(paypal, name, session_start_time = purchase_time, nomatch = NULL, roll = Inf) %>%
        chain %>%
        mutate(purchase_time = NULL) %>%
        chain %>%
        group_by(name) %>%
        mutate(num_purchases = length(unique(payment_id))) %>%
        group_by(payment_id) %>%
        mutate(num_previous_visits = .N) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Session immediately before a purchase, if any.", {
    expected <- data.table::copy(paypal)[, session_id := website[paypal,
                                                                 .(session_id),
                                                                 on = .(name, session_start_time = purchase_time),
                                                                 roll = Inf]]
    expected <- website[expected, on = .(name, session_id)]
    expected <- expected[complete.cases(expected)]

    ans <- paypal %>%
        start_expr %>%
        mutate_join(website, name, purchase_time = session_start_time, .SDcols = "session_id", roll = Inf) %>%
        mutate_join(website, session_id, .SDcols = "session_start_time") %>%
        chain(.by_ref = FALSE) %>%
        filter_sd(.SDcols = everything()) %>%
        end_expr

    expect_identical(data.table::setkey(ans, NULL), data.table::setcolorder(expected, names(ans)))
})

test_that("inner_join before or after a filter", {
    ans <- website %>%
        start_expr %>%
        inner_join(paypal, name) %>%
        frame_append(allow = TRUE) %>%
        filter(session_id < 10L, payment_id < 10L) %>%
        chain %>%
        mutate_sd(as.POSIXct(round(.COL, "days")), .SDcols = ends_with("time")) %>%
        end_expr

    ans2 <- website %>%
        start_expr %>%
        filter(session_id < 10L) %>%
        inner_join(paypal, name) %>%
        frame_append(allow = TRUE) %>%
        filter(payment_id < 10L) %>%
        chain %>%
        mutate_sd(as.POSIXct(round(.COL, "days")), .SDcols = ends_with("time")) %>%
        end_expr

    expect_identical(ans, ans2)
})

test_that("anti_join before or after a filter", {
    ans <- website %>%
        start_expr %>%
        anti_join(paypal, name) %>%
        filter(session_id < 10L) %>%
        chain %>%
        mutate_sd(as.POSIXct(round(.COL, "days")), .SDcols = ends_with("time")) %>%
        end_expr

    ans2 <- website %>%
        start_expr %>%
        filter(session_id < 10L) %>%
        anti_join(paypal, name) %>%
        chain %>%
        mutate_sd(as.POSIXct(round(.COL, "days")), .SDcols = ends_with("time")) %>%
        end_expr

    expect_identical(ans, ans2)
})

test_that("right_join before or after a filter", {
    expected <- website[paypal, on = "name", allow = TRUE][session_id < 10L & payment_id < 10L]

    ans <- website %>%
        start_expr %>%
        right_join(paypal, name) %>%
        frame_append(allow = TRUE) %>%
        filter(session_id < 10L, payment_id < 10L) %>%
        end_expr

    expect_identical(ans, expected)

    expected <- website[session_id < 10L][paypal, on = "name", allow = TRUE]

    ans <- website %>%
        start_expr %>%
        filter(session_id < 10L) %>%
        right_join(paypal, name) %>%
        frame_append(allow = TRUE) %>%
        filter(payment_id < 10L) %>%
        end_expr

    expect_identical(ans, expected)
})
