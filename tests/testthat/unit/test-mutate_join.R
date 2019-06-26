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
        mutate_join(lhs, x, .SDcols = y, mult = "first", allow = FALSE) %>%
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

test_that("Summarizing with mutating self join works.", {
    expected <- data.table::copy(paypal)[, min_pt := paypal[paypal, on = "name", by = .EACHI,
                                                            list(min_pt = min(purchase_time))
                                                            ][, .(min_pt)]
                                         ]

    ans <- paypal %>%
        (data.table::copy) %>%
        start_expr %>%
        mutate_join(, name, .SDcols = .(min_pt = min(purchase_time))) %>%
        end_expr

    expect_identical(ans, expected)
})

# https://stackoverflow.com/q/56710801/5793905
test_that("Muating non-equi join works.", {
    df <- read.table(header = TRUE, text = '
User  Stamp          activity   Score
1     2019-06-20     "Car"      4500
1     2019-06-18     "Car"      600
1     2019-06-15     "Walk"     650
1     2019-06-21     "Ride"     790
2     2019-06-21     "Car"      800
2     2019-06-23     "Car"      500
3     2019-06-11     "Walk"     900
4     2019-06-15     "Walk"     200
4     2019-06-12     "Walk"     900')

    expected <- data.table::as.data.table(df)
    expected[, Stamp := as.POSIXct(Stamp)]
    expected[, window_start := Stamp - as.difftime(8, unit = "days")]

    expected[, `:=`(
        c("proportion_walk", "mean_score"),
        expected[expected,
                 .(proportion_walk = mean(activity == "Walk"), mean_score = mean(Score)),
                 # assuming shown lhs is the only one with window_start...
                 on = .(User, window_start <= Stamp, Stamp < Stamp),
                 by = .EACHI
                 ][, .(proportion_walk, mean_score)]
    )]

    ans <- df %>%
        (data.table::as.data.table) %>%
        start_expr %>%
        mutate(Stamp = as.POSIXct(Stamp)) %>%
        mutate(window_start = Stamp - as.difftime(8, unit="days")) %>%
        mutate_join(, User, Stamp > Stamp, Stamp >= window_start,
                    .SDcols = .(proportion_walk = mean(activity == "Walk"),
                                mean_score = mean(Score))) %>%
        end_expr

    expect_identical(expected, ans)
})

test_that("mutate_join's .by_each parameter yields the expected results.", {
    expected <- data.table::copy(rhs)[, N := lhs[.SD, .(.N), on = "x"]]

    ans <- rhs %>%
        start_expr %>%
        mutate_join(lhs, x, .SDcols = .(.N)) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    ans <- rhs %>%
        start_expr %>%
        mutate_join(lhs, x, .SDcols = ".N") %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    ans <- rhs %>%
        start_expr %>%
        mutate_join(lhs, x, .SDcols = .(N = length(.I)), .by_each = FALSE) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- data.table::copy(rhs)[, N := lhs[.SD, .(.N), on = "x", by = .EACHI][, .(N)]]

    ans <- rhs %>%
        start_expr %>%
        mutate_join(lhs, x, .SDcols = .(.N), .by_each = TRUE) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    ans <- rhs %>%
        start_expr %>%
        mutate_join(lhs, x, .SDcols = .(N = length(.I))) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)
})
