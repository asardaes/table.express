context("  Transmute")

test_that("An empty clause is not an error.", {
    ans <- DT %>% start_expr %>% transmute() %>% end_expr
    expect_identical(ans, DT)
})

test_that("Transmuting by value without parsing works.", {
    ans <- DT %>% start_expr %>% transmute(ans = mpg * 2) %>% end_expr
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans_name <- "ans"
    ans <- DT %>% start_expr %>% transmute(!!ans_name := mpg * 2) %>% end_expr
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans <- DT %>% start_expr %>% transmute(mpg2 = mpg * 2, disp0.5 = disp / 2) %>% end_expr
    expect_identical(ncol(ans), 2L)
    expect_identical(ans$mpg2, DT$mpg * 2)
    expect_identical(ans$disp0.5, DT$disp / 2)
})

test_that("Transmuting by value with parsing works.", {
    ans <- DT %>% start_expr %>% transmute(ans = "mpg * 2", .parse = TRUE) %>% end_expr
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans_name <- "ans"
    ans <- DT %>% start_expr %>% transmute(!!ans_name := "mpg * 2", .parse = TRUE) %>% end_expr
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans <- DT %>% start_expr %>% transmute(mpg2 = "mpg * 2", disp0.5 = "disp / 2", .parse = TRUE) %>% end_expr

    expect_identical(ncol(ans), 2L)
    expect_identical(ans$mpg2, DT$mpg * 2)
    expect_identical(ans$disp0.5, DT$disp / 2)
})

test_that("Eager versions of transmute work.", {
    expected <- DT[, .(mpg = mpg * 2, hp = cumsum(hp), vs = max(vs))]
    ans <- transmute(DT, mpg = mpg * 2, hp = cumsum(hp), vs = max(vs))
    expect_identical(ans, expected)

    expected <- DT[, .(mpg = mpg * 2, hp = cumsum(hp)), by = .(vs, am)]
    ans <- DT %>% group_by(vs, am) %>% transmute(mpg = mpg * 2, hp = cumsum(hp))
    expect_identical(ans, expected)

    expected <- data.table::copy(DT)[, .(mpg = mpg * 2, hp = cumsum(hp)), keyby = .(vs, am)]
    ans <- DT %>% (data.table::copy) %>% key_by(vs, am) %>% transmute(mpg = mpg * 2, hp = cumsum(hp))
    expect_identical(ans, expected)
})

test_that("Preventing enlisting during transmutation works.", {
    expected <- DT[, mpg + hp]
    ans <- transmute(DT, mpg + hp, .enlist = FALSE)
    expect_identical(ans, expected)

    expect_warning(regexp = "1 expression", {
        ans <- transmute(DT, mpg + hp, vs * am, .enlist = FALSE)
    })

    expect_identical(ans, expected)
})

test_that("Expressions in sequential transmute can use variables from previous expressions and play nicely with group_by.", {
    expected <- DT[, .(cum_sum = cumsum(mpg)), by = .(gear)][, foo := cum_sum / 10]

    ans <- DT %>%
        group_by(gear) %>%
        transmute(cum_sum = cumsum(mpg), foo = cum_sum / 10, .sequential = TRUE)

    expect_equal(ans, expected)
})
