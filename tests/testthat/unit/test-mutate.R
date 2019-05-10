context("  Mutate")

test_that("Mutating by reference without parsing works.", {
    dt <- data.table::copy(DT)

    ans <- dt %>% start_expr %>% mutate(ans = mpg * 2) %>% end_expr
    expect_identical(ncol(ans), ncol(dt))
    expect_identical(dt$ans, dt$mpg * 2)

    prev_ncol <- ncol(dt)
    dt %>% start_expr %>% mutate(ans = NULL) %>% end_expr
    expect_identical(ncol(dt), prev_ncol - 1L)

    prev_ncol <- ncol(dt)
    dt %>% start_expr %>% mutate(mpg2 = mpg * 2, disp0.5 = disp / 2) %>% end_expr
    expect_identical(ncol(dt), prev_ncol + 2L)
    expect_identical(dt$mpg2, dt$mpg * 2)
    expect_identical(dt$disp0.5, dt$disp / 2)
})

test_that("Mutating by reference with parsing works.", {
    dt <- data.table::copy(DT)

    ans <- dt %>% start_expr %>% mutate(ans = "mpg * 2", .parse = TRUE) %>% end_expr
    expect_identical(ncol(ans), ncol(dt))
    expect_identical(dt$ans, dt$mpg * 2)

    prev_ncol <- ncol(dt)
    dt %>% start_expr %>% mutate(ans = "NULL", .parse = TRUE) %>% end_expr
    expect_identical(ncol(dt), prev_ncol - 1L)

    prev_ncol <- ncol(dt)
    dt %>% start_expr %>% mutate(mpg2 = "mpg * 2", disp0.5 = "disp / 2", .parse = TRUE) %>% end_expr
    expect_identical(ncol(dt), prev_ncol + 2L)
    expect_identical(dt$mpg2, dt$mpg * 2)
    expect_identical(dt$disp0.5, dt$disp / 2)
})

test_that("Mutating by value without parsing works.", {
    ans <- DT %>% start_expr %>% mutate(ans = mpg * 2, .by_ref = FALSE) %>% end_expr
    expect_identical(ncol(ans), ncol(DT) + 1L)
    expect_identical(ans$ans, ans$mpg * 2)

    ans <- ans %>% start_expr %>% mutate(ans = NULL, .by_ref = FALSE) %>% end_expr
    expect_identical(ans, DT)

    ans <- DT %>% start_expr %>% mutate(mpg2 = mpg * 2, disp0.5 = disp / 2, .by_ref = FALSE) %>% end_expr
    expect_identical(ncol(ans), ncol(DT) + 2L)
    expect_identical(ans$mpg2, DT$mpg * 2)
    expect_identical(ans$disp0.5, DT$disp / 2)
})

test_that("Mutating by value with parsing works.", {
    ans <- DT %>% start_expr %>% mutate(ans = "mpg * 2", .parse = TRUE, .by_ref = FALSE) %>% end_expr
    expect_identical(ncol(ans), ncol(DT) + 1L)
    expect_identical(ans$ans, ans$mpg * 2)

    ans <- ans %>% start_expr %>% mutate(ans = "NULL", .parse = TRUE, .by_ref = FALSE) %>% end_expr
    expect_identical(ans, DT)

    ans <- DT %>%
        start_expr %>%
        mutate(mpg2 = "mpg * 2", disp0.5 = "disp / 2", .parse = TRUE, .by_ref = FALSE) %>%
        end_expr

    expect_identical(ncol(ans), ncol(DT) + 2L)
    expect_identical(ans$mpg2, DT$mpg * 2)
    expect_identical(ans$disp0.5, DT$disp / 2)
})
