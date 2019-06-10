context("  Mutate")

test_that("Mutating by reference without parsing works.", {
    dt <- data.table::copy(DT)

    ans <- dt %>% start_expr %>% mutate(ans = mpg * 2) %>% end_expr
    expect_identical(ncol(ans), ncol(dt))
    expect_identical(dt$ans, dt$mpg * 2)

    ans_name <- "ans"
    ans <- dt %>% start_expr %>% mutate(!!ans_name := mpg * 2) %>% end_expr
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

    ans_name <- "ans"
    ans <- dt %>% start_expr %>% mutate(!!ans_name := "mpg * 2", .parse = TRUE) %>% end_expr
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
    ans <- DT %>% start_expr %>% mutate(ans = mpg * 2) %>% end_expr(.by_ref = FALSE)
    expect_identical(ncol(ans), ncol(DT) + 1L)
    expect_identical(ans$ans, ans$mpg * 2)

    ans <- ans %>% start_expr %>% mutate(ans = NULL) %>% end_expr(.by_ref = FALSE)
    expect_equal(ans, DT)

    ans_name <- "ans"
    ans <- DT %>% start_expr %>% mutate(!!ans_name := mpg * 2) %>% end_expr(.by_ref = FALSE)
    expect_identical(ncol(ans), ncol(DT) + 1L)
    expect_identical(ans$ans, ans$mpg * 2)

    ans <- DT %>% start_expr %>% mutate(mpg2 = mpg * 2, disp0.5 = disp / 2) %>% end_expr(.by_ref = FALSE)
    expect_identical(ncol(ans), ncol(DT) + 2L)
    expect_identical(ans$mpg2, DT$mpg * 2)
    expect_identical(ans$disp0.5, DT$disp / 2)
})

test_that("Mutating by value with parsing works.", {
    ans <- DT %>% start_expr %>% mutate(ans = "mpg * 2", .parse = TRUE) %>% end_expr(.by_ref = FALSE)
    expect_identical(ncol(ans), ncol(DT) + 1L)
    expect_identical(ans$ans, ans$mpg * 2)

    ans <- ans %>% start_expr %>% mutate(ans = "NULL", .parse = TRUE) %>% end_expr(.by_ref = FALSE)
    expect_equal(ans, DT)

    ans_name <- "ans"
    ans <- DT %>% start_expr %>% mutate(!!ans_name := "mpg * 2", .parse = TRUE) %>% end_expr(.by_ref = FALSE)
    expect_identical(ncol(ans), ncol(DT) + 1L)
    expect_identical(ans$ans, ans$mpg * 2)

    ans <- DT %>%
        start_expr %>%
        mutate(mpg2 = "mpg * 2", disp0.5 = "disp / 2", .parse = TRUE) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ncol(ans), ncol(DT) + 2L)
    expect_identical(ans$mpg2, DT$mpg * 2)
    expect_identical(ans$disp0.5, DT$disp / 2)
})

test_that("Mutations with parens expressions work if .unquote_names = FALSE.", {
    ans <- DT %>% start_expr %>% mutate((c("a", "b")) := .(1, 2), .unquote_names = FALSE) %>% end_expr(.by_ref = FALSE)
    expect_identical(ncol(ans), ncol(DT) + 2L)
    expect_identical(ans$a, rep(1, nrow(DT)))
    expect_identical(ans$b, rep(2, nrow(DT)))

    lhs <- c("a", "b")
    ans <- DT %>% start_expr %>% mutate((lhs) := .(1, 2), .unquote_names = FALSE) %>% end_expr(.by_ref = FALSE)
    expect_identical(ncol(ans), ncol(DT) + 2L)
    expect_identical(ans$a, rep(1, nrow(DT)))
    expect_identical(ans$b, rep(2, nrow(DT)))

    ans %>% start_expr %>% mutate((lhs) := NULL, .unquote_names = FALSE) %>% end_expr
    expect_equal(ans, DT)

    expect_warning({
        ans <- ans %>% start_expr %>% mutate((lhs) := 1, mpg = 0, .unquote_names = FALSE) %>% end_expr(.by_ref = FALSE)
    })

    expect_identical(ncol(ans), ncol(DT) + 2L)
    expect_identical(ans$a, rep(1, nrow(DT)))
    expect_identical(ans$b, rep(1, nrow(DT)))
})
