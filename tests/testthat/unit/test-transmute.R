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
