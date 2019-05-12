context("  Transmute")

test_that("Transmuting by value without parsing works.", {
    ans <- DT  %>% transmute(ans = mpg * 2)
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans_name <- "ans"
    ans <- DT  %>% transmute(!!ans_name := mpg * 2)
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans <- DT  %>% transmute(mpg2 = mpg * 2, disp0.5 = disp / 2)
    expect_identical(ncol(ans), 2L)
    expect_identical(ans$mpg2, DT$mpg * 2)
    expect_identical(ans$disp0.5, DT$disp / 2)
})

test_that("Transmuting by value with parsing works.", {
    ans <- DT  %>% transmute(ans = "mpg * 2", .parse = TRUE)
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans_name <- "ans"
    ans <- DT  %>% transmute(!!ans_name := "mpg * 2", .parse = TRUE)
    expect_identical(ncol(ans), 1L)
    expect_identical(ans$ans, DT$mpg * 2)

    ans <- DT %>% transmute(mpg2 = "mpg * 2", disp0.5 = "disp / 2", .parse = TRUE)

    expect_identical(ncol(ans), 2L)
    expect_identical(ans$mpg2, DT$mpg * 2)
    expect_identical(ans$disp0.5, DT$disp / 2)
})

test_that("Referencing newly created variables works.", {
    ans <- DT  %>% transmute(first = mpg * 2, second = first - 2)
    expect_identical(ncol(ans), 2L)
    expect_identical(ans$first, DT$mpg * 2)
    expect_identical(ans$second, DT$mpg * 2 - 2)
})
