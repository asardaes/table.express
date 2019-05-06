context("  Arrange (a.k.a Order By)")

test_that("The arrange verb works for single arguments regardless of the where clause.", {
    expected <- DT[order(mpg)]

    ans <- DT %>% arrange(mpg) %>% eval_expr
    expect_identical(ans, expected)

    ans <- DT %>% arrange("mpg", .parse = TRUE) %>% eval_expr
    expect_identical(ans, expected)

    ans <- DT %>% where() %>% arrange(mpg) %>% eval_expr
    expect_identical(ans, expected)

    expected <- DT[vs == 0L][order(mpg)]

    ans <- DT %>% where(vs == 0L) %>% arrange(mpg) %>% eval_expr
    expect_identical(ans, expected)

    ans <- DT %>% where("vs == 0L", .parse = TRUE) %>% arrange("mpg", .parse = TRUE) %>% eval_expr
    expect_identical(ans, expected)
})

test_that("The order_by verb works for multiple arguments regardless of the where clause.", {
    expected <- DT[order(mpg, -cyl)]

    ans <- DT %>% order_by(mpg, -cyl) %>% eval_expr
    expect_identical(ans, expected)

    ans <- DT %>% order_by("mpg", "-cyl", .parse = TRUE) %>% eval_expr
    expect_identical(ans, expected)

    ans <- DT %>% where() %>% order_by(mpg, -cyl) %>% eval_expr
    expect_identical(ans, expected)

    expected <- DT[vs == 0L][order(mpg, -cyl)]

    ans <- DT %>% where(vs == 0L) %>% order_by(mpg, -cyl) %>% eval_expr
    expect_identical(ans, expected)

    ans <- DT %>% where("vs == 0L", .parse = TRUE) %>% order_by("mpg", "-cyl", .parse = TRUE) %>% eval_expr
    expect_identical(ans, expected)
})
