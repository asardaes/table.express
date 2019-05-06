context("  Arrange (a.k.a Order By)")

test_that("The arrange verb works for single arguments regardless of the where clause.", {
    expected <- DT[order(mpg)]

    ans <- DT %>% start_expr %>% arrange(mpg) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% arrange("mpg", .parse = TRUE) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% where() %>% arrange(mpg) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[vs == 0L][order(mpg)]

    ans <- DT %>% start_expr %>% where(vs == 0L) %>% arrange(mpg) %>% end_expr
    expect_identical(ans, expected)

    desired_vs <- 0L

    ans <- DT %>% start_expr %>% where(vs == desired_vs) %>% arrange(mpg) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% where(vs == !!desired_vs) %>% arrange(mpg) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>%
        start_expr %>%
        where("vs == 0L", .parse = TRUE) %>%
        arrange("mpg", .parse = TRUE) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("The order_by verb works for multiple arguments regardless of the where clause.", {
    expected <- DT[order(mpg, -cyl)]

    ans <- DT %>% start_expr %>% order_by(mpg, -cyl) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% order_by("mpg", "-cyl", .parse = TRUE) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% where() %>% order_by(mpg, -cyl) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[vs == 0L][order(mpg, -cyl)]

    ans <- DT %>% start_expr %>% where(vs == 0L) %>% order_by(mpg, -cyl) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>%
        start_expr %>%
        where("vs == 0L", .parse = TRUE) %>%
        order_by("mpg", "-cyl", .parse = TRUE) %>%
        end_expr

    expect_identical(ans, expected)
})
