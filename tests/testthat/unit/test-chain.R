context("  Chain")

test_that("Chaining expression builders works.", {
    expected <- DT[vs == 0L][order(mpg)]

    ans <- DT %>% start_expr %>% where(vs == 0L) %>% chain %>% order_by(mpg) %>% end_expr
    expect_identical(ans, expected)

    data.table::setindex(DT, NULL)
})
