context("  Filter SD")

test_that("Filtering SD works for functions.", {
    expected <- DT[mpg > 20 & qsec > 20]

    ans <- DT %>% start_expr %>% filter_sd(`>`, 20, .SDcols = c("mpg", "qsec")) %>% end_expr
    expect_identical(ans, expected)

    val <- 20
    ans <- DT %>% start_expr %>% filter_sd(`>`, !!val, .SDcols = c("mpg", "qsec")) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Filtering SD works for calls.", {
    expected <- DT[mpg > 20 & qsec > 20]

    ans <- DT %>% start_expr %>% filter_sd(.COL > 20, .SDcols = c("mpg", "qsec")) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% filter_sd(20 < .COL, .SDcols = c("mpg", "qsec")) %>% end_expr
    expect_identical(ans, expected)

    val <- 20
    ans <- DT %>% start_expr %>% filter_sd(.COL > !!val, .SDcols = c("mpg", "qsec")) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Filtering SD works with tidyselect helpers.", {
    expected <- DT[mpg > 0 & am > 0]

    ans <- DT %>% start_expr %>% filter_sd(.COL > 0, .SDcols = contains("m")) %>% end_expr
    expect_identical(expected, ans)
})
