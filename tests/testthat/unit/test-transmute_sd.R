context("  Transmute SD")

test_that("Transmuting SD without all columns works.", {
    expected <- DT[, lapply(.SD, mad, low = TRUE)]

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = TRUE) %>% end_expr
    expect_identical(ans, expected)

    arg <- TRUE
    ans <- DT %>% start_expr %>% transmute_sd(mad, low = arg) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = !!arg) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Transmuting SD with subset of columns works.", {
    sd_cols <- c("mpg", "disp")
    expected <- DT[, lapply(.SD, mad, low = TRUE), .SDcols = sd_cols]

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = TRUE, .SDcols = sd_cols) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = TRUE, .SDcols = !!sd_cols) %>% end_expr
    expect_identical(ans, expected)

    arg <- TRUE
    ans <- DT %>% start_expr %>% transmute_sd(mad, low = arg, .SDcols = !!sd_cols) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = !!arg, .SDcols = !!sd_cols) %>% end_expr
    expect_identical(ans, expected)
})
