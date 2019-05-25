context("  Transmute SD")

test_that("Transmuting SD for all columns works.", {
    expected <- DT[, lapply(.SD, mad, low = TRUE)]

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = TRUE) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mad(.COL, ...), low = TRUE) %>% end_expr
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

    ans <- DT %>% start_expr %>% transmute_sd(mad(.COL, ...), low = TRUE, .SDcols = sd_cols) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = TRUE, .SDcols = !!sd_cols) %>% end_expr
    expect_identical(ans, expected)

    arg <- TRUE
    ans <- DT %>% start_expr %>% transmute_sd(mad, low = arg, .SDcols = !!sd_cols) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = !!arg, .SDcols = !!sd_cols) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Transmuting SD with single column works.", {
    sd_cols <- "mpg"
    expected <- DT[, lapply(.SD, mad, low = TRUE), .SDcols = sd_cols]

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = TRUE, .SDcols = "mpg") %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = TRUE, .SDcols = sd_cols) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mad(.COL, ...), low = TRUE, .SDcols = sd_cols) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = TRUE, .SDcols = !!sd_cols) %>% end_expr
    expect_identical(ans, expected)

    arg <- TRUE
    ans <- DT %>% start_expr %>% transmute_sd(mad, low = arg, .SDcols = !!sd_cols) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mad, low = !!arg, .SDcols = !!sd_cols) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Transmuting SD with dynamic .SDcols works.", {
    expected <- DT[, .(disp = disp * 2, drat = drat * 2)]

    ans <- DT %>% start_expr %>% transmute_sd(.COL * 2, .SDcols = c("disp", "drat")) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(.COL * 2, .SDcols = grepl("^d", .COLNAME)) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[, .(drat = drat * 2, wt = wt * 2)]

    ans <- DT %>% start_expr %>% transmute_sd(.COL * 2, .SDcols = any(.COL < 5 & .COL %% 1 != 0)) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Transmuting SD with tidy selectors works.", {
    expected <- data.table::copy(DT)[, `:=`(disp = disp * 2, drat = drat * 2)][, .(disp, drat)]

    ans <- DT %>% start_expr %>% transmute_sd(.COL * 2, .SDcols = c("disp", "drat")) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(.COL * 2, .SDcols = starts_with("d")) %>% end_expr
    expect_identical(ans, expected)
})
