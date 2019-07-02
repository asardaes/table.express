context("  Transmute SD")

test_that("Transmuting SD for all columns works.", {
    expected <- DT[, lapply(.SD, mad, low = TRUE)]

    ans <- DT %>% start_expr %>% transmute_sd(, mad, low = TRUE) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(, mad(.COL, ...), low = TRUE) %>% end_expr
    expect_identical(ans, expected)

    arg <- TRUE
    ans <- DT %>% start_expr %>% transmute_sd(, mad, low = arg) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(, mad, low = !!arg) %>% end_expr
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

    expect_error(regexp = "single logical", {
        DT %>% start_expr %>% transmute_sd(.COL * 2, .SDcols = .COL < 5 & .COL %% 1 != 0) %>% end_expr
    })
})

test_that("Transmuting SD with tidy selectors works.", {
    expected <-DT[, .(disp = disp * 2, drat = drat * 2)]

    ans <- DT %>% start_expr %>% transmute_sd(.COL * 2, .SDcols = c("disp", "drat")) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(.COL * 2, .SDcols = starts_with("d")) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Transmuting SD with :-calls works.", {
    expected <- DT[, .(disp = disp * 2, hp = hp * 2, drat = drat * 2)]

    ans <- DT %>% start_expr %>% transmute_sd(disp:drat, .COL * 2) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(3:5, .COL * 2) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(3:drat, .COL * 2) %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[, .(disp = cumsum(disp), hp = cumsum(hp), drat = cumsum(drat))]

    ans <- DT %>% start_expr %>% transmute_sd(disp:drat, cumsum) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(3:5, cumsum) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(3:drat, cumsum) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Transmuting SD with list of functions works.", {
    sd_cols <- c("mpg", "cyl", "disp")

    expected <- DT[, c(min = lapply(.SD, min), max = lapply(.SD, max)), .SDcols = sd_cols]

    ans <- DT %>% start_expr %>% transmute_sd(mpg:disp, .(min, max(.COL))) %>% end_expr
    data.table::setcolorder(ans, names(expected))
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% transmute_sd(mpg:disp, .(min, max)) %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[, c(min = lapply(.SD, min)), .SDcols = sd_cols]

    ans <- DT %>% start_expr %>% transmute_sd(mpg:disp, .(min = min)) %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[, c(minimum = lapply(.SD, min)), .SDcols = sd_cols]

    ans <- DT %>% start_expr %>% transmute_sd(mpg:disp, .(minimum = min)) %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[, c(lapply(.SD, min), lapply(.SD, max)), .SDcols = sd_cols]
    data.table::setnames(expected, as.character(t(outer(c("minimum", "maximum"), sd_cols, paste, sep = "."))))

    ans <- DT %>% start_expr %>% transmute_sd(mpg:disp, .(minimum = min, maximum = max(.COL))) %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[, c(minimum = lapply(.SD, min), maximum = lapply(.SD, max)), .SDcols = sd_cols]

    ans <- DT %>% start_expr %>% transmute_sd(mpg:disp, .(minimum = min, maximum = max)) %>% end_expr
    expect_identical(ans, expected)
})
