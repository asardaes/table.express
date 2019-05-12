context("  Mutate SD")

test_that("Mutating SD with function and subset of columns works.", {
    dt <- data.table::copy(DT)
    expected <- dt[, `:=`(mpg = log(mpg, base = 10), disp = log(disp, base = 10))]

    sd_cols <- c("mpg", "disp")

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(log, base = 10, .SDcols = sd_cols) %>% end_expr
    expect_identical(ans, expected)

    dt <- data.table::copy(DT)
    arg <- 10
    ans <- dt %>% start_expr %>% mutate_sd(log, base = arg, .SDcols = sd_cols) %>% end_expr
    expect_identical(ans, expected)

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(log, base = !!arg, .SDcols = sd_cols) %>% end_expr
    expect_identical(ans, expected)

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(log, "base = 10", .SDcols = sd_cols, .parse = TRUE) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Mutating SD with call and subset of columns works.", {
    dt <- data.table::copy(DT)
    expected <- dt[, `:=`(mpg = log(mpg, base = 10), disp = log(disp, base = 10))]

    sd_cols <- c("mpg", "disp")

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(log(.COL, ...), base = 10, .SDcols = sd_cols) %>% end_expr
    expect_identical(ans, expected)

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd("log(.COL, ...)", base = 10, .SDcols = sd_cols, .parse = TRUE) %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    dt <- data.table::copy(DT)
    expected <- dt[, `:=`(mpg = mpg * 2, disp = disp * 2)]

    ans <- DT %>% start_expr %>% mutate_sd(.COL * 2, .SDcols = sd_cols) %>% end_expr(.by_ref = FALSE)
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% mutate_sd(".COL * 2", .SDcols = sd_cols, .parse = TRUE) %>% end_expr(.by_ref = FALSE)
    expect_identical(ans, expected)
})
