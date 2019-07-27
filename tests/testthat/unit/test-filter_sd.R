context("  Filter SD")

test_that("Filtering SD works for functions.", {
    expected <- DT[mpg > 20 & qsec > 20]

    ans <- DT %>% start_expr %>% filter_sd(`>`, 20, .SDcols = c("mpg", "qsec")) %>% end_expr
    expect_identical(ans, expected)

    val <- 20
    ans <- DT %>% start_expr %>% filter_sd(`>`, !!val, .SDcols = c("mpg", "qsec")) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Filtering SD works for predicates.", {
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

test_that("Filtering SD with .COL predicates works.", {
    chosen <- names(DT)[as.logical(DT[, lapply(.SD, function(col) { any(col == 1) })])]
    expected <- DT[rep(list(1), length(chosen)), on = chosen]
    ans <- DT %>% start_expr %>% filter_sd(.COL == 1, .SDcols = any(.COL == 1)) %>% end_expr
    expect_identical(ans, expected)

    expect_error(regexp = "single logical", {
        DT %>% start_expr %>% filter_sd(.COL == 1, .SDcols = .COL == 1) %>% end_expr
    })
})

test_that("Filtering SD with .COLNAME predicates works.", {
    expected <- DT[drat > 3 & wt > 3]
    ans <- filter_sd(DT, grepl("t$", .COLNAME), .COL > 3)
    expect_identical(ans, expected)
})

test_that("Filtering SD with :-calls works.", {
    expected <- DT[vs == 1 & am == 1]

    ans <- DT %>% start_expr %>% filter_sd(vs:am, .COL == 1) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% filter_sd(8:9, .COL == 1) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% filter_sd(vs:9, .COL == 1) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Filtering SD when which = TRUE works.", {
    expected <- DT[vs == 1 & am == 1, which = TRUE]
    ans <- DT %>% start_expr %>% filter_sd(vs:am, .COL == 1, which = TRUE) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Eager filter_sd works.", {
    expected <- DT[vs == 1 & am == 1, which = TRUE]
    ans <- DT %>% filter_sd(vs:am, .COL == 1, which = TRUE)
    expect_identical(ans, expected)

    expected <- data.table::copy(DT)[vs == 1 & am == 1, flag := TRUE]
    ans <- data.table::copy(DT) %>% filter_sd(vs:am, .COL == 1, .expr = TRUE) %>% mutate(flag = TRUE)
    expect_identical(ans, expected)
})

test_that("filter_sd with formulas works.", {
    expected <- DT[drat > 3 & wt > 3]
    ans <- filter_sd(DT, ~ grepl("t$", .y), ~ .x > 3)
    expect_identical(ans, expected)
})
