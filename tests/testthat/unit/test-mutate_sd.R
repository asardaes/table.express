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

test_that("Renaming while mutating SD with function and subset of columns works.", {
    dt <- data.table::copy(DT)
    expected <- dt[, `:=`(log_mpg = log(mpg, base = 10), log_disp = log(disp, base = 10))]

    sd_cols <- c("mpg", "disp")

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(log, base = 10, .SDcols = sd_cols, .prefix = "log_") %>% end_expr
    expect_identical(ans, expected)

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(log, base = 10, .SDcols = sd_cols, .prefix = "log_", .suffix = "ignored") %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    dt <- data.table::copy(DT)
    expected <- dt[, `:=`(mpg_log = log(mpg, base = 10), disp_log = log(disp, base = 10))]

    sd_cols <- c("mpg", "disp")

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(log, base = 10, .SDcols = sd_cols, .suffix = "_log") %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    dt <- data.table::copy(DT)
    expected <- dt[, `:=`(loga.mpg = log(mpg, base = 10), loga.disp = log(disp, base = 10))]

    sd_cols <- c("mpg", "disp")

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(.(loga = log), base = 10, .SDcols = sd_cols) %>% end_expr
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

test_that("Mutating SD with a list of expressions/functions works.", {
    expected <- data.table::copy(DT)[, `:=`(mpg = log(mpg, base = 10), am = log(am, base = 10), x = 1L, y = 2L)]

    ans <- DT %>%
        start_expr %>%
        mutate_sd(c("mpg", "am", "x", "y"), .(log, log(.COL), 1L, 2L), base = 10) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- data.table::copy(DT)[, `:=`(mpg_log = log(mpg, base = 10), am = log(am, base = 10), x = 1L, y = 2L)]

    ans <- DT %>%
        start_expr %>%
        mutate_sd(c("mpg", "am", "not_x", "y"), .(mpg_log = log, log(.COL), x = 1L, 2L), base = 10) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- data.table::copy(DT)[, `:=`(log.mpg = log(mpg),
                                            log.am = log(am),
                                            exp.mpg = exp(mpg),
                                            exp.am = exp(am))]

    ans <- data.table::copy(DT) %>%
        mutate_sd(c("mpg", "am"), .(log, exp), .pairwise = FALSE)

    expect_identical(ans, expected)
})

test_that("Mutating SD with tidyselect helpers works.", {
    dt <- data.table::copy(DT)
    expected <- dt[, `:=`(mpg = log(mpg, base = 10), am = log(am, base = 10))]

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(log, base = 10, .SDcols = contains("m")) %>% end_expr
    expect_identical(ans, expected)

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd("log(.COL, ...)", base = 10, .SDcols = contains("m"), .parse = TRUE) %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    dt <- data.table::copy(DT)
    expected <- dt[, `:=`(mpg = mpg * 2, am = am * 2)]

    ans <- DT %>% start_expr %>% mutate_sd(.COL * 2, .SDcols = contains("m")) %>% end_expr(.by_ref = FALSE)
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% mutate_sd(".COL * 2", .SDcols = contains("m"), .parse = TRUE) %>% end_expr(.by_ref = FALSE)
    expect_identical(ans, expected)
})

test_that("Mutating SD with .COL predicates works.", {
    dt <- data.table::copy(DT)
    chosen <- names(DT)[as.logical(DT[, lapply(.SD, function(col) { any(col %% 1 != 0L) })])]
    expected <- dt[, (chosen) := lapply(.SD, as.integer), .SDcols = chosen]

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(as.integer, .SDcols = any(.COL %% 1 != 0L)) %>% end_expr

    expect_identical(ans, expected)
})

test_that("Mutating SD with :-calls works.", {
    dt <- data.table::copy(DT)
    expected <- dt[, c("mpg", "cyl", "disp") := -1L]

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(mpg:disp, -1L) %>% end_expr
    expect_identical(ans, expected)

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(1:3L, -1L) %>% end_expr
    expect_identical(ans, expected)

    dt <- data.table::copy(DT)
    ans <- dt %>% start_expr %>% mutate_sd(mpg:3, -1L) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Eager mutation of SD works.", {
    expected <- data.table::copy(DT)[, c("mpg", "cyl") := lapply(.SD, "*", 2), .SDcols = c("mpg", "cyl")]
    ans <- data.table::copy(DT) %>% mutate_sd(c("mpg", "cyl"), .COL * 2)
    expect_identical(ans, expected)

    expected <- data.table::copy(DT)[vs == 0, c("mpg", "cyl") := lapply(.SD, "*", 2), .SDcols = c("mpg", "cyl")]
    ans <- data.table::copy(DT) %>% where(vs == 0) %>% mutate_sd(c("mpg", "cyl"), .COL * 2)
    expect_identical(ans, expected)
})

test_that("mutate_sd with formulas works.", {
    expected <- data.table::copy(DT)[, c("mpg", "cyl") := list(mpg / 2, NULL)]
    ans <- mutate_sd(data.table::copy(DT), c("mpg", "cyl"), list(~ . / 2, ~ NULL))
    expect_identical(ans, expected)

    expected <- data.table::copy(DT)[, c("mpg", "am") := list(0, 1)]
    ans <- mutate_sd(data.table::copy(DT), ~ grepl("m", .y), list(~ 0, ~ 1))
    expect_identical(ans, expected)

    expected <- data.table::copy(DT)[, c("vs", "am") := lapply(.SD, function(.x) { replace(.x, .x == 0, NA_real_) }),
                                     .SDcols = c("vs", "am")]
    ans <- mutate_sd(data.table::copy(DT), ~ any(. == 0), ~ replace(.x, .x == 0, NA_real_))
    expect_identical(ans, expected)

    expect_error(mutate_sd(DT, ~ .x == 0, ~ replace(.x, .x == 0, NA_real_)), "single logical")
})
