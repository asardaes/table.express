context("  Chain *_sd verbs")

test_that("Chain filter_sd -> transmute_sd -> mutate_sd", {
    matches_cardinal <- function(x) { grepl("north|east|south|west", x, ignore.case = TRUE) }

    expected <- state[matches_cardinal(region) & matches_cardinal(division) & matches_cardinal(name),
                      lapply(.SD, `^`, 2),
                      .SDcols = c("center_x", "center_y")
                      ][, `:=`(c("center_x", "center_y"), lapply(.SD, function(x) { x * pi / 180 }))]

    ans <- state %>%
        start_expr %>%
        filter_sd(matches_cardinal, .SDcols = c("region", "division", "name")) %>%
        transmute_sd(.COL ^ 2, .SDcols = starts_with("center_")) %>%
        mutate_sd(.COL * pi / 180, .SDcols = c("center_x", "center_y")) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- state %>%
        start_expr %>%
        filter_sd(matches_cardinal, .SDcols = c("region", "division", "name")) %>%
        transmute_sd(.COL ^ 2, .SDcols = starts_with("center_")) %>%
        chain %>%
        mutate_sd(.COL * pi / 180, .SDcols = everything()) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Chain mutate_sd -> filter_sd -> transmute_sd", {
    sd_cols <- c("gear", "carb")
    expected <- data.table::copy(DT)[, `:=`((sd_cols), lapply(.SD, as.integer)), .SDcols = sd_cols
                                     ][gear %% 2 == 0 & carb %% 2 == 0,
                                       lapply(.SD, cumsum),
                                       .SDcols = setdiff(names(DT), sd_cols)]

    ans <- DT %>%
        start_expr %>%
        mutate_sd(as.integer, .SDcols = sd_cols) %>%
        chain(.by_ref = FALSE) %>%
        filter_sd(.COL %% 2 == 0, .SDcols = sd_cols) %>%
        transmute_sd(cumsum, .SDcols = setdiff(names(DT), sd_cols)) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- DT %>%
        start_expr %>%
        mutate_sd(as.integer, .SDcols = sd_cols) %>%
        transmute_sd(cumsum, .SDcols = setdiff(names(DT), sd_cols)) %>%
        filter_sd(.COL %% 2 == 0, .SDcols = sd_cols) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)
})

test_that("Chain transmute_sd -> filter_sd -> transmute_sd", {
    sd_cols <- c("gear", "carb")
    expected <- DT[, lapply(.SD, as.integer), .SDcols = sd_cols
                   ][gear %% 2 == 0 & carb %% 2 == 0, lapply(.SD, cumsum)]

    ans <- DT %>%
        start_expr %>%
        transmute_sd(as.integer, .SDcols = sd_cols) %>%
        chain %>%
        filter_sd(.COL %% 2 == 0, .SDcols = sd_cols) %>%
        transmute_sd(, cumsum) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- DT %>%
        start_expr %>%
        transmute_sd(as.integer, .SDcols = sd_cols) %>%
        filter_sd(.COL %% 2 == 0, .SDcols = sd_cols) %>%
        transmute_sd(, cumsum) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Chain mutate_sd -> mutate_sd", {
    sd_cols <- c("drat", "wt")
    expected <- data.table::copy(DT)[, `:=`((sd_cols), lapply(.SD, round)), .SDcols = sd_cols
                                     ][, `:=`((sd_cols), lapply(.SD, sum)), .SDcols = sd_cols, by = cyl]

    ans <- DT %>%
        (data.table::copy) %>%
        start_expr %>%
        mutate_sd(round, .SDcols = sd_cols) %>%
        mutate_sd(sum, .SDcols = sd_cols) %>%
        group_by(cyl) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- DT %>%
        (data.table::copy) %>%
        start_expr %>%
        mutate_sd(round, .SDcols = sd_cols) %>%
        chain %>%
        mutate_sd(sum, .SDcols = sd_cols) %>%
        group_by(cyl) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Chain filter_sd -> filter_sd", {
    expected <- DT[vs == 0 & am == 0][mpg > 17 & qsec > 17]

    ans <- DT %>%
        start_expr %>%
        filter_sd(.COL == 0, .SDcols = c("vs", "am")) %>%
        filter_sd(.COL > 17, .SDcols = c("mpg", "qsec")) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Chain filter_sd -> mutate_sd -> filter_sd", {
    expected <- DT[vs == 0 & am == 0][, disp := disp / 10][, hp := hp / 10][mpg > 17 & qsec > 17]

    ans <- DT %>%
        start_expr %>%
        filter_sd(.COL == 0, .SDcols = c("vs", "am")) %>%
        chain %>%
        mutate_sd(.COL / 10, .SDcols = c("disp", "hp")) %>%
        chain %>%
        filter_sd(.COL > 17, .SDcols = c("mpg", "qsec")) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Chain filter_sd -> transmute_sd -> filter_sd", {
    expected <- DT[vs == 0 & am == 0][, lapply(.SD, `*`, 2), .SDcols = c("mpg", "qsec")][mpg > 34 & qsec > 34]

    ans <- DT %>%
        start_expr %>%
        filter_sd(.COL == 0, .SDcols = c("vs", "am")) %>%
        transmute_sd(.COL * 2, .SDcols = c("mpg", "qsec")) %>%
        filter_sd(.COL > 34, .SDcols = c("mpg", "qsec")) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Chain filter_sd -> filter_on -> filter_sd", {
    expected <- state[income > 5000 & area > 5000][.("West", "Pacific")][population > 1000 & income > 1000]

    ans <- state %>%
        start_expr %>%
        filter_sd(.COL > 5000, .SDcols = c("income", "area")) %>%
        filter_on(region = "West", division = "Pacific") %>%
        filter_sd(.COL > 1000, .SDcols = c("population", "income")) %>%
        end_expr

    expect_identical(ans, expected)
})
