context("  Chain *_sd verbs")

test_that("Chaining *_sd verbs works as expected.", {
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

    # ----------------------------------------------------------------------------------------------

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
