context("  Filtering on secondary indices plus other operations")

test_that("filter_on works with other dplyr verbs as expected.", {
    expected <- data.table::copy(DT)[list(1, 1), ans := mpg + cyl, by = "carb", on = c("vs", "am")]

    ans <- DT %>%
        start_expr %>%
        filter_on(vs = 1, am = 1) %>%
        mutate(ans = mpg + cyl) %>%
        group_by(carb) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    ans <- DT %>%
        group_by(carb) %>%
        filter_on(vs = 1, am = 1) %>%
        mutate(ans = mpg + cyl)

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[list(4, 1), .(ans = 2 * disp - mean(hp)), keyby = "gear", on = c("gear", "carb")]

    ans <- DT %>%
        start_expr %>%
        filter_on(gear = 4, carb = 1) %>%
        transmute(ans = 2 * disp - mean(hp)) %>%
        key_by(gear) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- DT %>%
        key_by(gear) %>%
        filter_on(gear = 4, carb = 1) %>%
        transmute(ans = 2 * disp - mean(hp))

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- data.table::copy(state)[list("Mountain", "West"),
                                        `:=`(foo = abs(center_x - center_y),
                                             bar = sd(income) * sd(population)),
                                        on = c("division", "region")]

    ans <- state %>%
        start_expr %>%
        filter_on(division = "Mountain", region = "West") %>%
        mutate(foo = abs(center_x - center_y), bar = sd(income) * sd(population)) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    ans <- data.table::copy(state) %>%
        filter_on(division = "Mountain", region = "West", .expr = TRUE) %>%
        mutate(foo = abs(center_x - center_y), bar = sd(income) * sd(population))

    expect_identical(ans, expected)
})
