context("  Mutating SD plus other operations")

test_that("mutate_sd works with other dplyr verbs as expected.", {
    expected <- data.table::copy(DT)[vs == 0, `:=`(c("drat", "wt"), lapply(.SD, floor)), by = "carb", .SDcols = c("drat", "wt")]

    ans <- DT %>%
        start_expr %>%
        mutate_sd(floor, .SDcols = ends_with("t")) %>%
        where(vs == 0) %>%
        group_by(carb) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- data.table::copy(state)[region == "West",
                                        `:=`(c("population", "income"), lapply(.SD, function(x) {
                                            (x - mean(x)) / sd(x)
                                        })),
                                        by = "division",
                                        .SDcols = c("population", "income")]

    ans <- state %>%
        start_expr %>%
        mutate_sd(function(x) { (x - mean(x)) / sd(x) }, .SDcols = one_of("population", "income")) %>%
        where(region == "West") %>%
        group_by(division) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)

    foo <- function(x) { (x - mean(x)) / sd(x) }
    ans <- state %>%
        start_expr %>%
        mutate_sd(foo, .SDcols = one_of("population", "income")) %>%
        where(region == "West") %>%
        group_by(division) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)
})
