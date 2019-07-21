context("  Complex mutations")

test_that("Mutating a subset of rows works.", {
    expected <- data.table::copy(state)[grepl("^N", region) & grepl("^N", name), abb := paste("N", abb, sep = "-")]

    ans <- data.table::copy(state) %>%
        start_expr %>%
        mutate(abb = paste("N", abb, sep = "-")) %>%
        where(grepl("^N", region) & grepl("^N", name)) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- data.table::copy(state) %>%
        start_expr %>%
        mutate(abb = paste("N", abb, sep = "-")) %>%
        where(grepl("^N", region), grepl("^N", name)) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- data.table::copy(state) %>%
        where(grepl("^N", region), grepl("^N", name)) %>%
        mutate(abb = paste("N", abb, sep = "-"))

    expect_identical(ans, expected)
})

test_that("Mutating by group works.", {
    expected <- data.table::copy(state)[, abb := paste(.GRP, abb, sep = "-"), by = .(region, division)]

    ans <- data.table::copy(state) %>%
        start_expr %>%
        mutate(abb = paste(.GRP, abb, sep = "-")) %>%
        group_by(region, division) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- data.table::copy(state) %>%
        group_by(region, division) %>%
        mutate(abb = paste(.GRP, abb, sep = "-"))

    expect_identical(ans, expected)
})

test_that("Mutating subset by group works.", {
    expected <- data.table::copy(state)[area > 50000, abb := paste(.GRP, abb, sep = "-"), by = .(region, division)]

    ans <- state %>%
        start_expr %>%
        mutate(abb = paste(.GRP, abb, sep = "-")) %>%
        where(area > 50000) %>%
        group_by(region, division) %>%
        end_expr(.by_ref = FALSE)

    expect_identical(ans, expected)
    expect_false(identical(ans, state))

    ans <- data.table::copy(state) %>%
        group_by(region, division) %>%
        where(area > 50000) %>%
        mutate(abb = paste(.GRP, abb, sep = "-"))

    expect_identical(ans, expected)
    expect_false(identical(ans, state))

    # ----------------------------------------------------------------------------------------------

    expected <- data.table::copy(state)[area > 50000, abb := paste(.GRP, abb, sep = "-"), by = .(region, division)
                                        ][area <= 50000, abb := paste(0, abb, sep = "-")]

    ans <- state %>%
        start_expr %>%
        mutate(abb = paste(.GRP, abb, sep = "-")) %>%
        where(area > 50000) %>%
        group_by(region, division) %>%
        chain(.by_ref = FALSE) %>%
        mutate(abb = paste(0, abb, sep = "-")) %>%
        where(area <= 50000) %>%
        end_expr

    expect_identical(ans, expected)
    expect_false(identical(ans, state))

    ans <- data.table::copy(state) %>%
        group_by(region, division) %>%
        where(area > 50000) %>%
        mutate(abb = paste(.GRP, abb, sep = "-")) %>%
        where(area <= 50000) %>%
        mutate(abb = paste(0, abb, sep = "-"))

    expect_identical(ans, expected)
    expect_false(identical(ans, state))
})
