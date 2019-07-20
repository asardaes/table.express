context("  Filter on")

test_that("The filter_on verb works as expected.", {
    ans <- DT %>% start_expr %>% filter_on(cyl = 6, am = 0) %>% end_expr
    expect_identical(ans, DT[.(6, 0), on = c("cyl", "am")])

    ans <- DT %>% start_expr %>% filter_on(cyl = 6, am = 0, mult = "first") %>% end_expr
    expect_identical(ans, DT[.(6, 0), on = c("cyl", "am"), mult = "first"])

    ans <- DT %>% start_expr %>% filter_on(cyl = 6, am = 0, mult = "last") %>% end_expr
    expect_identical(ans, DT[.(6, 0), on = c("cyl", "am"), mult = "last"])

    # ----------------------------------------------------------------------------------------------

    expected <- data.table::setkey(data.table::copy(DT), cyl, am)[.(6, 0)]

    ans <- DT %>%
        data.table::copy(.) %>%
        data.table::setkey(cyl, am) %>%
        start_expr %>% filter_on(cyl = 6, 0) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    ans <- DT %>% start_expr %>% filter_on(cyl = 10) %>% end_expr
    expect_identical(nrow(ans), 1L)
    expect_true(any(sapply(ans, is.na)))

    ans <- DT %>% start_expr %>% filter_on(cyl = 10, nomatch = NULL) %>% end_expr
    expect_identical(nrow(ans), 0L)
})

test_that("The filter_on semantics can be negated.", {
    expected <- DT[!list(0), on = "am"]
    ans <- DT %>% start_expr %>% filter_on(am = 0, .negate = TRUE) %>% end_expr
    expect_identical(ans, expected)
})

test_that("The filter_on verb works for several values per key.", {
    expected <- state[.(c("South", "West"), c("South Atlantic", "Pacific"))]

    ans <- state %>%
        start_expr %>%
        filter_on(region = c("South", "West"), division = c("South Atlantic", "Pacific")) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- state[.(unique(region), c("South Atlantic", "Pacific"))]

    ans <- state %>%
        start_expr %>%
        filter_on(region = unique(region), division = c("South Atlantic", "Pacific")) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("The filter_on verb works when which = TRUE.", {
    expected <- data.table::copy(DT)[.(1, 0), on = c("vs", "am"), which = TRUE]
    ans <- DT %>% start_expr %>% filter_on(vs = 1, am = 0, which = TRUE) %>% end_expr(.by_ref = FALSE)
    expect_identical(ans, expected)
})

test_that("Eager filter_on works.", {
    expected <- data.table::copy(DT)[.(1, 0), on = c("vs", "am"), which = TRUE]
    ans <- data.table::copy(DT) %>% filter_on(vs = 1, am = 0, which = TRUE)
    expect_identical(ans, expected)

    expected <- data.table::copy(DT)[.(1, 0), flag := TRUE, on = c("vs", "am")]
    ans <- data.table::copy(DT) %>% filter_on(vs = 1, am = 0, .expr = TRUE) %>% mutate(flag = TRUE)
    expect_identical(ans, expected)
})
