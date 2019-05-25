context("  Filter on")

test_that("An unnamed value results in an error.", {
    expect_error({
        DT %>% start_expr %>% filter_on("foo") %>% end_expr
    })
})

test_that("The filter_on verb works as expected.", {
    ans <- DT %>% start_expr %>% filter_on(cyl = 6, am = 0) %>% end_expr
    expect_identical(ans, DT[.(6, 0), on = c("cyl", "am")])

    ans <- DT %>% start_expr %>% filter_on(cyl = 6, am = 0, mult = "first") %>% end_expr
    expect_identical(ans, DT[.(6, 0), on = c("cyl", "am"), mult = "first"])

    ans <- DT %>% start_expr %>% filter_on(cyl = 6, am = 0, mult = "last") %>% end_expr
    expect_identical(ans, DT[.(6, 0), on = c("cyl", "am"), mult = "last"])

    ans <- DT %>% start_expr %>% filter_on(cyl = 10) %>% end_expr
    expect_identical(nrow(ans), 1L)
    expect_true(any(sapply(ans, is.na)))

    ans <- DT %>% start_expr %>% filter_on(cyl = 10, nomatch = NULL) %>% end_expr
    expect_identical(nrow(ans), 0L)
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
