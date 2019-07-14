context("  Filter (a.k.a Where)")

test_that("An empty clause is not an error.", {
    ans <- DT %>% start_expr %>% where() %>% end_expr
    expect_identical(ans, DT)

    ans <- DT %>% start_expr %>% filter() %>% end_expr
    expect_identical(ans, DT)
})

test_that("The where verb works with simple numeric indices.", {
    ans <- DT %>% start_expr %>% where(1L) %>% end_expr
    expect_identical(nrow(ans), 1L)

    ans <- DT %>% start_expr %>% where(1:2) %>% end_expr
    expect_identical(nrow(ans), 2L)

    ans <- DT %>% start_expr %>% where(c(1L, 3L)) %>% end_expr
    expect_identical(nrow(ans), 2L)

    ans <- DT %>% start_expr %>% where(-c(1L, 3L)) %>% end_expr
    expect_identical(nrow(ans), nrow(DT) - 2L)
    expect_identical(ans, DT[-c(1L, 3L)])

    i <- 1:2

    ans <- DT %>% start_expr %>% where(i) %>% end_expr
    expect_identical(ans, DT[1:2])

    ans <- DT %>% start_expr %>% where(!!i) %>% end_expr
    expect_identical(ans, DT[1:2])
})

test_that("The where verb works when parsing simple numeric indices.", {
    ans <- DT %>% start_expr %>% where("1L", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 1L)

    ans <- DT %>% start_expr %>% where("1:2", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 2L)

    ans <- DT %>% start_expr %>% where("c(1L, 3L)", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 2L)

    ans <- DT %>% start_expr %>% where("-c(1L, 3L)", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), nrow(DT) - 2L)
})

test_that("Computing expressions in filter works.", {
    ans <- DT %>% start_expr %>% filter(vs == 0L) %>% end_expr
    expect_identical(nrow(ans), 18L)

    ans <- DT %>% start_expr %>% filter(vs == 0L & am == 0L) %>% end_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% start_expr %>% filter(vs == 0L, am == 0L) %>% end_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% start_expr %>% filter(vs == 0L | am == 0L) %>% end_expr
    expect_identical(nrow(ans), 25L)

    ans <- DT %>% start_expr %>% filter(vs == 0L, am == 0L, .collapse = `|`) %>% end_expr
    expect_identical(nrow(ans), 25L)
    expect_identical(ans, DT[vs == 0L | am == 0L])

    i <- 0L

    ans <- DT %>% start_expr %>% filter(vs == i) %>% end_expr
    expect_identical(nrow(ans), 18L)

    ans <- DT %>% start_expr %>% filter(vs == i & am == 0L) %>% end_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% start_expr %>% filter(vs == i, am == 0L) %>% end_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% start_expr %>% filter(vs == i | am == 0L) %>% end_expr
    expect_identical(nrow(ans), 25L)

    ans <- DT %>% start_expr %>% filter(vs == i, am == 0L, .collapse = `|`) %>% end_expr
    expect_identical(ans, DT[vs == i | am == 0L])

    ans <- DT %>% start_expr %>% filter(vs == !!i, am == 0L, .collapse = `|`) %>% end_expr
    expect_identical(ans, DT[vs == i | am == 0L])
})

test_that("Parsing computing expressions in filter works.", {
    ans <- DT %>% start_expr %>% filter("vs == 0L", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 18L)

    ans <- DT %>% start_expr %>% filter("vs == 0L & am == 0L", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% start_expr %>% filter("vs == 0L", "am == 0L", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% start_expr %>% filter("vs == 0L | am == 0L", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 25L)

    ans <- DT %>% start_expr %>% filter("vs == 0L", "am == 0L", .parse = TRUE, .collapse = `|`) %>% end_expr
    expect_identical(nrow(ans), 25L)
    expect_identical(ans, DT[vs == 0L | am == 0L])

    i <- 0L

    ans <- DT %>% start_expr %>% filter("vs == i", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 18L)

    ans <- DT %>% start_expr %>% filter("vs == i & am == 0L", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% start_expr %>% filter("vs == i", "am == 0L", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% start_expr %>% filter("vs == i | am == 0L", .parse = TRUE) %>% end_expr
    expect_identical(nrow(ans), 25L)

    ans <- DT %>% start_expr %>% filter("vs == i", "am == 0L", .parse = TRUE, .collapse = `|`) %>% end_expr
    expect_identical(ans, DT[vs == i | am == 0L])
})

test_that("The filter verb works with several values per key for primary keys.", {
    expected <- state[.(c("South", "West"), c("South Atlantic", "Pacific"))]

    ans <- state %>%
        start_expr %>%
        filter(.(c("South", "West"), c("South Atlantic", "Pacific"))) %>%
        end_expr

    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- state[.(unique(region), c("South Atlantic", "Pacific"))]

    ans <- state %>%
        start_expr %>%
        filter(.(unique(region), c("South Atlantic", "Pacific"))) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Filtering when passing which = TRUE works.", {
    expected <- DT[cyl == 6 & mpg > 20, which = TRUE]
    ans <- DT %>% start_expr %>% filter(cyl == 6, mpg > 20, which = TRUE) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Nesting expressions in where/filter works.", {
    expected <- lhs[lhs[, .I[v == max(v)], by=x]$V1]

    ans <- lhs %>%
        start_expr %>%
        where(nest_expr(.start = FALSE, { .[, .I[v == max(v)], by=x]$V1 })) %>%
        end_expr

    expect_identical(ans, expected)
})

test_that("Eager version of filter works.", {
    expected <- DT[vs == 0 & am == 0]
    ans <- filter(DT, vs == 0, am == 0)
    expect_identical(ans, expected)

    expected <- DT[vs == 0 | gear == 4]
    ans <- filter(DT, vs == 0, gear == 4, .collapse = `|`)
    expect_identical(ans, expected)
})

test_that("Semi-eager version of where works.", {
    expected <- DT[vs == 0 & am == 0, .(mean(mpg), max(hp), min(disp))]
    ans <- DT %>% where(vs == 0, am == 0) %>% transmute(mean(mpg), max(hp), min(disp))
    expect_identical(ans, expected)
})
