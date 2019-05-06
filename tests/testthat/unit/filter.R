context("  Filter (a.k.a Where)")

test_that("An empty clause is not an error.", {
    ans <- DT %>% where() %>% eval_expr
    expect_identical(ans, DT)

    ans <- DT %>% filter() %>% eval_expr
    expect_identical(ans, DT)
})

test_that("The where verb works with simple numeric indices.", {
    ans <- DT %>% where(1L) %>% eval_expr
    expect_identical(nrow(ans), 1L)

    ans <- DT %>% where(1:2) %>% eval_expr
    expect_identical(nrow(ans), 2L)

    ans <- DT %>% where(c(1L, 3L)) %>% eval_expr
    expect_identical(nrow(ans), 2L)

    ans <- DT %>% where(-c(1L, 3L)) %>% eval_expr
    expect_identical(nrow(ans), nrow(DT) - 2L)
    expect_identical(ans, DT[-c(1L, 3L)])

    i <- 1:2
    ans <- DT %>% where(i) %>% eval_expr
    expect_identical(nrow(ans), 2L)
    expect_identical(ans, DT[1:2])
})

test_that("The where verb works when parsing simple numeric indices.", {
    ans <- DT %>% where("1L", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 1L)

    ans <- DT %>% where("1:2", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 2L)

    ans <- DT %>% where("c(1L, 3L)", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 2L)

    ans <- DT %>% where("-c(1L, 3L)", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), nrow(DT) - 2L)
})

test_that("Computing expressions in filter works.", {
    ans <- DT %>% filter(vs == 0L) %>% eval_expr
    expect_identical(nrow(ans), 18L)

    ans <- DT %>% filter(vs == 0L & am == 0L) %>% eval_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% filter(vs == 0L, am == 0L) %>% eval_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% filter(vs == 0L | am == 0L) %>% eval_expr
    expect_identical(nrow(ans), 25L)

    ans <- DT %>% filter(vs == 0L, am == 0L, .collapse = `|`) %>% eval_expr
    expect_identical(nrow(ans), 25L)
    expect_identical(ans, DT[vs == 0L | am == 0L])

    i <- 0L

    ans <- DT %>% filter(vs == i) %>% eval_expr
    expect_identical(nrow(ans), 18L)

    ans <- DT %>% filter(vs == i & am == 0L) %>% eval_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% filter(vs == i, am == 0L) %>% eval_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% filter(vs == i | am == 0L) %>% eval_expr
    expect_identical(nrow(ans), 25L)

    ans <- DT %>% filter(vs == i, am == 0L, .collapse = `|`) %>% eval_expr
    expect_identical(nrow(ans), 25L)
    expect_identical(ans, DT[vs == i | am == 0L])
})

test_that("Parsing computing expressions in filter works.", {
    ans <- DT %>% filter("vs == 0L", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 18L)

    ans <- DT %>% filter("vs == 0L & am == 0L", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% filter("vs == 0L", "am == 0L", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% filter("vs == 0L | am == 0L", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 25L)

    ans <- DT %>% filter("vs == 0L", "am == 0L", .parse = TRUE, .collapse = `|`) %>% eval_expr
    expect_identical(nrow(ans), 25L)
    expect_identical(ans, DT[vs == 0L | am == 0L])

    i <- 0L

    ans <- DT %>% filter("vs == i", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 18L)

    ans <- DT %>% filter("vs == i & am == 0L", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% filter("vs == i", "am == 0L", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 12L)

    ans <- DT %>% filter("vs == i | am == 0L", .parse = TRUE) %>% eval_expr
    expect_identical(nrow(ans), 25L)

    ans <- DT %>% filter("vs == i", "am == 0L", .parse = TRUE, .collapse = `|`) %>% eval_expr
    expect_identical(nrow(ans), 25L)
    expect_identical(ans, DT[vs == i | am == 0L])
})
