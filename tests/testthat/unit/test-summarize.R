context("  Summarize")

test_that("An empty clause is not an error.", {
    ans <- DT %>% start_expr %>% summarize() %>% end_expr
    expect_identical(ans, DT)

    ans <- DT %>% start_expr %>% summarise() %>% end_expr
    expect_identical(ans, DT)
})

test_that("Summarize works for functions optimized with GForce.", {
    first <- data.table::first
    last <- data.table::last

    expected <- DT[, .(min(mpg), max(cyl), mean(disp), median(hp), var(drat), sd(wt), sum(qsec), prod(vs), first(gear), last(carb))]
    ans <- summarise(DT, min(mpg), max(cyl), mean(disp), median(hp), var(drat), sd(wt), sum(qsec), prod(vs), first(gear), last(carb))
    expect_equal(ans, expected)

    expected <- DT[, .(min(mpg), max(cyl), mean(disp), median(hp), var(drat), sd(wt), sum(qsec), prod(vs), first(gear), last(carb)), by=am]
    ans <- DT %>% group_by(am) %>% summarize(min(mpg), max(cyl), mean(disp), median(hp), var(drat), sd(wt), sum(qsec), prod(vs), first(gear), last(carb))
    expect_equal(ans, expected)
})

test_that("Summarize works for functions *not* optimized with GForce.", {
    expected <- DT[, .(ans = Reduce("+", mpg))]
    ans <- summarise(DT, ans = Reduce("+", mpg))
    expect_equal(ans, expected)

    expected <- DT[, .(ans = Reduce("+", mpg)), by=am]
    ans <- DT %>% group_by(am) %>% summarize(ans = Reduce("+", mpg))
    expect_equal(ans, expected)

    expect_error(summarize(DT, mpg = cumsum(mpg)), "length 1")
})

test_that("Expressions in summarize can use variables from previous expressions and play nicely with group_by.", {
    expected <- DT[, .(ans = Reduce("+", mpg)), by = .(gear)][, foo := ans / 10]

    ans <- DT %>%
        group_by(gear) %>%
        summarize(ans = Reduce("+", mpg), foo = ans / 10)

    expect_equal(ans, expected)
})
