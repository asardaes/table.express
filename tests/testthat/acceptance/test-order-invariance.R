context("  Clause order invariance")

test_that("Order of clauses is not important for a single 'query'.", {
    expected <- DT[vs == 1L | am == 1L, .(mpg = mean(mpg), disp = sd(disp)), by = gear]

    clauses <- list(
        rlang::expr(filter(vs == 1L, am == 1L, .collapse = `|`)),
        rlang::expr(select(mpg = mean(mpg), disp = sd(disp))),
        rlang::expr(group_by(gear))
    )

    for (ignored in 1L:50L) {
        clauses_shuffled <- sample(clauses)
        expr_clauses <- c(list(rlang::expr(start_expr)), clauses_shuffled, list(rlang::expr(end_expr)))
        e <- table.express:::reduce_expr(expr_clauses, rlang::expr(DT), rlang::expr(`%>%`))
        ans <- rlang::eval_tidy(e)
        expect_equal(ans, expected)
    }
})
