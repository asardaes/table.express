context("  Clause order invariance")

test_that("Order of clauses is not important for a single 'query'.", {
    expected <- DT[vs == 0L & am == 0L, mpg:disp]

    clauses <- list(
        rlang::expr(filter(vs == 0L, am == 0L)),
        rlang::expr(select(mpg:disp))
    )

    for (ignored in 1L:50L) {
        clauses_shuffled <- sample(clauses)
        expr_clauses <- c(list(rlang::expr(start_expr)), clauses_shuffled, list(rlang::expr(end_expr)))
        e <- table.express:::squash_expr(expr_clauses, rlang::expr(DT), rlang::expr(`%>%`))
        ans <- rlang::eval_tidy(e)
        expect_identical(ans, expected)
    }
})
