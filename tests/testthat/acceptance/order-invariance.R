context("  Clause urder invariance")

test_that("Order of clauses is not important for a single 'query'.", {
    expected <- DT[vs == 0L & am == 0L, mpg:disp]

    clauses <- list(
        rlang::expr(filter(vs == 0L, am == 0L)),
        rlang::expr(select(mpg:disp))
    )

    for (ignored in 1L:50L) {
        clauses_shuffled <- sample(clauses)
        e <- table.express:::squash_expr(clauses_shuffled, rlang::expr(DT), rlang::expr(`%>%`))
        ans <- rlang::eval_tidy(e) %>% eval_expr
        expect_identical(ans, expected)
    }
})
