context("  End expression")

test_that("end_expr works as expected.", {
    expect_error(end_expr(NULL))
    expect_true(is.function(end_expr(.parent_env = NULL)))

    eb <- ExprBuilder$new(DT)
    expect_identical(end_expr(eb), DT)
    expect_identical(end_expr(eb, environment()), DT)
})
