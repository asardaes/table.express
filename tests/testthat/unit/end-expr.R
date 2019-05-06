context("  End expression")

test_that("The ExprBuilder's end_expr method works as expected.", {
    expect_error(table.express:::end_expr.ExprBuilder(NULL))
    expect_true(is.function(table.express:::end_expr.ExprBuilder(.parent_env = NULL)))

    eb <- ExprBuilder$new(DT)
    expect_identical(end_expr(eb), DT)
    expect_identical(end_expr(eb, .parent_env = environment()), DT)
})
