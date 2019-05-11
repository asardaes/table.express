context("  End expression")

test_that("The ExprBuilder's end_expr method works as expected.", {
    eb <- ExprBuilder$new(DT)
    expect_identical(end_expr(eb), DT)
    expect_identical(end_expr(eb, .parent_env = environment()), DT)
})
