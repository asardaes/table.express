context("  Utils")

test_that("to_expr returns a classed error when appropriate.", {
    obj <- list(1L, "a")
    expect_error(to_expr(obj))

    ans <- tryCatch(to_expr(obj), table.express.invalid_argument_class_error = function(err) { err$obj })
    expect_identical(ans, obj)
})
