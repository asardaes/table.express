context("  ExprBuilder")

test_that("Constructor only accepts certain data types.", {
    for (input in list(list(0L), NA, NULL, "foo", 0L)) {
        expect_error(ExprBuilder$new(input))
    }
})

test_that("Methods work.", {
    b <- ExprBuilder$new(DT)
    e <- capture.output(print(b))
    expect_identical(e, rlang::as_label(b$expr))
})
