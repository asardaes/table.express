context("  ExprBuilder")

test_that("Constructor only accepts certain data types.", {
    for (input in list(list(0L), NA, NULL, "foo", 0L)) {
        expect_error(ExprBuilder$new(input))
    }
})

test_that("Print method works.", {
    b <- ExprBuilder$new(DT)
    e <- capture.output(print(b))
    expect_identical(e, rlang::as_label(b$expr))
})

test_that("Warning is given when replacing clauses", {
    b <- ExprBuilder$new(DT)

    b$select <- "hi"
    expect_warning(b$select <- "bye")

    b$where <- "hi"
    expect_warning(b$where <- "bye")
})

test_that("The expr field is read only.", {
    b <- ExprBuilder$new(DT)
    expect_error(b$expr <- "hi")
})
