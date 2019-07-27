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

    expr <- rlang::expr(foo(bar))
    b$set_j(expr, FALSE)
    expect_warning(b$set_j(expr, FALSE))

    b$set_i(expr, FALSE)
    expect_warning(b$set_i(expr, FALSE))

    b$set_by(expr, FALSE)
    expect_warning(b$set_by(expr, FALSE))
})

test_that("The expr field is read only.", {
    b <- ExprBuilder$new(DT)
    expect_error(b$expr <- "hi")
})

test_that("Overriding values with eval's ellipsis works.", {
    b <- DT %>% start_expr %>% select(1L)
    ans <- b$eval(rlang::current_env(), TRUE, .DT_ = DT[, -1L])
    expect_identical(ans, DT[, .(cyl)])
})

test_that("chain_if_set works.", {
    b1 <- DT %>% start_expr
    b2 <- b1$chain_if_set(".j")
    expect_identical(b1, b2)

    select(b1, 1L)
    b2 <- b1$chain_if_set(".j")
    expect_false(identical(b1, b2))
})

test_that("Chained pronouns are also passed after frame chaining.", {
    eb <- lhs %>%
        start_expr %>%
        left_join(rhs, x, v) %>%
        mutate(y = y) %>%
        mutate(z = 0L)

    expect_identical(eb$.__enclos_env__$private$.dt_pronouns[[1L]], lhs)
})

test_that("Nested expressions are correctly evaluated.", {
    eb <- ExprBuilder$new(DT)
    foo <- function(...) {
        eb$seek_and_nestroy(rlang::exprs(nest_expr(!!!rlang::enexprs(...))))
    }

    # ----------------------------------------------------------------------------------------------

    expected <- DT %>% start_expr %>% transmute(max(mpg)) %>% end_expr
    ans <- foo(ignored = transmute(max(mpg)))
    expect_identical(ans[[1L]], rlang::expr(.NEST_0_))
    expect_identical(eb$.__enclos_env__$private$.nested[[1L]], expected)

    ans <- foo("transmute(max(mpg))", .parse = TRUE)
    expect_identical(ans[[1L]], rlang::expr(.NEST_1_))
    expect_identical(eb$.__enclos_env__$private$.nested[[2L]], expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT %>% start_expr %>% transmute(max(mpg)) %>% end_expr %>% round
    ans <- foo(.end = FALSE, transmute(max(mpg)), end_expr, round)
    expect_identical(ans[[1L]], rlang::expr(.NEST_2_))
    expect_identical(eb$.__enclos_env__$private$.nested[[3L]], expected)

    # ----------------------------------------------------------------------------------------------

    ans <- foo(.start = FALSE, { round(.[, .(max(mpg))]) })
    expect_identical(ans[[1L]], rlang::expr(.NEST_3_))
    expect_identical(eb$.__enclos_env__$private$.nested[[4L]], expected)
})

test_that("Nested expressions are correctly chained.", {
    eb <- ExprBuilder$new(DT)
    expected <- DT %>% start_expr %>% transmute(max(mpg)) %>% end_expr

    foo <- function(...) {
        eb$seek_and_nestroy(rlang::exprs(nest_expr(!!!rlang::enexprs(...))))
    }

    foo(ignored = transmute(max(mpg)))

    eb <- eb$chain()
    expect_identical(eb$.__enclos_env__$private$.nested[[1L]], expected)
})

test_that("Eager versions of ExprBuilder cannot be chained.", {
    expect_error(EagerExprBuilder$new(data.table::data.table())$chain(), "cannot be chained")
    expect_error(EagerExprBuilder$new(data.table::data.table())$chain_if_set(), "cannot be chained")
})
