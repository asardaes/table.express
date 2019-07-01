context("  Distinct")

test_that("Distinct works.", {
    expected <- DT[, .SD[1L], by = .(vs, am)]

    ans <- DT %>% start_expr %>% distinct(vs, am) %>% end_expr
    expect_identical(ans, expected)
})
