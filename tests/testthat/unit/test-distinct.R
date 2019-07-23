context("  Distinct")

test_that("Basic distinct keeping all columns works.", {
    expected <- DT[, .SD[1L], by = .(vs, am)]
    ans <- DT %>% start_expr %>% distinct(vs, am) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Distinct returning more than one row rowks.", {
    expected <- DT[, .SD[1:2], by = .(vs, am)]
    ans <- DT %>% start_expr %>% distinct(vs, am, .n = 1:2) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[, .SD[.N], by = .(vs, am)]
    ans <- DT %>% distinct(vs, am, .n = .N)
    expect_identical(ans, expected)
})

test_that("Distinct keeping only used columns works.", {
    expected <- DT[, unique(.SD), .SDcols = c("vs", "am")]
    ans <- DT %>% start_expr %>% distinct(vs, am, .keep = FALSE) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Distinct creating columns works.", {
    expected <- DT[, .SD[1L], by = .(amvs = vs + am), .SDcols = names(DT)]
    ans <- DT %>% start_expr %>% distinct(amvs = vs + am, .keep = names(DT)) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Eager version of distinct works.", {
    expected <- DT[, .SD[1L], by = .(vs, am)]
    ans <- DT %>% distinct(vs, am)
    expect_identical(ans, expected)
})
