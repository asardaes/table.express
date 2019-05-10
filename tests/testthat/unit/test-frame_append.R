context("  Frame append")

test_that("frame_append works.", {
    ans <- DT %>% start_expr %>% frame_append(a = 1)
    expect_identical(1L, length(ans$appends))

    ans <- DT %>% start_expr %>% frame_append(a = 1, b = c("a", "b"))
    expect_identical(2L, length(ans$appends))

    ans <- DT %>% start_expr %>% frame_append(foo = bar)
    expect_identical(1L, length(ans$appends))

    bar <- 1:2
    bak <- c("a", "b")
    ans <- DT %>% start_expr %>% frame_append(foo = !!bar, baz = !!bak)
    expect_identical(2L, length(ans$appends))

    foo <- "foo"
    baz <- "baz"
    ans <- DT %>% start_expr %>% frame_append(!!foo := !!bar, !!baz := !!bak)
    expect_identical(2L, length(ans$appends))
})
