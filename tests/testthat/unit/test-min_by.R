context("  Min by")

test_that("min_by for single .col works.", {
    expected <- DT[DT[, .I[mpg == min(mpg)]]]
    ans <- DT %>% start_expr %>% min_by("mpg") %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[DT[, .I[mpg == min(mpg)], by = "vs"]$V1]
    ans <- DT %>% start_expr %>% min_by("mpg", vs) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[DT[, .I[mpg == min(mpg)], by = c("vs", "am")]$V1]
    ans <- DT %>% start_expr %>% min_by("mpg", vs, "am") %>% end_expr
    expect_identical(ans, expected)
})

test_that("min_by for multiple .col works.", {
    expected <- DT[DT[, .I[vs == min(vs) & am == min(am)]]]
    ans <- DT %>% start_expr %>% min_by(c("vs", "am")) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[DT[, .I[vs == min(vs) & am == min(am)], by = "cyl"]$V1]
    ans <- DT %>% start_expr %>% min_by(c("vs", "am"), cyl) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[DT[, .I[vs == min(vs) & am == min(am)], by = c("cyl", "carb")]$V1]
    ans <- DT %>% start_expr %>% min_by(c("vs", "am"), cyl, "carb") %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[DT[, .I[gear == min(gear) | carb == min(carb)], by = "cyl"]$V1]
    ans <- DT %>% start_expr %>% min_by(c("gear", "carb"), cyl, .some = TRUE) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Eager min_by works.", {
    expected <- DT[DT[, .I[mpg == min(mpg)], by = "vs"]$V1]
    ans <- DT %>% min_by("mpg", vs)
    expect_identical(ans, expected)

    expected <- data.table::copy(DT)[DT[, .I[mpg == min(mpg)], by = "vs"]$V1, flag := TRUE]
    ans <- data.table::copy(DT) %>% min_by("mpg", vs, .expr = TRUE) %>% mutate(flag = TRUE)
    expect_identical(ans, expected)
})
