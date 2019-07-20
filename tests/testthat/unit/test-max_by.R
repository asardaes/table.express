context("  Max by")

test_that("max_by for single .col works.", {
    expected <- DT[DT[, .I[mpg == max(mpg)]]]
    ans <- DT %>% start_expr %>% max_by("mpg") %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[DT[, .I[mpg == max(mpg)], by = "vs"]$V1]
    ans <- DT %>% start_expr %>% max_by("mpg", vs) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[DT[, .I[mpg == max(mpg)], by = c("vs", "am")]$V1]
    ans <- DT %>% start_expr %>% max_by("mpg", vs, "am") %>% end_expr
    expect_identical(ans, expected)
})

test_that("max_by for multiple .col works.", {
    expected <- DT[DT[, .I[vs == max(vs) & am == max(am)]]]
    ans <- DT %>% start_expr %>% max_by(c("vs", "am")) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[DT[, .I[vs == max(vs) & am == max(am)], by = "cyl"]$V1]
    ans <- DT %>% start_expr %>% max_by(c("vs", "am"), cyl) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[DT[, .I[vs == max(vs) & am == max(am)], by = c("cyl", "carb")]$V1]
    ans <- DT %>% start_expr %>% max_by(c("vs", "am"), cyl, "carb") %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[DT[, .I[gear == max(gear) | carb == max(carb)], by = "cyl"]$V1]
    ans <- DT %>% start_expr %>% max_by(c("gear", "carb"), cyl, .some = TRUE) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Eager max_by works.", {
    expected <- DT[DT[, .I[mpg == max(mpg)], by = "vs"]$V1]
    ans <- DT %>% max_by("mpg", vs)
    expect_identical(ans, expected)

    expected <- data.table::copy(DT)[DT[, .I[mpg == max(mpg)], by = "vs"]$V1, flag := TRUE]
    ans <- data.table::copy(DT) %>% max_by("mpg", vs, .expr = TRUE) %>% mutate(flag = TRUE)
    expect_identical(ans, expected)
})
