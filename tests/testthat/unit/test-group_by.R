context("  Group by")

test_that("The group_by verb works as expected.", {
    expected <- DT[, .(ans = mean(mpg)), by = vs]

    ans <- DT %>% start_expr %>% select(ans = mean(mpg)) %>% group_by(vs) %>% end_expr
    expect_identical(ans, expected)

    expected <- DT[, .(mean = mean(mpg), sd = sd(mpg)), by = .(vs, am)]

    ans <- DT %>%
        start_expr %>%
        select(mean = mean(mpg), sd = sd(mpg)) %>%
        group_by(vs, am) %>%
        end_expr

    expect_identical(ans, expected)

    ans <- DT %>%
        start_expr %>%
        select(mean = mean(mpg), sd = sd(mpg)) %>%
        group_by("vs", "am", .parse = TRUE) %>%
        end_expr

    expect_identical(ans, expected)

    desired_group <- "vs"

    ans <- DT %>%
        start_expr %>%
        select(mean = mean(mpg), sd = sd(mpg)) %>%
        group_by(!!desired_group, "am", .parse = TRUE) %>%
        end_expr

    expect_identical(ans, expected)
})
