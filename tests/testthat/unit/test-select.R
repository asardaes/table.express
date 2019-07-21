context("  Select")

test_that("An empty clause is not an error.", {
    ans <- DT %>% start_expr %>% select() %>% end_expr
    expect_identical(ans, DT)
})

test_that("The select verb works with single columns.", {
    expectations <- function(ans) {
        expect_identical(ncol(ans), 1L)
        expect_identical(ans$mpg, mtcars$mpg)
    }

    ans <- DT %>% start_expr %>% select(mpg) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select(1L) %>% end_expr
    expectations(ans)

    v <- rlang::sym("mpg")
    ans <- DT %>% start_expr %>% select(!!v) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select("mpg", .parse = TRUE) %>% end_expr
    expectations(ans)

    v <- "mpg"
    ans <- DT %>% start_expr %>% select(!!v, .parse = TRUE) %>% end_expr
    expectations(ans)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[, .(mpg = 1:32)]
    ans <- DT %>% start_expr %>% select(mpg = 1:32) %>% end_expr
    expect_identical(ans, expected)
})

test_that("The select verb works with multiple columns.", {
    expectations <- function(ans) {
        expect_identical(ncol(ans), 2L)
        expect_identical(ans$mpg, mtcars$mpg)
        expect_identical(ans$cyl, mtcars$cyl)
    }

    ans <- DT %>% start_expr %>% select(mpg, cyl) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select(mpg:cyl) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select(1:2) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select(1L:2L) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select(1L, 2L) %>% end_expr
    expectations(ans)

    ans <- DT %>% start_expr %>% select("mpg", "cyl", .parse = TRUE) %>% end_expr
    expectations(ans)

    v <- c("mpg", "cyl")
    ans <- DT %>% start_expr %>% select(!!!v, .parse = TRUE) %>% end_expr
    expectations(ans)

    v <- rlang::syms(c("mpg", "cyl"))
    ans <- DT %>% start_expr %>% select(!!!v) %>% end_expr
    expectations(ans)
})

test_that("Selection's semantics can be negated.", {
    expected <- DT[, !c("mpg", "am")]

    ans <- DT %>% start_expr %>% select(mpg, "am", .negate = TRUE) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% select(1L, 9L, .negate = TRUE) %>% end_expr
    expect_identical(ans, expected)

    # ----------------------------------------------------------------------------------------------

    expected <- DT[, !c("mpg", "cyl", "disp", "am", "drat", "qsec")]
    ans <- DT %>% start_expr %>% select(mpg:disp, contains("m"), 5L, qsec, .negate = TRUE) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Select with tidyselect works.", {
    expectations <- function(ans, nc) {
        expect_identical(nrow(ans), nrow(DT))
        expect_identical(ncol(ans), nc)
    }

    ans <- DT %>% start_expr %>% select(starts_with("M", T)) %>% end_expr
    expectations(ans, 1L)

    ans <- DT %>% start_expr %>% select(ends_with("M", T)) %>% end_expr
    expectations(ans, 1L)

    ans <- DT %>% start_expr %>% select(contains("M", T)) %>% end_expr
    expectations(ans, 2L)

    ans <- DT %>% start_expr %>% select(matches(".*M.*", T)) %>% end_expr
    expectations(ans, 2L)

    ans <- DT %>% start_expr %>% select(one_of("mpg", "am")) %>% end_expr
    expectations(ans, 2L)

    ans <- DT %>% start_expr %>% select(last_col()) %>% end_expr
    expectations(ans, 1L)

    ans <- DT %>% start_expr %>% select(everything()) %>% end_expr
    expectations(ans, 11L)

    colnames(DT) <- paste0("var", 0:10)
    ans <- DT %>% start_expr %>% select(num_range("var", 0:1)) %>% end_expr
    expectations(ans, 2L)
})

test_that("Combining tidyselect::everything with other expressions works like in dplyr.", {
    expected <- data.table::setcolorder(data.table::copy(DT), "carb")

    ans <- DT %>% start_expr %>% select(carb, everything()) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% select(carb, everything(), everything()) %>% end_expr
    expect_identical(ans, expected)

    ans <- DT %>% start_expr %>% select(carb, mpg:gear, everything()) %>% end_expr
    expect_identical(ans, expected)
})

test_that("Eager versions of select work.", {
    expected <- DT %>% start_expr %>% select(mpg:disp, 5:6, last_col()) %>% end_expr
    ans <- DT %>% select(mpg:disp, 5:6, last_col())
    expect_identical(ans, expected)

    expected <- DT %>% start_expr %>% group_by(vs, am) %>% select(.SD[1:2]) %>% end_expr
    ans <- DT %>% group_by(vs, am) %>% select(.SD[1:2])
    expect_identical(ans, expected)
})
